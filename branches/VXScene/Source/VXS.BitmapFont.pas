//
// This unit is part of the VXScene Project, http://glscene.org
//
{ 
  Bitmap Fonts management classes
}
unit VXS.BitmapFont;

{$I VXScene.inc}

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  System.Types,
  System.UITypes,
  FMX.Graphics,
  FMX.Types,

  VXS.PersistentClasses,
  VXS.XOpenGL,
  VXS.Scene,
  VXS.VectorGeometry,
  VXS.Context,
  VXS.CrossPlatform,
  VXS.Texture,
  VXS.State,
  VXS.Utils,
  VXS.Graphics,
  VXS.Color,
  VXS.BaseClasses,
  VXS.RenderContextInfo,
  VXS.TextureFormat,
  VXS.VectorTypes;

type

  { An individual character range in a bitmap font.
    A range allows mapping ASCII characters to character tiles in a font
    bitmap, tiles are enumerated line then column (raster). }
  TVXBitmapFontRange = class(TCollectionItem)
  private
    function GetStartASCII: WideString;
    function GetStopASCII: WideString;
  protected
    FStartASCII, FStopASCII: WideChar;
    FStartGlyphIdx, FStopGlyphIdx, FCharCount: Integer;
    procedure SetStartASCII(const val: WideString);
    procedure SetStopASCII(const val: WideString);
    procedure SetStartGlyphIdx(val: Integer);
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange;
  published
    property StartASCII: WideString read GetStartASCII write SetStartASCII;
    property StopASCII: WideString read GetStopASCII write SetStopASCII;
    property StartGlyphIdx: Integer read FStartGlyphIdx write SetStartGlyphIdx;
    property StopGlyphIdx: Integer read FStopGlyphIdx;
    property CharCount: Integer read FCharCount;
  end;

  TVXBitmapFontRanges = class(TCollection)
  private
    FCharCount: Integer;
  protected
    FOwner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TVXBitmapFontRange);
    function GetItems(index: Integer): TVXBitmapFontRange;
    function CalcCharacterCount: Integer;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function Add: TVXBitmapFontRange; overload;
    function Add(const StartASCII, StopASCII: WideChar)
      : TVXBitmapFontRange; overload;
    function Add(const StartASCII, StopASCII: AnsiChar)
      : TVXBitmapFontRange; overload;
    function FindItemID(ID: Integer): TVXBitmapFontRange;
    property Items[index: Integer]: TVXBitmapFontRange read GetItems
      write SetItems; default;
    { Converts an ASCII character into a tile index.
      Return -1 if character cannot be rendered. }
    function CharacterToTileIndex(aChar: WideChar): Integer;
    function TileIndexToChar(aIndex: Integer): WideChar;
    procedure NotifyChange;
    { Total number of characters in the ranges; cached for performance }
    property CharacterCount: Integer read FCharCount;
  end;

  PCharInfo = ^TCharInfo;
  TCharInfo = record
    l, t, w: word;
  end;

  { Provides access to individual characters in a BitmapFont.
    Only fixed-width bitmap fonts are supported, the characters are enumerated
    in a raster fashion (line then column).
    Transparency is all or nothing, the transparent color being that of the
    top left pixel of the Glyphs bitmap.
    Performance note: as usual, for best performance, you base font bitmap
    dimensions should be close to a power of two, and have at least 1 pixel
    spacing between characters (horizontally and vertically) to avoid artefacts
    when rendering with linear filtering. }
  TVXCustomBitmapFont = class(TVXUpdateAbleComponent)
  private
    FRanges: TVXBitmapFontRanges;
    FGlyphs: TVXPicture;
    FCharWidth, FCharHeight: Integer;
    FGlyphsIntervalX, FGlyphsIntervalY: Integer;
    FHSpace, FVSpace, FHSpaceFix: Integer;
    FUsers: TList;
    FMinFilter: TVXMinFilter;
    FMagFilter: TVXMagFilter;
    FTextureWidth, FTextureHeight: Integer;
    FTextRows, FTextCols: Integer;
    FGlyphsAlpha: TVXTextureImageAlpha;
    FTextures: TList;
    FTextureModified: boolean;
    FLastTexture: TVXTextureHandle;
  protected
    FChars: array of TCharInfo;
    FCharsLoaded: boolean;
    procedure ResetCharWidths(w: Integer = -1);
    procedure SetCharWidths(index, value: Integer);
    procedure SetRanges(const val: TVXBitmapFontRanges);
    procedure SetGlyphs(const val: TVXPicture);
    procedure SetCharWidth(const val: Integer);
    procedure SetCharHeight(const val: Integer);
    procedure SetGlyphsIntervalX(const val: Integer);
    procedure SetGlyphsIntervalY(const val: Integer);
    procedure OnGlyphsChanged(Sender: TObject);
    procedure SetHSpace(const val: Integer);
    procedure SetVSpace(const val: Integer);
    procedure SetMagFilter(AValue: TVXMagFilter);
    procedure SetMinFilter(AValue: TVXMinFilter);
    procedure SetGlyphsAlpha(val: TVXTextureImageAlpha);
    procedure TextureChanged;
    procedure FreeTextureHandle; virtual;
    function TextureFormat: Integer; virtual;
    procedure InvalidateUsers;
    function CharactersPerRow: Integer;
    procedure GetCharTexCoords(Ch: WideChar;
      var TopLeft, BottomRight: TTexPoint);
    procedure GetICharTexCoords(var ARci: TVXRenderContextInfo; Chi: Integer;
      out TopLeft, BottomRight: TTexPoint);
    procedure PrepareImage(var ARci: TVXRenderContextInfo); virtual;
    procedure PrepareParams(var ARci: TVXRenderContextInfo);
    { A single bitmap containing all the characters.
      The transparent color is that of the top left pixel. }
    property Glyphs: TVXPicture read FGlyphs write SetGlyphs;
    {  Nb of horizontal pixels between two columns in the Glyphs. }
    property GlyphsIntervalX: Integer read FGlyphsIntervalX
      write SetGlyphsIntervalX;
    { Nb of vertical pixels between two rows in the Glyphs. }
    property GlyphsIntervalY: Integer read FGlyphsIntervalY
      write SetGlyphsIntervalY;
    { Ranges allow converting between ASCII and tile indexes.
      See TVXCustomBitmapFontRange. }
    property Ranges: TVXBitmapFontRanges read FRanges write SetRanges;
    { Width of a single character. }
    property CharWidth: Integer read FCharWidth write SetCharWidth default 16;
    { Pixels in between rendered characters (horizontally). }
    property HSpace: Integer read FHSpace write SetHSpace default 1;
    { Pixels in between rendered lines (vertically). }
    property VSpace: Integer read FVSpace write SetVSpace default 1;
    { Horizontal spacing fix offset.
      This property is for internal use, and is added to the hspacing
      of each character when rendering, typically to fix extra spacing. }
    property HSpaceFix: Integer read FHSpaceFix write FHSpaceFix;
    property MagFilter: TVXMagFilter read FMagFilter write SetMagFilter
      default maLinear;
    property MinFilter: TVXMinFilter read FMinFilter write SetMinFilter
      default miLinear;
    property GlyphsAlpha: TVXTextureImageAlpha read FGlyphsAlpha
      write FGlyphsAlpha default tiaDefault;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterUser(anObject: TVXBaseSceneObject); virtual;
    procedure UnRegisterUser(anObject: TVXBaseSceneObject); virtual;
    { Renders the given string at current position or at position given by the optional position variable.
      The current matrix is blindly used, meaning you can render all kinds
      of rotated and linear distorted text with this method, OpenGL
      Enable states are also possibly altered. }
    procedure RenderString(var ARci: TVXRenderContextInfo;
      const aText: UnicodeString; aAlignment: TAlignment;
      aLayout: TVXTextLayout; const aColor: TColorVector;
      aPosition: PVector = nil; aReverseY: boolean = False); overload; virtual;
    { A simpler canvas-style TextOut helper for RenderString.
      The rendering is reversed along Y by default, to allow direct use
      with TVXCanvas }
    procedure TextOut(var rci: TVXRenderContextInfo; X, Y: Single;
      const Text: UnicodeString; const Color: TColorVector); overload;
    procedure TextOut(var rci: TVXRenderContextInfo; X, Y: Single;
      const Text: UnicodeString; const Color: TColor); overload;
    function TextWidth(const Text: UnicodeString): Integer;
    function CharacterToTileIndex(aChar: WideChar): Integer; virtual;
    function TileIndexToChar(aIndex: Integer): WideChar; virtual;
    function CharacterCount: Integer; virtual;
    { Get the actual width for this char. }
    function GetCharWidth(Ch: WideChar): Integer;
    { Get the actual pixel width for this string. }
    function CalcStringWidth(const aText: UnicodeString): Integer;
      overload; virtual;
    // make texture if needed
    procedure CheckTexture(var ARci: TVXRenderContextInfo);
    { Height of a single character. }
    property CharHeight: Integer read FCharHeight write SetCharHeight
      default 16;
    property TextureWidth: Integer read FTextureWidth write FTextureWidth;
    property TextureHeight: Integer read FTextureHeight write FTextureHeight;
  end;

  { See TVXCustomBitmapFont.
    This class only publuishes some of the properties. }
  TVXBitmapFont = class(TVXCustomBitmapFont)
  published
    property Glyphs;
    property GlyphsIntervalX;
    property GlyphsIntervalY;
    property Ranges;
    property CharWidth;
    property CharHeight;
    property HSpace;
    property VSpace;
    property MagFilter;
    property MinFilter;
    property GlyphsAlpha;
  end;

  TVXFlatTextOption = (ftoTwoSided);
  TVXFlatTextOptions = set of TVXFlatTextOption;

  { A 2D text displayed and positionned in 3D coordinates.
    The FlatText uses a character font defined and stored by a TVXBitmapFont
    component. Default character scale is 1 font pixel = 1 space unit. }
  TVXFlatText = class(TVXImmaterialSceneObject)
  private
    FBitmapFont: TVXCustomBitmapFont;
    FText: UnicodeString;
    FAlignment: TAlignment;
    FLayout: TVXTextLayout;
    FModulateColor: TVXColor;
    FOptions: TVXFlatTextOptions;
  protected
    procedure SetBitmapFont(const val: TVXCustomBitmapFont);
    procedure SetText(const val: UnicodeString);
    procedure SetAlignment(const val: TAlignment);
    procedure SetLayout(const val: TVXTextLayout);
    procedure SetModulateColor(const val: TVXColor);
    procedure SetOptions(const val: TVXFlatTextOptions);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var rci: TVXRenderContextInfo;
      renderSelf, renderChildren: boolean); override;
    procedure Assign(Source: TPersistent); override;
  published
    { Refers the bitmap font to use.
      The referred bitmap font component stores and allows access to
      individual character bitmaps. }
    property BitmapFont: TVXCustomBitmapFont read FBitmapFont
      write SetBitmapFont;
    { Text to render.
      Be aware that only the characters available in the bitmap font will
      be rendered. CR LF sequences are allowed. }
    property Text: UnicodeString read FText write SetText;
    { Controls the text alignment (horizontal).
      Possible values : taLeftJustify, taRightJustify, taCenter }
    property Alignment: TAlignment read FAlignment write SetAlignment;
    { Controls the text layout (vertical).
      Possible values : tlTop, tlCenter, tlBottom }
    property Layout: TVXTextLayout read FLayout write SetLayout;
    { Color modulation, can be used for fade in/out too. }
    property ModulateColor: TVXColor read FModulateColor write SetModulateColor;
    { Flat text options.
      ftoTwoSided : when set the text will be visible from its two
      sides even if faceculling is on (at the scene-level).  }
    property Options: TVXFlatTextOptions read FOptions write SetOptions;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TVXBitmapFontRange ------------------
// ------------------

constructor TVXBitmapFontRange.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TVXBitmapFontRange.Destroy;
begin
  inherited;
end;

procedure TVXBitmapFontRange.Assign(Source: TPersistent);
begin
  if Source is TVXBitmapFontRange then
  begin
    FStartASCII := TVXBitmapFontRange(Source).FStartASCII;
    FStopASCII := TVXBitmapFontRange(Source).FStopASCII;
    FStartGlyphIdx := TVXBitmapFontRange(Source).FStartGlyphIdx;
    NotifyChange;
  end
  else
    inherited;
end;

procedure TVXBitmapFontRange.NotifyChange;
begin
  FCharCount := Integer(FStopASCII) - Integer(FStartASCII) + 1;
  FStopGlyphIdx := FStartGlyphIdx + FCharCount - 1;
  if Assigned(Collection) then
    (Collection as TVXBitmapFontRanges).NotifyChange;
end;

function TVXBitmapFontRange.GetDisplayName: string;
begin
  Result := Format('ASCII [#%d, #%d] -> Glyphs [%d, %d]',
    [Integer(FStartASCII), Integer(FStopASCII), StartGlyphIdx, StopGlyphIdx]);
end;

function TVXBitmapFontRange.GetStartASCII: WideString;
begin
  Result := FStartASCII;
end;

function TVXBitmapFontRange.GetStopASCII: WideString;
begin
  Result := FStopASCII;
end;

procedure TVXBitmapFontRange.SetStartASCII(const val: WideString);
begin
  if (Length(val) > 0) and (val[1] <> FStartASCII) then
  begin
    FStartASCII := val[1];
    if FStartASCII > FStopASCII then
      FStopASCII := FStartASCII;
    NotifyChange;
  end;
end;

procedure TVXBitmapFontRange.SetStopASCII(const val: WideString);
begin
  if (Length(val) > 0) and (FStopASCII <> val[1]) then
  begin
    FStopASCII := val[1];
    if FStopASCII < FStartASCII then
      FStartASCII := FStopASCII;
    NotifyChange;
  end;
end;

procedure TVXBitmapFontRange.SetStartGlyphIdx(val: Integer);
begin
  val := MaxInteger(0, val);
  if val <> FStartGlyphIdx then
  begin
    FStartGlyphIdx := val;
    NotifyChange;
  end;
end;

// ------------------
// ------------------ TVXBitmapFontRanges ------------------
// ------------------

constructor TVXBitmapFontRanges.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  inherited Create(TVXBitmapFontRange);
end;

destructor TVXBitmapFontRanges.Destroy;
begin
  inherited;
end;

function TVXBitmapFontRanges.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TVXBitmapFontRanges.SetItems(index: Integer;
  const val: TVXBitmapFontRange);
begin
  inherited Items[index] := val;
end;

function TVXBitmapFontRanges.GetItems(index: Integer): TVXBitmapFontRange;
begin
  Result := TVXBitmapFontRange(inherited Items[index]);
end;

function TVXBitmapFontRanges.Add: TVXBitmapFontRange;
begin
  Result := (inherited Add) as TVXBitmapFontRange;
end;

function TVXBitmapFontRanges.Add(const StartASCII, StopASCII: WideChar)
  : TVXBitmapFontRange;
begin
  Result := Add;
  Result.StartASCII := StartASCII;
  Result.StopASCII := StopASCII;
end;

function TVXBitmapFontRanges.Add(const StartASCII, StopASCII: AnsiChar)
  : TVXBitmapFontRange;
begin
  Result := Add(CharToWideChar(StartASCII), CharToWideChar(StopASCII));
end;

function TVXBitmapFontRanges.FindItemID(ID: Integer): TVXBitmapFontRange;
begin
  Result := (inherited FindItemID(ID)) as TVXBitmapFontRange;
end;

function TVXBitmapFontRanges.CharacterToTileIndex(aChar: WideChar): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    with Items[i] do
    begin
      if (aChar >= FStartASCII) and (aChar <= FStopASCII) then
      begin
        Result := StartGlyphIdx + Integer(aChar) - Integer(FStartASCII);
        Break;
      end;
    end;
end;

function TVXBitmapFontRanges.TileIndexToChar(aIndex: Integer): WideChar;
var
  i: Integer;
begin
  Result := #0;
  for i := 0 to Count - 1 do
    with Items[i] do
    begin
      if (aIndex >= StartGlyphIdx) and (aIndex <= StopGlyphIdx) then
      begin
        Result := WideChar(aIndex - StartGlyphIdx + Integer(FStartASCII));
        Break;
      end;
    end;
end;

procedure TVXBitmapFontRanges.Update(Item: TCollectionItem);
begin
  inherited;
  NotifyChange;
end;

procedure TVXBitmapFontRanges.NotifyChange;
begin
  FCharCount := CalcCharacterCount;

  if Assigned(FOwner) then
  begin
    if FOwner is TVXBaseSceneObject then
      TVXBaseSceneObject(FOwner).StructureChanged
    else if FOwner is TVXCustomBitmapFont then
      TVXCustomBitmapFont(FOwner).NotifyChange(Self);
  end;
end;

function TVXBitmapFontRanges.CalcCharacterCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    with Items[i] do
      Inc(Result, Integer(FStopASCII) - Integer(FStartASCII) + 1);
end;

// ------------------
// ------------------ TVXCustomBitmapFont ------------------
// ------------------

constructor TVXCustomBitmapFont.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRanges := TVXBitmapFontRanges.Create(Self);
  FGlyphs := TVXPicture.Create(AOwner);
  FGlyphs.Bitmap.OnChange := OnGlyphsChanged;
  FCharWidth := 16;
  FCharHeight := 16;
  FHSpace := 1;
  FVSpace := 1;
  FUsers := TList.Create;
  FMinFilter := miLinear;
  FMagFilter := maLinear;
  FTextures := TList.Create;
  FTextureModified := true;
end;

destructor TVXCustomBitmapFont.Destroy;
begin
  FreeTextureHandle;
  inherited Destroy;
  FTextures.Free;
  FRanges.Free;
  FGlyphs.Free;
  Assert(FUsers.Count = 0);
  FUsers.Free;
end;

function TVXCustomBitmapFont.GetCharWidth(Ch: WideChar): Integer;
var
  chi: Integer;
begin
  chi := CharacterToTileIndex(ch);
  if Length(FChars) = 0 then
    ResetCharWidths;
  if chi >= 0 then
    Result := FChars[chi].w
  else
    Result := 0;
end;

function TVXCustomBitmapFont.CalcStringWidth(const aText
  : UnicodeString): Integer;
var
  i: Integer;
begin
  if aText <> '' then
  begin
    Result := -HSpace + Length(aText) * (HSpaceFix + HSpace);
    for i := 1 to Length(aText) do
      Result := Result + GetCharWidth(aText[i]);
  end
  else
    Result := 0;
end;

procedure TVXCustomBitmapFont.ResetCharWidths(w: Integer = -1);
var
  i: Integer;
begin
  FCharsLoaded := False;
  i := CharacterCount;
  if Length(FChars) < i then
    SetLength(FChars, i);
  if w < 0 then
    w := CharWidth;
  for i := 0 to High(FChars) do
    FChars[i].w := w;
end;

procedure TVXCustomBitmapFont.SetCharWidths(index, value: Integer);
begin
  if index >= 0 then
    FChars[index].w := value;
end;

procedure TVXCustomBitmapFont.SetRanges(const val: TVXBitmapFontRanges);
begin
  FRanges.Assign(val);
  InvalidateUsers;
end;

procedure TVXCustomBitmapFont.SetGlyphs(const val: TVXPicture);
begin
  FGlyphs.Assign(val);
end;

procedure TVXCustomBitmapFont.SetCharWidth(const val: Integer);
begin
  if val <> FCharWidth then
  begin
    if val > 1 then
      FCharWidth := val
    else
      FCharWidth := 1;
    InvalidateUsers;
  end;
end;

procedure TVXCustomBitmapFont.SetCharHeight(const val: Integer);
begin
  if val <> FCharHeight then
  begin
    if val > 1 then
      FCharHeight := val
    else
      FCharHeight := 1;
    InvalidateUsers;
  end;
end;

procedure TVXCustomBitmapFont.SetGlyphsIntervalX(const val: Integer);
begin
  if val > 0 then
    FGlyphsIntervalX := val
  else
    FGlyphsIntervalX := 0;
  InvalidateUsers;
end;

procedure TVXCustomBitmapFont.SetGlyphsIntervalY(const val: Integer);
begin
  if val > 0 then
    FGlyphsIntervalY := val
  else
    FGlyphsIntervalY := 0;
  InvalidateUsers;
end;

procedure TVXCustomBitmapFont.SetHSpace(const val: Integer);
begin
  if val <> FHSpace then
  begin
    FHSpace := val;
    InvalidateUsers;
  end;
end;

procedure TVXCustomBitmapFont.SetVSpace(const val: Integer);
begin
  if val <> FVSpace then
  begin
    FVSpace := val;
    InvalidateUsers;
  end;
end;

procedure TVXCustomBitmapFont.SetMagFilter(AValue: TVXMagFilter);
begin
  if AValue <> FMagFilter then
  begin
    FMagFilter := AValue;
    TextureChanged;
    InvalidateUsers;
  end;
end;

procedure TVXCustomBitmapFont.SetMinFilter(AValue: TVXMinFilter);
begin
  if AValue <> FMinFilter then
  begin
    FMinFilter := AValue;
    TextureChanged;
    InvalidateUsers;
  end;
end;

procedure TVXCustomBitmapFont.SetGlyphsAlpha(val: TVXTextureImageAlpha);
begin
  if val <> FGlyphsAlpha then
  begin
    FGlyphsAlpha := val;
    TextureChanged;
    InvalidateUsers;
  end;
end;

procedure TVXCustomBitmapFont.OnGlyphsChanged(Sender: TObject);
begin
  InvalidateUsers;
  // when empty, width is 0 and roundup give 1
  if not Glyphs.Bitmap.IsEmpty then
  begin
    if FTextureWidth = 0 then
      FTextureWidth := RoundUpToPowerOf2(Glyphs.Bitmap.Width);
    if FTextureHeight = 0 then
      FTextureHeight := RoundUpToPowerOf2(Glyphs.Bitmap.Height);
  end;
end;

procedure TVXCustomBitmapFont.RegisterUser(anObject: TVXBaseSceneObject);
begin
  Assert(FUsers.IndexOf(anObject) < 0);
  FUsers.Add(anObject);
end;

procedure TVXCustomBitmapFont.UnRegisterUser(anObject: TVXBaseSceneObject);
begin
  FUsers.Remove(anObject);
end;

procedure TVXCustomBitmapFont.PrepareImage(var ARci: TVXRenderContextInfo);
var
  bitmap: TBitmap;
  bitmap32: TVXBitmap32;
  cap: Integer;
  X, Y, w, h: Integer;
  t: TVXTextureHandle;
begin
  // only check when really used
  if FTextureWidth = 0 then
  begin
    FTextureWidth := ARci.VXStates.MaxTextureSize;
    if FTextureWidth > 512 then
      FTextureWidth := 512;
    if FTextureWidth < 64 then
      FTextureWidth := 64;
  end;
  if FTextureHeight = 0 then
  begin
    FTextureHeight := ARci.VXStates.MaxTextureSize;
    if FTextureHeight > 512 then
      FTextureHeight := 512;
    if FTextureHeight < 64 then
      FTextureHeight := 64;
  end;

  X := 0;
  Y := 0;
  w := Glyphs.Bitmap.Width;
  h := Glyphs.Bitmap.Height;

  // was an error...
  FTextRows := 1 + (h - 1) div FTextureHeight;
  FTextCols := 1 + (w - 1) div FTextureWidth;

  bitmap := TBitmap.Create;
  with bitmap do
  begin
{$IFDEF MSWINDOWS}
   { TODO : E2129 Cannot assign to a read-only property }
    (*PixelFormat := TPixelFormat.RGBA32F;*)
{$ENDIF}
    Width  := RoundUpToPowerOf2(FTextureWidth);
    Height := RoundUpToPowerOf2(FTextureHeight);
  end;

  bitmap32 := TVXBitmap32.Create;

  while (X < w) and (Y < h) do
  begin
    t := TVXTextureHandle.Create;
    FTextures.Add(t);
    // prepare handle
    t.AllocateHandle;
    // texture registration
    t.Target := ttTexture2D;
    ARci.VXStates.TextureBinding[0, ttTexture2D] := t.Handle;

    // copy data
    { TODO : E2003 Undeclared identifier: 'Draw', need to use DrawBitmap() }
    (*bitmap.Canvas.Draw(-X, -Y, Glyphs.Bitmap);*)
    // Clipboard.Assign(bitmap);
    bitmap32.Assign(bitmap);
    bitmap32.Narrow;
    with bitmap32 do
    begin
      case FGlyphsAlpha of
        tiaAlphaFromIntensity:
          SetAlphaFromIntensity;
        tiaSuperBlackTransparent:
          SetAlphaTransparentForColor($000000);
        tiaLuminance:
          SetAlphaFromIntensity;
        tiaLuminanceSqrt:
          begin
            SetAlphaFromIntensity;
            SqrtAlpha;
          end;
        tiaOpaque:
          SetAlphaToValue(255);
        tiaDefault, tiaTopLeftPointColorTransparent:
          SetAlphaTransparentForColor(Data[Width * (Height - 1)]);
      else
        Assert(False);
      end;
      RegisterAsOpenVXTexture(t, not(FMinFilter in [miNearest, miLinear]),
        TextureFormat, cap, cap, cap);
    end;

    PrepareParams(ARci);
    t.NotifyDataUpdated;

    Inc(X, FTextureWidth);
    if X >= w then
    begin
      Inc(Y, FTextureHeight);
      X := 0;
    end;
  end;
  bitmap.Free;
  bitmap32.Free;
end;

procedure TVXCustomBitmapFont.PrepareParams(var ARci: TVXRenderContextInfo);
const
  cTextureMagFilter: array [maNearest .. maLinear] of Cardinal = (GL_NEAREST, GL_LINEAR);
  cTextureMinFilter: array [miNearest .. miLinearMipmapLinear] of Cardinal =
    (GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST, GL_LINEAR_MIPMAP_NEAREST,
    GL_NEAREST_MIPMAP_LINEAR, GL_LINEAR_MIPMAP_LINEAR);
begin

  with ARci.VxStates do
  begin
    UnpackAlignment := 4;
    UnpackRowLength := 0;
    UnpackSkipRows := 0;
    UnpackSkipPixels := 0;
  end;

  begin
    glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, cTextureMinFilter[FMinFilter]);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, cTextureMagFilter[FMagFilter]);
  end;
end;

function TVXCustomBitmapFont.TileIndexToChar(aIndex: Integer): WideChar;
begin
  Result := FRanges.TileIndexToChar(aIndex);
end;

function TVXCustomBitmapFont.CharacterToTileIndex(aChar: WideChar): Integer;
begin
  Result := FRanges.CharacterToTileIndex(aChar);
end;

procedure TVXCustomBitmapFont.RenderString(var ARci: TVXRenderContextInfo;
  const aText: UnicodeString; aAlignment: TAlignment; aLayout: TVXTextLayout;
  const aColor: TColorVector; aPosition: PVector = nil;
  aReverseY: boolean = False);

  function AlignmentAdjustement(p: Integer): Single;
  var
    i: Integer;
  begin
    i := 0;
    while (p <= Length(aText)) and (aText[p] <> #13) do
    begin
      Inc(p);
      Inc(i);
    end;
    case aAlignment of
      taLeftJustify:
        Result := 0;
      taRightJustify:
        Result := -CalcStringWidth(Copy(aText, p - i, i))
    else // taCenter
      Result := Round(-CalcStringWidth(Copy(aText, p - i, i)) * 0.5);
    end;
  end;

  function LayoutAdjustement: Single;
  var
    i, n: Integer;
  begin
    n := 1;
    for i := 1 to Length(aText) do
      if aText[i] = #13 then
        Inc(n);
    case TVXTextLayout(aLayout) of
      tlTop:  Result := 0;
      tlBottom: Result := (n * (CharHeight + VSpace) - VSpace);
    else // tlCenter
      Result := Round((n * (CharHeight + VSpace) - VSpace) * 0.5);
    end;
  end;

var
  i, chi: Integer;
  pch: PCharInfo;
  TopLeft, BottomRight: TTexPoint;
  vTopLeft, vBottomRight: TVector;
  deltaV, spaceDeltaH: Single;
  currentChar: WideChar;
begin
  if (aText = '') then
    Exit;
  // prepare texture if necessary
  CheckTexture(ARci);
  // precalcs
  if Assigned(aPosition) then
    MakePoint(vTopLeft, aPosition.X + AlignmentAdjustement(1),
      aPosition.Y + LayoutAdjustement, 0)
  else
    MakePoint(vTopLeft, AlignmentAdjustement(1), LayoutAdjustement, 0);
  deltaV := -(CharHeight + VSpace);
  if aReverseY then
    vBottomRight.Y := vTopLeft.Y + CharHeight
  else
    vBottomRight.Y := vTopLeft.Y - CharHeight;
  vBottomRight.Z := 0;
  vBottomRight.W := 1;
  spaceDeltaH := GetCharWidth(#32) + HSpaceFix + HSpace;
  // set states
  with ARci.VXStates do
  begin
    ActiveTextureEnabled[ttTexture2D] := true;
    Disable(stLighting);
    Enable(stBlend);
    SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
    FLastTexture := nil;
  end;

  // start rendering
  glColor4fv(@aColor);
  glBegin(GL_QUADS);
  for i := 1 to Length(aText) do
  begin
    currentChar := WideChar(aText[i]);
    case currentChar of
      #0 .. #12, #14 .. #31:
        ; // ignore
      #13:
        begin
          if Assigned(aPosition) then
            vTopLeft.X := aPosition.X + AlignmentAdjustement(i + 1)
          else
            vTopLeft.X := AlignmentAdjustement(i + 1);
          vTopLeft.Y := vTopLeft.Y + deltaV;
          if aReverseY then
            vBottomRight.Y := vTopLeft.Y + CharHeight
          else
            vBottomRight.Y := vTopLeft.Y - CharHeight;
        end;
      #32:
        vTopLeft.X := vTopLeft.X + spaceDeltaH;
    else
      chi := CharacterToTileIndex(currentChar);
      if chi < 0 then
        continue; // not found
      pch := @FChars[chi];
      if pch.w > 0 then

        begin
          GetICharTexCoords(ARci, chi, TopLeft, BottomRight);
          vBottomRight.X := vTopLeft.X + pch.w;

          glTexCoord2fv(@TopLeft);
          glVertex4fv(@vTopLeft);

          glTexCoord2f(TopLeft.S, BottomRight.t);
          glVertex2f(vTopLeft.X, vBottomRight.Y);

          glTexCoord2fv(@BottomRight);
          glVertex4fv(@vBottomRight);

          glTexCoord2f(BottomRight.S, TopLeft.t);
          glVertex2f(vBottomRight.X, vTopLeft.Y);

          vTopLeft.X := vTopLeft.X + pch.w + HSpace;
        end;
    end;
  end;
  glEnd;
  // unbind texture
  ARci.VXStates.TextureBinding[0, ttTexture2D] := 0;
  ARci.VXStates.ActiveTextureEnabled[ttTexture2D] := False;
end;

procedure TVXCustomBitmapFont.TextOut(var rci: TVXRenderContextInfo; X, Y: Single;
  const Text: UnicodeString; const Color: TColorVector);
var
  V: TVector;
begin
  V.X := X;
  V.Y := Y;
  V.Z := 0;
  V.W := 1;
  RenderString(rci, Text, taLeftJustify, tlTop, Color, @V, true);
end;

procedure TVXCustomBitmapFont.TextOut(var rci: TVXRenderContextInfo; X, Y: Single;
  const Text: UnicodeString; const Color: TColor);
begin
  TextOut(rci, X, Y, Text, ConvertWinColor(Color));
end;

function TVXCustomBitmapFont.TextWidth(const Text: UnicodeString): Integer;
begin
  Result := CalcStringWidth(Text);
end;

function TVXCustomBitmapFont.CharactersPerRow: Integer;
begin
  if FGlyphs.Bitmap.Width > 0 then
    Result := (FGlyphs.Bitmap.Width + FGlyphsIntervalX)
      div (FGlyphsIntervalX + FCharWidth)
  else
    Result := 0;
end;

function TVXCustomBitmapFont.CharacterCount: Integer;
begin
  Result := FRanges.CharacterCount;
end;

procedure TVXCustomBitmapFont.GetCharTexCoords(Ch: WideChar;
  var TopLeft, BottomRight: TTexPoint);
var
  chi, tileIndex: Integer;
  ci: TCharInfo;
  r: Integer;
begin
  chi := CharacterToTileIndex(ch);
  if not FCharsLoaded then
  begin
    ResetCharWidths;
    FCharsLoaded := true;
    r := CharactersPerRow;
    for tileIndex := 0 to CharacterCount - 1 do
    begin
      FChars[tileIndex].l := (tileIndex mod r) * (CharWidth + GlyphsIntervalX);
      FChars[tileIndex].t := (tileIndex div r) * (CharHeight + GlyphsIntervalY);
    end;
  end;

  if (chi < 0) or (chi >= CharacterCount) then
  begin
    // invalid char
    TopLeft := NullTexPoint;
    BottomRight := NullTexPoint;
    Exit;
  end;

  ci := FChars[chi];
  ci.l := ci.l mod FTextureWidth;
  ci.t := ci.t mod FTextureHeight;

  TopLeft.S := ci.l / FTextureWidth;
  TopLeft.t := 1 - ci.t / FTextureHeight;
  BottomRight.S := (ci.l + ci.w) / FTextureWidth;
  BottomRight.t := 1 - (ci.t + CharHeight) / FTextureHeight;
end;

// TileIndexToTexCoords it also activates the target texture
procedure TVXCustomBitmapFont.GetICharTexCoords(var ARci: TVXRenderContextInfo;
  Chi: Integer; out TopLeft, BottomRight: TTexPoint);
var
  tileIndex: Integer;
  ci: TCharInfo;
  t: TVXTextureHandle;
  r, c: Integer;
begin
  if not FCharsLoaded then
  begin
    r := CharactersPerRow;
    if r = 0 then
      Exit;
    ResetCharWidths;
    FCharsLoaded := true;
    for tileIndex := 0 to CharacterCount - 1 do
    begin
      FChars[tileIndex].l := (tileIndex mod r) * (CharWidth + GlyphsIntervalX);
      FChars[tileIndex].t := (tileIndex div r) * (CharHeight + GlyphsIntervalY);
    end;
  end;

  if (chi < 0) or (chi >= CharacterCount) then
  begin
    // invalid char
    TopLeft := NullTexPoint;
    BottomRight := NullTexPoint;
    Exit;
  end;

  ci := FChars[chi];

  c := ci.l div FTextureWidth;
  r := ci.t div FTextureHeight;
  ci.l := ci.l mod FTextureWidth;
  ci.t := ci.t mod FTextureHeight;
  t := FTextures[r * FTextCols + c];

  TopLeft.S := ci.l / FTextureWidth;
  TopLeft.t := 1 - ci.t / FTextureHeight;
  BottomRight.S := (ci.l + ci.w) / FTextureWidth;
  BottomRight.t := 1 - (ci.t + CharHeight) / FTextureHeight;

  if t <> FLastTexture then
    begin
      FLastTexture := t;
      glEnd;
      ARci.VXStates.TextureBinding[0, ttTexture2D] := t.Handle;
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
      glBegin(GL_QUADS);
    end;
end;

procedure TVXCustomBitmapFont.InvalidateUsers;
var
  i: Integer;
begin
  FCharsLoaded := False;
  FTextureModified := true;
  for i := FUsers.Count - 1 downto 0 do
    TVXBaseSceneObject(FUsers[i]).NotifyChange(Self);
end;

procedure TVXCustomBitmapFont.FreeTextureHandle;
var
  i: Integer;
begin
  FTextureModified := true;
  for i := 0 to FTextures.Count - 1 do
    TObject(FTextures[i]).Free;
  FTextures.Clear;
end;

procedure TVXCustomBitmapFont.TextureChanged;
begin
  FTextureModified := true;
end;

// force texture when needed
procedure TVXCustomBitmapFont.CheckTexture(var ARci: TVXRenderContextInfo);
var
  i: Integer;
begin
  // important: IsDataNeedUpdate might come from another source!
  for i := 0 to FTextures.Count - 1 do
    FTextureModified := FTextureModified or TVXTextureHandle(FTextures[i])
      .IsDataNeedUpdate;

  if FTextureModified then
  begin
    FreeTextureHandle; // instances are recreated in prepare
    PrepareImage(ARci);
    FTextureModified := False;
  end;
end;

function TVXCustomBitmapFont.TextureFormat: Integer;
begin
  Result := GL_RGBA;
end;

// ------------------
// ------------------ TVXFlatText ------------------
// ------------------

constructor TVXFlatText.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FModulateColor := TVXColor.CreateInitialized(Self, clrWhite);
end;

destructor TVXFlatText.Destroy;
begin
  FModulateColor.Free;
  BitmapFont := nil;
  inherited;
end;

procedure TVXFlatText.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FBitmapFont) then
    BitmapFont := nil;
  inherited;
end;

procedure TVXFlatText.SetBitmapFont(const val: TVXCustomBitmapFont);
begin
  if val <> FBitmapFont then
  begin
    if Assigned(FBitmapFont) then
      FBitmapFont.UnRegisterUser(Self);
    FBitmapFont := val;
    if Assigned(FBitmapFont) then
    begin
      FBitmapFont.RegisterUser(Self);
      FBitmapFont.FreeNotification(Self);
    end;
    StructureChanged;
  end;
end;

procedure TVXFlatText.SetText(const val: UnicodeString);
begin
  FText := val;
  StructureChanged;
end;

procedure TVXFlatText.SetAlignment(const val: TAlignment);
begin
  FAlignment := val;
  StructureChanged;
end;

procedure TVXFlatText.SetLayout(const val: TVXTextLayout);
begin
  FLayout := val;
  StructureChanged;
end;

procedure TVXFlatText.SetModulateColor(const val: TVXColor);
begin
  FModulateColor.Assign(val);
end;

procedure TVXFlatText.SetOptions(const val: TVXFlatTextOptions);
begin
  if val <> FOptions then
  begin
    FOptions := val;
    StructureChanged;
  end;
end;

procedure TVXFlatText.DoRender(var rci: TVXRenderContextInfo;
  renderSelf, renderChildren: boolean);
begin
  if Assigned(FBitmapFont) and (Text <> '') then
  begin
    rci.VXStates.PolygonMode := pmFill;
    if FModulateColor.Alpha <> 1 then
    begin
      rci.VXStates.Enable(stBlend);
      rci.VXStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
    end;
    if ftoTwoSided in FOptions then
      rci.VXStates.Disable(stCullFace);
    FBitmapFont.RenderString(rci, Text, FAlignment, FLayout,
      FModulateColor.Color);
  end;
  if Count > 0 then
    Self.renderChildren(0, Count - 1, rci);
end;

procedure TVXFlatText.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVXFlatText) then
  begin
    BitmapFont := TVXFlatText(Source).BitmapFont;
    Text := TVXFlatText(Source).Text;
    Alignment := TVXFlatText(Source).Alignment;
    Layout := TVXFlatText(Source).Layout;
    ModulateColor := TVXFlatText(Source).ModulateColor;
    Options := TVXFlatText(Source).Options;
  end;
  inherited Assign(Source);
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

RegisterClasses([TVXBitmapFont, TVXFlatText]);

end.
