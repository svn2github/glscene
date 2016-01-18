//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{ 
  Bitmap Fonts management classes
}
unit GLS.BitmapFont;

{$I GLScene.inc}

{$DEFINE GLS_UNICODE_SUPPORT}

interface

uses
  System.Classes, System.SysUtils, System.Types, System.UITypes,
  FMX.Graphics, FMX.Types,
  //GLS
  GLS.Scene, GLS.VectorGeometry, GLS.Context, GLS.CrossPlatform,
  GLS.Texture, GLS.State, GLS.Utils, GLS.Graphics, GLS.Color, GLS.BaseClasses,
  GLS.RenderContextInfo, GLS.TextureFormat,
  GLS.OpenGLTokens, GLS.XOpenGL, GLS.VectorTypes;

type
{$IFNDEF GLS_UNICODE_SUPPORT}
  UnicodeString = WideString; // Use WideString for earlier versions
{$ENDIF}

  // TVKBitmapFontRange
  //
  { An individual character range in a bitmap font.<p>
    A range allows mapping ASCII characters to character tiles in a font
    bitmap, tiles are enumerated line then column (raster). }
  TVKBitmapFontRange = class(TCollectionItem)
  private
    function GetStartASCII: WideString;
    function GetStopASCII: WideString;
  protected
    { Protected Declarations }
    FStartASCII, FStopASCII: WideChar;
    FStartGlyphIdx, FStopGlyphIdx, FCharCount: Integer;
    procedure SetStartASCII(const val: WideString);
    procedure SetStopASCII(const val: WideString);
    procedure SetStartGlyphIdx(val: Integer);
    function GetDisplayName: string; override;
  public
    { Public Declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange;
  published
    { Published Declarations }
    property StartASCII: WideString read GetStartASCII write SetStartASCII;
    property StopASCII: WideString read GetStopASCII write SetStopASCII;
    property StartGlyphIdx: Integer read FStartGlyphIdx write SetStartGlyphIdx;
    property StopGlyphIdx: Integer read FStopGlyphIdx;
    property CharCount: Integer read FCharCount;
  end;

  // TVKBitmapFontRanges
  //
  TVKBitmapFontRanges = class(TCollection)
  private
    FCharCount: Integer;
  protected
    { Protected Declarations }
    FOwner: TComponent;

    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TVKBitmapFontRange);
    function GetItems(index: Integer): TVKBitmapFontRange;
    function CalcCharacterCount: Integer;
    procedure Update(Item: TCollectionItem); override;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    function Add: TVKBitmapFontRange; overload;
    function Add(const StartASCII, StopASCII: WideChar)
      : TVKBitmapFontRange; overload;
    function Add(const StartASCII, StopASCII: AnsiChar)
      : TVKBitmapFontRange; overload;
    function FindItemID(ID: Integer): TVKBitmapFontRange;
    property Items[index: Integer]: TVKBitmapFontRange read GetItems
      write SetItems; default;

    { Converts an ASCII character into a tile index.<p>
      Return -1 if character cannot be rendered. }
    function CharacterToTileIndex(aChar: WideChar): Integer;
    function TileIndexToChar(aIndex: Integer): WideChar;
    procedure NotifyChange;

    // Total number of characters in the ranges; cached for performance
    property CharacterCount: Integer read FCharCount;
  end;

  PCharInfo = ^TCharInfo;

  TCharInfo = record
    l, t, w: word;
  end;

  // TVKCustomBitmapFont
  //
  { Provides access to individual characters in a BitmapFont.<p>
    Only fixed-width bitmap fonts are supported, the characters are enumerated
    in a raster fashion (line then column).
    Transparency is all or nothing, the transparent color being that of the
    top left pixel of the Glyphs bitmap.<p>
    Performance note: as usual, for best performance, you base font bitmap
    dimensions should be close to a power of two, and have at least 1 pixel
    spacing between characters (horizontally and vertically) to avoid artefacts
    when rendering with linear filtering. }
  TVKCustomBitmapFont = class(TVKUpdateAbleComponent)
  private
    { Private Declarations }
    FRanges: TVKBitmapFontRanges;
    FGlyphs: TVKPicture;
    FCharWidth, FCharHeight: Integer;
    FGlyphsIntervalX, FGlyphsIntervalY: Integer;
    FHSpace, FVSpace, FHSpaceFix: Integer;
    FUsers: TList;
    FMinFilter: TVKMinFilter;
    FMagFilter: TVKMagFilter;
    FTextureWidth, FTextureHeight: Integer;
    FTextRows, FTextCols: Integer;
    FGlyphsAlpha: TVKTextureImageAlpha;
    FTextures: TList;
    FTextureModified: boolean;
    FLastTexture: TVKTextureHandle;
  protected
    { Protected Declarations }
    FChars: array of TCharInfo;
    FCharsLoaded: boolean;
    procedure ResetCharWidths(w: Integer = -1);
    procedure SetCharWidths(index, value: Integer);

    procedure SetRanges(const val: TVKBitmapFontRanges);
    procedure SetGlyphs(const val: TVKPicture);
    procedure SetCharWidth(const val: Integer);
    procedure SetCharHeight(const val: Integer);
    procedure SetGlyphsIntervalX(const val: Integer);
    procedure SetGlyphsIntervalY(const val: Integer);
    procedure OnGlyphsChanged(Sender: TObject);
    procedure SetHSpace(const val: Integer);
    procedure SetVSpace(const val: Integer);
    procedure SetMagFilter(AValue: TVKMagFilter);
    procedure SetMinFilter(AValue: TVKMinFilter);
    procedure SetGlyphsAlpha(val: TVKTextureImageAlpha);

    procedure TextureChanged;
    procedure FreeTextureHandle; dynamic;
    function TextureFormat: Integer; dynamic;

    procedure InvalidateUsers;
    function CharactersPerRow: Integer;
    procedure GetCharTexCoords(Ch: WideChar;
      var TopLeft, BottomRight: TTexPoint);
    procedure GetICharTexCoords(var ARci: TRenderContextInfo; Chi: Integer;
      out TopLeft, BottomRight: TTexPoint);
    procedure PrepareImage(var ARci: TRenderContextInfo); virtual;
    procedure PrepareParams(var ARci: TRenderContextInfo);

    { A single bitmap containing all the characters.<p>
      The transparent color is that of the top left pixel. }
    property Glyphs: TVKPicture read FGlyphs write SetGlyphs;
    {  Nb of horizontal pixels between two columns in the Glyphs. }
    property GlyphsIntervalX: Integer read FGlyphsIntervalX
      write SetGlyphsIntervalX;
    { Nb of vertical pixels between two rows in the Glyphs. }
    property GlyphsIntervalY: Integer read FGlyphsIntervalY
      write SetGlyphsIntervalY;
    { Ranges allow converting between ASCII and tile indexes.<p>
      See TVKCustomBitmapFontRange. }
    property Ranges: TVKBitmapFontRanges read FRanges write SetRanges;

    { Width of a single character. }
    property CharWidth: Integer read FCharWidth write SetCharWidth default 16;
    { Pixels in between rendered characters (horizontally). }
    property HSpace: Integer read FHSpace write SetHSpace default 1;
    { Pixels in between rendered lines (vertically). }
    property VSpace: Integer read FVSpace write SetVSpace default 1;
    { Horizontal spacing fix offset.<p>
      This property is for internal use, and is added to the hspacing
      of each character when rendering, typically to fix extra spacing. }
    property HSpaceFix: Integer read FHSpaceFix write FHSpaceFix;

    property MagFilter: TVKMagFilter read FMagFilter write SetMagFilter
      default maLinear;
    property MinFilter: TVKMinFilter read FMinFilter write SetMinFilter
      default miLinear;
    property GlyphsAlpha: TVKTextureImageAlpha read FGlyphsAlpha
      write FGlyphsAlpha default tiaDefault;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RegisterUser(anObject: TVKBaseSceneObject); virtual;
    procedure UnRegisterUser(anObject: TVKBaseSceneObject); virtual;

    { Renders the given string at current position or at position given by the optional position variable.<p>
      The current matrix is blindly used, meaning you can render all kinds
      of rotated and linear distorted text with this method, OpenGL
      Enable states are also possibly altered. }
    procedure RenderString(var ARci: TRenderContextInfo;
      const aText: UnicodeString; aAlignment: TAlignment;
      aLayout: TVKTextLayout; const aColor: TColorVector;
      aPosition: PVector = nil; aReverseY: boolean = False); overload; virtual;

    { A simpler canvas-style TextOut helper for RenderString.<p>
      The rendering is reversed along Y by default, to allow direct use
      with TVKCanvas }
    procedure TextOut(var rci: TRenderContextInfo; X, Y: Single;
      const Text: UnicodeString; const Color: TColorVector); overload;
    procedure TextOut(var rci: TRenderContextInfo; X, Y: Single;
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
    procedure CheckTexture(var ARci: TRenderContextInfo);

    { Height of a single character. }
    property CharHeight: Integer read FCharHeight write SetCharHeight
      default 16;

    property TextureWidth: Integer read FTextureWidth write FTextureWidth;
    property TextureHeight: Integer read FTextureHeight write FTextureHeight;
  end;

  // TVKBitmapFont
  //
  { See TVKCustomBitmapFont.<p>
    This class only publuishes some of the properties. }
  TVKBitmapFont = class(TVKCustomBitmapFont)
  published
    { Published Declarations }
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

  // TVKFlatTextOptions
  //
  TVKFlatTextOption = (ftoTwoSided);
  TVKFlatTextOptions = set of TVKFlatTextOption;

  // TVKFlatText
  //
  { A 2D text displayed and positionned in 3D coordinates.<p>
    The FlatText uses a character font defined and stored by a TVKBitmapFont
    component. Default character scale is 1 font pixel = 1 space unit. }
  TVKFlatText = class(TVKImmaterialSceneObject)
  private
    { Private Declarations }
    FBitmapFont: TVKCustomBitmapFont;
    FText: UnicodeString;
    FAlignment: TAlignment;
    FLayout: TVKTextLayout;
    FModulateColor: TVKColor;
    FOptions: TVKFlatTextOptions;

  protected
    { Protected Declarations }
    procedure SetBitmapFont(const val: TVKCustomBitmapFont);
    procedure SetText(const val: UnicodeString);
    procedure SetAlignment(const val: TAlignment);
    procedure SetLayout(const val: TVKTextLayout);
    procedure SetModulateColor(const val: TVKColor);
    procedure SetOptions(const val: TVKFlatTextOptions);

    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoRender(var rci: TRenderContextInfo;
      renderSelf, renderChildren: boolean); override;

    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }
    { Refers the bitmap font to use.
      The referred bitmap font component stores and allows access to
      individual character bitmaps. }
    property BitmapFont: TVKCustomBitmapFont read FBitmapFont
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
    property Layout: TVKTextLayout read FLayout write SetLayout;
    { Color modulation, can be used for fade in/out too. }
    property ModulateColor: TVKColor read FModulateColor write SetModulateColor;
    { Flat text options.<p>
       <li>ftoTwoSided : when set the text will be visible from its two
      sides even if faceculling is on (at the scene-level).
     }
    property Options: TVKFlatTextOptions read FOptions write SetOptions;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TVKBitmapFontRange ------------------
// ------------------

// Create
//
constructor TVKBitmapFontRange.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

// Destroy
//
destructor TVKBitmapFontRange.Destroy;
begin
  inherited;
end;

// Assign
//
procedure TVKBitmapFontRange.Assign(Source: TPersistent);
begin
  if Source is TVKBitmapFontRange then
  begin
    FStartASCII := TVKBitmapFontRange(Source).FStartASCII;
    FStopASCII := TVKBitmapFontRange(Source).FStopASCII;
    FStartGlyphIdx := TVKBitmapFontRange(Source).FStartGlyphIdx;
    NotifyChange;
  end
  else
    inherited;
end;

// NotifyChange
//
procedure TVKBitmapFontRange.NotifyChange;
begin
  FCharCount := Integer(FStopASCII) - Integer(FStartASCII) + 1;
  FStopGlyphIdx := FStartGlyphIdx + FCharCount - 1;
  if Assigned(Collection) then
    (Collection as TVKBitmapFontRanges).NotifyChange;
end;

// GetDisplayName
//
function TVKBitmapFontRange.GetDisplayName: string;
begin
  Result := Format('ASCII [#%d, #%d] -> Glyphs [%d, %d]',
    [Integer(FStartASCII), Integer(FStopASCII), StartGlyphIdx, StopGlyphIdx]);
end;

function TVKBitmapFontRange.GetStartASCII: WideString;
begin
  Result := FStartASCII;
end;

function TVKBitmapFontRange.GetStopASCII: WideString;
begin
  Result := FStopASCII;
end;

// SetStartASCII
//
procedure TVKBitmapFontRange.SetStartASCII(const val: WideString);
begin
  if (Length(val) > 0) and (val[1] <> FStartASCII) then
  begin
    FStartASCII := val[1];
    if FStartASCII > FStopASCII then
      FStopASCII := FStartASCII;
    NotifyChange;
  end;
end;

// SetStopASCII
//
procedure TVKBitmapFontRange.SetStopASCII(const val: WideString);
begin
  if (Length(val) > 0) and (FStopASCII <> val[1]) then
  begin
    FStopASCII := val[1];
    if FStopASCII < FStartASCII then
      FStartASCII := FStopASCII;
    NotifyChange;
  end;
end;

// SetStartGlyphIdx
//
procedure TVKBitmapFontRange.SetStartGlyphIdx(val: Integer);
begin
  val := MaxInteger(0, val);
  if val <> FStartGlyphIdx then
  begin
    FStartGlyphIdx := val;
    NotifyChange;
  end;
end;

// ------------------
// ------------------ TVKBitmapFontRanges ------------------
// ------------------

// Create
//
constructor TVKBitmapFontRanges.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  inherited Create(TVKBitmapFontRange);
end;

// Destroy
//
destructor TVKBitmapFontRanges.Destroy;
begin
  inherited;
end;

// GetOwner
//
function TVKBitmapFontRanges.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// SetItems
//

procedure TVKBitmapFontRanges.SetItems(index: Integer;
  const val: TVKBitmapFontRange);
begin
  inherited Items[index] := val;
end;

// GetItems
//
function TVKBitmapFontRanges.GetItems(index: Integer): TVKBitmapFontRange;
begin
  Result := TVKBitmapFontRange(inherited Items[index]);
end;

// Add
//
function TVKBitmapFontRanges.Add: TVKBitmapFontRange;
begin
  Result := (inherited Add) as TVKBitmapFontRange;
end;

// Add
//
function TVKBitmapFontRanges.Add(const StartASCII, StopASCII: WideChar)
  : TVKBitmapFontRange;
begin
  Result := Add;
  Result.StartASCII := StartASCII;
  Result.StopASCII := StopASCII;
end;

// Add
//
function TVKBitmapFontRanges.Add(const StartASCII, StopASCII: AnsiChar)
  : TVKBitmapFontRange;
begin
  Result := Add(CharToWideChar(StartASCII), CharToWideChar(StopASCII));
end;

// FindItemID
//
function TVKBitmapFontRanges.FindItemID(ID: Integer): TVKBitmapFontRange;
begin
  Result := (inherited FindItemID(ID)) as TVKBitmapFontRange;
end;

// CharacterToTileIndex
//
function TVKBitmapFontRanges.CharacterToTileIndex(aChar: WideChar): Integer;
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

function TVKBitmapFontRanges.TileIndexToChar(aIndex: Integer): WideChar;
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

procedure TVKBitmapFontRanges.Update(Item: TCollectionItem);
begin
  inherited;
  NotifyChange;
end;

// NotifyChange
//
procedure TVKBitmapFontRanges.NotifyChange;
begin
  FCharCount := CalcCharacterCount;

  if Assigned(FOwner) then
  begin
    if FOwner is TVKBaseSceneObject then
      TVKBaseSceneObject(FOwner).StructureChanged
    else if FOwner is TVKCustomBitmapFont then
      TVKCustomBitmapFont(FOwner).NotifyChange(Self);
  end;
end;

// CharacterCount
//
function TVKBitmapFontRanges.CalcCharacterCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    with Items[i] do
      Inc(Result, Integer(FStopASCII) - Integer(FStartASCII) + 1);
end;

// ------------------
// ------------------ TVKCustomBitmapFont ------------------
// ------------------

// Create
//
constructor TVKCustomBitmapFont.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRanges := TVKBitmapFontRanges.Create(Self);
  FGlyphs := TVKPicture.Create(AOwner);
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

// Destroy
//
destructor TVKCustomBitmapFont.Destroy;
begin
  FreeTextureHandle;
  inherited Destroy;
  FTextures.Free;
  FRanges.Free;
  FGlyphs.Free;
  Assert(FUsers.Count = 0);
  FUsers.Free;
end;

// GetCharWidth
//
function TVKCustomBitmapFont.GetCharWidth(Ch: WideChar): Integer;
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

// CalcStringWidth
//
function TVKCustomBitmapFont.CalcStringWidth(const aText
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

// ResetCharWidths
//
procedure TVKCustomBitmapFont.ResetCharWidths(w: Integer = -1);
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

// SetCharWidths
//
procedure TVKCustomBitmapFont.SetCharWidths(index, value: Integer);
begin
  if index >= 0 then
    FChars[index].w := value;
end;

// SetRanges
//
procedure TVKCustomBitmapFont.SetRanges(const val: TVKBitmapFontRanges);
begin
  FRanges.Assign(val);
  InvalidateUsers;
end;

// SetGlyphs
//
procedure TVKCustomBitmapFont.SetGlyphs(const val: TVKPicture);
begin
  FGlyphs.Assign(val);
end;

// SetCharWidth
//
procedure TVKCustomBitmapFont.SetCharWidth(const val: Integer);
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

// SetCharHeight
//
procedure TVKCustomBitmapFont.SetCharHeight(const val: Integer);
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

// SetGlyphsIntervalX
//
procedure TVKCustomBitmapFont.SetGlyphsIntervalX(const val: Integer);
begin
  if val > 0 then
    FGlyphsIntervalX := val
  else
    FGlyphsIntervalX := 0;
  InvalidateUsers;
end;

// SetGlyphsIntervalY
//
procedure TVKCustomBitmapFont.SetGlyphsIntervalY(const val: Integer);
begin
  if val > 0 then
    FGlyphsIntervalY := val
  else
    FGlyphsIntervalY := 0;
  InvalidateUsers;
end;

// SetHSpace
//
procedure TVKCustomBitmapFont.SetHSpace(const val: Integer);
begin
  if val <> FHSpace then
  begin
    FHSpace := val;
    InvalidateUsers;
  end;
end;

// SetVSpace
//
procedure TVKCustomBitmapFont.SetVSpace(const val: Integer);
begin
  if val <> FVSpace then
  begin
    FVSpace := val;
    InvalidateUsers;
  end;
end;

// SetMagFilter
//
procedure TVKCustomBitmapFont.SetMagFilter(AValue: TVKMagFilter);
begin
  if AValue <> FMagFilter then
  begin
    FMagFilter := AValue;
    TextureChanged;
    InvalidateUsers;
  end;
end;

// SetMinFilter
//
procedure TVKCustomBitmapFont.SetMinFilter(AValue: TVKMinFilter);
begin
  if AValue <> FMinFilter then
  begin
    FMinFilter := AValue;
    TextureChanged;
    InvalidateUsers;
  end;
end;

// SetGlyphsAlpha
//
procedure TVKCustomBitmapFont.SetGlyphsAlpha(val: TVKTextureImageAlpha);
begin
  if val <> FGlyphsAlpha then
  begin
    FGlyphsAlpha := val;
    TextureChanged;
    InvalidateUsers;
  end;
end;

// OnGlyphsChanged
//
procedure TVKCustomBitmapFont.OnGlyphsChanged(Sender: TObject);
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

// RegisterUser
//
procedure TVKCustomBitmapFont.RegisterUser(anObject: TVKBaseSceneObject);
begin
  Assert(FUsers.IndexOf(anObject) < 0);
  FUsers.Add(anObject);
end;

// UnRegisterUser
//
procedure TVKCustomBitmapFont.UnRegisterUser(anObject: TVKBaseSceneObject);
begin
  FUsers.Remove(anObject);
end;

// PrepareImage
//
procedure TVKCustomBitmapFont.PrepareImage(var ARci: TRenderContextInfo);
var
  bitmap: TVKBitmap;
  bitmap32: TVKBitmap32;
  cap: Integer;
  X, Y, w, h: Integer;
  t: TVKTextureHandle;
begin
  // only check when really used
  if FTextureWidth = 0 then
  begin
    FTextureWidth := ARci.GLStates.MaxTextureSize;
    if FTextureWidth > 512 then
      FTextureWidth := 512;
    if FTextureWidth < 64 then
      FTextureWidth := 64;
  end;
  if FTextureHeight = 0 then
  begin
    FTextureHeight := ARci.GLStates.MaxTextureSize;
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

  bitmap := TVKBitmap.Create;
  with bitmap do
  begin
{$IFDEF MSWINDOWS}
   { TODO : E2129 Cannot assign to a read-only property }
    (*PixelFormat := TPixelFormat.RGBA32F;*)
{$ENDIF}
    Width  := RoundUpToPowerOf2(FTextureWidth);
    Height := RoundUpToPowerOf2(FTextureHeight);
  end;

  bitmap32 := TVKBitmap32.Create;

  while (X < w) and (Y < h) do
  begin
    t := TVKTextureHandle.Create;
    FTextures.Add(t);
    // prepare handle
    t.AllocateHandle;
    // texture registration
    t.Target := ttTexture2D;
    ARci.GLStates.TextureBinding[0, ttTexture2D] := t.Handle;

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
      RegisterAsOpenGLTexture(t, not(FMinFilter in [miNearest, miLinear]),
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

// PrepareParams
//
procedure TVKCustomBitmapFont.PrepareParams(var ARci: TRenderContextInfo);
const
  cTextureMagFilter: array [maNearest .. maLinear] of TGLenum = (GL_NEAREST,
    GL_LINEAR);
  cTextureMinFilter: array [miNearest .. miLinearMipmapLinear] of TGLenum =
    (GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST, GL_LINEAR_MIPMAP_NEAREST,
    GL_NEAREST_MIPMAP_LINEAR, GL_LINEAR_MIPMAP_LINEAR);
begin

  with ARci.GLStates do
  begin
    UnpackAlignment := 4;
    UnpackRowLength := 0;
    UnpackSkipRows := 0;
    UnpackSkipPixels := 0;
  end;

  with GL do
  begin
    Hint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

    TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

    TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
      cTextureMinFilter[FMinFilter]);
    TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,
      cTextureMagFilter[FMagFilter]);
  end;
end;

function TVKCustomBitmapFont.TileIndexToChar(aIndex: Integer): WideChar;
begin
  Result := FRanges.TileIndexToChar(aIndex);
end;

function TVKCustomBitmapFont.CharacterToTileIndex(aChar: WideChar): Integer;
begin
  Result := FRanges.CharacterToTileIndex(aChar);
end;

// RenderString
//
procedure TVKCustomBitmapFont.RenderString(var ARci: TRenderContextInfo;
  const aText: UnicodeString; aAlignment: TAlignment; aLayout: TVKTextLayout;
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
    case TVKTextLayout(aLayout) of
      tlTop:
        Result := 0;
      tlBottom:
        Result := (n * (CharHeight + VSpace) - VSpace);
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
    MakePoint(vTopLeft, aPosition.V[0] + AlignmentAdjustement(1),
      aPosition.V[1] + LayoutAdjustement, 0)
  else
    MakePoint(vTopLeft, AlignmentAdjustement(1), LayoutAdjustement, 0);
  deltaV := -(CharHeight + VSpace);
  if aReverseY then
    vBottomRight.V[1] := vTopLeft.V[1] + CharHeight
  else
    vBottomRight.V[1] := vTopLeft.V[1] - CharHeight;
  vBottomRight.V[2] := 0;
  vBottomRight.V[3] := 1;
  spaceDeltaH := GetCharWidth(#32) + HSpaceFix + HSpace;
  // set states
  with ARci.GLStates do
  begin
    ActiveTextureEnabled[ttTexture2D] := true;
    Disable(stLighting);
    Enable(stBlend);
    SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
    FLastTexture := nil;
  end;

  // start rendering
  GL.Color4fv(@aColor);
  GL.Begin_(GL_QUADS);
  for i := 1 to Length(aText) do
  begin
    currentChar := WideChar(aText[i]);
    case currentChar of
      #0 .. #12, #14 .. #31:
        ; // ignore
      #13:
        begin
          if Assigned(aPosition) then
            vTopLeft.V[0] := aPosition.V[0] + AlignmentAdjustement(i + 1)
          else
            vTopLeft.V[0] := AlignmentAdjustement(i + 1);
          vTopLeft.V[1] := vTopLeft.V[1] + deltaV;
          if aReverseY then
            vBottomRight.V[1] := vTopLeft.V[1] + CharHeight
          else
            vBottomRight.V[1] := vTopLeft.V[1] - CharHeight;
        end;
      #32:
        vTopLeft.V[0] := vTopLeft.V[0] + spaceDeltaH;
    else
      chi := CharacterToTileIndex(currentChar);
      if chi < 0 then
        continue; // not found
      pch := @FChars[chi];
      if pch.w > 0 then
        with GL do
        begin
          GetICharTexCoords(ARci, chi, TopLeft, BottomRight);
          vBottomRight.V[0] := vTopLeft.V[0] + pch.w;

          TexCoord2fv(@TopLeft);
          Vertex4fv(@vTopLeft);

          TexCoord2f(TopLeft.S, BottomRight.t);
          Vertex2f(vTopLeft.V[0], vBottomRight.V[1]);

          TexCoord2fv(@BottomRight);
          Vertex4fv(@vBottomRight);

          TexCoord2f(BottomRight.S, TopLeft.t);
          Vertex2f(vBottomRight.V[0], vTopLeft.V[1]);

          vTopLeft.V[0] := vTopLeft.V[0] + pch.w + HSpace;
        end;
    end;
  end;
  GL.End_;
  // unbind texture
  ARci.GLStates.TextureBinding[0, ttTexture2D] := 0;
  ARci.GLStates.ActiveTextureEnabled[ttTexture2D] := False;
end;

// TextOut
//
procedure TVKCustomBitmapFont.TextOut(var rci: TRenderContextInfo; X, Y: Single;
  const Text: UnicodeString; const Color: TColorVector);
var
  V: TVector;
begin
  V.X := X;
  V.Y := Y;
  V.Z := 0;
  V.w := 1;
  RenderString(rci, Text, taLeftJustify, tlTop, Color, @V, true);
end;

// TextOut
//

procedure TVKCustomBitmapFont.TextOut(var rci: TRenderContextInfo; X, Y: Single;
  const Text: UnicodeString; const Color: TColor);
begin
  TextOut(rci, X, Y, Text, ConvertWinColor(Color));
end;

// TextWidth
//
function TVKCustomBitmapFont.TextWidth(const Text: UnicodeString): Integer;
begin
  Result := CalcStringWidth(Text);
end;

// CharactersPerRow
//
function TVKCustomBitmapFont.CharactersPerRow: Integer;
begin
  if FGlyphs.Bitmap.Width > 0 then
    Result := (FGlyphs.Bitmap.Width + FGlyphsIntervalX)
      div (FGlyphsIntervalX + FCharWidth)
  else
    Result := 0;
end;

function TVKCustomBitmapFont.CharacterCount: Integer;
begin
  Result := FRanges.CharacterCount;
end;

procedure TVKCustomBitmapFont.GetCharTexCoords(Ch: WideChar;
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

// TileIndexToTexCoords
// it also activates the target texture
//
procedure TVKCustomBitmapFont.GetICharTexCoords(var ARci: TRenderContextInfo;
  Chi: Integer; out TopLeft, BottomRight: TTexPoint);
var
  tileIndex: Integer;
  ci: TCharInfo;
  t: TVKTextureHandle;
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
    with GL do
    begin
      FLastTexture := t;
      End_;
      ARci.GLStates.TextureBinding[0, ttTexture2D] := t.Handle;
      TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
      Begin_(GL_QUADS);
    end;
end;

// InvalidateUsers
//
procedure TVKCustomBitmapFont.InvalidateUsers;
var
  i: Integer;
begin
  FCharsLoaded := False;
  FTextureModified := true;
  for i := FUsers.Count - 1 downto 0 do
    TVKBaseSceneObject(FUsers[i]).NotifyChange(Self);
end;

// FreeTextureHandle
//
procedure TVKCustomBitmapFont.FreeTextureHandle;
var
  i: Integer;
begin
  FTextureModified := true;
  for i := 0 to FTextures.Count - 1 do
    TObject(FTextures[i]).Free;
  FTextures.Clear;
end;

procedure TVKCustomBitmapFont.TextureChanged;
begin
  FTextureModified := true;
end;

// force texture when needed
procedure TVKCustomBitmapFont.CheckTexture(var ARci: TRenderContextInfo);
var
  i: Integer;
begin
  // important: IsDataNeedUpdate might come from another source!
  for i := 0 to FTextures.Count - 1 do
    FTextureModified := FTextureModified or TVKTextureHandle(FTextures[i])
      .IsDataNeedUpdate;

  if FTextureModified then
  begin
    FreeTextureHandle; // instances are recreated in prepare
    PrepareImage(ARci);
    FTextureModified := False;
  end;
end;

// TextureFormat
//
function TVKCustomBitmapFont.TextureFormat: Integer;
begin
  Result := GL_RGBA;
end;

// ------------------
// ------------------ TVKFlatText ------------------
// ------------------

// Create
//
constructor TVKFlatText.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FModulateColor := TVKColor.CreateInitialized(Self, clrWhite);
end;

// Destroy
//
destructor TVKFlatText.Destroy;
begin
  FModulateColor.Free;
  BitmapFont := nil;
  inherited;
end;

// Notification
//
procedure TVKFlatText.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FBitmapFont) then
    BitmapFont := nil;
  inherited;
end;

// SetBitmapFont
//
procedure TVKFlatText.SetBitmapFont(const val: TVKCustomBitmapFont);
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

// SetText
//
procedure TVKFlatText.SetText(const val: UnicodeString);
begin
  FText := val;
  StructureChanged;
end;

// SetAlignment
//
procedure TVKFlatText.SetAlignment(const val: TAlignment);
begin
  FAlignment := val;
  StructureChanged;
end;

// SetLayout
//
procedure TVKFlatText.SetLayout(const val: TVKTextLayout);
begin
  FLayout := val;
  StructureChanged;
end;

// SetModulateColor
//
procedure TVKFlatText.SetModulateColor(const val: TVKColor);
begin
  FModulateColor.Assign(val);
end;

// SetOptions
//
procedure TVKFlatText.SetOptions(const val: TVKFlatTextOptions);
begin
  if val <> FOptions then
  begin
    FOptions := val;
    StructureChanged;
  end;
end;

// DoRender
//
procedure TVKFlatText.DoRender(var rci: TRenderContextInfo;
  renderSelf, renderChildren: boolean);
begin
  if Assigned(FBitmapFont) and (Text <> '') then
  begin
    rci.GLStates.PolygonMode := pmFill;
    if FModulateColor.Alpha <> 1 then
    begin
      rci.GLStates.Enable(stBlend);
      rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
    end;
    if ftoTwoSided in FOptions then
      rci.GLStates.Disable(stCullFace);
    FBitmapFont.RenderString(rci, Text, FAlignment, FLayout,
      FModulateColor.Color);
  end;
  if Count > 0 then
    Self.renderChildren(0, Count - 1, rci);
end;

// Assign
//
procedure TVKFlatText.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVKFlatText) then
  begin
    BitmapFont := TVKFlatText(Source).BitmapFont;
    Text := TVKFlatText(Source).Text;
    Alignment := TVKFlatText(Source).Alignment;
    Layout := TVKFlatText(Source).Layout;
    ModulateColor := TVKFlatText(Source).ModulateColor;
    Options := TVKFlatText(Source).Options;
  end;
  inherited Assign(Source);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// class registrations
RegisterClasses([TVKBitmapFont, TVKFlatText]);

end.
