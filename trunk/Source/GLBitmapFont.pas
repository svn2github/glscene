//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLBitmapFont<p>

  Bitmap Fonts management classes for GLScene<p>

 <b>History : </b><font size=-1><ul>
      <li>13/05/11 - Yar - Adapted to unicode (by Gabriel Corneanu)
      <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>05/07/10 - Yar - Now HSpace and VSpace can take negative values (thanks Sandor Domokos) (BugtrackerID = 3024975)
      <li>22/04/10 - Yar - Fixes after GLState revision
      <li>05/03/10 - DanB - More state added to TGLStateCache
      <li>24/02/10 - Yar - Bugfix in TGLCustomBitmapFont.PrepareImage when image is not RGBA8
      <li>25/01/10 - Yar - Replace Char to AnsiChar
      <li>11/11/09 - DaStr - Added Delphi 2009 compatibility (thanks mal)
      <li>16/10/08 - UweR - Removed unneeded typecast in TBitmapFontRange.SetStartGlyphIdx
      <li>06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
      <li>30/03/07 - DaStr - Added $I GLScene.inc
      <li>22/12/06 - LC - Fixed TGLCustomBitmapFont.RenderString, it now unbinds the texture.
                          Bugtracker ID=1619243 (thanks Da Stranger)
      <li>09/03/05 - EG - Fixed space width during rendering
      <li>12/15/04 - Eugene Kryukov - Moved FCharRects to protected declaration in TGLCustomBitmapFont
      <li>18/10/04 - NelC - Fixed a texture reset bug in RenderString
      <li>28/06/04 - LR - Change TTextLayout to TGLTextLayout for Linux
      <li>27/06/04 - NelC - Added TGLFlatText.Assign
      <li>01/03/04 - SG - TGLCustomBitmapFont.RenderString now saves GL_CURRENT_BIT state
      <li>01/07/03 - EG - TGLCustomBitmapFont.TextOut now saves and restore state
      <li>07/05/03 - EG - TGLFlatText Notification fix, added Options
      <li>30/10/02 - EG - Added TGLFlatText
      <li>29/09/02 - EG - Added TexCoords LUT, faster RenderString,
                          removed TBitmapFontRange.Widths
      <li>28/09/02 - EG - Introduced TGLCustomBitmapFont
      <li>06/09/02 - JAJ - Prepared for TGLWindowsBitmapFont
      <li>28/08/02 - EG - Repaired fixed CharWidth, variable CharWidth not yet repaired
      <li>12/08/02 - JAJ - Merged Dual Development, Alpha Channel and CharWidth are now side by side
      <li>UNKNOWN  - EG - Added Alpha Channel.
      <li>02/06/02 - JAJ - Modified to flexible character width
      <li>20/01/02 - EG - Dropped 'Graphics' dependency
      <li>10/09/01 - EG - Fixed visibility of tile 0
      <li>12/08/01 - EG - Completely rewritten handles management
      <li>21/02/01 - EG - Now XOpenGL based (multitexture)
    <li>15/01/01 - EG - Creation
 </ul></font>
}
unit GLBitmapFont;

{$I GLScene.inc}

interface

uses
  Classes, Graphics,
  GLScene, VectorGeometry, GLContext, GLCrossPlatform,
  GLTexture, GLState, GLUtils, GLGraphics, GLColor, BaseClasses,
  GLRenderContextInfo, GLTextureFormat;

type

  // TBitmapFontRange
  //
    {: An individual character range in a bitmap font.<p>
       A range allows mapping ASCII characters to character tiles in a font
       bitmap, tiles are enumerated line then column (raster). }
  TBitmapFontRange = class(TCollectionItem)
  private
    { Private Declarations }
    FStartASCII, FStopASCII: widechar;
    FStartGlyphIdx, FStopGlyphIdx, FCharCount: Integer;

  protected
    { Protected Declarations }
    procedure SetStartASCII(const val: widechar);
    procedure SetStopASCII(const val: widechar);
    procedure SetStartGlyphIdx(const val: Integer);
    function GetDisplayName: string; override;

  public
    { Public Declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange;
  published
    { Published Declarations }
     property StartASCII: widechar read FStartASCII write SetStartASCII;
     property StopASCII: widechar read FStopASCII write SetStopASCII;
    property StartGlyphIdx: Integer read FStartGlyphIdx write SetStartGlyphIdx;
     property StopGlyphIdx: Integer read FStopGlyphIdx;
     property CharCount: integer read FCharCount;
  end;

  // TBitmapFontRanges
  //
  TBitmapFontRanges = class(TCollection)
  private
    FCharCount : integer;
  protected
    { Protected Declarations }
    FOwner: TComponent;

    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TBitmapFontRange);
    function GetItems(index: Integer): TBitmapFontRange;
    function CalcCharacterCount: Integer;
    procedure Update(Item: TCollectionItem); override;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    function Add: TBitmapFontRange; overload;
    function Add(startASCII, stopASCII: widechar): TBitmapFontRange; overload;
    function FindItemID(ID: Integer): TBitmapFontRange;
    property Items[index: Integer]: TBitmapFontRange read GetItems write SetItems; default;

    {: Converts an ASCII character into a tile index.<p>
       Return -1 if character cannot be rendered. }
    function CharacterToTileIndex(aChar: widechar): Integer;
    function TileIndexToChar(aIndex: integer): widechar;
    procedure NotifyChange;

         //: Total number of characters in the ranges; cached for performance
    property CharacterCount: Integer read FCharCount;
  end;

  TDynIntegerArray = array of Integer;

  // TGLCustomBitmapFont
  //
    {: Provides access to individual characters in a BitmapFont.<p>
       Only fixed-width bitmap fonts are supported, the characters are enumerated
       in a raster fashion (line then column).<br>
       Transparency is all or nothing, the transparent color being that of the
       top left pixel of the Glyphs bitmap.<p>
       Performance note: as usual, for best performance, you base font bitmap
       dimensions should be close to a power of two, and have at least 1 pixel
       spacing between characters (horizontally and vertically) to avoid artefacts
       when rendering with linear filtering. }
  TGLCustomBitmapFont = class(TGLUpdateAbleComponent)
  private
    { Private Declarations }
    FRanges: TBitmapFontRanges;
    FGlyphs: TGLPicture;
    FCharWidth, FCharHeight: Integer;
    FGlyphsIntervalX, FGlyphsIntervalY: Integer;
    FHSpace, FVSpace, FHSpaceFix: Integer;
    FUsers: TList;
    FMinFilter: TGLMinFilter;
    FMagFilter: TGLMagFilter;
    FTextureWidth, FTextureHeight: Integer;
    FGlyphsAlpha: TGLTextureImageAlpha;
  protected
    { Protected Declarations }
    FCharRects: array of TVector;
    FCharWidths: TDynIntegerArray;
    FTextureHandle: TGLTextureHandle;
    property CharWidths: TDynIntegerArray read FCharWidths write FCharWidths;
    procedure ResetCharWidths(w: Integer = -1);
    procedure SetCharWidths(index, value: Integer);
    procedure SetCharRects(index: Integer; const p: TVector);

    procedure SetRanges(const val: TBitmapFontRanges);
    procedure SetGlyphs(const val: TGLPicture);
    procedure SetCharWidth(const val: Integer);
    procedure SetCharHeight(const val: Integer);
    procedure SetGlyphsIntervalX(const val: Integer);
    procedure SetGlyphsIntervalY(const val: Integer);
    procedure OnGlyphsChanged(Sender: TObject);
    procedure SetHSpace(const val: Integer);
    procedure SetVSpace(const val: Integer);
    procedure SetMagFilter(AValue: TGLMagFilter);
    procedure SetMinFilter(AValue: TGLMinFilter);
    procedure SetGlyphsAlpha(val: TGLTextureImageAlpha);

    procedure FreeTextureHandle; dynamic;
    function TextureFormat: Integer; dynamic;

    procedure InvalidateUsers;
    function CharactersPerRow: Integer;
    procedure GetCharTexCoords(ch: widechar; var topLeft, bottomRight: TTexPoint);
    procedure GetICharTexCoords(chi: integer; var topLeft, bottomRight: TTexPoint);
    procedure PrepareImage; virtual;
    procedure PrepareParams(var rci: TRenderContextInfo);

    {: A single bitmap containing all the characters.<p>
       The transparent color is that of the top left pixel. }
    property Glyphs: TGLPicture read FGlyphs write SetGlyphs;
    {: Nb of horizontal pixels between two columns in the Glyphs. }
    property GlyphsIntervalX: Integer read FGlyphsIntervalX write SetGlyphsIntervalX;
    {: Nb of vertical pixels between two rows in the Glyphs. }
    property GlyphsIntervalY: Integer read FGlyphsIntervalY write SetGlyphsIntervalY;
    {: Ranges allow converting between ASCII and tile indexes.<p>
       See TGLCustomBitmapFontRange. }
    property Ranges: TBitmapFontRanges read FRanges write SetRanges;

    {: Width of a single character. }
    property CharWidth: Integer read FCharWidth write SetCharWidth default 16;
    {: Pixels in between rendered characters (horizontally). }
    property HSpace: Integer read FHSpace write SetHSpace default 1;
    {: Pixels in between rendered lines (vertically). }
    property VSpace: Integer read FVSpace write SetVSpace default 1;
    {: Horizontal spacing fix offset.<p>
       This property is for internal use, and is added to the hspacing
       of each character when rendering, typically to fix extra spacing. }
    property HSpaceFix: Integer read FHSpaceFix write FHSpaceFix;

    property MagFilter: TGLMagFilter read FMagFilter write SetMagFilter default maLinear;
    property MinFilter: TGLMinFilter read FMinFilter write SetMinFilter default miLinear;
    property GlyphsAlpha: TGLTextureImageAlpha read FGlyphsAlpha write FGlyphsAlpha default tiaDefault;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RegisterUser(anObject: TGLBaseSceneObject); virtual;
    procedure UnRegisterUser(anObject: TGLBaseSceneObject); virtual;

    {: Renders the given string at current position or at position given by the optional position variable.<p>
       The current matrix is blindly used, meaning you can render all kinds
       of rotated and linear distorted text with this method, OpenGL
       Enable states are also possibly altered. }
    procedure RenderString(var rci: TRenderContextInfo;
      const aText: UnicodeString; aAlignment: TAlignment;
      aLayout: TGLTextLayout; const aColor: TColorVector;
      aPosition: PVector = nil; aReverseY: Boolean = False); overload; virtual;
    {: Wide string variant. Warning - dummy method. }
    Procedure RenderString(var rci: TRenderContextInfo;
      const aText: WideString; aAlignment: TAlignment;
      aLayout: TGLTextLayout; const aColor: TColorVector;
      aPosition: PVector = nil; aReverseY: Boolean = False); overload; virtual;

    {: A simpler canvas-style TextOut helper for RenderString.<p>
       The rendering is reversed along Y by default, to allow direct use
       with TGLCanvas }
    procedure TextOut(var rci: TRenderContextInfo; x, y: Single; const text: UnicodeString; const color: TColorVector); overload;
    procedure TextOut(var rci: TRenderContextInfo; x, y: Single; const text: UnicodeString; const color: TColor); overload;
    function TextWidth(const text: UnicodeString): Integer;

    function  CharacterToTileIndex(aChar: widechar): Integer; virtual;
    function  TileIndexToChar(aIndex: integer): widechar; virtual;
    function  CharacterCount: integer; virtual;

    {: Get the actual width for this char. }
    function  GetCharWidth(ch: widechar): Integer;
    {: Get the actual pixel width for this string. }
    function  CalcStringWidth(const aText: UnicodeString): Integer; overload; virtual;

    // make texture if needed
    procedure CheckTexture(var rci: TRenderContextInfo);

    {: Wide string variant. Warning - dummy method. }
    function CalcStringWidth(const aText: WideString): Integer; overload; virtual;

    {: Height of a single character. }
    property CharHeight: Integer read FCharHeight write SetCharHeight default 16;
  end;

  // TGLBitmapFont
  //
    {: See TGLCustomBitmapFont.<p>
       This class only publuishes some of the properties. }
  TGLBitmapFont = class(TGLCustomBitmapFont)
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

  // TGLFlatTextOptions
  //
  TGLFlatTextOption = (ftoTwoSided);
  TGLFlatTextOptions = set of TGLFlatTextOption;

  // TGLFlatText
  //
  {: A 2D text displayed and positionned in 3D coordinates.<p>
     The FlatText uses a character font defined and stored by a TGLBitmapFont
     component. Default character scale is 1 font pixel = 1 space unit. }
  TGLFlatText = class(TGLImmaterialSceneObject)
  private
    { Private Declarations }
    FBitmapFont: TGLCustomBitmapFont;
    FText: UnicodeString;
    FAlignment: TAlignment;
    FLayout: TGLTextLayout;
    FModulateColor: TGLColor;
    FOptions: TGLFlatTextOptions;

  protected
    { Protected Declarations }
    procedure SetBitmapFont(const val: TGLCustomBitmapFont);
    procedure SetText(const val: UnicodeString);
    procedure SetAlignment(const val: TAlignment);
    procedure SetLayout(const val: TGLTextLayout);
    procedure SetModulateColor(const val: TGLColor);
    procedure SetOptions(const val: TGLFlatTextOptions);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoRender(var rci: TRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;

    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }
      {: Refers the bitmap font to use.<p>
         The referred bitmap font component stores and allows access to
         individual character bitmaps. }
    property BitmapFont: TGLCustomBitmapFont read FBitmapFont write SetBitmapFont;
    {: Text to render.<p>
       Be aware that only the characters available in the bitmap font will
       be rendered. CR LF sequences are allowed. }
    property Text: UnicodeString read FText write SetText;
    {: Controls the text alignment (horizontal).<p>
       Possible values : taLeftJustify, taRightJustify, taCenter }
    property Alignment: TAlignment read FAlignment write SetAlignment;
    {: Controls the text layout (vertical).<p>
       Possible values : tlTop, tlCenter, tlBottom }
    property Layout: TGLTextLayout read FLayout write SetLayout;
    {: Color modulation, can be used for fade in/out too.}
    property ModulateColor: TGLColor read FModulateColor write SetModulateColor;
    {: Flat text options.<p>
       <ul><li>ftoTwoSided : when set the text will be visible from its two
       sides even if faceculling is on (at the scene-level).
       </ul> }
    property Options: TGLFlatTextOptions read FOptions write SetOptions;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGLTokens, XOpenGL {$IFDEF GLS_DELPHI}, VectorTypes{$ENDIF};

// ------------------
// ------------------ TBitmapFontRange ------------------
// ------------------

// Create
//

constructor TBitmapFontRange.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

// Destroy
//

destructor TBitmapFontRange.Destroy;
begin
  inherited;
end;

// Assign
//

procedure TBitmapFontRange.Assign(Source: TPersistent);
begin
  if Source is TBitmapFontRange then
  begin
    FStartASCII := TBitmapFontRange(Source).FStartASCII;
    FStopASCII := TBitmapFontRange(Source).FStopASCII;
    FStartGlyphIdx := TBitmapFontRange(Source).FStartGlyphIdx;
    NotifyChange;
  end;
  inherited;
end;

// NotifyChange
//

procedure TBitmapFontRange.NotifyChange;
begin
  FCharCount    := integer(FStopASCII) - integer(FStartASCII) + 1;
  FStopGlyphIdx := FStartGlyphIdx + FCharCount - 1;
  if Assigned(Collection) then
    (Collection as TBitmapFontRanges).NotifyChange;
end;

// GetDisplayName
//

function TBitmapFontRange.GetDisplayName: string;
begin
  Result := Format('ASCII [#%d, #%d] -> Glyphs [%d, %d]',
                  [Integer(StartASCII), Integer(StopASCII), StartGlyphIdx, StopGlyphIdx]);
end;

// SetStartASCII
//

procedure TBitmapFontRange.SetStartASCII(const val: widechar);
begin
  if val <> FStartASCII then
  begin
    FStartASCII := val;
    if FStartASCII > FStopASCII then
      FStopASCII := FStartASCII;
    NotifyChange;
  end;
end;

// SetStopASCII
//

procedure TBitmapFontRange.SetStopASCII(const val: widechar);
begin
  if FStopASCII <> val then
  begin
    FStopASCII := val;
    if FStopASCII < FStartASCII then
      FStartASCII := FStopASCII;
    NotifyChange;
  end;
end;

// SetStartGlyphIdx
//

procedure TBitmapFontRange.SetStartGlyphIdx(const val: Integer);
begin
  if val >= 0 then
    FStartGlyphIdx := val
  else FStartGlyphIdx := 0;
  NotifyChange;
end;

// ------------------
// ------------------ TBitmapFontRanges ------------------
// ------------------

// Create
//

constructor TBitmapFontRanges.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  inherited Create(TBitmapFontRange);
end;

// Destroy
//

destructor TBitmapFontRanges.Destroy;
begin
  inherited;
end;

// GetOwner
//

function TBitmapFontRanges.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// SetItems
//

procedure TBitmapFontRanges.SetItems(index: Integer; const val: TBitmapFontRange);
begin
  inherited Items[index] := val;
end;

// GetItems
//

function TBitmapFontRanges.GetItems(index: Integer): TBitmapFontRange;
begin
  Result := TBitmapFontRange(inherited Items[index]);
end;

// Add
//

function TBitmapFontRanges.Add: TBitmapFontRange;
begin
  Result := (inherited Add) as TBitmapFontRange;
end;

// Add
//

function TBitmapFontRanges.Add(startASCII, stopASCII: widechar): TBitmapFontRange;
begin
  Result := Add;
  Result.StartASCII := startASCII;
  Result.StopASCII := stopASCII;
end;

// FindItemID
//

function TBitmapFontRanges.FindItemID(ID: Integer): TBitmapFontRange;
begin
  Result := (inherited FindItemID(ID)) as TBitmapFontRange;
end;

// CharacterToTileIndex
//

function TBitmapFontRanges.CharacterToTileIndex(aChar: widechar): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    with Items[i] do
    begin
      if (aChar >= StartASCII) and (aChar <= StopASCII) then
      begin
        Result := StartGlyphIdx + Integer(aChar) - Integer(StartASCII);
        Break;
      end;
    end;
end;

function TBitmapFontRanges.TileIndexToChar(aIndex: integer): widechar;
var
  i: Integer;
begin
  Result := #0;
  for i := 0 to Count - 1 do
    with Items[i] do
    begin
      if (aIndex >= StartGlyphIdx) and (aIndex <= StopGlyphIdx) then
      begin
        Result := widechar(aIndex - StartGlyphIdx + Integer(StartASCII));
        Break;
      end;
    end;
end;

procedure TBitmapFontRanges.Update(Item: TCollectionItem);
begin
  inherited;
  NotifyChange;
end;

// NotifyChange
//

procedure TBitmapFontRanges.NotifyChange;
begin
  FCharCount := CalcCharacterCount;

  if Assigned(FOwner) and (FOwner is TGLBaseSceneObject) then
    TGLBaseSceneObject(FOwner).StructureChanged;
end;

// CharacterCount
//

function TBitmapFontRanges.CalcCharacterCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    with Items[i] do
      Inc(Result, Integer(StopASCII) - Integer(StartASCII) + 1);
end;

// ------------------
// ------------------ TGLCustomBitmapFont ------------------
// ------------------

// Create
//

constructor TGLCustomBitmapFont.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRanges := TBitmapFontRanges.Create(Self);
  FGlyphs := TGLPicture.Create;
  FGlyphs.OnChange := OnGlyphsChanged;
  FCharWidth := 16;
  FCharHeight := 16;
  FHSpace := 1;
  FVSpace := 1;
  FUsers := TList.Create;
  FMinFilter := miLinear;
  FMagFilter := maLinear;
  FTextureHandle := TGLTextureHandle.Create;
end;

// Destroy
//

destructor TGLCustomBitmapFont.Destroy;
begin
  inherited Destroy;
  FTextureHandle.Free;
  FRanges.Free;
  FGlyphs.Free;
  Assert(FUsers.Count = 0);
  FUsers.Free;
end;

// force texture when needed
procedure TGLCustomBitmapFont.CheckTexture(var rci: TRenderContextInfo);
begin
  if FTextureHandle.IsDataNeedUpdate then
  begin
    // prepare handle
    FTextureHandle.AllocateHandle;
    // texture registration
    FTextureHandle.Target := ttTexture2D;
    rci.GLStates.TextureBinding[0, ttTexture2D] := FTextureHandle.Handle;
    PrepareImage;
    PrepareParams(rci);
    FTextureHandle.NotifyDataUpdated;
  end;
end;

// GetCharWidth
//

function TGLCustomBitmapFont.GetCharWidth(ch: widechar): Integer;
var
  chi: Integer;
begin
  if Length(FCharWidths) = 0 then
    ResetCharWidths;
  chi := CharacterToTileIndex(ch);
  if chi >= 0 then
       Result := FCharWidths[chi]
  else Result := 0;
end;

// CalcStringWidth
//

function TGLCustomBitmapFont.CalcStringWidth(const aText: UnicodeString): Integer;
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

function TGLCustomBitmapFont.CalcStringWidth(const aText: WideString): Integer;
begin
  Result := CalcStringWidth(UnicodeString(aText));
end;

// ResetCharWidths
//

procedure TGLCustomBitmapFont.ResetCharWidths(w: Integer = -1);
var
  i: Integer;
begin
  i := CharacterCount;
  if i > GLS_FONT_CHARS_COUNT then
    i := GLS_FONT_CHARS_COUNT;

  if Length(FCharWidths) < i then
      SetLength(FCharWidths, i);
  if w < 0 then w := CharWidth;
  for i := 0 to High(FCharWidths) do
    FCharWidths[i] := w;
end;

// SetCharWidths
//

procedure TGLCustomBitmapFont.SetCharWidths(index, value: Integer);
begin
  if index >= 0 then
    FCharWidths[index] := value;
end;

// SetCharRects
//

procedure TGLCustomBitmapFont.SetCharRects(index: Integer; const p: TVector);
begin
  if Length(FCharRects) < CharacterCount then
    Setlength(FCharRects, CharacterCount);
  if index < CharacterCount then
    FCharRects[index] := p;
end;

// SetRanges
//

procedure TGLCustomBitmapFont.SetRanges(const val: TBitmapFontRanges);
begin
  FRanges.Assign(val);
  InvalidateUsers;
end;

// SetGlyphs
//

procedure TGLCustomBitmapFont.SetGlyphs(const val: TGLPicture);
begin
  FGlyphs.Assign(val);
end;

// SetCharWidth
//

procedure TGLCustomBitmapFont.SetCharWidth(const val: Integer);
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

procedure TGLCustomBitmapFont.SetCharHeight(const val: Integer);
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

procedure TGLCustomBitmapFont.SetGlyphsIntervalX(const val: Integer);
begin
  if val > 0 then
    FGlyphsIntervalX := val
  else
    FGlyphsIntervalX := 0;
  InvalidateUsers;
end;

// SetGlyphsIntervalY
//

procedure TGLCustomBitmapFont.SetGlyphsIntervalY(const val: Integer);
begin
  if val > 0 then
    FGlyphsIntervalY := val
  else
    FGlyphsIntervalY := 0;
  InvalidateUsers;
end;

// SetHSpace
//

procedure TGLCustomBitmapFont.SetHSpace(const val: Integer);
begin
  if val <> FHSpace then
  begin
    FHSpace := val;
    InvalidateUsers;
  end;
end;

// SetVSpace
//

procedure TGLCustomBitmapFont.SetVSpace(const val: Integer);
begin
  if val <> FVSpace then
  begin
    FVSpace := val;
    InvalidateUsers;
  end;
end;

// SetMagFilter
//

procedure TGLCustomBitmapFont.SetMagFilter(AValue: TGLMagFilter);
begin
  if AValue <> FMagFilter then
  begin
    FMagFilter := AValue;
    FTextureHandle.NotifyChangesOfData;
    InvalidateUsers;
  end;
end;

// SetMinFilter
//

procedure TGLCustomBitmapFont.SetMinFilter(AValue: TGLMinFilter);
begin
  if AValue <> FMinFilter then
  begin
    FMinFilter := AValue;
    FTextureHandle.NotifyChangesOfData;
    InvalidateUsers;
  end;
end;

// SetGlyphsAlpha
//

procedure TGLCustomBitmapFont.SetGlyphsAlpha(val: TGLTextureImageAlpha);
begin
  if val <> FGlyphsAlpha then
  begin
    FGlyphsAlpha := val;
    FTextureHandle.NotifyChangesOfData;
    InvalidateUsers;
  end;
end;

// OnGlyphsChanged
//

procedure TGLCustomBitmapFont.OnGlyphsChanged(Sender: TObject);
begin
  InvalidateUsers;
end;

// RegisterUser
//

procedure TGLCustomBitmapFont.RegisterUser(anObject: TGLBaseSceneObject);
begin
  Assert(FUsers.IndexOf(anObject) < 0);
  FUsers.Add(anObject);
end;

// UnRegisterUser
//

procedure TGLCustomBitmapFont.UnRegisterUser(anObject: TGLBaseSceneObject);
begin
  FUsers.Remove(anObject);
end;

// PrepareImage
//

procedure TGLCustomBitmapFont.PrepareImage;
var
  bitmap: TGLBitmap;
  bitmap32: TGLBitmap32;
  cap: Integer;
begin
  bitmap := TGLBitmap.Create;
  with bitmap do
  begin
    PixelFormat := glpf24bit;
    Width := RoundUpToPowerOf2(Glyphs.Width);
    Height := RoundUpToPowerOf2(Glyphs.Height);
    Canvas.Draw(0, 0, Glyphs.Graphic);
  end;
  bitmap32 := TGLBitmap32.Create;
  bitmap32.Assign(bitmap);
  bitmap.Free;
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
      tiaDefault,
        tiaTopLeftPointColorTransparent:
        SetAlphaTransparentForColor(Data[Width * (Height - 1)]);
    else
      Assert(False);
    end;
    RegisterAsOpenGLTexture(
      FTextureHandle,
      not (FMinFilter in [miNearest, miLinear]),
      TextureFormat,
      FTextureWidth,
      FTextureHeight,
      cap);
    Free;
  end;
end;

// PrepareParams
//

procedure TGLCustomBitmapFont.PrepareParams(var rci: TRenderContextInfo);
const
  cTextureMagFilter: array[maNearest..maLinear] of TGLEnum =
    (GL_NEAREST, GL_LINEAR);
  cTextureMinFilter: array[miNearest..miLinearMipmapLinear] of TGLEnum =
    (GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST,
    GL_LINEAR_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_LINEAR,
    GL_LINEAR_MIPMAP_LINEAR);
begin

  with rci.GLStates do
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

    TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, cTextureMinFilter[FMinFilter]);
    TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, cTextureMagFilter[FMagFilter]);
  end;
end;

function TGLCustomBitmapFont.TileIndexToChar(aIndex: integer): widechar;
begin
  Result := FRanges.TileIndexToChar(aIndex);
end;

function TGLCustomBitmapFont.CharacterToTileIndex(aChar: widechar): Integer;
begin
  Result := FRanges.CharacterToTileIndex(aChar);
end;

// RenderString
//

procedure TGLCustomBitmapFont.RenderString(var rci: TRenderContextInfo;
      const aText: UnicodeString; aAlignment: TAlignment;
      aLayout: TGLTextLayout; const aColor: TColorVector;
      aPosition: PVector = nil; aReverseY: Boolean = False);

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
      taLeftJustify: Result := 0;
      taRightJustify: Result := -CalcStringWidth(Copy(aText, p - i, i))
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
    case TGLTextLayout(aLayout) of
      tlTop: Result := 0;
      tlBottom: Result := (n * (CharHeight + VSpace) - VSpace);
    else // tlCenter
      Result := Round((n * (CharHeight + VSpace) - VSpace) * 0.5);
    end;
  end;

var
  i, chi: Integer;
  topLeft, bottomRight: TTexPoint;
  vTopLeft, vBottomRight: TVector;
  deltaH, deltaV, spaceDeltaH: Single;
  currentChar: widechar;
begin
  if (aText = '') then
    Exit;
  // prepare texture if necessary
  CheckTexture(rci);
  // precalcs
  if Assigned(aPosition) then
    MakePoint(vTopLeft, aPosition[0] + AlignmentAdjustement(1), aPosition[1] + LayoutAdjustement, 0)
  else
    MakePoint(vTopLeft, AlignmentAdjustement(1), LayoutAdjustement, 0);
  deltaV := -(CharHeight + VSpace);
  if aReverseY then
    vBottomRight[1] := vTopLeft[1] + CharHeight
  else
    vBottomRight[1] := vTopLeft[1] - CharHeight;
  vBottomRight[2] := 0;
  vBottomRight[3] := 1;
  spaceDeltaH := GetCharWidth(#32) + HSpaceFix + HSpace;
  // set states
  with rci.GLStates do
  begin
    ActiveTextureEnabled[ttTexture2D] := True;
    Disable(stLighting);
    Enable(stBlend);
    SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
    TextureBinding[0, ttTexture2D] := FTextureHandle.Handle;
  end;

  GL.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  // start rendering
  GL.Color4fv(@aColor);
  GL.Begin_(GL_QUADS);
  for i := 1 to Length(aText) do
  begin
    currentChar := aText[i];
    case currentChar of
      #0..#12, #14..#31: ; // ignore
      #13:
        begin
          if Assigned(aPosition) then
            vTopLeft[0] := aPosition[0] + AlignmentAdjustement(i + 1)
          else
            vTopLeft[0] := AlignmentAdjustement(i + 1);
          vTopLeft[1] := vTopLeft[1] + deltaV;
          if aReverseY then
            vBottomRight[1] := vTopLeft[1] + CharHeight
          else
            vBottomRight[1] := vTopLeft[1] - CharHeight;
        end;
      #32: vTopLeft[0] := vTopLeft[0] + spaceDeltaH;
    else
      chi := CharacterToTileIndex(currentChar);
      if chi < 0 then continue; //not found
      deltaH := CharWidths[chi];
      if deltaH > 0 then
      with GL do
      begin
        GetICharTexCoords(chi, topLeft, bottomRight);
        vBottomRight[0] := vTopLeft[0] + deltaH;

        TexCoord2fv(@topLeft);
        Vertex4fv(@vTopLeft);

        TexCoord2f(topLeft.S, bottomRight.T);
        Vertex2f(vTopLeft[0], vBottomRight[1]);

        TexCoord2fv(@bottomRight);
        Vertex4fv(@vBottomRight);

        TexCoord2f(bottomRight.S, topLeft.T);
        Vertex2f(vBottomRight[0], vTopLeft[1]);

        vTopLeft[0] := vTopLeft[0] + deltaH + HSpace;
      end;
    end;
  end;
  GL.End_;
  // unbind texture
  rci.GLStates.TextureBinding[0, ttTexture2d] := 0;
  rci.GLStates.ActiveTextureEnabled[ttTexture2D] := False;
end;

Procedure TGLCustomBitmapFont.RenderString(var rci: TRenderContextInfo;
      const aText: WideString; aAlignment: TAlignment;
      aLayout: TGLTextLayout; const aColor: TColorVector;
      aPosition: PVector = nil; aReverseY: Boolean = False);
begin
  RenderString(rci, UnicodeString(aText), aAlignment, aLayout, aColor, aPosition, aReverseY);
end;

// TextOut
//

procedure TGLCustomBitmapFont.TextOut(var rci: TRenderContextInfo;
  x, y: Single; const text: UnicodeString; const color: TColorVector);
var
  v: TVector;
begin
  v[0] := x;
  v[1] := y;
  v[2] := 0;
  v[3] := 1;
  RenderString(rci, text, taLeftJustify, tlTop, color, @v, True);
end;

// TextOut
//

procedure TGLCustomBitmapFont.TextOut(var rci: TRenderContextInfo;
  x, y: Single; const text: UnicodeString; const color: TColor);
begin
  TextOut(rci, x, y, text, ConvertWinColor(color));
end;

// TextWidth
//

function TGLCustomBitmapFont.TextWidth(const text: UnicodeString): Integer;
begin
  Result := CalcStringWidth(text);
end;

// CharactersPerRow
//

function TGLCustomBitmapFont.CharactersPerRow: Integer;
begin
  if FGlyphs.Width > 0 then
    Result := (FGlyphs.Width + FGlyphsIntervalX) div (FGlyphsIntervalX + FCharWidth)
  else
    Result := 0;
end;

function TGLCustomBitmapFont.CharacterCount: integer;
begin
  Result := FRanges.CharacterCount;
end;

procedure TGLCustomBitmapFont.GetCharTexCoords(ch: widechar; var topLeft, bottomRight: TTexPoint);
begin
  GetICharTexCoords(CharacterToTileIndex(ch), topLeft, bottomRight);
end;

// TileIndexToTexCoords
//

procedure TGLCustomBitmapFont.GetICharTexCoords(chi: integer; var topLeft, bottomRight: TTexPoint);
var
  carX, carY: Integer;
  tileIndex: Integer;
  p: PVector;
begin
  if Length(FCharRects) = 0 then
  begin
    ResetCharWidths;
    SetLength(FCharRects, CharacterCount);
    for tileIndex := 0 to CharacterCount - 1 do
    begin
      p := @FCharRects[tileIndex];
      carX := (tileIndex mod CharactersPerRow) * (CharWidth + GlyphsIntervalX);
      carY := (tileIndex div CharactersPerRow) * (CharHeight + GlyphsIntervalY);
      p[0] := (carX + 0.05) / FTextureWidth;
      p[1] := (FTextureHeight - (carY + 0.05)) / FTextureHeight;
      p[2] := (carX + CharWidths[tileIndex] - 0.05) / FTextureWidth;
      p[3] := (FTextureHeight - (carY + CharHeight - 0.05)) / FTextureHeight;
    end;
  end;

  if chi < 0 then
  begin
    //invalid char
    topLeft := NullTexPoint;
    bottomRight := NullTexPoint;
    exit;
  end;
  if chi >= Length(FCharRects) then
    chi := 0;
  p := @FCharRects[chi];
  topLeft.S := p[0];
  topLeft.T := p[1];
  bottomRight.S := p[2];
  bottomRight.T := p[3];
end;

// InvalidateUsers
//

procedure TGLCustomBitmapFont.InvalidateUsers;
var
  i: Integer;
begin
  ResetCharWidths;
  SetLength(FCharRects, 0);
  for i := FUsers.Count - 1 downto 0 do
    TGLBaseSceneObject(FUsers[i]).NotifyChange(Self);
end;

// FreeTextureHandle
//

procedure TGLCustomBitmapFont.FreeTextureHandle;
begin
  FTextureHandle.DestroyHandle;
end;

// TextureFormat
//

function TGLCustomBitmapFont.TextureFormat: Integer;
begin
  Result := GL_RGBA;
end;

// ------------------
// ------------------ TGLFlatText ------------------
// ------------------

// Create
//

constructor TGLFlatText.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FModulateColor := TGLColor.CreateInitialized(Self, clrWhite);
end;

// Destroy
//

destructor TGLFlatText.Destroy;
begin
  FModulateColor.Free;
  BitmapFont := nil;
  inherited;
end;

// Notification
//

procedure TGLFlatText.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FBitmapFont) then
    BitmapFont := nil;
  inherited;
end;

// SetBitmapFont
//

procedure TGLFlatText.SetBitmapFont(const val: TGLCustomBitmapFont);
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

procedure TGLFlatText.SetText(const val: UnicodeString);
begin
  FText := val;
  StructureChanged;
end;

// SetAlignment
//

procedure TGLFlatText.SetAlignment(const val: TAlignment);
begin
  FAlignment := val;
  StructureChanged;
end;

// SetLayout
//

procedure TGLFlatText.SetLayout(const val: TGLTextLayout);
begin
  FLayout := val;
  StructureChanged;
end;

// SetModulateColor
//

procedure TGLFlatText.SetModulateColor(const val: TGLColor);
begin
  FModulateColor.Assign(val);
end;

// SetOptions
//

procedure TGLFlatText.SetOptions(const val: TGLFlatTextOptions);
begin
  if val <> FOptions then
  begin
    FOptions := val;
    StructureChanged;
  end;
end;

// DoRender
//

procedure TGLFlatText.DoRender(var rci: TRenderContextInfo;
  renderSelf, renderChildren: Boolean);
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
    FBitmapFont.RenderString(rci, Text, FAlignment, FLayout, FModulateColor.Color);
  end;
  if Count > 0 then
    Self.RenderChildren(0, Count - 1, rci);
end;

// Assign
//

procedure TGLFlatText.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLFlatText) then
  begin
    BitmapFont := TGLFlatText(Source).BitmapFont;
    Text := TGLFlatText(Source).Text;
    Alignment := TGLFlatText(Source).Alignment;
    Layout := TGLFlatText(Source).Layout;
    ModulateColor := TGLFlatText(Source).ModulateColor;
    Options := TGLFlatText(Source).Options;
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
  RegisterClasses([TGLBitmapFont, TGLFlatText]);

end.

