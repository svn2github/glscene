//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLBitmapFont<p>

  Bitmap Fonts management classes for GLScene<p>

 <b>History : </b><font size=-1><ul>
      <li>16/05/11 - Yar - Redesign to use multiple textures (by Gabriel Corneanu)
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

{$IFDEF GLS_DELPHI_2009_UP}
{$DEFINE GLS_UNICODE_SUPPORT}
{$ENDIF}
{$IFDEF FPC}
{$DEFINE GLS_UNICODE_SUPPORT}
{$ENDIF}

interface

uses
  Classes,
  Graphics,
  GLScene,
  VectorGeometry,
  GLContext,
  GLCrossPlatform,
  GLState,
  GLUtils,
  GLGraphics,
  GLColor,
  BaseClasses,
  GLTexture,
  GLMaterial,
  GLS_Material,
  GLRenderContextInfo,
  GLPipelineTransformation,
  GLS_Mesh,
  GLS_DrawTechnique,
  GLTextureFormat,
  XCollection;

type
  TGLCustomBitmapFont = class;

{$IFNDEF GLS_UNICODE_SUPPORT}
  UnicodeString = WideString; //Use WideString for earlier versions
{$ENDIF}

  // TBitmapFontRange
  //
    {: An individual character range in a bitmap font.<p>
       A range allows mapping ASCII characters to character tiles in a font
       bitmap, tiles are enumerated line then column (raster). }
  TBitmapFontRange = class(TCollectionItem)
  private
    { Private Declarations }
    FStartASCII, FStopASCII: WideChar;
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
    destructor  Destroy; override;

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
    function Add(startASCII, stopASCII: AnsiChar): TBitmapFontRange; overload;
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

  PCharInfo = ^TCharInfo;
  TCharInfo = record
    l, t, w : word;
  end;

  //  TGLTextureFontBook
  //
  TGLTextureFontBook = class(TGLTextureImageEx)
  private
    procedure SetImage(const Value: TGLImage);
    function GetImage: TGLImage;
    function GetPageCount: Integer;
    procedure Validate;
  protected
    { Protected Declarations }
    FPages: array of TGLTextureHandle;
    FFont: TGLCustomBitmapFont;
  public
    { Public Declarations }
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;

    procedure NewPage;
    procedure DoOnPrepare(Sender: TGLContext); override;
    procedure Apply(var ARci: TRenderContextInfo); override;
    property Image: TGLImage read GetImage write SetImage;
    property PageCount: Integer read GetPageCount;
  end;

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
    FTextureWidth, FTextureHeight: Integer;
    FTextRows, FTextCols: integer;
    FTextures: TList;
    FLastPageIndex: Integer;
    FTextureModified: boolean;
    function GetGlyphsAlpha: TGLTextureImageAlpha;
    function GetMagFilter: TGLMagFilter;
    function GetMinFilter: TGLMinFilter;
  protected
    { Protected Declarations }
    FFontBook: TGLTextureFontBook;
    FFontSampler: TGLTextureSampler;
    FFontMaterial: TGLLibMaterialEx;
    FChars      : array of TCharInfo;
    FCharsLoaded: boolean;
    procedure ResetCharWidths(w: Integer = -1);
    procedure SetCharWidths(index, value: Integer);

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

    procedure TextureChanged;
    function  TextureFormat: TGLInternalFormat; dynamic;

    procedure InvalidateUsers;
    function  CharactersPerRow: Integer;
    procedure GetCharTexCoords(ch: widechar; var topLeft, bottomRight: TTexPoint);
    procedure GetICharTexCoords(var ABatches: TDrawBatchArray;
      chi: Integer; out topLeft, bottomRight: TTexPoint);
    procedure PrepareFontBook; virtual;
    procedure OnShaderInitialize(Sender: TGLBaseShaderModel); virtual;

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

    property MagFilter: TGLMagFilter read GetMagFilter write SetMagFilter default maLinear;
    property MinFilter: TGLMinFilter read GetMinFilter write SetMinFilter default miLinear;
    property GlyphsAlpha: TGLTextureImageAlpha read GetGlyphsAlpha write SetGlyphsAlpha default tiaDefault;

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
    procedure RenderString(var ARci: TRenderContextInfo;
      const aText: UnicodeString; aAlignment: TAlignment;
      aLayout: TGLTextLayout; const aColor: TColorVector;
      aPosition: PVector = nil; aReverseY: Boolean = False); overload; virtual; deprecated;

    procedure BuildString(var ABatches: TDrawBatchArray;
      const aText: UnicodeString; aAlignment: TAlignment;
      aLayout: TGLTextLayout; const aColor: TColorVector;
      aPosition: PVector = nil; aReverseY: Boolean = False); virtual;

    {: A simpler canvas-style TextOut helper for RenderString.<p>
       The rendering is reversed along Y by default, to allow direct use
       with TGLCanvas }
    procedure TextOut(var ARci: TRenderContextInfo; x, y: Single; const text: UnicodeString; const color: TColorVector); overload; deprecated;
    procedure TextOut(var ARci: TRenderContextInfo; x, y: Single; const text: UnicodeString; const color: TColor); overload; deprecated;
    function  TextWidth(const text: UnicodeString): Integer;

    function  CharacterToTileIndex(aChar: widechar): Integer; virtual;
    function  TileIndexToChar(aIndex: integer): widechar; virtual;
    function  CharacterCount: integer; virtual;

    {: Get the actual width for this char. }
    function  GetCharWidth(ch: widechar): Integer;
    {: Get the actual pixel width for this string. }
    function  CalcStringWidth(const aText: UnicodeString): Integer; overload; virtual;

    {: Height of a single character. }
    property CharHeight: Integer read FCharHeight write SetCharHeight default 16;

    property TextureWidth : integer read FTextureWidth write FTextureWidth;
    property TextureHeight: integer read FTextureHeight write FTextureHeight;
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


  // TGLAbstractText
  //
  TGLAbstractText = class(TGLImmaterialSceneObject)
  protected
    { Protected Declarations }
    FBitmapFont: TGLCustomBitmapFont;
    FText: UnicodeString;
    FAlignment: TAlignment;
    FLayout: TGLTextLayout;
    FModulateColor: TGLColor;
    FBatches: TDrawBatchArray;
    FTransformation: TTransformationRec;
    FFinishEvent: TFinishTaskEvent;
    procedure RegisterBatches;
    procedure DoBuild; virtual; stdcall;
    procedure FreeBatches;
    procedure SetBitmapFont(const val: TGLCustomBitmapFont);
    procedure SetText(const val: UnicodeString);
    procedure SetAlignment(const val: TAlignment);
    procedure SetLayout(const val: TGLTextLayout);
    procedure SetModulateColor(const val: TGLColor);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

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
  TGLFlatText = class(TGLAbstractText)
  private
    { Private Declarations }
    FOptions: TGLFlatTextOptions;
    procedure SetOptions(const val: TGLFlatTextOptions);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure DoRender(var ARci: TRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
  published
    { Published Declarations }
    property BitmapFont;
    property Text;
    property Alignment;
    property Layout;
    property ModulateColor;
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

uses
{$IFDEF GLS_DELPHI}
  VectorTypes,
{$ENDIF}
{$IFDEF GLS_SERVICE_CONTEXT}
  SyncObjs,
{$ENDIF}
  SysUtils,
  OpenGLTokens,
  GLS_ShaderParameter;

{$IFDEF GLS_REGION}{$REGION 'TBitmapFontRange'}{$ENDIF}

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

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TBitmapFontRanges'}{$ENDIF}

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

// Add
//

function TBitmapFontRanges.Add(startASCII, stopASCII: AnsiChar): TBitmapFontRange;
begin
  Result := Add(WideChar(startASCII), WideChar(stopASCII));
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

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLCustomBitmapFont'}{$ENDIF}

// Create
//

constructor TGLCustomBitmapFont.Create(AOwner: TComponent);
const
  cBitmapFontMaterialName = 'GLScene_BitmapFont_Material';
  cSamplerName = 'GLScene_BitmapFont_Sampler';
  cFontVertexShader120 =
    '#version 120'#10#13 +
    'attribute vec3 Position;'#10#13 +
    'attribute vec2 TexCoord0;'#10#13 +
    'attribute vec4 Color;'#10#13 +
    'varying vec2 v2f_TexCoord0;'#10#13 +
    'varying vec4 v2f_Color;'#10#13 +
    'uniform mat4 ModelViewProjectionMatrix;'#10#13 +
    'void main() {'#10#13 +
    ' gl_Position = ModelViewProjectionMatrix * vec4(Position,1.0);'#10#13 +
    ' v2f_TexCoord0 = TexCoord0;'#10#13 +
    ' v2f_Color = Color; }';
  cFontFragmentShader120 =
    '#version 120'#10#13 +
    'varying vec2 v2f_TexCoord0;'#10#13 +
    'varying vec4 v2f_Color;'#10#13 +
    'uniform sampler2D Font;'#10#13 +
    'void main() {'#10#13 +
    ' vec4 Color = texture2D(Font, v2f_TexCoord0);'#10#13 +
    ' gl_FragColor = v2f_Color * Color; }'#10#13;
var
  LShader: TGLShaderEx;
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
  FTextures := TList.Create;

  // Create and setup material
  FFontBook := TGLTextureFontBook.Create(GetInternalMaterialLibrary.Components);
  FFontBook.FFont := Self;
  FFontBook.InternalFormat := TextureFormat;
  FFontSampler := GetInternalMaterialLibrary.AddSampler(cSamplerName);
  FFontSampler.FilteringQuality := tfIsotropic;
  FFontSampler.MinFilter := miLinear;
  FFontMaterial := GetInternalMaterialLibrary.Materials.Add;
  with FFontMaterial do
  begin
    Name := GetInternalMaterialLibrary.Materials.MakeUniqueName(cBitmapFontMaterialName);
    FixedFunction.BlendingMode := bmTransparency;
    FixedFunction.MaterialOptions := [moNoLighting, moIgnoreFog];
    FixedFunction.DepthProperties.DepthTest := False;
    FixedFunction.DepthProperties.DepthWrite := False;
    FixedFunction.Texture.Enabled := True;
    FixedFunction.Texture.LibTextureName := FFontBook.Name;
    FixedFunction.Texture.LibSamplerName := FFontSampler.Name;
    FixedFunction.Texture.EnvMode := tmModulate;
    // GLSL 120
    LShader := GetInternalMaterialLibrary.AddShader(cInternalShader);
    LShader.ShaderType := shtVertex;
    LShader.Source.Add(cFontVertexShader120);
    ShaderModel3.LibVertexShaderName := LShader.Name;
    LShader := GetInternalMaterialLibrary.AddShader(cInternalShader);
    LShader.ShaderType := shtFragment;
    LShader.Source.Add(cFontFragmentShader120);
    ShaderModel3.LibFragmentShaderName := LShader.Name;
    OnSM3UniformInitialize := OnShaderInitialize;
    ShaderModel3.Enabled := True;
  end;
  FTextureModified := true;
end;

procedure TGLCustomBitmapFont.OnShaderInitialize(Sender: TGLBaseShaderModel);
begin
  with Sender do
  begin
    Uniforms['ModelViewProjectionMatrix'].AutoSetMethod := cafWorldViewProjectionMatrix;
    Uniforms['Font'].TextureName := FFontBook.Name;
    Uniforms['Font'].SamplerName := FFontSampler.Name;
  end;
end;

// Destroy
//

destructor TGLCustomBitmapFont.Destroy;
begin
  inherited Destroy;
  FRanges.Free;
  FGlyphs.Free;
  FTextures.Free;
  Assert(FUsers.Count = 0);
  FUsers.Free;
  FFontMaterial.Destroy;
  FFontBook.Destroy;
  FFontSampler.Destroy;
end;

// GetCharWidth
//

function TGLCustomBitmapFont.GetCharWidth(ch: widechar): Integer;
var
  chi: Integer;
begin
  chi := CharacterToTileIndex(ch);
  if Length(FChars) = 0 then
    ResetCharWidths;
  if chi >= 0 then
       Result := FChars[chi].w
  else Result := 0;
end;

function TGLCustomBitmapFont.GetGlyphsAlpha: TGLTextureImageAlpha;
begin
  Result := FFontBook.ImageAlpha;
end;

procedure TGLCustomBitmapFont.BuildString(var ABatches: TDrawBatchArray; const aText: UnicodeString; aAlignment: TAlignment; aLayout: TGLTextLayout;
  const aColor: TColorVector; aPosition: PVector; aReverseY: Boolean);


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
    pch   : PCharInfo;
    topLeft, bottomRight: TTexPoint;
    vTopLeft, vBottomRight: TVector;
    deltaV, spaceDeltaH: Single;
    currentChar: widechar;

begin
  if (aText = '') then
    Exit;
  // prepare texture if necessary
  if FTextureModified then
  begin
    PrepareFontBook;
    FTextureModified := false;
  end;

  FLastPageIndex := -1;
  if Length(ABatches) = 0 then
  begin
    SetLength(ABatches, FFontBook.PageCount);
    ABatches[0].InstancesChain := TInstancesChain.Create;
  end;

  // Add color as instance
  with ABatches[0].InstancesChain do
  begin
    Lock;
    try
      Clear;
      Attributes[attrColor] := True;
      AttributesType[attrColor] := GLSLType4f;
      AttributeLists[attrColor].Add(aColor[0], aColor[1], aColor[2], aColor[3]);
    finally
      UnLock;
    end;
  end;
  for I := High(ABatches) downto 1 do
    ABatches[I].InstancesChain := ABatches[0].InstancesChain;

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

  for i := 1 to Length(aText) do
  begin
    currentChar := WideChar(aText[i]);
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
      pch := @FChars[chi];
      if pch.w > 0 then
      begin
        GetICharTexCoords(ABatches, chi, topLeft, bottomRight);
        vBottomRight[0] := vTopLeft[0] + pch.w;

        with ABatches[FLastPageIndex].Mesh do
        begin
          Attribute2f(attrPosition, vTopLeft[0], vTopLeft[1]);
          Attribute2f(attrTexCoord0, topLeft);
          EmitVertex;

          Attribute2f(attrPosition, vTopLeft[0], vBottomRight[1]);
          Attribute2f(attrTexCoord0, topLeft.S, bottomRight.T);
          EmitVertex;

          Attribute2f(attrPosition, vBottomRight[0], vTopLeft[1]);
          Attribute2f(attrTexCoord0, bottomRight.S, topLeft.T);
          EmitVertex;
          EmitVertex;

          Attribute2f(attrPosition, vTopLeft[0], vBottomRight[1]);
          Attribute2f(attrTexCoord0, topLeft.S, bottomRight.T);
          EmitVertex;

          Attribute2f(attrPosition, vBottomRight[0], vBottomRight[1]);
          Attribute2f(attrTexCoord0, bottomRight.S, bottomRight.T);
          EmitVertex;

          vTopLeft[0] := vTopLeft[0] + pch.w + HSpace;
        end;
      end;
    end;
  end;
  ABatches[FLastPageIndex].Mesh.EndAssembly;
  ABatches[FLastPageIndex].Mesh.Unlock;
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

// ResetCharWidths
//

procedure TGLCustomBitmapFont.ResetCharWidths(w: Integer = -1);
var
  i: Integer;
begin
  FCharsLoaded := false;
  i := CharacterCount;
  if Length(FChars) < i then
      SetLength(FChars, i);
  if w < 0 then w := CharWidth;
  for i := 0 to High(FChars) do
    FChars[i].w := w;
end;

// SetCharWidths
//

procedure TGLCustomBitmapFont.SetCharWidths(index, value: Integer);
begin
  if index >= 0 then
    FChars[index].w := value;
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
  FFontSampler.MagFilter := AValue;
end;

// SetMinFilter
//

procedure TGLCustomBitmapFont.SetMinFilter(AValue: TGLMinFilter);
begin
  FFontSampler.MinFilter := AValue;
end;

// SetGlyphsAlpha
//

procedure TGLCustomBitmapFont.SetGlyphsAlpha(val: TGLTextureImageAlpha);
begin
  FFontBook.ImageAlpha := val;
end;

// OnGlyphsChanged
//

procedure TGLCustomBitmapFont.OnGlyphsChanged(Sender: TObject);
begin
  InvalidateUsers;
  //when empty, width is 0 and roundup give 1
  if not Glyphs.Graphic.Empty then
  begin
    if FTextureWidth = 0 then
      FTextureWidth := RoundUpToPowerOf2(Glyphs.Width);
    if FTextureHeight = 0 then
      FTextureHeight := RoundUpToPowerOf2(Glyphs.Height);
  end;
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

procedure TGLCustomBitmapFont.PrepareFontBook;
var
  LBitmap: TGLBitmap;
  x, y, w, h : integer;
begin
  //only check when really used
  if FTextureWidth = 0 then
  begin
    FTextureWidth  := CurrentGLContext.GLStates.MaxTextureSize;
    if FTextureWidth > 512 then FTextureWidth := 512;
    if FTextureWidth < 64  then FTextureWidth := 64;
  end;
  if FTextureHeight = 0 then
  begin
    FTextureHeight := CurrentGLContext.GLStates.MaxTextureSize;
    if FTextureHeight > 512 then FTextureHeight := 512;
    if FTextureHeight < 64  then FTextureHeight := 64;
  end;

  x := 0; y := 0; w := Glyphs.Width; h := Glyphs.Height;

  //was an error...
  FTextRows := 1 + (h - 1) div FTextureHeight;
  FTextCols := 1 + (w - 1) div FTextureWidth;

  LBitmap := TGLBitmap.Create;
  with LBitmap do
  begin
    PixelFormat := glpf32bit;
    Width  := RoundUpToPowerOf2(FTextureWidth);
    Height := RoundUpToPowerOf2(FTextureHeight);
  end;

  while (x < w) and (y < h) do
  begin
    FFontBook.NewPage;
    // copy full or part of glyphs
    LBitmap.Canvas.Draw(-x, -y, Glyphs.Graphic);

    if not Assigned(FFontBook.Image) then
      FFontBook.Image := TGLImage.Create;
    FFontBook.Image.Assign(LBitmap);
    FFontBook.Image.Narrow;
    with FFontBook.Image do
    begin
      case FFontBook.ImageAlpha of
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

      FFontBook.FullTransfer;
      FFontBook.Validate;
    end;

    Inc(x, FTextureWidth);
    if x >= w then
    begin
      Inc(y, FTextureHeight);
      x := 0;
    end;
  end;
  LBitmap.Free;
  FFontBook.Image.Free;
  FFontBook.Image := nil;
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

procedure TGLCustomBitmapFont.RenderString(var ARci: TRenderContextInfo;
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
  pch   : PCharInfo;
  topLeft, bottomRight: TTexPoint;
  vTopLeft, vBottomRight: TVector;
  deltaV, spaceDeltaH: Single;
  currentChar: widechar;
begin
  if (aText = '') then
    Exit;
  // prepare texture if necessary
  if FTextureModified then
  begin
    PrepareFontBook;
    FTextureModified := false;
  end;

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

    // start rendering
  GL.Color4fv(@aColor);
  GL.Begin_(GL_QUADS);
  for i := 1 to Length(aText) do
  begin
    currentChar := WideChar(aText[i]);
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
      pch := @FChars[chi];
      if pch.w > 0 then
      with GL do
      begin
//        GetICharTexCoords(ARci, chi, topLeft, bottomRight);
        vBottomRight[0] := vTopLeft[0] + pch.w;

        TexCoord2fv(@topLeft);
        Vertex4fv(@vTopLeft);

        TexCoord2f(topLeft.S, bottomRight.T);
        Vertex2f(vTopLeft[0], vBottomRight[1]);

        TexCoord2fv(@bottomRight);
        Vertex4fv(@vBottomRight);

        TexCoord2f(bottomRight.S, topLeft.T);
        Vertex2f(vBottomRight[0], vTopLeft[1]);

        vTopLeft[0] := vTopLeft[0] + pch.w + HSpace;
      end;
    end;
  end;
  GL.End_;
  // unbind texture
  ARci.GLStates.TextureBinding[0, ttTexture2d] := 0;
  ARci.GLStates.ActiveTextureEnabled[ttTexture2D] := False;
end;

// TextOut
//

procedure TGLCustomBitmapFont.TextOut(var ARci: TRenderContextInfo;
  x, y: Single; const text: UnicodeString; const color: TColorVector);
var
  v: TVector;
  LBatches: TDrawBatchArray;
  I: Integer;
begin
  v[0] := x;
  v[1] := y;
  v[2] := 0;
  v[3] := 1;
  LBatches := nil;
  BuildString(LBatches, text, taLeftJustify, tlTop, color, @v, True);
  for I := 0 to High(LBatches) do
  begin
    TGLScene(ARci.scene).RenderManager.DrawTechnique.DrawBatch(ARci, LBatches[I]);
    LBatches[I].Mesh.Destroy;
  end;
  if Length(LBatches) > 0 then
    LBatches[0].InstancesChain.Destroy;
end;

// TextOut
//

procedure TGLCustomBitmapFont.TextOut(var ARci: TRenderContextInfo;
  x, y: Single; const text: UnicodeString; const color: TColor);
begin
  TextOut(ARci, x, y, text, ConvertWinColor(color));
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
var
  chi, tileIndex: Integer;
  ci: TCharInfo;
  r : integer;
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
    //invalid char
    topLeft := NullTexPoint;
    bottomRight := NullTexPoint;
    exit;
  end;

  ci := FChars[chi];
  ci.l := ci.l mod FTextureWidth;
  ci.t := ci.t mod FTextureHeight;

  topLeft.S := ci.l / FTextureWidth;
  topLeft.T := 1 - ci.t / FTextureHeight;
  bottomRight.S := (ci.l + ci.w) / FTextureWidth;
  bottomRight.T := 1 - (ci.t + CharHeight) / FTextureHeight;
end;

// TileIndexToTexCoords
// it also activates the target texture

procedure TGLCustomBitmapFont.GetICharTexCoords(var ABatches: TDrawBatchArray;
  chi: Integer; out topLeft, bottomRight: TTexPoint);
var
  tileIndex, pageIndex: Integer;
  ci: TCharInfo;
  r,c : integer;
begin
  if not FCharsLoaded then
  begin
    r := CharactersPerRow;
    if r = 0 then exit;
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
    //invalid char
    topLeft := NullTexPoint;
    bottomRight := NullTexPoint;
    exit;
  end;

  ci := FChars[chi];

  c := ci.l div FTextureWidth;
  r := ci.t div FTextureHeight;
  ci.l := ci.l mod FTextureWidth;
  ci.t := ci.t mod FTextureHeight;
  pageIndex := r * FTextCols + c;

  topLeft.S := ci.l / FTextureWidth;
  topLeft.T := 1 - ci.t / FTextureHeight;
  bottomRight.S := (ci.l + ci.w) / FTextureWidth;
  bottomRight.T := 1 - (ci.t + CharHeight) / FTextureHeight;

  if pageIndex <> FLastPageIndex then
  begin
    if FLastPageIndex > -1 then
    begin
      ABatches[FLastPageIndex].Mesh.TagInteger := pageIndex;
      ABatches[FLastPageIndex].Mesh.EndAssembly;
      ABatches[FLastPageIndex].Mesh.UnLock;
    end;
    FLastPageIndex := pageIndex;
    if not Assigned(ABatches[FLastPageIndex].Mesh) then
    begin
      ABatches[FLastPageIndex].Mesh := TMeshAtom.Create;
      ABatches[FLastPageIndex].Material := FFontMaterial;
      ABatches[FLastPageIndex].Mesh.Lock;
      ABatches[FLastPageIndex].Mesh.DeclareAttribute(attrPosition, GLSLType2f);
      ABatches[FLastPageIndex].Mesh.DeclareAttribute(attrTexCoord0, GLSLType2f);
    end
    else
      ABatches[FLastPageIndex].Mesh.Lock;
    ABatches[FLastPageIndex].Mesh.BeginAssembly(mpTRIANGLES);
  end;
end;

function TGLCustomBitmapFont.GetMagFilter: TGLMagFilter;
begin
  Result := FFontSampler.MagFilter;
end;

function TGLCustomBitmapFont.GetMinFilter: TGLMinFilter;
begin
  Result := FFontSampler.MinFilter;
end;

// InvalidateUsers
//

procedure TGLCustomBitmapFont.InvalidateUsers;
var
  i: Integer;
begin
  FCharsLoaded := false;
  FTextureModified := true;
  for i := FUsers.Count - 1 downto 0 do
    TGLBaseSceneObject(FUsers[i]).NotifyChange(Self);
end;

procedure TGLCustomBitmapFont.TextureChanged;
begin
  FTextureModified := true;
end;

// TextureFormat
//

function TGLCustomBitmapFont.TextureFormat: TGLInternalFormat;
begin
  Result := tfRGBA8;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLAbstractText'}{$ENDIF}

// Notification
//

procedure TGLAbstractText.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FBitmapFont) then
    BitmapFont := nil;
  inherited;
end;

// SetBitmapFont
//

procedure TGLAbstractText.SetBitmapFont(const val: TGLCustomBitmapFont);
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

procedure TGLAbstractText.SetText(const val: UnicodeString);
begin
  FText := val;
  StructureChanged;
end;

// SetAlignment
//

procedure TGLAbstractText.SetAlignment(const val: TAlignment);
begin
  FAlignment := val;
  StructureChanged;
end;

// SetLayout
//

procedure TGLAbstractText.SetLayout(const val: TGLTextLayout);
begin
  FLayout := val;
  StructureChanged;
end;

// SetModulateColor
//

procedure TGLAbstractText.SetModulateColor(const val: TGLColor);
begin
  FModulateColor.Assign(val);
end;

procedure TGLAbstractText.RegisterBatches;
var
  I: Integer;
begin
  if Assigned(Scene) then
  begin
    for I := 0 to High(FBatches) do
    begin
      Scene.RenderManager.RegisterBatch(FBatches[I]);
      FBatches[I].Mesh.TagName := Format('%s_part%d', [ClassName, I]);
      FBatches[I].Transformation := @FTransformation;
    end;
  end;
end;

procedure TGLAbstractText.DoBuild;
begin
  FreeBatches;
  if Assigned(FBitmapFont) and (Text <> '') then
  begin
    FBitmapFont.BuildString(FBatches, Text, FAlignment, FLayout, FModulateColor.Color);
    RegisterBatches;
  end;
  ClearStructureChanged;

  if not IsMainThread and Assigned(Scene) then
    Scene.NotifyChange(Self);
end;

procedure TGLAbstractText.FreeBatches;
var
  I: Integer;
begin
  if Assigned(Scene) then
    for I := 0 to High(FBatches) do
      Scene.RenderManager.UnRegisterBatch(FBatches[I]);

  for I := 0 to High(FBatches) do
    FBatches[I].Mesh.Free;
  if Length(FBatches) > 0 then
    FBatches[0].InstancesChain.Free;
  SetLength(FBatches, 0);
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLFlatText'}{$ENDIF}

// Create
//

constructor TGLFlatText.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDeferredDraw, osNoVisibilityCulling];
  FModulateColor := TGLColor.CreateInitialized(Self, clrWhite);
end;

// Destroy
//

destructor TGLFlatText.Destroy;
begin
  FModulateColor.Free;
  BitmapFont := nil;
  FreeBatches;
  FFinishEvent.Free;
  inherited;
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

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLFlatText'}{$ENDIF}

// DoRender
//

procedure TGLFlatText.DoRender(var ARci: TRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);

  procedure PrepareSelf;
  var
    I: Integer;
  begin
    if ocStructure in Changes then
    begin
{$IFDEF GLS_SERVICE_CONTEXT}
      if not (osStreamDraw in ObjectStyle) and IsServiceContextAvaible then
      begin
        if not Assigned(FFinishEvent) then
        begin
          FFinishEvent := TFinishTaskEvent.Create;
          AddTaskForServiceContext(DoBuild, FFinishEvent);
        end
        else if FFinishEvent.WaitFor(0) = wrSignaled then
        begin
          FFinishEvent.ResetEvent;
          AddTaskForServiceContext(DoBuild, FFinishEvent);
        end;
        exit;
      end
      else
{$ENDIF GLS_SERVICE_CONTEXT}
        DoBuild;
    end;

    if ARenderSelf then
    begin
      FTransformation := ARci.PipelineTransformation.StackTop;
      for I := High(FBatches) downto 0 do
        FBatches[I].Order := ARci.orderCounter;
    end;
  end;

begin
  PrepareSelf;

  if ARenderChildren then
    RenderChildren(0, Count - 1, ARci);
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

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'TGLTextureFontBook'}{$ENDIF}

procedure TGLTextureFontBook.Apply(var ARci: TRenderContextInfo);
begin
  if Assigned(ARci.mesh) and (ARci.mesh is TMeshAtom) then
    FHandle := FPages[ TMeshAtom(ARci.mesh).TagInteger ];
  inherited Apply(ARci);
end;

constructor TGLTextureFontBook.Create(AOwner: TXCollection);
begin
  inherited;
end;

destructor TGLTextureFontBook.Destroy;
var
  I: Integer;
begin
  if Length(FPages) > 0 then
  begin
    FHandle := FPages[0];
    for I := 1 to High(FPages) do
      FPages[I].Free;
    SetLength(FPages, 0);
  end;
  inherited Destroy;
end;

procedure TGLTextureFontBook.DoOnPrepare(Sender: TGLContext);
begin
  FHandle.AllocateHandle;
  FFont.FTextureModified := FFont.FTextureModified or FHandle.IsDataNeedUpdate;
end;

function TGLTextureFontBook.GetImage: TGLImage;
begin
  Result := TGLImage(FImage);
end;

function TGLTextureFontBook.GetPageCount: Integer;
begin
  Result := Length(FPages);
end;

procedure TGLTextureFontBook.NewPage;
var
  I: Integer;
begin
  I := Length(FPages);
  SetLength(FPages, I+1);
  if I = 0 then
    FPages[0] := FHandle
  else
  begin
    FPages[I] := TGLTextureHandle.Create;
    FHandle := FPages[I];
    FHandle.OnPrapare := FPages[0].OnPrapare;
  end;
  FHandle.AllocateHandle;
  FHandle.Target := ttTexture2D;
  CurrentGLContext.GLStates.TextureBinding[0, ttTexture2D] := FHandle.Handle;
end;

procedure TGLTextureFontBook.SetImage(const Value: TGLImage);
begin
  FImage := TGLBaseImage(Value);
end;

procedure TGLTextureFontBook.Validate;
begin
  FIsValid := True;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

initialization

  RegisterClasses([TGLBitmapFont, TGLFlatText, TGLTextureFontBook]);

end.
