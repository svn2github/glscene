//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLS.WindowsFont<p>

  TFont Import into a BitmapFont using variable width...<p>

 <b>History : </b><font size=-1><ul>
      <li>04/12/14 - PW - Corrected the usage of pixel formats for Lazarus (by Gabriel Corneanu)
       <li>29/05/11 - Yar - Unicode support for Unix OSes (by Gabriel Corneanu)
      <li>16/05/11 - Yar - Redesign to use multiple textures (by Gabriel Corneanu)
      <li>13/05/11 - Yar - Adapted to unicode (by Gabriel Corneanu)
      <li>23/08/10 - Yar - Added GLS.OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>06/06/10 - Yar - Added "VectorTypes.pas" unit to uses
      <li>25/01/10 - Yar - Bugfix in LoadWindowsFont with zero width of char
                          (thanks olkondr)
                          Replace Char to AnsiChar
      <li>11/11/09 - DaStr - Added Delphi 2009 compatibility (thanks mal)
      <li>17/03/07 - DaStr - Dropped Kylix support in favor of FPC (BugTracekrID=1681585)
      <li>12/15/04 - Eugene Kryukov - Added TGLStoredBitmapFont
      <li>03/07/04 - LR - Added ifdef for Graphics uses
      <li>29/09/02 - EG - Fixed transparency, style fixes, prop defaults fixed,
                          dropped interface dependency, texture size auto computed,
                          fixed italics spacing, uses LUM+ALPHA texture
      <li>06/09/02 - JAJ - Fixed alot of bugs... Expecially designtime updating bugs..
      <li>12/08/02 - JAJ - Made into a standalone unit...
 </ul></font>
}
unit GLS.WindowsFont;

interface

{$I GLScene.inc}

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  System.Classes,
  System.Math,
  System.SysUtils,
  System.Types,
  System.UITypes,
  FMX.Graphics,

  GLS.Scene,
  GLS.Texture,
  GLS.BitmapFont,
  GLS.RenderContextInfo,
  GLS.VectorLists,
  GLS.CrossPlatform,
  GLS.Utils,
  GLS.VectorGeometry,
  GLS.OpenGLTokens,
  GLS.ApplicationFileIO,
  GLS.VectorTypes;

type

  // TGLWindowsBitmapFont
  //
  {: A bitmap font automatically built from a TFont.<p>
     It works like a TGLBitmapfont, you set ranges and which chars are assigned
     to which indexes, however here you also set the Font property to any TFont
     available to the system and it renders in GLScene as close to that font
     as posible, on some font types this is 100% on some a slight difference
     in spacing can occur at most 1 pixel per char on some char combinations.<p>
     Ranges must be sorted in ascending ASCII order and should not overlap.
     As the font texture is automatically layed out, the Ranges StartGlyphIdx
     property is ignored and replaced appropriately. }
  TGLWindowsBitmapFont = class(TGLCustomBitmapFont)
  private
    { Private Declarations }
    FFont: TFont;
    procedure SetList(const AList : TIntegerList);
  protected
    { Protected Declarations }
    procedure SetFont(value: TFont);
    procedure LoadWindowsFont; virtual;
    function  StoreRanges: Boolean;

    procedure PrepareImage(var ARci: TRenderContextInfo); override;
    function  TextureFormat: Integer; override;
    procedure StreamlineRanges;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure NotifyChange(Sender: TObject); override;

    function FontTextureWidth: Integer;
    function FontTextureHeight: Integer;

    procedure EnsureString(const s : String); overload;
    procedure EnsureChars(const AStart, AEnd: widechar);

    property Glyphs;

  published
    { Published Declarations }
      {: The font used to prepare the texture.<p>
         Note: the font color is ignored. }
    property Font: TFont read FFont write SetFont;

    property HSpace;
    property VSpace;
    property MagFilter;
    property MinFilter;
    property Ranges stored StoreRanges;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
  cDefaultLast = '}';

{$IFDEF MSWINDOWS}
Var
  Win32PlatformIsUnicode : Boolean;
{$ENDIF}

// ------------------
// ------------------ TGLWindowsBitmapFont ------------------
// ------------------

// Create
//

constructor TGLWindowsBitmapFont.Create(AOwner: TComponent);
begin
  inherited;
  FFont := TFont.Create;
  { TODO : E2003 Undeclared identifier: 'Color' }
  (*FFont.Color := TColors.White;*)
  FFont.OnChanged := NotifyChange;
  GlyphsAlpha := tiaAlphaFromIntensity;
  EnsureChars(' ', cDefaultLast);
end;

// Destroy
//

destructor TGLWindowsBitmapFont.Destroy;
begin
  FFont.Free;
  Ranges.Clear; 
  inherited;
end;

// FontTextureWidth
//

function TGLWindowsBitmapFont.FontTextureWidth: Integer;
begin
  Result := Glyphs.Bitmap.Width;
end;

// FontTextureHeight
//

function TGLWindowsBitmapFont.FontTextureHeight: Integer;
begin
  Result := Glyphs.Bitmap.Height;
end;

// SetFont
//

procedure TGLWindowsBitmapFont.SetFont(value: TFont);
begin
  FFont.Assign(value);
end;

// NotifyChange
//

procedure TGLWindowsBitmapFont.NotifyChange(Sender: TObject);
begin
  StreamlineRanges;
  FreeTextureHandle;
  InvalidateUsers;
  inherited;
end;

// LoadWindowsFont
//

procedure TGLWindowsBitmapFont.LoadWindowsFont;

  procedure ComputeCharRects(bitmap: TGLBitmap);
  var
    px, py, cw, n, x, y: Integer;
    PaddedHeight : integer;
    buffer : array[0..2] of WideChar;
    p : PCharInfo;
    r : TGLRect;
  begin
    buffer[1] := WideChar(#32);
    buffer[2] := WideChar(#0);
    PaddedHeight:= CharHeight + GlyphsIntervalY;
    x := bitmap.Width; y := bitmap.Height;
    px := 0;
    py := 0;
    if y < CharHeight then px := x;
    p  := @FChars[0];
    for n := 0 to CharacterCount - 1 do
    begin
      cw := p.w;
      if cw > 0 then
      begin
        Inc(cw, GlyphsIntervalX);

        if px + cw > x then
        begin
          px := 0;
          Inc(py, PaddedHeight);
          if py + PaddedHeight > y then
          begin
            py := bitmap.Height;
            y  := py + TextureHeight;
            bitmap.Height := y;
            with bitmap.Canvas do
            begin
              { TODO : E2003 Undeclared identifier: 'Brush' }
              (*
              Brush.Style := bsSolid;
              Brush.Color := TColors.Black;
              *)
              { TODO : E2250 There is no overloaded version of 'FillRect', not enouph arguments }
              (*FillRect(Rect(0, py, x, y));*)
            end;
          end;
        end;

        if Assigned(bitmap) then
        begin
          //+1 makes right align (padding left);
          // I prefer padding right for compatibility with bitmap font...
          p.l := px;
          //should make it consistent, same as above
          p.t := py;

          r.Left := px;
          r.Top  := py;
          r.Right  := px + cw;
          r.Bottom := py + PaddedHeight;
          buffer[0] := TileIndexToChar(n);
          // Draw the Char, the trailing space is to properly handle the italics.
          // credits to the Unicode version of SynEdit for this function call. GPL/MPL as GLScene
          { TODO : E2003 Undeclared identifier: 'Handle' }
          (*ExtTextOutW(bitmap.Canvas.Handle, p.l, p.t, ETO_CLIPPED, @r, buffer, 1, nil);*)
       end;
        Inc(px, cw);
      end
      else
      begin
        p.l := 0;
        p.t := 0;
      end;
      inc(p);
    end;
  end;

  // credits to the Unicode version of SynEdit for this function. GPL/MPL as GLScene
  function GetTextSize(DC: HDC; Str: PWideChar; Count: Integer): TSize;
    var tm: TTextMetricW;
  begin
    Result.cx := 0;
    Result.cy := 0;
    GetTextExtentPoint32W(DC, Str, Count, Result);
    if not Win32PlatformIsUnicode then
    begin
      GetTextMetricsW(DC, tm);
      if tm.tmPitchAndFamily and TMPF_TRUETYPE <> 0 then
        Result.cx := Result.cx - tm.tmOverhang
      else
        Result.cx := tm.tmAveCharWidth * Count;
    end;
  end;

var
  bitmap: TGLBitmap;
  ch: widechar;
  i, cw, nbChars, n: Integer;
begin
  InvalidateUsers;
  Glyphs.Bitmap.OnChange := nil;
  //accessing Bitmap might trigger onchange
  bitmap := Glyphs.Bitmap;

  bitmap.Height      := 0;
  //due to lazarus doesn't properly support pixel formats
    { TODO : E2129 Cannot assign to a read-only property }
     (*bitmap.PixelFormat := TPixelFormat.RGBA; //in VCL glpf32bit;*)

  with bitmap.Canvas do
  begin
    { TODO : E2129 Cannot assign to a read-only property }
    (*
    Font := Self.Font;
    Font.Color := TColors.White;
    *)
    // get characters dimensions for the font
    // character size without padding; paddings are used from GlyphsInterval
    { TODO : E2250 There is no overloaded version of 'MaxInteger' that can be called with these arguments }
    (*
    CharWidth  := Round(MaxInteger(TextWidth('M'), TextWidth('W'), TextWidth('_')));
    CharHeight := TextHeight('"_pI|,');
    *)
    // used for padding
    GlyphsIntervalX := 1;
    GlyphsIntervalY := 1;
    if TFontStyle.fsItalic in Font.Style then
    begin
      // italics aren't properly acknowledged in font width
      HSpaceFix := -(CharWidth div 3);
      CharWidth := CharWidth - HSpaceFix;
    end
    else
      HSpaceFix := 0;
  end;

  nbChars := CharacterCount;

  // Retrieve width of all characters (texture width)
  ResetCharWidths(0);
  n := 0;
  for i := 0 to nbChars - 1 do
  begin
    ch := TileIndexToChar(i);
    { TODO : E2003 Undeclared identifier: 'Handle' }
    (*cw := GetTextSize(bitmap.canvas.Handle, @ch, 1).cx-HSpaceFix;*)
    n  := n + cw + GlyphsIntervalX;
    SetCharWidths(i, cw);
  end;
  //try to make best guess...
  //~total pixels, including some waste (10%)
  n := n * (CharHeight + GlyphsIntervalY) * 11 div 10;
  TextureWidth := min(512, RoundUpToPowerOf2( round(sqrt(n)) ));
  TextureHeight := min(512, RoundUpToPowerOf2( n div TextureWidth));

  bitmap.Width := TextureWidth;

  ComputeCharRects(bitmap);
  FCharsLoaded := true;
  Glyphs.Bitmap.OnChange := OnGlyphsChanged;
end;

// StoreRanges
//

function TGLWindowsBitmapFont.StoreRanges: Boolean;
begin
  Result := (Ranges.Count <> 1) or (Ranges[0].StartASCII[1] <> ' ') or (Ranges[0].StopASCII[1] <> cDefaultLast);
end;

type
  TFriendlyRange = class(TBitmapFontRange);

procedure TGLWindowsBitmapFont.StreamlineRanges;
var
  I, C: Integer;
begin
  C := 0;
  for I := 0 to Ranges.Count - 1 do
  begin
    TFriendlyRange(Ranges[I]).FStartGlyphIdx := C;
    Inc(C, Ranges[I].CharCount);
    TFriendlyRange(Ranges[I]).FStopGlyphIdx := MaxInteger(C - 1, 0);
  end;
end;

procedure TGLWindowsBitmapFont.SetList(const AList: TIntegerList);
var
  i : integer;
  f, n, s : integer;
begin
  //add existing ranges
  for I := 0 to Ranges.Count - 1 do
    with Ranges.Items[I] do
      AList.AddSerie(integer(StartASCII[1]), 1, CharCount);

  AList.SortAndRemoveDuplicates;

  Ranges.Clear;
  Ranges.BeginUpdate;
  if AList.Count > 0 then
  begin
    i := 0;
    while (i < AList.Count) and (AList[i] < 32) do inc(i);
    while i < AList.Count do
    begin
      f := AList[i]; n := f; s := Ranges.CharacterCount;
      while (i < AList.Count) and (n = AList[i]) do
      begin
        inc(i);
        inc(n);
      end;
      Ranges.Add(widechar(f), widechar(pred(n))).StartGlyphIdx := s;
    end;
  end;

  Ranges.EndUpdate;
  TextureChanged;
  InvalidateUsers;
end;

//add characters to internal list
procedure TGLWindowsBitmapFont.EnsureChars(const AStart, AEnd: widechar);
var
  c : WideChar;
  ACharList : TIntegerList;
begin
  ACharList := TIntegerList.Create;
  for c := AStart to AEnd do
      ACharList.Add(integer(c));
  SetList(ACharList);
  ACharList.Free;
end;

//add characters to internal list
procedure TGLWindowsBitmapFont.EnsureString(const s: String);
var
  i : Integer;
  ACharList : TIntegerList;
begin
  ACharList := TIntegerList.Create;
  for i := Low(s) to High(s) do
      ACharList.Add(integer(s[i]));
  SetList(ACharList);
  ACharList.Free;
end;

// PrepareImage
//

procedure TGLWindowsBitmapFont.PrepareImage(var ARci: TRenderContextInfo);
begin
  LoadWindowsFont;
  inherited PrepareImage(ARci);
end;

// TextureFormat
//

function TGLWindowsBitmapFont.TextureFormat: Integer;
begin
  Result := GL_ALPHA;
end;

initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  Win32PlatformIsUnicode := (Win32Platform = VER_PLATFORM_WIN32_NT);

   // class registrations
  RegisterClasses([TGLWindowsBitmapFont]);

end.
