//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLWindowsFont<p>

  TFont Import into a BitmapFont using variable width...<p>

 <b>History : </b><font size=-1><ul>
      <li>13/05/11 - Yar - Adapted to unicode (by Gabriel Corneanu)
      <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>06/06/10 - Yar - Added VectorTypes to uses
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
unit GLWindowsFont;

interface

{$INCLUDE GLScene.inc}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ELSE FPC}
  LCLIntf, LCLType, Types, LCLProc,
{$ENDIF}
  GLBitmapFont,
  Classes,
  GLScene,
  GLTexture,
  Graphics,
  VectorLists,
  GLCrossPlatform;

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

    procedure PrepareImage; override;
    function TextureFormat: Integer; override;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure NotifyChange(Sender: TObject); override;

    function FontTextureWidth: Integer;
    function FontTextureHeight: Integer;

    procedure EnsureString(const s : UnicodeString); overload;
    procedure EnsureChars(const AStart, AEnd: widechar);

    property Glyphs;

  published
    { Published Declarations }
      {: The font used to prepare the texture.<p>
         Note: the font color is ignored. }
    property Font: TFont read FFont write SetFont;

    property MagFilter;
    property MinFilter;
    property Ranges stored StoreRanges;
  end;

  {: Inheritance from TGLWindowsBitmapFont which can load from file.<p>
  }
  TGLStoredBitmapFont = class(TGLWindowsBitmapFont)
  private
    FLoaded: boolean;
  protected
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure NotifyChange(Sender: TObject); override;
    procedure LoadWindowsFont; override;
    procedure LoadFromFile(AFileName: string);
    procedure SaveToFile(AFileName: string);
  published
    { Published Declarations }
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  SysUtils,
  VectorGeometry,
  OpenGLTokens,
  ApplicationFileIO
  {$IFDEF GLS_DELPHI}, VectorTypes{$ENDIF};

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
  FFont.Color := clWhite;
  FFont.OnChange := NotifyChange;
  GlyphsAlpha := tiaAlphaFromIntensity;
  EnsureChars(' ', cDefaultLast);
end;

// Destroy
//

destructor TGLWindowsBitmapFont.Destroy;
begin
  FFont.Free;
  inherited;
end;

// FontTextureWidth
//

function TGLWindowsBitmapFont.FontTextureWidth: Integer;
begin
  Result := Glyphs.Width;
end;

// FontTextureHeight
//

function TGLWindowsBitmapFont.FontTextureHeight: Integer;
begin
  Result := Glyphs.Height;
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
  FreeTextureHandle;
  InvalidateUsers;
  inherited;
end;

// LoadWindowsFont
//

procedure TGLWindowsBitmapFont.LoadWindowsFont;

  function ComputeCharRects(x, y: Integer; canvas: TCanvas): Integer;
  var
    px, py, cw, n: Integer;
    rect: TGLRect;
    buffer : array[0..2] of WideChar;
{$IFNDEF MSWINDOWS}
    utfs: string;
    utfbuffer: array[0..3] of Char;
    i: integer;
{$ENDIF}
 begin
    buffer[1] := WideChar(#32);
    buffer[2] := WideChar(#0);

    Result := 0;
    px := 0;
    py := 0;
    for n := 0 to CharacterCount - 1 do
    begin
      cw := CharWidths[n];
      if cw > 0 then
      begin
        Inc(cw, 2);

        if px + cw > x then
        begin
          px := 0;
          Inc(py, CharHeight);
          if py + CharHeight > y then Break;
        end;

        if Assigned(canvas) then
        begin
          SetCharRects(n, VectorMake((px + 1.05) / x,
            (y - (py + 0.05)) / y,
            (px + cw - 1.05) / x,
            (y - (py + CharHeight - 0.05)) / y));
          rect.Left := px;
          rect.Top := py;
          rect.Right := px + cw;
          rect.Bottom := py + CharHeight;
          buffer[0] := TileIndexToChar(n);
          // Draw the Char, the trailing space is to properly handle the italics.
{$IFDEF MSWINDOWS}
          // credits to the Unicode version of SynEdit for this function call. GPL/MPL as GLScene
          Windows.ExtTextOutW(Canvas.Handle, px+1, py+1, ETO_CLIPPED, @rect, buffer, 2, nil);
{$ELSE}
          utfs := UTF16ToUTF8(buffer[0]);
          utfbuffer[0] := utfs[1];
          i := 1;
          if Length(utfs)>1 then
          begin
            utfbuffer[1] := utfs[2];
            inc(i);
          end;
          utfbuffer[i] := #32;
          Inc(i);
          utfbuffer[i] := #0;
          LCLIntf.ExtTextOut(Canvas.Handle, px+1, py+1, ETO_CLIPPED, @rect, utfbuffer, i, nil);
{$ENDIF}
        end;
        Inc(px, cw);
      end
      else
      begin
        SetCharRects(n, NullHmgVector);
      end;
      Inc(Result);
    end;
  end;

  // credits to the Unicode version of SynEdit for this function. GPL/MPL as GLScene
  function GetTextSize(DC: HDC; Str: PWideChar; Count: Integer): TSize;
  var
    {$IFDEF FPC}
    {$IFDEF MSWINDOWS}
    tm:  FPCLPTextMetric;
    {$ELSE}
    LString: string;
    {$ENDIF}
    {$ELSE}
    tm: TTextMetricA;
    {$ENDIF}
  begin
    Result.cx := 0;
    Result.cy := 0;
{$IFDEF MSWINDOWS}
    GetTextExtentPoint32W(DC, Str, Count, Result);
    if not Win32PlatformIsUnicode then
    begin
      GetTextMetrics(DC, tm);
      if tm.tmPitchAndFamily and TMPF_TRUETYPE <> 0 then
        Result.cx := Result.cx - tm.tmOverhang
      else
        Result.cx := tm.tmAveCharWidth * Count;
    end;
{$ELSE}
    LString := UTF16ToUTF8(WideString(Str));
    GetTextExtentPoint32(DC, PChar(LString), UTF8Length(LString), Result);
{$ENDIF}
  end;

var
  bitmap: TGLBitmap;
  ch: widechar;
  x, y, i, cw: Integer;
  nbChars: Integer;

begin
  InvalidateUsers;
  bitmap := Glyphs.Bitmap;
  Glyphs.OnChange := nil;

  bitmap.PixelFormat := glpf32bit;
  with bitmap.Canvas do
  begin
    Font := Self.Font;
    Font.Color := clWhite;
    // get characters dimensions for the font
    CharWidth := Round(2 + MaxInteger(TextWidth('M'), TextWidth('W'), TextWidth('_')));
    CharHeight := 2 + TextHeight('"_pI|,');
    if fsItalic in Font.Style then
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
  for i := 0 to nbChars - 1 do
  begin
    ch := TileIndexToChar(i);
    cw := GetTextSize(bitmap.canvas.Handle, @ch, 1).cx-HSpaceFix;
    SetCharWidths(i, cw);
  end;

  // compute texture size: look for best fill ratio
  // and as square a texture as possible
  x := 32; y := 32;
  // compute the number of characters that fit
  while ComputeCharRects(x, y, nil) < nbChars do
  begin
    //increase vertical first, it's more likely that's not multiple...
    if x < y then
         x := x * 2
    else y := y * 2;

    if (x > 512) or (y > 512) then
    begin
      Font.Size := 6;
      raise Exception.Create('Characters are too large or too many. Unable to create font texture.');
    end;
  end;

{$IFDEF GLS_DELPHI_2009_UP}
  bitmap.SetSize(x, y);
{$ELSE}
  bitmap.Width := x;
  bitmap.Height := y;
{$ENDIF}

  with bitmap.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBlack;
    FillRect(Rect(0, 0, x, y));
  end;

  ComputeCharRects(x, y, bitmap.Canvas);
  Glyphs.Graphic.Modified := True;
  Glyphs.OnChange := OnGlyphsChanged;
end;

// StoreRanges
//

function TGLWindowsBitmapFont.StoreRanges: Boolean;
begin
  Result := (Ranges.Count <> 1) or (Ranges[0].StartASCII <> ' ') or (Ranges[0].StopASCII <> cDefaultLast);
end;

procedure TGLWindowsBitmapFont.SetList(const AList: TIntegerList);
var
  i : integer;
  f, n, s : integer;
begin
  //add existing ranges
  for I := 0 to Ranges.Count - 1 do
    with Ranges.Items[I] do
      AList.AddSerie(integer(StartASCII), 1, CharCount);

  AList.SortAndRemoveDuplicates;

  Ranges.Clear;
  Ranges.BeginUpdate;
  if AList.Count > 0 then
  begin
    i := 0;
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
  FTextureHandle.NotifyChangesOfData;
  InvalidateUsers;
end;

//add characters to internal list
procedure TGLWindowsBitmapFont.EnsureChars(const AStart, AEnd: widechar);
var
  c : widechar;
  ACharList : TIntegerList;
begin
  ACharList := TIntegerList.Create;
  for c := AStart to AEnd do
      ACharList.Add(integer(c));
  SetList(ACharList);
  ACharList.Free;
end;

//add characters to internal list
procedure TGLWindowsBitmapFont.EnsureString(const s: UnicodeString);
var
  i : integer;
  ACharList : TIntegerList;
begin
  ACharList := TIntegerList.Create;
  for i := 1 to length(s) do
      ACharList.Add(integer(s[i]));
  SetList(ACharList);
  ACharList.Free;
end;

// PrepareImage
//

procedure TGLWindowsBitmapFont.PrepareImage;
begin
  LoadWindowsFont;
  inherited;
end;

// TextureFormat
//

function TGLWindowsBitmapFont.TextureFormat: Integer;
begin
  Result := GL_ALPHA;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{ TGLStoredBitmapFont }

constructor TGLStoredBitmapFont.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TGLStoredBitmapFont.Destroy;
begin
  inherited;
end;

procedure TGLStoredBitmapFont.NotifyChange(Sender: TObject);
begin
  inherited;
end;

procedure TGLStoredBitmapFont.LoadFromFile(AFileName: string);
var
  S: TStream;
  Count, i: integer;
  F1, F2, F3, F4: single;
  I1: integer;
  Bmp: TBitmap;
begin
  if FileStreamExists(AFileName) then
  begin
    S := CreateFileStream(AFileName, fmOpenRead);
    try
      FLoaded := true;
      { Load Glyphs }
      Bmp := TBitmap.Create;
      Bmp.LoadFromStream(S);
      Glyphs.Assign(Bmp);
      Bmp.Free;

      FreeTextureHandle;
      InvalidateUsers;
      { Load font settings }
      // char props
      S.Read(I1, SizeOf(CharWidth));
      CharWidth := I1;
      S.Read(I1, SizeOf(CharHeight));
      CharHeight := I1;
      // char rects
      S.Read(Count, SizeOf(Count));
      SetLength(FCharRects, Count);
      for i := 0 to High(FCharRects) do
      begin
        S.Read(F1, SizeOf(FCharRects[i][0]));
        S.Read(F2, SizeOf(FCharRects[i][1]));
        S.Read(F3, SizeOf(FCharRects[i][2]));
        S.Read(F4, SizeOf(FCharRects[i][3]));
        SetCharRects(i, VectorMake(F1, F2, F3, F4));
      end;
      // char wds
      S.Read(Count, SizeOf(Count));
      SetLength(FCharWidths, Count);
      for i := 0 to High(CharWidths) do
      begin
        S.Read(I1, SizeOf(CharWidths[i]));
        SetCharWidths(i, I1);
      end;
    finally
      S.Free;
    end;
  end;
end;

procedure TGLStoredBitmapFont.SaveToFile(AFileName: string);
var
  S: TStream;
  i, Count: integer;
  Bmp: TBitmap;
begin
  if Glyphs.Graphic = nil then
    Exit;

  S := CreateFileStream(AFileName, fmCreate);
  try
    { Save glyphs }
    Bmp := TBitmap.Create;
    Bmp.Assign(Glyphs.Graphic);
    Bmp.SaveToStream(S);
    Bmp.Free;
    { Save settings }
    // char props
    S.Write(CharWidth, SizeOf(CharWidth));
    S.Write(CharHeight, SizeOf(CharHeight));
    // char rects
    Count := Length(FCharRects);
    S.Write(Count, SizeOf(Count));
    for i := 0 to High(FCharRects) do
    begin
      S.Write(FCharRects[i][0], SizeOf(FCharRects[i][0]));
      S.Write(FCharRects[i][1], SizeOf(FCharRects[i][1]));
      S.Write(FCharRects[i][2], SizeOf(FCharRects[i][2]));
      S.Write(FCharRects[i][3], SizeOf(FCharRects[i][3]));
    end;
    // char wds
    Count := Length(CharWidths);
    S.Write(Count, SizeOf(Count));
    for i := 0 to High(CharWidths) do
      S.Write(CharWidths[i], SizeOf(CharWidths[i]));
  finally
    S.Free;
  end;
end;

procedure TGLStoredBitmapFont.LoadWindowsFont;
begin
  if not FLoaded then
    inherited;
end;

initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
{$IFDEF MSWINDOWS}
  Win32PlatformIsUnicode := (Win32Platform = VER_PLATFORM_WIN32_NT);
{$ENDIF}

   // class registrations
  RegisterClasses([TGLWindowsBitmapFont, TGLStoredBitmapFont]);

end.

