// TFont Import into a BitmapFont using variable width...
{: TFont Import into a BitmapFont using variable width...<p>
	<b>History : </b><font size=-1><ul>
      <li>06/09/02 - JAJ - Fixed alot of bugs... Expecially designtime updating bugs..
      <li>12/08/02 - JAJ - Made into a standalone unit...
	</ul></font>
}
unit GLWindowsFont;

interface

Uses
  GLBitMapFont, Graphics, Classes, GLScene, GLTexture, GLCrossPlatform;

Type

{: It works like a GLBitmapfont, you set ranges and which chars are assigned to which indexes,
however here you also set the Font property to any TFont availible to the system and it renders
in GLScene as close to that font as posible, on some font types this is 100% on some a slight
difference in spacing can occur at most 1 pixel per char on some char combinations.

This component should be ready for cross-platform development.}
  TWindowsBitmapFont = class (TBitmapFont, IChangeNotifier)
  private
    FFont : TFont;
    FWidth : Integer;
    FHeight : Integer;
    FBGColor : TColor;
    FMaxHeight : Integer;
  protected
    Procedure SetFont(value : TFont);
    Procedure SetWidth(value : Integer);
    Procedure SetHeight(value : Integer);
    Procedure SetBGColor(value : TColor);
    procedure LoadWindowsFont;

    procedure PrepareImage; override;
    procedure Changed;
  public
    Constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure NotifyChange(Sender : TObject); override;
    property MaxHeight : Integer read FMaxHeight;
  published
    property Font : TFont read FFont write SetFont;
    property Width : Integer read FWidth write SetWidth;
    property Height : Integer read FHeight write SetHeight;
    property BGColor : TColor read FBGColor write SetBGColor;
    property GlyphsAlpha;
  End;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GLScene', [TWindowsBitmapFont]);
end;

{ TWindowsBitmapFont }

constructor TWindowsBitmapFont.Create(AOwner: TComponent);
begin
  inherited;
  Width := 256;
  Height := 256;

  BGColor := clRed; // just to stay out of the way...

  CharWidth := 32;
  CharHeight := 32;

  FFont := TFont.Create;
  FFont.Color := clWhite;
  FFont.FontAdapter := Self;
  GlyphsAlpha := tiaTopLeftPointColorTransparent;
end;

procedure TWindowsBitmapFont.SetFont(value: TFont);
begin
  FFont.Assign(value);
end;

procedure TWindowsBitmapFont.NotifyChange(Sender : TObject);

Begin
  FreeTextureHandle;
  InvalidateUsers;
  inherited;
End;

procedure TWindowsBitmapFont.LoadWindowsFont;

Procedure FixOverlappingFontRangeWidths;

Var
  ThisFontRange : TBitMapFontRange;
  CheckingFontRange : TBitMapFontRange;
  XC,YC : Integer;
  ThisEnd, CheckingEnd : Integer;
  Posi : Integer;

Begin
  For XC := Ranges.Count-1 downto 1 do
  Begin
    ThisFontRange := Ranges[XC];
    ThisEnd := ThisFontRange.StartGlyphIdx+Byte(ThisFontRange.StopASCII)-Byte(ThisFontRange.StartASCII);
    For YC := XC-1 downto 0 do
    Begin
      CheckingFontRange := Ranges[YC];
      CheckingEnd := CheckingFontRange.StartGlyphIdx+Byte(CheckingFontRange.StopASCII)-Byte(CheckingFontRange.StartASCII);

      If (ThisFontRange.StartGlyphIdx > CheckingEnd) or (CheckingFontRange.StartGlyphIdx > ThisEnd) then continue;

      For posi := ThisFontRange.StartGlyphIdx to ThisEnd do
      Begin
        If (posi > CheckingEnd) or (CheckingFontRange.StartGlyphIdx > posi) then continue;
        CheckingFontRange.Widths[posi-CheckingFontRange.StartGlyphIdx] := ThisFontRange.Widths[posi-ThisFontRange.StartGlyphIdx];
      End;
    End;
  End;
end;


Var
  BitMap : TBitMap;
  FontRange  : TBitMapFontRange;
  ch : Char;
  TilePosi : Integer;
  X,Y : Integer;
  XC : Integer;
  CharsPerRow : Integer;
  MaxChars    : Integer;
  CharRect    : TGLRect;

Begin
  BitMap := Glyphs.Bitmap;  // First Get the Bitmap with which you're working...

  BitMap.PixelFormat := pf32bit; // could be a lot less, if font color is white, do something about it?
  BitMap.Canvas.brush.Style := bssolid;
  BitMap.Canvas.brush.Color := FBgColor;

  CharsPerRow := FWidth div CharWidth; // calculate how many chars that can be on one row.
  MaxChars := CharsPerRow*(FHeight div CharHeight); // calculate how many chars that can be within Width & Height

  BitMap.Canvas.FillRect(Rect(0,0,BitMap.Width,BitMap.Height)); // Clear Canvas... Not really neaded as we clear each rect before drawing on it. Just makes it a lot nicer...

  BitMap.Canvas.Font.assign(Font); // This is the real part! SET THE FONT!
  FMaxHeight := 0;

  For XC := 0 to Ranges.Count-1 do // run through the ranges...
  Begin
    FontRange := Ranges.Items[XC];
    TilePosi := FontRange.StartGlyphIdx;
    For ch := FontRange.StartASCII to FontRange.StopASCII do // run through the chars in each range...
    Begin
      FontRange.Widths[Byte(ch)-Byte(FontRange.StartASCII)] := BitMap.Canvas.TextWidth(ch)+1;

      If BitMap.Canvas.TextHeight(ch) > FMaxHeight then FMaxHeight := BitMap.Canvas.TextHeight(ch);

      Assert(TilePosi < MaxChars,'MatchWindowsFont: Too many chars on a too small Image... Use higher width and height settings...');

      X := (TilePosi mod CharsPerRow); // calc x grid
      Y := (TilePosi div CharsPerRow); // calc y grid

      X := X*CharWidth;  // calc grid x pixel position
      Y := Y*CharHeight; // calc grid y pixel position

      CharRect := Rect(X,Y,X+CharWidth,Y+CharHeight); // Calc Grid Rect!
      BitMap.Canvas.FillRect(CharRect);       // clear this char! Needed if uppercase and lowercase is on same spot!
//      BitMap.Canvas.brush.Style := bsClear;
      BitMap.Canvas.TextRect(CharRect,X,Y,ch); // Draw the Char!
//      BitMap.Canvas.brush.Style := bssolid;
      CharRect.Right := CharRect.Left+FontRange.Widths[Byte(ch)-Byte(FontRange.StartASCII)];

      inc(TilePosi); // goto next tile...
    End;
  End;

  FixOverlappingFontRangeWidths; // allows a..z ontop A..Z shown correctly...

  BitMap.Canvas.brush.Style := bsDiagCross;
  BitMap.Canvas.FrameRect(Rect(-1,-1,-1,-1));
End;

procedure TWindowsBitmapFont.PrepareImage;

Begin
  LoadWindowsFont;
  inherited;
End;

procedure TWindowsBitmapFont.Changed;

Begin
  NotifyChange(Self);
End;

destructor TWindowsBitmapFont.Destroy;

begin
  FFont.Free;
  inherited;
end;

procedure TWindowsBitmapFont.SetBGColor(value: TColor);
begin
  FBGColor := value;
  NotifyChange(Self);
end;

procedure TWindowsBitmapFont.SetHeight(value: Integer);
begin
  FHeight := value;
  Glyphs.Bitmap.Height := value;
  NotifyChange(Self);
end;

procedure TWindowsBitmapFont.SetWidth(value: Integer);
begin
  FWidth := value;
  Glyphs.Bitmap.Width := value;
  NotifyChange(Self);
end;

end.
