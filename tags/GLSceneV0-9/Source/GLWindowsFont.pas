// TFont Import into a BitmapFont using variable width...
{: TFont Import into a BitmapFont using variable width...<p>

	<b>History : </b><font size=-1><ul>
      <li>12/08/02 - JAJ - Made into a standalone unit...
	</ul></font>
}
unit GLWindowsFont;

interface

Uses
  GLBitMapFont, Graphics, Classes, GLScene, Types;

Type
  TWindowsBitmapFont = class (TBitmapFont)
  private
    FFont : TFont;
    FWidth : Integer;
    FHeight : Integer;
    FBGColor : TColor;
    FReload : Boolean;
  protected
    Procedure SetFont(value : TFont);
    Procedure SetReload(value : Boolean);
    procedure LoadWindowsFont;
  public
    Constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure RegisterUser(anObject : TGLBaseSceneObject); override;

  published
    property Font : TFont read FFont write SetFont;
    property Width : Integer read FWidth write FWidth;
    property Height : Integer read FHeight write FHeight;
    property Reload : Boolean read FReload write SetReload;
    property BGColor : TColor read FBGColor write FBGColor;
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
  FWidth := 256;
  FHeight := 256;
  FFont := TFont.Create;
  FReload := False;
end;

procedure TWindowsBitmapFont.SetFont(value: TFont);
begin
  FFont.Assign(value);
end;

Procedure TWindowsBitmapFont.SetReload(value : Boolean);

Begin
  LoadWindowsFont;
End;

procedure TWindowsBitmapFont.LoadWindowsFont;

Var
  BitMap : TBitMap;
  FontRange  : TBitMapFontRange;
  ch : Char;
  TilePosi : Integer;
  X,Y : Integer;
  XC : Integer;
  CharsPerRow : Integer;
  MaxChars    : Integer;
  CharRect    : TRect;

Begin
  BitMap := Glyphs.Bitmap;  // First Get the Bitmap with which you're working...

  BitMap.PixelFormat := pf4bit;
  BitMap.Width := FWidth;    // Set its width
  BitMap.Height := FHeight;  // and height!
  BitMap.Canvas.brush.Style := bssolid;
  BitMap.Canvas.brush.Color := FBgColor;

  CharsPerRow := FWidth div CharWidth; // calculate how many chars that can be on one row.
  MaxChars := CharsPerRow*(FHeight div CharHeight); // calculate how many chars that can be within Width & Height

  BitMap.Canvas.FillRect(Rect(0,0,BitMap.Width,BitMap.Height)); // Clear Canvas... Not really neaded as we clear each rect before drawing on it. Just makes it a lot nicer...

  BitMap.Canvas.Font.assign(Font); // This is the real part! SET THE FONT!
  MaxHeight := 0;

  For XC := 0 to Ranges.Count-1 do // run through the ranges...
  Begin
    FontRange := Ranges.Items[XC];
    TilePosi := FontRange.StartGlyphIdx;
    For ch := FontRange.StartASCII to FontRange.StopASCII do // run through the chars in each range...
    Begin
      FontRange.Widths[Byte(ch)-Byte(FontRange.StartASCII)] := BitMap.Canvas.TextWidth(ch)+1;

      If BitMap.Canvas.TextHeight(ch) > MaxHeight then MaxHeight := BitMap.Canvas.TextHeight(ch);

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

  BitMap.Canvas.brush.Style := bsDiagCross;
  BitMap.Canvas.FrameRect(Rect(-1,-1,-1,-1));

  InvalidateUsers;
End;

destructor TWindowsBitmapFont.Destroy;

begin
  FFont.Free;
  inherited;
end;

procedure TWindowsBitmapFont.RegisterUser(anObject : TGLBaseSceneObject);

Begin
  Reload := True;
  inherited;
End;


end.
