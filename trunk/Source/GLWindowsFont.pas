// GLWindowsFont
{: TFont Import into a BitmapFont using variable width...<p>

	<b>History : </b><font size=-1><ul>
      <li>28/09/02 - EG - Fixed transparency, style fixes, prop defaults fixed,
                          dropped interface dependency, texture size auto computed,
                          fixed italics spacing
      <li>06/09/02 - JAJ - Fixed alot of bugs... Expecially designtime updating bugs..
      <li>12/08/02 - JAJ - Made into a standalone unit...
	</ul></font>
}
unit GLWindowsFont;

interface

Uses
  GLBitmapFont, Graphics, Classes, GLScene, GLTexture, GLCrossPlatform;

Type

   // TWindowsBitmapFont
   //
   {: A bitmap font automatically built from a TFont.<p>
      It works like a TGLBitmapfont, you set ranges and which chars are assigned
      to which indexes, however here you also set the Font property to any TFont
      available to the system and it renders in GLScene as close to that font
      as posible, on some font types this is 100% on some a slight difference
      in spacing can occur at most 1 pixel per char on some char combinations.<br>
      This component should be ready for cross-platform development.
   }
   TWindowsBitmapFont = class (TGLCustomBitmapFont)
      private
	      { Private Declarations }
         FFont : TFont;
         FMaxHeight : Integer;

      protected
	      { Protected Declarations }
         procedure SetFont(value : TFont);
         procedure LoadWindowsFont;

         procedure PrepareImage; override;

      public
	      { Public Declarations }
         constructor Create(AOwner : TComponent); override;
         destructor  Destroy; override;

         procedure NotifyChange(Sender : TObject); override;

         property MaxHeight : Integer read FMaxHeight;
         function FontTextureWidth : Integer;
         function FontTextureHeight : Integer;

      published
	      { Published Declarations }
         {: The font used to prepare the texture.<p>
            Note: the font color is ignored. }
         property Font : TFont read FFont write SetFont;

			property MagFilter;
			property MinFilter;
         property Ranges;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, Geometry;

// ------------------
// ------------------ TWindowsBitmapFont ------------------
// ------------------

// Create
//
constructor TWindowsBitmapFont.Create(AOwner: TComponent);
begin
   inherited;

   FFont:=TFont.Create;
   FFont.Color:=clWhite;
   FFont.OnChange:=NotifyChange;
   GlyphsAlpha:=tiaAlphaFromIntensity;

   Ranges.Add('!', '}');

   LoadWindowsFont;   
end;

// Destroy
//
destructor TWindowsBitmapFont.Destroy;
begin
   FFont.Free;
   inherited;
end;

// FontTextureWidth
//
function TWindowsBitmapFont.FontTextureWidth : Integer;
begin
   Result:=Glyphs.Width;
end;

// FontTextureHeight
//
function TWindowsBitmapFont.FontTextureHeight : Integer;
begin
   Result:=Glyphs.Height;
end;

// SetFont
//
procedure TWindowsBitmapFont.SetFont(value: TFont);
begin
   FFont.Assign(value);
end;

// NotifyChange
//
procedure TWindowsBitmapFont.NotifyChange(Sender : TObject);
begin
   FreeTextureHandle;
   InvalidateUsers;
   inherited;
end;

// LoadWindowsFont
//
procedure TWindowsBitmapFont.LoadWindowsFont;

   procedure FixOverlappingFontRangeWidths;
   var
      fontRange : TBitMapFontRange;
      checkingFontRange : TBitMapFontRange;
      xC, yC : Integer;
      thisEnd, checkingEnd : Integer;
      posi : Integer;
   begin
      for xC:=Ranges.Count-1 downto 1 do begin
         fontRange:=Ranges[xC];
         thisEnd:=fontRange.StartGlyphIdx+Byte(fontRange.StopASCII)-Byte(fontRange.StartASCII);
         for yC:=xC-1 downto 0 do begin
            checkingFontRange:=Ranges[yC];
            checkingEnd:=CheckingFontRange.StartGlyphIdx+Byte(CheckingFontRange.StopASCII)
                                                        -Byte(CheckingFontRange.StartASCII);
            if (fontRange.StartGlyphIdx<=checkingEnd) and (checkingFontRange.StartGlyphIdx<=thisEnd) then begin
               for posi:=fontRange.StartGlyphIdx to thisEnd do begin
                  if (posi<=checkingEnd) and (checkingFontRange.StartGlyphIdx<=posi) then
                     checkingFontRange.Widths[posi-checkingFontRange.StartGlyphIdx]:=fontRange.Widths[posi-fontRange.StartGlyphIdx];
               end;
            end;
         end;
      end;
   end;

var
   bitmap : TBitMap;
   fontRange  : TBitmapFontRange;
   ch : Char;
   tilePosi : Integer;
   x, y, xC, th : Integer;
   charsPerRow, maxChars, nbChars, n : Integer;
   charRect : TGLRect;
   fillRatio, bestFillRatio : Single;
   bestX, bestY : Integer;
begin
   bitmap:=Glyphs.Bitmap;

   bitmap.PixelFormat:=pf32bit;
   with bitmap.Canvas do begin
      Font:=Self.Font;
      Font.Color:=clWhite;

      // get characters dimensions for the font
      CharWidth:=Round(2+MaxFloat(TextWidth('M'), TextWidth('W'), TextWidth('_')));
      CharHeight:=2+TextHeight('"_pI|,');

      if fsItalic in Font.Style then begin
         // italics aren't properly acknowledged in font width
         HSpaceFix:=-(CharWidth div 3);
         CharWidth:=CharWidth-HSpaceFix;
      end;
   end;

   nbChars:=Ranges.CharacterCount;

   // compute texture size (look for best fill ratio)
   bestFillRatio:=0;
   bestX:=0;
   bestY:=0;
   y:=64; while y<=512 do begin
      x:=64; while x<=512 do begin
         n:=(x div CharWidth)*(y div CharHeight);
         if n>=nbChars then begin
            fillRatio:=(n*CharWidth*CharHeight)/(x*y);
            if fillRatio>bestFillRatio then begin
               bestX:=x;
               bestY:=y;
               bestFillRatio:=fillRatio;
            end;
         end;
         x:=2*x;
      end;
      y:=y*2;
   end;

   if bestFillRatio=0 then
      raise Exception.Create('Characters are too large or too many. Unable to create font texture.');

   bitmap.Width:=bestX;
   bitmap.Height:=bestY;

   with bitmap.Canvas do begin
      Brush.Style:=bsSolid;
      Brush.Color:=clBlack;
      FillRect(Rect(0,0, bestX, bestY));
   end;

   charsPerRow:=bestX div CharWidth; // calculate how many chars that can be on one row.
   maxChars:=charsPerRow*(bestY div CharHeight); // calculate how many chars that can be within Width & Height

   FMaxHeight:=0;

   // run through the ranges...
   for xC:=0 to Ranges.Count-1 do begin
      fontRange:=Ranges.Items[xC];
      tilePosi:=FontRange.StartGlyphIdx;
      // run through the chars in each range...
      for ch:=fontRange.StartASCII to fontRange.StopASCII do begin
         fontRange.Widths[Byte(ch)-Byte(fontRange.StartASCII)]:=bitmap.Canvas.TextWidth(ch)+1-HSpaceFix;

         th:=bitmap.Canvas.TextHeight(ch);
         if th>FMaxHeight then
            FMaxHeight:=th;

         Assert(tilePosi<maxChars, 'MatchWindowsFont: Too many chars on a too small Image... Use higher width and height settings...');

         x:=(tilePosi mod CharsPerRow); // calc x grid
         y:=(tilePosi div CharsPerRow); // calc y grid

         x:=x*CharWidth;  // calc grid x pixel position
         y:=y*CharHeight; // calc grid y pixel position

         CharRect:=Rect(x, y, x+CharWidth, y+CharHeight); // Calc Grid Rect!
         // clear this char! Needed if uppercase and lowercase is on same spot!
         bitmap.Canvas.FillRect(CharRect);
         // Draw the Char! The trailing space is to properly handle the italics!
         bitmap.Canvas.TextRect(CharRect, x+1, y+1, ch+' ');
         CharRect.Right:=CharRect.Left+FontRange.Widths[Byte(ch)-Byte(fontRange.StartASCII)];

         Inc(tilePosi); // goto next tile...
      end;
   end;

   FixOverlappingFontRangeWidths; // allows a..z ontop A..Z shown correctly...
end;

// PrepareImage
//
procedure TWindowsBitmapFont.PrepareImage;
begin
   LoadWindowsFont;
   inherited;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClasses([TWindowsBitmapFont]);

end.
