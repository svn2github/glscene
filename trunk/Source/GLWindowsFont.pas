// GLWindowsFont
{: TFont Import into a BitmapFont using variable width...<p>

	<b>History : </b><font size=-1><ul>
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

{$include GLScene.inc}

uses
  GLBitmapFont, Classes, GLScene, GLTexture, GLCrossPlatform,
  {$IFDEF MSWINDOWS}
  Graphics
  {$ENDIF}
  {$IFDEF LINUX}
  QGraphics
  {$ENDIF}
  ;

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
   TGLWindowsBitmapFont = class (TGLCustomBitmapFont)
      private
	      { Private Declarations }
         FFont : TFont;

      protected
	      { Protected Declarations }
         procedure SetFont(value : TFont);
         procedure LoadWindowsFont;
         function StoreRanges : Boolean;

         procedure PrepareImage; override;
         function TextureFormat : Integer; override;

      public
	      { Public Declarations }
         constructor Create(AOwner : TComponent); override;
         destructor  Destroy; override;

         procedure NotifyChange(Sender : TObject); override;

         function FontTextureWidth : Integer;
         function FontTextureHeight : Integer;

         property Glyphs;

      published
	      { Published Declarations }
         {: The font used to prepare the texture.<p>
            Note: the font color is ignored. }
         property Font : TFont read FFont write SetFont;

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

uses SysUtils, VectorGeometry, OpenGL1x;

// ------------------
// ------------------ TGLWindowsBitmapFont ------------------
// ------------------

// Create
//
constructor TGLWindowsBitmapFont.Create(AOwner: TComponent);
begin
   inherited;

   FFont:=TFont.Create;
   FFont.Color:=clWhite;
   FFont.OnChange:=NotifyChange;
   GlyphsAlpha:=tiaAlphaFromIntensity;

   Ranges.Add(' ', '}');

   LoadWindowsFont;   
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
function TGLWindowsBitmapFont.FontTextureWidth : Integer;
begin
   Result:=Glyphs.Width;
end;

// FontTextureHeight
//
function TGLWindowsBitmapFont.FontTextureHeight : Integer;
begin
   Result:=Glyphs.Height;
end;

// SetFont
//
procedure TGLWindowsBitmapFont.SetFont(value: TFont);
begin
   FFont.Assign(value);
end;

// NotifyChange
//
procedure TGLWindowsBitmapFont.NotifyChange(Sender : TObject);
begin
   FreeTextureHandle;
   InvalidateUsers;
   inherited;
end;

// LoadWindowsFont
//
procedure TGLWindowsBitmapFont.LoadWindowsFont;
var
   textureWidth, textureHeight : Integer;

   function ComputeCharRects(x, y : Integer; canvas : TCanvas) : Integer;
   var
      px, py, cw, n : Integer;
      rect : TGLRect;
   begin
      Result:=0;
      n:=0;
      px:=0;
      py:=0;
      while n<256 do begin
         cw:=CharWidths[n];
         if cw>0 then begin
            Inc(cw, 2);
            if Assigned(canvas) then begin
               SetCharRects(n, VectorMake((px+1.05)/textureWidth,
                                          (textureHeight-(py+0.05))/textureHeight,
                                          (px+cw-1.05)/textureWidth,
                                          (textureHeight-(py+CharHeight-0.05))/textureHeight));
               rect.Left:=px;
               rect.Top:=py;
               rect.Right:=px+cw;
               rect.Bottom:=py+CharHeight;
               // Draw the Char, the trailing space is to properly handle the italics.
               canvas.TextRect(rect, px+1, py+1, Char(n)+' ');
            end;
            if ((n<255) and (px+cw+CharWidths[n+1]+2<=x)) or (n=255) then
               Inc(px, cw)
            else begin
               px:=0;
               Inc(py, CharHeight);
               if py+CharHeight>y then Break;
            end;
            Inc(Result);
         end;
         Inc(n);
      end;
   end;


var
   bitmap : TGLBitMap;
   fontRange  : TBitmapFontRange;
   ch : Char;
   x, y, i, cw : Integer;
   nbChars, n : Integer;
   texMem, bestTexMem : Integer;
begin
   InvalidateUsers;
   bitmap:=Glyphs.Bitmap;
   Glyphs.OnChange:=nil;

   bitmap.PixelFormat:=glpf32bit;
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

   // Retrieve width of all characters (texture width)
   ResetCharWidths(0);
   for i:=0 to Ranges.Count-1 do begin
      fontRange:=Ranges.Items[i];
      for ch:=fontRange.StartASCII to fontRange.StopASCII do begin
         cw:=bitmap.Canvas.TextWidth(ch)-HSpaceFix;
         SetCharWidths(Integer(ch), cw);
      end;
   end;

   // compute texture size: look for best fill ratio
   // and as square a texture as possible
   bestTexMem:=MaxInt;
   textureWidth:=0;
   textureHeight:=0;
   y:=64; while y<=512 do begin
      x:=64; while x<=512 do begin
         // compute the number of characters that fit
         n:=ComputeCharRects(x, y, nil);
         if n=nbChars then begin
            texMem:=x*y;
            if (texMem<bestTexMem) or ((texMem=bestTexMem) and (Abs(x-y)<Abs(textureWidth-textureHeight))) then begin
               textureWidth:=x;
               textureHeight:=y;
               bestTexMem:=texMem;
            end;
         end;
         x:=2*x;
      end;
      y:=y*2;
   end;

   if bestTexMem=MaxInt then begin
      Font.Size:=6;
      raise Exception.Create('Characters are too large or too many. Unable to create font texture.');
   end;

   bitmap.Width:=textureWidth;
   bitmap.Height:=textureHeight;

   with bitmap.Canvas do begin
      Brush.Style:=bsSolid;
      Brush.Color:=clBlack;
      FillRect(Rect(0, 0, textureWidth, textureHeight));
   end;

   ComputeCharRects(textureWidth, textureHeight, bitmap.Canvas);

   Glyphs.OnChange:=OnGlyphsChanged;
end;

// StoreRanges
//
function TGLWindowsBitmapFont.StoreRanges : Boolean;
begin
   Result:=(Ranges.Count<>1) or (Ranges[0].StartASCII<>' ') or (Ranges[0].StopASCII<>'}');
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
function TGLWindowsBitmapFont.TextureFormat : Integer;
begin
   Result:=GL_ALPHA;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClasses([TGLWindowsBitmapFont]);

end.
