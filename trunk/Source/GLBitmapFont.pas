// GLBitmapFont
{: Bitmap Fonts management classes for GLScene<p>

	<b>History : </b><font size=-1><ul>
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

interface

uses Classes, GLScene, Geometry, GLMisc, StdCtrls, GLContext, GLCrossPlatform,
   GLTexture;

type

	// TBitmapFontRange
	//
   {: An individual character range in a bitmap font.<p>
      A range allows mapping ASCII characters to character tiles in a font
      bitmap, tiles are enumerated line then column (raster). }
	TBitmapFontRange = class (TCollectionItem)
	   private
	      { Private Declarations }
         FStartASCII, FStopASCII : Char;
         FStartGlyphIdx : Integer;

	   protected
	      { Protected Declarations }
         procedure SetStartASCII(const val : Char);
         procedure SetStopASCII(const val : Char);
         procedure SetStartGlyphIdx(const val : Integer);
         function GetDisplayName : String; override;

      public
         Widths : Array of Integer;
	      { Public Declarations }
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

	   published
	      { Published Declarations }
         property StartASCII : Char read FStartASCII write SetStartASCII;
         property StopASCII : Char read FStopASCII write SetStopASCII;
         property StartGlyphIdx : Integer read FStartGlyphIdx write SetStartGlyphIdx;
	end;

	// TBitmapFontRanges
	//
	TBitmapFontRanges = class (TCollection)
	   protected
	      { Protected Declarations }
	      owner : TComponent;
	      function GetOwner: TPersistent; override;
         procedure SetItems(index : Integer; const val : TBitmapFontRange);
	      function GetItems(index : Integer) : TBitmapFontRange;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent);
         function Add: TBitmapFontRange;
	      function FindItemID(ID: Integer): TBitmapFontRange;
	      property Items[index : Integer] : TBitmapFontRange read GetItems write SetItems; default;

         {: Converts an ASCII character into a tile index.<p>
            Return -1 if character cannot be rendered. }
	      function CharacterToTileIndex(aChar : Char) : Integer;
         procedure NotifyChange;
   end;

	// TBitmapFont
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
	TBitmapFont = class (TComponent)
	   private
	      { Private Declarations }
         FRanges : TBitmapFontRanges;
         FGlyphs : TGLPicture;
         FCharWidth, FCharHeight : Integer;
         FGlyphsIntervalX, FGlyphsIntervalY : Integer;
         FHSpace, FVSpace : Integer;
         FUsers : TList;
         FTextureHandle : TGLTextureHandle;
         FHandleIsDirty : Boolean;
			FMinFilter : TGLMinFilter;
			FMagFilter : TGLMagFilter;
         FTextureWidth, FTextureHeight : Integer;
         FGlyphsAlpha : TGLTextureImageAlpha;

	   protected
	      { Protected Declarations }
         procedure SetRanges(const val : TBitmapFontRanges);
         procedure SetGlyphs(const val : TGLPicture);
         procedure SetCharWidth(const val : Integer);
         procedure SetCharHeight(const val : Integer);
         procedure SetGlyphsIntervalX(const val : Integer);
         procedure SetGlyphsIntervalY(const val : Integer);
	      procedure OnGlyphsChanged(Sender : TObject);
         procedure SetHSpace(const val : Integer);
         procedure SetVSpace(const val : Integer);
			procedure SetMagFilter(AValue: TGLMagFilter);
			procedure SetMinFilter(AValue: TGLMinFilter);
         procedure SetGlyphsAlpha(val : TGLTextureImageAlpha);

	      procedure InvalidateUsers;
	      function CharactersPerRow : Integer;
	      procedure TileIndexToTexCoords(tileIndex : Integer; var topLeft, bottomRight : TTexPoint; ThisCharWidth : Single = -1);
         procedure PrepareImage;
         procedure PrepareParams;

	   public
              MaxHeight   : Integer;

	      { Public Declarations }
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
              Function  CalcCharWidth(Ch : Char) : Integer;
              Function  CalcStringWidth(St : String) : Integer;

	      procedure RegisterUser(anObject : TGLBaseSceneObject); virtual;
	      procedure UnRegisterUser(anObject : TGLBaseSceneObject); virtual;

         {: Renders the given string at current position.<p>
            The current matrix is blindly used, meaning you can render all kinds
            of rotated and linear distorted text with this method. }
	      procedure RenderString(const aString : String; alignment : TAlignment;
                                layout : TTextLayout; color : TColorVector; position : PVector = Nil);
	   published
	      { Published Declarations }
         {: A single bitmap containing all the characters.<p>
            The transparent color is that of the top left pixel. }
         property Glyphs : TGLPicture read FGlyphs write SetGlyphs;
         {: Nb of horizontal pixels between two columns in the Glyphs. }
         property GlyphsIntervalX : Integer read FGlyphsIntervalX write SetGlyphsIntervalX;
         {: Nb of vertical pixels between two rows in the Glyphs. }
         property GlyphsIntervalY : Integer read FGlyphsIntervalY write SetGlyphsIntervalY;
         {: Ranges allow converting between ASCII and tile indexes.<p>
            See TBitmapFontRange. }
         property Ranges : TBitmapFontRanges read FRanges write SetRanges;

         {: Width of a single character. }
         property CharWidth : Integer read FCharWidth write SetCharWidth default 16;
         {: Height of a single character. }
         property CharHeight : Integer read FCharHeight write SetCharHeight default 16;
         {: Pixels in between rendered characters (horizontally). }
         property HSpace : Integer read FHSpace write SetHSpace default 1;
         {: Pixels in between rendered lines (vertically). }
         property VSpace : Integer read FVSpace write SetVSpace default 1;

			property MagFilter: TGLMagFilter read FMagFilter write SetMagFilter default maLinear;
			property MinFilter: TGLMinFilter read FMinFilter write SetMinFilter default miLinear;
	end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL12, GLGraphics, XOpenGL;

// ------------------
// ------------------ TBitmapFontRange ------------------
// ------------------

// Create
//
constructor TBitmapFontRange.Create(Collection : TCollection);
begin
  inherited Create(Collection);
  SetLength(Widths,0);
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
	if Source is TBitmapFontRange then begin
      FStartASCII:=TBitmapFontRange(Source).FStartASCII;
      FStopASCII:=TBitmapFontRange(Source).FStopASCII;
      FStartGlyphIdx:=TBitmapFontRange(Source).FStartGlyphIdx;
	end;
	inherited;
end;

// GetDisplayName
//
function TBitmapFontRange.GetDisplayName : String;
begin
	Result:=Format('ASCII [#%d, #%d] -> Glyphs [%d, %d]',
                  [Integer(StartASCII), Integer(StopASCII), StartGlyphIdx,
                   StartGlyphIdx+Integer(StopASCII)-Integer(StartASCII)]);
end;

// SetStartASCII
//
procedure TBitmapFontRange.SetStartASCII(const val : Char);
Var
  XC : Integer;
begin
  FStartASCII:=val;
  if FStartASCII>FStopASCII then
    FStopASCII:=FStartASCII;
  SetLength(Widths,Byte(FStopASCII)-Byte(FStartASCII)+1);
  if Assigned(TBitmapFontRanges(Collection).owner) and (TBitmapFontRanges(Collection).owner is TBitmapFont) then
  Begin
    For XC := 0 to Length(Widths)-1 do
    Begin
      Widths[XC] := (TBitmapFontRanges(Collection).owner as TBitmapFont).charwidth;
    End;
  End;
  TBitmapFontRanges(Collection).NotifyChange;
end;

// SetStopASCII
//
procedure TBitmapFontRange.SetStopASCII(const val : Char);
Var
  XC : Integer;
begin
   FStopASCII:=val;
   if FStopASCII<FStartASCII then
      FStartASCII:=FStopASCII;
   SetLength(Widths,Byte(FStopASCII)-Byte(FStartASCII)+1);
   if Assigned(TBitmapFontRanges(Collection).owner) and (TBitmapFontRanges(Collection).owner is TBitmapFont) then
   Begin
     For XC := 0 to Length(Widths)-1 do
     Begin
       Widths[XC] := (TBitmapFontRanges(Collection).owner as TBitmapFont).charwidth;
     End;
   End;
   TBitmapFontRanges(Collection).NotifyChange;
end;

// SetStartGlyphIdx
//
procedure TBitmapFontRange.SetStartGlyphIdx(const val : Integer);
begin
   if val>=0 then
      FStartGlyphIdx:=val
   else FStartGlyphIdx:=0;
   TBitmapFontRanges(Collection).NotifyChange;
end;

// ------------------
// ------------------ TBitmapFontRanges ------------------
// ------------------

constructor TBitmapFontRanges.Create(AOwner : TComponent);
begin
	Owner:=AOwner;
	inherited Create(TBitmapFontRange);
end;

function TBitmapFontRanges.GetOwner: TPersistent;
begin
	Result:=Owner;
end;

procedure TBitmapFontRanges.SetItems(index : Integer; const val : TBitmapFontRange);
begin
	inherited Items[index]:=val;
end;

function TBitmapFontRanges.GetItems(index : Integer) : TBitmapFontRange;
begin
	Result:=TBitmapFontRange(inherited Items[index]);
end;

function TBitmapFontRanges.Add: TBitmapFontRange;
begin
	Result:=(inherited Add) as TBitmapFontRange;
end;

function TBitmapFontRanges.FindItemID(ID: Integer): TBitmapFontRange;
begin
	Result:=(inherited FindItemID(ID)) as TBitmapFontRange;
end;

// CharacterToTileIndex
//
function TBitmapFontRanges.CharacterToTileIndex(aChar : Char) : Integer;
var
   i : Integer;
begin
   Result:=-1;
   for i:=0 to Count-1 do with Items[i] do
      if (aChar>=StartASCII) and (aChar<=StopASCII) then begin
         Result:=StartGlyphIdx+Integer(aChar)-Integer(StartASCII);
         Break;
      end;
end;

// NotifyChange
//
procedure TBitmapFontRanges.NotifyChange;
begin
   if Assigned(Owner) and (Owner is TGLBaseSceneObject) then
      TGLBaseSceneObject(Owner).StructureChanged;
end;

// ------------------
// ------------------ TBitmapFont ------------------
// ------------------

// Creat
//
constructor TBitmapFont.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   FRanges:=TBitmapFontRanges.Create(Self);
   FGlyphs:=TGLPicture.Create;
   FGlyphs.OnChange:=OnGlyphsChanged;
   FCharWidth:=16;
   FCharHeight:=16;
   FHSpace:=1;
   FVSpace:=1;
   FUsers:=TList.Create;
   FHandleIsDirty:=True;
   FMinFilter:=miLinear;
   FMagFilter:=maLinear;
   FTextureHandle:=TGLTextureHandle.Create;
end;

// Destroy
//
destructor TBitmapFont.Destroy;
begin
	inherited Destroy;
   FTextureHandle.Free;
   FRanges.Free;
   FGlyphs.Free;
   Assert(FUsers.Count=0);
   FUsers.Free;
end;

Function  TBitmapFont.CalcCharWidth(Ch : Char) : Integer;

Var
  I : Integer;
Begin
  for i:=0 to FRanges.Count-1 do
  with FRanges.Items[i] do
  if (Ch>=StartASCII) and (Ch<=StopASCII) then
  begin
    Result := FRanges.Items[i].Widths[Byte(Ch)-Byte(StartASCII)];
    Break;
  end;
End;

Function  TBitmapFont.CalcStringWidth(St : String) : Integer;

Var
  I : Integer;
Begin
  Result := 0;
  for i:=1 to Length(St)-1 do
  Begin
    Result := Result+CalcCharWidth(St[i]);
    Inc(Result,HSpace);
  end;
  If St <> '' then Dec(Result,HSpace);
End;


// SetRanges
//
procedure TBitmapFont.SetRanges(const val : TBitmapFontRanges);
begin
   FRanges.Assign(val);
   InvalidateUsers;
end;

// SetGlyphs
//
procedure TBitmapFont.SetGlyphs(const val : TGLPicture);
begin
   FGlyphs.Assign(val);
end;

// SetCharWidth
//
procedure TBitmapFont.SetCharWidth(const val : Integer);
begin
   if val>1 then
      FCharWidth:=val
   else FCharWidth:=1;
   InvalidateUsers;
end;

// SetCharHeight
//
procedure TBitmapFont.SetCharHeight(const val : Integer);
begin
   if val>1 then
      FCharHeight:=val
   else FCharHeight:=1;
   InvalidateUsers;
end;

// SetGlyphsIntervalX
//
procedure TBitmapFont.SetGlyphsIntervalX(const val : Integer);
begin
   if val>0 then
      FGlyphsIntervalX:=val
   else FGlyphsIntervalX:=0;
   InvalidateUsers;
end;

// SetGlyphsIntervalY
//
procedure TBitmapFont.SetGlyphsIntervalY(const val : Integer);
begin
   if val>0 then
      FGlyphsIntervalY:=val
   else FGlyphsIntervalY:=0;
   InvalidateUsers;
end;

// SetHSpace
//
procedure TBitmapFont.SetHSpace(const val : Integer);
begin
   if val>0 then
      FHSpace:=val
   else FHSpace:=0;
   InvalidateUsers;
end;

// SetVSpace
//
procedure TBitmapFont.SetVSpace(const val : Integer);
begin
   if val>0 then
      FVSpace:=val
   else FVSpace:=0;
   InvalidateUsers;
end;

// SetMagFilter
//
procedure TBitmapFont.SetMagFilter(AValue: TGLMagFilter);
begin
	if AValue <> FMagFilter then begin
		FMagFilter:=AValue;
      FHandleIsDirty:=True;
      InvalidateUsers;
	end;
end;

// SetMinFilter
//
procedure TBitmapFont.SetMinFilter(AValue: TGLMinFilter);
begin
	if AValue <> FMinFilter then begin
		FMinFilter:=AValue;
      FHandleIsDirty:=True;
      InvalidateUsers;
	end;
end;

// SetGlyphsAlpha
//
procedure TBitmapFont.SetGlyphsAlpha(val : TGLTextureImageAlpha);
begin
	if val<>FGlyphsAlpha then begin
		FGlyphsAlpha:=val;
      FHandleIsDirty:=True;
      InvalidateUsers;
	end;
end;

// OnGlyphsChanged
//
procedure TBitmapFont.OnGlyphsChanged(Sender : TObject);
begin
   InvalidateUsers;
end;

// RegisterUser
//
procedure TBitmapFont.RegisterUser(anObject : TGLBaseSceneObject);
begin
   Assert(FUsers.IndexOf(anObject)<0);
   FUsers.Add(anObject);
end;

// UnRegisterUser
//
procedure TBitmapFont.UnRegisterUser(anObject : TGLBaseSceneObject);
begin
   FUsers.Remove(anObject);
end;

// PrepareImage
//
procedure TBitmapFont.PrepareImage;
var
   bitmap : TGLBitmap;
   bitmap32 : TGLBitmap32;
begin
   bitmap:=TGLBitmap.Create;
   with bitmap do begin
      PixelFormat:=glpf24bit;
      Width:=RoundUpToPowerOf2(Glyphs.Width);
      Height:=RoundUpToPowerOf2(Glyphs.Height);
      Canvas.Draw(0, 0, Glyphs.Graphic);
   end;
   bitmap32:=TGLBitmap32.Create;
   bitmap32.Assign(bitmap);
   bitmap.Free;
   with bitmap32 do begin
      case FGlyphsAlpha of
         tiaAlphaFromIntensity :
            SetAlphaFromIntensity;
         tiaSuperBlackTransparent :
            SetAlphaTransparentForColor($000000);
         tiaLuminance :
            SetAlphaFromIntensity;
         tiaLuminanceSqrt : begin
            SetAlphaFromIntensity;
            SqrtAlpha;
         end;
         tiaDefault,
         tiaOpaque :
            SetAlphaToValue(255);
         tiaTopLeftPointColorTransparent :
            SetAlphaTransparentForColor(Data[Width*(Height-1)]);
      else
         Assert(False);
      end;
      RegisterAsOpenGLTexture(GL_TEXTURE_2D, MinFilter, GL_RGBA);
      FTextureWidth:=Width;
      FTextureHeight:=Height;
      Free;
   end;
end;

// PrepareParams
//
procedure TBitmapFont.PrepareParams;
const
	cTextureMagFilter : array [maNearest..maLinear] of TGLEnum =
							( GL_NEAREST, GL_LINEAR );
	cTextureMinFilter : array [miNearest..miLinearMipmapLinear] of TGLEnum =
							( GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST,
							  GL_LINEAR_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_LINEAR,
							  GL_LINEAR_MIPMAP_LINEAR );
begin
	glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
	glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
	glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
	glPixelStorei(GL_UNPACK_SKIP_ROWS, 0);
	glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, cTextureMinFilter[FMinFilter]);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, cTextureMagFilter[FMagFilter]);
end;

// RenderString
//
procedure TBitmapFont.RenderString(const aString : String; alignment : TAlignment; layout : TTextLayout; color : TColorVector; position : PVector = Nil);

   function AlignmentAdjustement(p : Integer) : Single;
   var
      i : Integer;
   begin
      i:=0;
      while (p<=Length(aString)) and (aString[p]<>#13) do begin
         Inc(p); Inc(i);
      end;
      case alignment of
         taLeftJustify : Result:=0;
//         taRightJustify : Result:=-(i*(CharWidth+HSpace)-HSpace)
         taRightJustify : Result:=-CalcStringWidth(Copy(aString,p-i,i))
      else // taCenter
//          Result:=-(i*(CharWidth+HSpace)-HSpace)/2;
          Result:=-CalcStringWidth(Copy(aString,p-i,i))/2;
      end;
   end;

   function LayoutAdjustement : Single;
   var
      i, n : Integer;
   begin
      n:=1;
      for i:=1 to Length(aString) do
         if aString[i]=#13 then Inc(n);

      case layout of
         tlTop : Result:=0;
         tlBottom : Result:=(n*(CharHeight+VSpace)-VSpace);
      else // tlCenter
         Result:=(n*(CharHeight+VSpace)-VSpace)/2;
      end;
   end;

Var
   i, idx : Integer;
   topLeft, bottomRight : TTexPoint;
   vTopLeft, vBottomRight : TVector;
   deltaH, deltaV : Single;
begin
   if (Glyphs.Width=0) or (aString='') then Exit;
   // prepare texture if necessary
   if FHandleIsDirty then
   begin
      // prepare handle
      if FTextureHandle.Handle = 0 then
      begin
         FTextureHandle.AllocateHandle;
         Assert(FTextureHandle.Handle<>0);
      end;
      SetGLCurrentTexture(0, GL_TEXTURE_2D, FTextureHandle.Handle);
      // texture registration
      if Glyphs.Width<>0 then
      begin
         PrepareImage;
         PrepareParams;
      end;
      FHandleIsDirty:=False;
   end;
   // precalcs
   MakePoint(vTopLeft, AlignmentAdjustement(1),  LayoutAdjustement, 0);

   If Assigned(position) then
   vTopLeft := VectorAdd(vTopLeft,position^);

   deltaH:=CharWidth+HSpace;
   deltaV:=-(CharHeight+VSpace);
   // set states
	glEnable(GL_TEXTURE_2D);
   glDisable(GL_LIGHTING);
	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   SetGLCurrentTexture(0, GL_TEXTURE_2D, FTextureHandle.Handle);
   glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
   // start rendering
   glBegin(GL_QUADS);
   for i:=1 to Length(aString) do
   begin
     case aString[i] of
         #0..#12, #14..#31 : ; // ignore
         #13 :
         begin
            vTopLeft[0]:=AlignmentAdjustement(i);
            vTopLeft[1]:=vTopLeft[1]+deltaV;

            If Assigned(position) then vTopLeft[0]:=vTopLeft[0]+position[0];
         end;
         else
         Begin
            idx:=Ranges.CharacterToTileIndex(aString[i]);
            deltaH := CalcCharWidth(aString[i]);
            MakePoint(vBottomRight, vTopLeft[0]+deltaH-1, vTopLeft[1]-CharHeight, 0);
            if idx>=0 then
            begin
              TileIndexToTexCoords(idx, topLeft, bottomRight, deltaH);

              glColor4fv(@color); // nel


              glTexCoord2fv(@topLeft);
              glVertex4fv(@vTopLeft);

              glTexCoord2f(topLeft.S, bottomRight.T);
              glVertex2f(vTopLeft[0], vBottomRight[1]);

              glTexCoord2fv(@bottomRight);
              glVertex4fv(@vBottomRight);

              glTexCoord2f(bottomRight.S, topLeft.T);
              glVertex2f(vBottomRight[0], vTopLeft[1]);
            end;
            vTopLeft[0]:=vTopLeft[0]+deltaH+HSpace;
         end;
     End;
  end;
  glEnd;
end;

// CharactersPerRow
//
function TBitmapFont.CharactersPerRow : Integer;
begin
   if FGlyphs.Width>0 then
   	Result:=(FGlyphs.Width+FGlyphsIntervalX) div (FGlyphsIntervalX+FCharWidth)
   else Result:=0;
end;

// TileIndexToTexCoords
//
procedure TBitmapFont.TileIndexToTexCoords(tileIndex : Integer;
                                           var topLeft, bottomRight : TTexPoint; ThisCharWidth : Single = -1);
var
   carX, carY : Integer;
begin
  If thisCharWidth = -1 then thisCharWidth := CharWidth;

   carX:=(tileIndex mod CharactersPerRow)*(CharWidth+GlyphsIntervalX);
   carY:=(tileIndex div CharactersPerRow)*(CharHeight+GlyphsIntervalY);
   topLeft.S:=(carX+0.05)/FTextureWidth;
   topLeft.T:=(FTextureHeight-(carY+0.05))/FTextureHeight;
   bottomRight.S:=(carX+ThisCharWidth-1.05)/FTextureWidth;
   bottomRight.T:=(FTextureHeight-(carY+CharHeight-0.05))/FTextureHeight;



   exit;
  If thisCharWidth = -1 then thisCharWidth := CharWidth;
//  thisCharWidth := CharWidth;
   carX:=(tileIndex mod CharactersPerRow)*(CharWidth+GlyphsIntervalX);
   carY:=(tileIndex div CharactersPerRow)*(CharHeight+GlyphsIntervalY);
   topLeft.S:=(carX+0.05)/FTextureWidth;
   topLeft.T:=FTextureHeight-(carY+0.05)/FTextureHeight;
   bottomRight.S:=(carX-0.05-(thisCharWidth/CharWidth))/FTextureWidth;
   bottomRight.T:=FTextureHeight-(carY+CharHeight-1.05)/FTextureHeight;
end;

// InvalidateUsers
//
procedure TBitmapFont.InvalidateUsers;
var
   i : Integer;
begin
   for i:=FUsers.Count-1 downto 0 do
      TGLBaseSceneObject(FUsers[i]).NotifyChange(Self);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClasses([TBitmapFont]);

end.

