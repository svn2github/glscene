// GLSpaceText
{: Win32 specific Context.<p>

	<b>History : </b><font size=-1><ul>
      <li>30/01/02 - EG - Text Alignment (Sören Mühlbauer)
      <li>28/12/01 - EG - Event persistence change (GliGli / Dephi bug)
	   <li>12/12/01 - EG - Creation (split from GLScene.pas)
	</ul></font>
}
unit GLSpaceText;

interface

{$i GLScene.inc}
{$IFDEF LINUX}{$Message Error 'Unit not supported'}{$ENDIF LINUX}

uses Windows, Classes, GLScene, Graphics, OpenGL12, GLTexture;

type

   // TSpaceTextCharRange
   //
   TSpaceTextCharRange = (stcrAlphaNum, stcrNumbers, stcrAll);

   // TGLTextHorzAdjust
   //
   TGLTextHorzAdjust = (haLeft, haCenter, haRight, haAligned, haCentrically, haFitIn);

   // TGLTextVertAdjust
   //
   TGLTextVertAdjust = (vaTop, vaCenter, vaBottom, vaBaseLine);

   // TGLTextAdjust
   //
   TGLTextAdjust = class(TPersistent)
      private
			{ Private Declarations }
         FHorz: TGLTextHorzAdjust;
         FVert: TGLTextVertAdjust;
         FOnChange: TNotifyEvent;
         procedure SetHorz(const Value: TGLTextHorzAdjust);
         procedure SetVert(const Value: TGLTextVertAdjust);

      public
			{ public Declarations }
         constructor Create;
         procedure Assign(Source: TPersistent); override;

         property OnChange: TNotifyEvent read FOnChange write FOnChange;

      published
			{ Published Declarations }
         property Horz: TGLTextHorzAdjust read FHorz write SetHorz default haLeft;
         property Vert: TGLTextVertAdjust read FVert write SetVert default vaBaseLine;
   end;

   // TSpaceText
   //
   {: Renders a text in 3D. }
   TSpaceText = class (TGLSceneObject)
      private
			{ Private Declarations }
         FFont       : TFont;
         FText       : String;
         FExtrusion  : Single;
         FAllowedDeviation : Single;
         FCharacterRange : TSpaceTextCharRange;
         FAdjust : TGLTextAdjust;
         FAspectRatio : Single;
         FOblique : Single;
         FTextHeight : Single;
         procedure SetCharacterRange(const val : TSpaceTextCharRange);
         procedure SetAllowedDeviation(const val : Single);
         procedure SetExtrusion(AValue: Single);
         procedure SetFont(AFont: TFont);
         procedure SetText(AText: String);
         procedure SetAdjust(const value : TGLTextAdjust);
         procedure SetAspectRatio(const value : Single);
         procedure SetOblique(const value : Single);
         procedure SetTextHeight(const value : Single);

		protected
			{ Protected Declarations }
         BaseList    : TGLuint;
         FontChanged : Boolean;
         procedure DestroyHandles; override;
         procedure OnFontChange(sender : TObject);
         procedure GetFirstAndLastChar(var firstChar, lastChar : Integer);

		public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure BuildList(var rci : TRenderContextInfo); override;
         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;

         function TextWidth(const str : String = '') : Single;
         function TextMaxHeight(const str : String = '') : Single;
         function TextMaxUnder(const str : String = '') : Single;
         procedure TextMetrics(const str : String; var width, maxHeight, maxUnder : Single);

		published
			{ Published Declarations }
         {: Adjusts the 3D font extrusion.<p>
            If Extrusion=0, the characters will be flat (2D), values >0 will
            give them a third dimension. }
         property Extrusion: Single read FExtrusion write SetExtrusion;
         property Font: TFont read FFont write SetFont;
         property Text: String read FText write SetText;
         {: Quality related, see Win32 help for wglUseFontOutlines }
         property AllowedDeviation : Single read FAllowedDeviation write SetAllowedDeviation;
         {: Character range to convert.<p>
            Converting less characters saves time and memory... }
         property CharacterRange : TSpaceTextCharRange read FCharacterRange write SetCharacterRange default stcrAll;
         property AspectRatio : Single read FAspectRatio write SetAspectRatio;
         property TextHeight : Single read FTextHeight write SetTextHeight;
         property Oblique : Single read FOblique write SetOblique;
         property Adjust : TGLTextAdjust read FAdjust write SetAdjust;
    end;

   // holds an entry in the font manager list (used in TSpaceText)
   PFontEntry        = ^TFontEntry;
   TFontEntry        = record
                         Name      : String;
                         Styles    : TFontStyles;
                         Extrusion : Single;
                         Base      : TGLuint;
                         RefCount  : Integer;
                         allowedDeviation : Single;
                         firstChar, lastChar : Integer;
                         glyphMetrics : array [0..255] of TGlyphMetricsFloat;
                       end;

   // TFontManager
   //
   {: Manages a list of fonts for which display lists were created. }
   TFontManager = class(TList)
	   public
			{ Public Declarations }
         destructor Destroy; override;
         function FindFont(AName: String; FStyles: TFontStyles; FExtrusion: Single;
                           FAllowedDeviation : Single;
                           FFirstChar, FLastChar : Integer) : PFontEntry;
         function FindFontByList(AList: TGLuint): PFontEntry;
         function GetFontBase(AName: String; FStyles: TFontStyles; FExtrusion: Single;
                              allowedDeviation : Single;
                              firstChar, lastChar : Integer) : TGLuint;
         procedure Release(List: TGLuint);
   end;

function FontManager : TFontManager;
procedure ReleaseFontManager;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils;

var
	vFontManager : TFontManager;

// FontManager
//
function FontManager : TFontManager;
begin
	if not Assigned(vFontManager) then
		vFontManager:=TFontManager.Create;
	Result:=vFontManager;
end;

// ReleaseFontManager
//
procedure ReleaseFontManager;
begin
   if Assigned(vFontManager) then begin
      vFontManager.Free;
      vFontManager:=nil;
   end;
end;

// ------------------
// ------------------ TGLTextAdjust ------------------
// ------------------

// Create
//
constructor TGLTextAdjust.Create;
begin
   inherited;
   FHorz:=haLeft;
   FVert:=vaBaseLine;
end;

// Assign
//
procedure TGLTextAdjust.Assign(source : TPersistent);
begin
   if Source is TGLTextAdjust then begin
      FHorz:=TGLTextAdjust(source).Horz;
      FVert:=TGLTextAdjust(source).Vert;
      if Assigned(FOnChange) then
         FOnChange(Self);
   end else inherited Assign(Source);
end;

// SetHorz
//
procedure TGLTextAdjust.SetHorz(const value : TGLTextHorzAdjust);
begin
   if FHorz<>value then begin
      FHorz:=value;
      if Assigned(FOnChange) then
         FOnChange(Self);
   end;
end;

// SetVert
//
procedure TGLTextAdjust.SetVert(const value : TGLTextVertAdjust);
begin
   if value<>FVert then begin
      FVert:=value;
      if Assigned(FOnChange) then
         FOnChange(Self);
   end;
end;

// ------------------
// ------------------ TSpaceText ------------------
// ------------------

// Create
//
constructor TSpaceText.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FFont:=TFont.Create;
   FFont.Name:='Arial';
   FontChanged:=True;
   FExtrusion:=0;
   CharacterRange:=stcrAll;
   FFont.OnChange:=OnFontChange;
   FAdjust:=TGLTextAdjust.Create;
   FAdjust.OnChange:=OnFontChange;
end;

// Destroy
//
destructor TSpaceText.Destroy;
begin
   FAdjust.OnChange:=nil;
   FAdjust.Free;
   FFont.OnChange:=nil;
   FFont.Free;
   inherited Destroy;
end;

// TextMetrics
//
procedure TSpaceText.TextMetrics(const str : String; var width, maxHeight, maxUnder : Single);
var
   i, firstChar, lastChar : Integer;
   buf : String;
   gmf : TGlyphMetricsFloat;
   fontEntry : PFontEntry;
begin
   width:=0;
   maxUnder:=0;
   maxHeight:=0;
   fontEntry:=FontManager.FindFontByList(BaseList);
   if Assigned(fontEntry) then begin
      GetFirstAndLastChar(firstChar, lastChar);
      if str='' then
         buf:=FText
      else buf:=str;
      for i:=1 to Length(buf) do begin
         gmf:=fontEntry.GlyphMetrics[Integer(buf[i])-firstChar];
         width:=width+gmf.gmfCellIncX;
         if gmf.gmfptGlyphOrigin.y>maxHeight then
            maxHeight:=gmf.gmfptGlyphOrigin.y;
         if gmf.gmfptGlyphOrigin.y-gmf.gmfBlackBoxY<maxUnder then
            maxUnder:=gmf.gmfptGlyphOrigin.y-gmf.gmfBlackBoxY;
      end;
   end;
end;

// TextWidth
//
function TSpaceText.TextWidth(const str : String = '') : Single;
var
   mh, mu : Single;
begin
   TextMetrics(str, Result, mh, mu);
end;

// TextMaxHeight
//
function TSpaceText.TextMaxHeight(const str : String = '') : Single;
var
   w, mu : Single;
begin
   TextMetrics(str, w, Result, mu);
end;

// TextMaxUnder
//
function TSpaceText.TextMaxUnder(const str : String = '') : Single;
var
   w, mh : Single;
begin
   TextMetrics(str, w, mh, Result);
end;

// BuildList
//
procedure TSpaceText.BuildList(var rci : TRenderContextInfo);
var
   textL, maxUnder, maxHeight : Single;
   charScale : Single;
begin
   if Length(FText)>0 then begin
      glPushMatrix;

      if FTextHeight<>0 then begin
         charScale:=FTextHeight/MaxHeight;
         glScalef(CharScale,CharScale,1);
      end;
      if FAspectRatio<>0 then
         glScalef(FAspectRatio, 1, 1);
      if FOblique<>0 then
         glRotatef(FOblique, 0, 0, 1);
         
      if (FAdjust.Horz<>haLeft) or (FAdjust.Vert<>vaBaseLine) then begin
         TextMetrics('', textL, maxHeight, maxUnder);
         case FAdjust.Horz of
            haLeft : ; // nothing
            haCenter : glTranslatef(-textL*0.5, 0, 0);
            haRight :  glTranslatef(-textL, 0, 0);
         end;
         case FAdjust.Vert of
            vaBaseLine : ; // nothing;
            vaBottom : glTranslatef(0, abs(maxUnder), 0);
            vaCenter : glTranslatef(0, abs(maxUnder)*0.5-maxHeight*0.5, 0);
            vaTop :    glTranslatef(0, -maxHeight, 0);
         end;
      end;

      glPushAttrib(GL_POLYGON_BIT);
      case FCharacterRange of
        stcrAlphaNum: glListBase(BaseList - 32);
        stcrNumbers: glListBase(BaseList - Cardinal('0'));
      else
        glListBase(BaseList);
      end;
      glCallLists(Length(FText), GL_UNSIGNED_BYTE, PChar(FText));
      glPopAttrib;

      glPopMatrix;
   end;
end;

// DoDestroyList
//
procedure TSpaceText.DestroyHandles;
begin
   ReleaseFontManager;
   inherited;
end;

// GetFirstAndLastChar
//
procedure TSpaceText.GetFirstAndLastChar(var firstChar, lastChar : Integer);
begin
   case FCharacterRange of
      stcrAlphaNum : begin
         firstChar:=32;
         lastChar:=127;
      end;
      stcrNumbers : begin
         firstChar:=Integer('0');
         lastChar:=Integer('9');
      end;
   else
      // stcrAll
      firstChar:=0;
      lastChar:=255;
   end;
end;

// DoRender
//
procedure TSpaceText.DoRender(var rci : TRenderContextInfo;
                              renderSelf, renderChildren : Boolean);
var
	firstChar, lastChar : Integer;
begin
	if FontChanged and (Length(FText)>0) then with FFont do begin
		FontManager.Release(BaseList);
      GetFirstAndLastChar(firstChar, lastChar);
		BaseList:=FontManager.GetFontBase(Name, Style, FExtrusion,
													 FAllowedDeviation, firstChar, lastChar);
		FontChanged:=False;
	end;
	inherited;
end;

// SetExtrusion
//
procedure TSpaceText.SetExtrusion(AValue: Single);
begin
   Assert(AValue>=0, 'Extrusion must be >=0');
	if FExtrusion<>AValue then begin
		FExtrusion:=AValue;
      OnFontChange(nil);
	end;
end;

// SetAllowedDeviation
//
procedure TSpaceText.SetAllowedDeviation(const val : Single);
begin
	if FAllowedDeviation<>val then begin
		FAllowedDeviation:=val;
      OnFontChange(nil);
	end;
end;

// SetCharacterRange
//
procedure TSpaceText.SetCharacterRange(const val : TSpaceTextCharRange);
begin
	if FCharacterRange<>val then begin
		FCharacterRange:=val;
      OnFontChange(nil);
	end;
end;

// SetFont
//
procedure TSpaceText.SetFont(AFont: TFont);
begin
   FFont.Assign(AFont);
   OnFontChange(nil);
end;

// OnFontChange
//
procedure TSpaceText.OnFontChange(sender : TObject);
begin
   FontChanged:=True;
   StructureChanged;
end;

// SetText
//
procedure TSpaceText.SetText(AText: String);
begin
   if FText<>AText then begin
      FText:=AText;
      StructureChanged;
   end;
end;

// SetAdjust
//
procedure TSpaceText.SetAdjust(const value : TGLTextAdjust);
begin
   FAdjust.Assign(Value);
   StructureChanged;
end;

// SetAspectRatio
//
procedure TSpaceText.SetAspectRatio(const value : Single);
begin
   if FAspectRatio<>value then begin
      FAspectRatio:=value;
      StructureChanged;
   end;
end;

// SetOblique
//
procedure TSpaceText.SetOblique(const value : Single);
begin
   if FOblique<>Value then begin
      FOblique:=value;
      StructureChanged;
   end;
end;

// SetTextHeight
//
procedure TSpaceText.SetTextHeight(const value : Single);
begin
   if value<>FTextHeight then begin
      FTextHeight:=value;
      StructureChanged;
   end;
end;

// ------------------
// ------------------ TFontManager ------------------
// ------------------

// Destroy
//
destructor TFontManager.Destroy;
var
   i : Integer;
begin
   for I:=0 to Count-1 do begin
      if TFontEntry(Items[I]^).Base<>0 then
         glDeleteLists(TFontEntry(Items[I]^).Base, 255);
      FreeMem(Items[I], SizeOf(TFontEntry));
   end;
   inherited Destroy;
end;

// FindFond
//
function TFontManager.FindFont(AName: String; FStyles: TFontStyles; FExtrusion: Single;
										 FAllowedDeviation : Single;
										 FFirstChar, FLastChar : Integer) : PFontEntry;
var
	i : Integer;
begin
	Result:=nil;
	// try to find an entry with the required attributes
	for I :=0 to Count-1 do with TFontEntry(Items[I]^) do
		if (CompareText(Name, AName) = 0) and (Styles = FStyles)
				and (Extrusion = FExtrusion) and (allowedDeviation=FAllowedDeviation)
				and (firstChar=FFirstChar)	and (lastChar=FLastChar) then begin
			// entry found
			Result:=Items[I];
			Break;
		end;
end;

// FindFontByList
//
function TFontManager.FindFontByList(AList: TGLuint): PFontEntry;
var
   i : Integer;
begin
   Result:=nil;
   // try to find an entry with the required attributes
   for I :=0 to Count-1 do
      with TFontEntry(Items[I]^) do
         if Base = AList then begin // entry found
            Result:=Items[I];
            Break;
         end;
end;

// GetFontBase
//
function TFontManager.GetFontBase(AName: String; FStyles: TFontStyles; FExtrusion: Single;
											 allowedDeviation : Single;
											 firstChar, lastChar : Integer) : TGLuint;
var
   NewEntry : PFontEntry;
	MemDC    : HDC;
	AFont    : TFont;
begin
   NewEntry:=FindFont(AName, FStyles, FExtrusion, allowedDeviation, firstChar, lastChar);
   if Assigned(NewEntry) then begin
	   Inc(NewEntry^.RefCount);
      Result:=NewEntry^.Base;
      Exit;
   end;
   // no entry found, so create one
   New(NewEntry);
   try
      NewEntry^.Name:=AName;
      NewEntry^.Styles:=FStyles;
      NewEntry^.Extrusion:=FExtrusion;
	   NewEntry^.RefCount:=1;
	   NewEntry^.firstChar:=firstChar;
	   NewEntry^.lastChar:=lastChar;
	   NewEntry^.allowedDeviation:=allowedDeviation;

      // create a font to be used while display list creation
      AFont:=TFont.Create;
      MemDC:=CreateCompatibleDC(0);
      try
         AFont.Name:=AName;
         AFont.Style:=FStyles;
         SelectObject(MemDC, AFont.Handle);
         NewEntry^.Base:=glGenLists(255);
		   if NewEntry^.Base = 0 then
			   raise Exception.Create('FontManager: no more display lists available');
		   if not OpenGL12.wglUseFontOutlines(MemDC, firstChar, lastChar-firstChar+1,
                                            NewEntry^.Base, allowedDeviation,
                                            FExtrusion, WGL_FONT_POLYGONS,
                                            @NewEntry^.GlyphMetrics) then
		  	raise Exception.Create('FontManager: font creation failed');
      finally
		   AFont.Free;
         DeleteDC(MemDC);
      end;
      Add(NewEntry);
      Result:=NewEntry^.Base;
   except
      if NewEntry^.Base<>0 then glDeleteLists(NewEntry^.Base, 255);
      Dispose(NewEntry);
      raise;
   end;
end;

// Release
//
procedure TFontManager.Release(List: TGLuint);
var
   entry : PFontEntry;
begin
  entry:=FindFontByList(List);
   if Assigned(entry) then begin
      Dec(entry^.RefCount);
      if entry^.RefCount = 0 then begin
         glDeleteLists(Entry^.Base, 255);
         Remove(Entry);
      end;
   end;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClass(TSpaceText);

finalization

   ReleaseFontManager;

end.
