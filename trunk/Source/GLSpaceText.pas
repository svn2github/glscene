// GLSpaceText
{: Win32 specific Context.<p>

	<b>History : </b><font size=-1><ul>
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
         procedure SetCharacterRange(const val : TSpaceTextCharRange);
         procedure SetAllowedDeviation(const val : Single);
         procedure SetExtrusion(AValue: Single);
         procedure SetFont(AFont: TFont);
         procedure SetText(AText: String);

		protected
			{ Protected Declarations }
         BaseList    : TGLuint;
         FontChanged : Boolean;
         procedure DestroyHandles; override;
         procedure OnFontChange(sender : TObject);

		public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure BuildList(var rci : TRenderContextInfo); override;
         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;

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

//----------------- TSpaceText ----------------------------------------------------

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
end;

// Destroy
//
destructor TSpaceText.Destroy;
begin
   FFont.OnChange:=nil;
   FFont.Free;
   inherited Destroy;
end;

// BuildList
//
procedure TSpaceText.BuildList(var rci : TRenderContextInfo);
begin
	if Length(FText) > 0 then begin
		// create texture coordinates if necessary
		//    if not (Material.Texture.Disabled)  and  not (Material.Texture.IsInherited) then
		glPushAttrib(GL_POLYGON_BIT);
		case FCharacterRange of
			stcrAlphaNum :	glListBase(BaseList-32);
			stcrNumbers : glListBase(BaseList-Cardinal('0'));
		else
			glListBase(BaseList);
		end;
		glCallLists(Length(FText), GL_UNSIGNED_BYTE, PChar(FText));
		glPopAttrib;
	end;
end;

// DoDestroyList
//
procedure TSpaceText.DestroyHandles;
begin
   ReleaseFontManager;
   inherited;
end;

// DoRender
//
procedure TSpaceText.DoRender(var rci : TRenderContextInfo;
                              renderSelf, renderChildren : Boolean);
var
	firstChar, lastChar : Integer;
begin
	if FontChanged and (Length(FText) > 0) then with FFont do begin
		FontManager.Release(BaseList);
		case FCharacterRange of
			stcrAlphaNum : begin
				firstChar:=32; lastChar:=127;
			end;
			stcrNumbers : begin
				firstChar:=Integer('0'); lastChar:=Integer('9');
			end;
		else
			// stcrAll
			firstChar:=0; lastChar:=255;
		end;
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

//----------------- TFontManager -----------------------------------------------

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
                                            FExtrusion, WGL_FONT_POLYGONS, nil) then
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
