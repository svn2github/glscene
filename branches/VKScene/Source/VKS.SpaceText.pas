//
// VKScene project, http://glscene.sourceforge.net 
//
{
  3D Text component. 

  Note: You can get valid extents (including AABB's) of this component only
  after it has been rendered for the first time. It means if you ask its
  extents during / after its creation, you will get zeros.

  Also extents are valid only when SpaceText has one line.  

}
unit VKS.SpaceText;

interface

{$I VKScene.inc}
{$IFDEF UNIX}{$MESSAGE Error 'Unit not supported'}{$ENDIF}

uses
  Winapi.Windows, 
  WinApi.Messages, 
  Winapi.OpenGL, 
  Winapi.OpenGLext,
  System.Classes, 
  System.UITypes, 
  System.SysUtils,
  FMX.Dialogs, 
  FMX.Graphics, 
  FMX.Controls, 
  VKS.Scene, 
  VKS.Texture, 
  VKS.Context, 
  VKS.VectorGeometry, 
  VKS.Strings,
  VKS.RenderContextInfo, 
  VKS.State;

type

  // TSpaceTextCharRange
  //
  TVKSpaceTextCharRange = (stcrDefault, stcrAlphaNum, stcrNumbers, stcrWide);

  // TVKTextHorzAdjust
  //
  // Note: haAligned, haCentrically, haFitIn have not been implemented!
  //
  TVKTextHorzAdjust = (haLeft, haCenter, haRight, haAligned,
    haCentrically, haFitIn);

  // TVKTextVertAdjust
  //
  TVKTextVertAdjust = (vaTop, vaCenter, vaBottom, vaBaseLine);

  // TVKTextAdjust
  //
  TVKTextAdjust = class(TPersistent)
  private
    { Private Declarations }
    FHorz: TVKTextHorzAdjust;
    FVert: TVKTextVertAdjust;
    FOnChange: TNotifyEvent;
    procedure SetHorz(const Value: TVKTextHorzAdjust);
    procedure SetVert(const Value: TVKTextVertAdjust);

  public
    { public Declarations }
    constructor Create;
    procedure Assign(Source: TPersistent); override;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;

  published
    { Published Declarations }
    property Horz: TVKTextHorzAdjust read FHorz write SetHorz default haLeft;
    property Vert: TVKTextVertAdjust read FVert write SetVert
      default vaBaseLine;
  end;

  // holds an entry in the font manager list (used in TVKSpaceText)
  PFontEntry = ^TFontEntry;

  TFontEntry = record
    Name: string;
    FVirtualHandle: TVKVirtualHandleTransf;
    Styles: TFontStyles;
    Extrusion: Single;
    RefCount: Integer;
    allowedDeviation: Single;
    firstChar, lastChar: Integer;
    glyphMetrics: array of TGlyphMetricsFloat;
    FClients: TList;
  end;

  // TVKSpaceText
  //
  { Renders a text in 3D. }
  TVKSpaceText = class(TVKSceneObject)
  private
    { Private Declarations }
    FFont: TFont;
    FExtrusion: Single;
    FAllowedDeviation: Single;
    FCharacterRange: TVKSpaceTextCharRange;
    FAdjust: TVKTextAdjust;
    FAspectRatio: Single;
    FOblique: Single;
    FTextHeight: Single;
    FLines: TStringList;
    procedure SetCharacterRange(const val: TVKSpaceTextCharRange);
    procedure SetAllowedDeviation(const val: Single);
    procedure SetExtrusion(AValue: Single);
    procedure SetFont(AFont: TFont);
    function GetText: WideString;
    procedure SetLines(const Value: TStringList);
    procedure SetText(const AText: WideString);
    procedure SetAdjust(const Value: TVKTextAdjust);
    procedure SetAspectRatio(const Value: Single);
    procedure SetOblique(const Value: Single);
    procedure SetTextHeight(const Value: Single);
  protected
    { Protected Declarations }
    FTextFontEntry: PFontEntry;
    FontChanged: Boolean;
    procedure DestroyHandle; override;
    procedure OnFontChange(sender: TObject);
    procedure GetFirstAndLastChar(var firstChar, lastChar: Integer);
    procedure DoOnLinesChange(sender: TObject); virtual;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var rci: TVKRenderContextInfo); override;
    procedure DoRender(var ARci: TVKRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;

    function TextWidth(const str: WideString = ''): Single;
    function TextMaxHeight(const str: WideString = ''): Single;
    function TextMaxUnder(const str: WideString = ''): Single;

    { Note: this fuction is valid only after text has been rendered
      the first time. Before that it returns zeros. }
    procedure TextMetrics(const str: WideString;
      out width, maxHeight, maxUnder: Single);
    procedure NotifyFontChanged;
    procedure NotifyChange(sender: TObject); override;
    procedure DefaultHandler(var Message); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function BarycenterAbsolutePosition: TVector; override;
  published
    { Published Declarations }
    { Adjusts the 3D font extrusion. 
      If Extrusion=0, the characters will be flat (2D), values >0 will
      give them a third dimension. }
    property Extrusion: Single read FExtrusion write SetExtrusion;
    property Font: TFont read FFont write SetFont;
    property Text: WideString read GetText write SetText stored False;
    property Lines: TStringList read FLines write SetLines;
    { Quality related, see Win32 help for wglUseFontOutlines }
    property allowedDeviation: Single read FAllowedDeviation
      write SetAllowedDeviation;
    { Character range to convert. 
      Converting less characters saves time and memory... }
    property CharacterRange: TVKSpaceTextCharRange read FCharacterRange
      write SetCharacterRange default stcrDefault;
    property AspectRatio: Single read FAspectRatio write SetAspectRatio;
    property TextHeight: Single read FTextHeight write SetTextHeight;
    property Oblique: Single read FOblique write SetOblique;
    property Adjust: TVKTextAdjust read FAdjust write SetAdjust;
  end;

  // TFontManager
  //
  { Manages a list of fonts for which display lists were created. }
  TFontManager = class(TList)
  private
    { Private Declarations }
    FCurrentBase: Integer;

  protected
    { Protected Declarations }
    procedure NotifyClients(Clients: TList);
    procedure VirtualHandleAlloc(sender: TVKVirtualHandle;
      var handle: Cardinal);
    procedure VirtualHandleDestroy(sender: TVKVirtualHandle;
      var handle: Cardinal);

  public
    { Public Declarations }
    constructor Create;
    destructor Destroy; override;

    function FindFont(AName: string; FStyles: TFontStyles; FExtrusion: Single;
      FAllowedDeviation: Single; FFirstChar, FLastChar: Integer): PFontEntry;
    function GetFontBase(AName: string; FStyles: TFontStyles;
      FExtrusion: Single; allowedDeviation: Single;
      firstChar, lastChar: Integer; client: TObject): PFontEntry;
    procedure Release(entry: PFontEntry; client: TObject);
  end;

function FontManager: TFontManager;
procedure ReleaseFontManager;

var
  vFontManagerMsgID: Cardinal;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
  cFontManagerMsg = 'GLScene FontManagerMessage';

var
  vFontManager: TFontManager;

  // FontManager
  //

function FontManager: TFontManager;
begin
  if not Assigned(vFontManager) then
    vFontManager := TFontManager.Create;
  Result := vFontManager;
end;

// ReleaseFontManager
//

procedure ReleaseFontManager;
begin
  if Assigned(vFontManager) then
  begin
    vFontManager.Free;
    vFontManager := nil;
  end;
end;

// ------------------
// ------------------ TVKTextAdjust ------------------
// ------------------

// Create
//

constructor TVKTextAdjust.Create;
begin
  inherited;
  FHorz := haLeft;
  FVert := vaBaseLine;
end;

// Assign
//

procedure TVKTextAdjust.Assign(Source: TPersistent);
begin
  if Source is TVKTextAdjust then
  begin
    FHorz := TVKTextAdjust(Source).Horz;
    FVert := TVKTextAdjust(Source).Vert;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end
  else
    inherited Assign(Source);
end;

// SetHorz
//

procedure TVKTextAdjust.SetHorz(const Value: TVKTextHorzAdjust);
begin
  if FHorz <> Value then
  begin
    FHorz := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

// SetVert
//

procedure TVKTextAdjust.SetVert(const Value: TVKTextVertAdjust);
begin
  if Value <> FVert then
  begin
    FVert := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

// ------------------
// ------------------ TVKSpaceText ------------------
// ------------------

// Create
//

constructor TVKSpaceText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  FFont.Family := 'Arial'; //in VCL FFont.Name
  FontChanged := True;
  CharacterRange := stcrDefault;
  FFont.OnChanged := OnFontChange;
  FAdjust := TVKTextAdjust.Create;
  FAdjust.OnChange := OnFontChange;
  FLines := TStringList.Create;
  FLines.OnChange := DoOnLinesChange;
end;

// Destroy
//

destructor TVKSpaceText.Destroy;
begin
  FAdjust.OnChange := nil;
  FAdjust.Free;
  FFont.OnChanged := nil;
  FFont.Free;
  FLines.Free;
  FontManager.Release(FTextFontEntry, Self);
  inherited Destroy;
end;

// TextMetrics
//

procedure TVKSpaceText.TextMetrics(const str: WideString;
  out width, maxHeight, maxUnder: Single);
var
  i, firstChar, lastChar, diff: Integer;
  buf: WideString;
  gmf: TGlyphMetricsFloat;
begin
  width := 0;
  maxUnder := 0;
  maxHeight := 0;
  if Assigned(FTextFontEntry) then
  begin
    GetFirstAndLastChar(firstChar, lastChar);
    if str = '' then
      buf := GetText
    else
      buf := str;
    for i := 1 to Length(buf) do
    begin
      diff := Integer(buf[i]) - firstChar;
      if diff > High(FTextFontEntry^.glyphMetrics) then
        continue;
      gmf := FTextFontEntry^.glyphMetrics[diff];
      width := width + gmf.gmfCellIncX;
      if gmf.gmfptGlyphOrigin.y > maxHeight then
        maxHeight := gmf.gmfptGlyphOrigin.y;
      if gmf.gmfptGlyphOrigin.y - gmf.gmfBlackBoxY < maxUnder then
        maxUnder := gmf.gmfptGlyphOrigin.y - gmf.gmfBlackBoxY;
    end;
  end;
end;

// TextWidth
//

function TVKSpaceText.TextWidth(const str: WideString = ''): Single;
var
  mh, mu: Single;
begin
  TextMetrics(str, Result, mh, mu);
end;

// TextMaxHeight
//

function TVKSpaceText.TextMaxHeight(const str: WideString = ''): Single;
var
  w, mu: Single;
begin
  TextMetrics(str, w, Result, mu);
end;

// TextMaxUnder
//

function TVKSpaceText.TextMaxUnder(const str: WideString = ''): Single;
var
  w, mh: Single;
begin
  TextMetrics(str, w, mh, Result);
end;

// Assign

procedure TVKSpaceText.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TVKSpaceText then
  begin
    FAdjust.Assign(TVKSpaceText(Source).FAdjust);
    FFont.Assign(TVKSpaceText(Source).FFont);
    FAllowedDeviation := TVKSpaceText(Source).allowedDeviation;
    FAspectRatio := TVKSpaceText(Source).FAspectRatio;
    FCharacterRange := TVKSpaceText(Source).CharacterRange;
    FExtrusion := TVKSpaceText(Source).FExtrusion;
    FOblique := TVKSpaceText(Source).FOblique;
    FLines.Text := TVKSpaceText(Source).FLines.Text;
    FTextHeight := TVKSpaceText(Source).FTextHeight;
    StructureChanged;
  end;
end;

// BuildList
//

procedure TVKSpaceText.BuildList(var rci: TVKRenderContextInfo);
var
  textL, maxUnder, maxHeight: Single;
  charScale: Single;
  i, j, k, c: Integer;
  glBase: GLuint;
  dirtyLine, cleanLine: WideString;
begin
  if Length(GetText) > 0 then
  begin
    glPushMatrix;

    // FAspectRatio ignore
    if FAspectRatio <> 0 then
      glScalef(FAspectRatio, 1, 1);
    if FOblique <> 0 then
      glRotatef(FOblique, 0, 0, 1);

    glBase := FTextFontEntry^.FVirtualHandle.handle;
    case FCharacterRange of
      stcrAlphaNum:
        glListBase(GLuint(Integer(glBase) - 32));
      stcrNumbers:
        glListBase(GLuint(Integer(glBase) - Integer('0')));
    else
      glListBase(glBase);
    end;

    rci.GLStates.PushAttrib([sttPolygon]);
    for i := 0 to FLines.Count - 1 do
    begin
      glPushMatrix;

      TextMetrics(FLines.Strings[i], textL, maxHeight, maxUnder);
      if (FAdjust.Horz <> haLeft) or (FAdjust.Vert <> vaBaseLine) or
        (FTextHeight <> 0) then
      begin
        if FTextHeight <> 0 then
        begin
          charScale := FTextHeight / maxHeight;
          glScalef(charScale, charScale, 1);
        end;
        case FAdjust.Horz of
          haLeft:
            ; // nothing
          haCenter:
            glTranslatef(-textL * 0.5, 0, 0);
          haRight:
            glTranslatef(-textL, 0, 0);
        end;
        case FAdjust.Vert of
          vaBaseLine:
            ; // nothing;
          vaBottom:
            glTranslatef(0, abs(maxUnder), 0);
          vaCenter:
            glTranslatef(0, abs(maxUnder) * 0.5 - maxHeight * 0.5, 0);
          vaTop:
            glTranslatef(0, -maxHeight, 0);
        end;
      end;

      glTranslatef(0, -i * (maxHeight + FAspectRatio), 0);
      if FCharacterRange = stcrWide then
      begin
        dirtyLine := FLines.Strings[i];
        SetLength(cleanLine, Length(dirtyLine));
        k := 1;
        for j := 1 to Length(dirtyLine) do
        begin
          c := Integer(dirtyLine[j]);
          if (c >= FTextFontEntry^.firstChar) and
            (c <= FTextFontEntry^.lastChar) then
          begin
            cleanLine[k] := dirtyLine[j];
            Inc(k);
          end;
        end;
        if k > 1 then
          glCallLists(k - 1, GL_UNSIGNED_SHORT, PWideChar(cleanLine))
      end
      else
        glCallLists(Length(FLines.Strings[i]), GL_UNSIGNED_BYTE,
          PGLChar(String(FLines.Strings[i])));
      glPopMatrix;
    end;
    rci.GLStates.PopAttrib();
    glPopMatrix;
  end;
end;

// DestroyHandle
//

procedure TVKSpaceText.DestroyHandle;
begin
  FontChanged := True;
  inherited;
end;

// GetFirstAndLastChar
//

procedure TVKSpaceText.GetFirstAndLastChar(var firstChar, lastChar: Integer);
begin
  case FCharacterRange of
    stcrAlphaNum:
      begin
        firstChar := 32;
        lastChar := 127;
      end;
    stcrNumbers:
      begin
        firstChar := Integer('0');
        lastChar := Integer('9');
      end;
    stcrDefault:
      begin
        firstChar := 0;
        lastChar := 255;
      end;
    stcrWide:
      begin
        firstChar := 0;
        lastChar := $077F;
      end;
  end;
end;

// DoRender
//

procedure TVKSpaceText.DoRender(var ARci: TVKRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  firstChar, lastChar: Integer;
begin
  if GetText <> '' then
  begin
    if Assigned(FTextFontEntry) then
      FTextFontEntry^.FVirtualHandle.AllocateHandle;
    if FontChanged or (Assigned(FTextFontEntry) and
      (FTextFontEntry^.FVirtualHandle.IsDataNeedUpdate)) then
      with FFont do
      begin
        FontManager.Release(FTextFontEntry, Self);
        GetFirstAndLastChar(firstChar, lastChar);
        FTextFontEntry := FontManager.GetFontBase(Name, Style, FExtrusion,
          FAllowedDeviation, firstChar, lastChar, Self);
        FontChanged := False;
        FTextFontEntry^.FVirtualHandle.NotifyDataUpdated;
      end;
  end;
  inherited;
end;

// SetExtrusion
//

procedure TVKSpaceText.SetExtrusion(AValue: Single);
begin
  Assert(AValue >= 0, 'Extrusion must be >=0');
  if FExtrusion <> AValue then
  begin
    FExtrusion := AValue;
    OnFontChange(nil);
  end;
end;

// SetAllowedDeviation
//

procedure TVKSpaceText.SetAllowedDeviation(const val: Single);
begin
  if FAllowedDeviation <> val then
  begin
    if val > 0 then
      FAllowedDeviation := val
    else
      FAllowedDeviation := 0;
    OnFontChange(nil);
  end;
end;

// SetCharacterRange
//

procedure TVKSpaceText.SetCharacterRange(const val: TVKSpaceTextCharRange);
begin
  if FCharacterRange <> val then
  begin
    FCharacterRange := val;
    OnFontChange(nil);
  end;
end;

// SetFont
//

procedure TVKSpaceText.SetFont(AFont: TFont);
begin
  FFont.Assign(AFont);
  OnFontChange(nil);
end;

// OnFontChange
//

procedure TVKSpaceText.OnFontChange(sender: TObject);
begin
  FontChanged := True;
  StructureChanged;
end;

// SetText
//

procedure TVKSpaceText.SetText(const AText: WideString);
begin
  if GetText <> AText then
  begin
    FLines.Text := AText;
    // StructureChanged is Called in DoOnLinesChange.
  end;
end;

procedure TVKSpaceText.DoOnLinesChange(sender: TObject);
begin
  StructureChanged;
end;

// SetAdjust
//

function TVKSpaceText.GetText: WideString;
begin
  if FLines.Count = 1 then
    Result := FLines[0]
  else
    Result := FLines.Text;
end;

// SetAdjust
//

procedure TVKSpaceText.SetLines(const Value: TStringList);
begin
  FLines.Assign(Value);
end;

// SetAdjust
//

procedure TVKSpaceText.SetAdjust(const Value: TVKTextAdjust);
begin
  FAdjust.Assign(Value);
  StructureChanged;
end;

// SetAspectRatio
//

procedure TVKSpaceText.SetAspectRatio(const Value: Single);
begin
  if FAspectRatio <> Value then
  begin
    FAspectRatio := Value;
    StructureChanged;
  end;
end;

// SetOblique
//

procedure TVKSpaceText.SetOblique(const Value: Single);
begin
  if FOblique <> Value then
  begin
    FOblique := Value;
    StructureChanged;
  end;
end;

// SetTextHeight
//

procedure TVKSpaceText.SetTextHeight(const Value: Single);
begin
  if Value <> FTextHeight then
  begin
    FTextHeight := Value;
    StructureChanged;
  end;
end;

// NotifyFontChanged
//

procedure TVKSpaceText.NotifyFontChanged;
begin
  FTextFontEntry := nil;
  FontChanged := True;
end;

// NotifyChange
//

procedure TVKSpaceText.NotifyChange(sender: TObject);
begin
  if sender is TFontManager then
    NotifyFontChanged
  else
    inherited;
end;

// DefaultHandler
//

procedure TVKSpaceText.DefaultHandler(var Message);
begin
  with TMessage(Message) do
  begin
    if Msg = vFontManagerMsgID then
      NotifyFontChanged
    else
      inherited;
  end;
end;

// BarycenterAbsolutePosition
//

function TVKSpaceText.BarycenterAbsolutePosition: TVector;
var
  lWidth, lHeightMax, lHeightMin: Single;
  AdjustVector: TVector;
begin
  TextMetrics(Text, lWidth, lHeightMax, lHeightMin);

  case FAdjust.FHorz of
    haLeft:
      AdjustVector.X := lWidth / 2;
    haCenter:
      AdjustVector.X := 0; // Nothing.
    haRight:
      AdjustVector.X := -lWidth / 2;
  else
    begin
      AdjustVector.X := 0;
      Assert(False, strErrorEx + strUnknownType); // Not implemented...
    end;
  end;

  case FAdjust.FVert of
    vaTop:
      AdjustVector.Y := -(abs(lHeightMin) * 0.5 + lHeightMax * 0.5);
    vaCenter:
      AdjustVector.Y := 0; // Nothing.
    vaBottom:
      AdjustVector.Y := (abs(lHeightMin) * 0.5 + lHeightMax * 0.5);
    vaBaseLine:
      AdjustVector.Y := -(abs(lHeightMin) * 0.5 - lHeightMax * 0.5);
  else
    begin
      AdjustVector.Y := 0;
      Assert(False, strErrorEx + strUnknownType); // Not implemented...
    end;
  end;

  AdjustVector.Z := -(FExtrusion / 2);
  AdjustVector.W := 1;
  Result := LocalToAbsolute(AdjustVector);
end;

// AxisAlignedDimensionsUnscaled
//

function TVKSpaceText.AxisAlignedDimensionsUnscaled: TVector;
var
  lWidth, lHeightMax, lHeightMin: Single;
  charScale: Single;
begin
  TextMetrics(Text, lWidth, lHeightMax, lHeightMin);

  if FTextHeight = 0 then
    charScale := 1
  else
    charScale := FTextHeight / lHeightMax;

  Result.X := lWidth / 2 * charScale;
  Result.Y := (lHeightMax + abs(lHeightMin)) / 2 * charScale;
  Result.Z := FExtrusion / 2;
  Result.W := 0;
end;

// ------------------
// ------------------ TFontManager ------------------
// ------------------

// Create
//

constructor TFontManager.Create;
begin
  inherited;
end;

// Destroy
//

destructor TFontManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    TFontEntry(Items[i]^).FVirtualHandle.Free;
    NotifyClients(TFontEntry(Items[i]^).FClients);
    TFontEntry(Items[i]^).FClients.Free;
    TFontEntry(Items[i]^).Name := '';
    FreeMem(Items[i], SizeOf(TFontEntry));
  end;
  inherited Destroy;
end;

// VirtualHandleAlloc
//

procedure TFontManager.VirtualHandleAlloc(sender: TVKVirtualHandle;
  var handle: Cardinal);
begin
  handle := FCurrentBase;
end;

// VirtualHandleDestroy
//

procedure TFontManager.VirtualHandleDestroy(sender: TVKVirtualHandle;
  var handle: Cardinal);
begin
  if handle <> 0 then
    glDeleteLists(handle, sender.Tag);
end;

// FindFond
//

function TFontManager.FindFont(AName: string; FStyles: TFontStyles;
  FExtrusion: Single; FAllowedDeviation: Single; FFirstChar, FLastChar: Integer)
  : PFontEntry;
var
  i: Integer;
begin
  Result := nil;
  // try to find an entry with the required attributes
  for i := 0 to Count - 1 do
    with TFontEntry(Items[i]^) do
      if (CompareText(Name, AName) = 0) and (Styles = FStyles) and
        (Extrusion = FExtrusion) and (allowedDeviation = FAllowedDeviation) and
        (firstChar = FFirstChar) and (lastChar = FLastChar) then
      begin
        // entry found
        Result := Items[i];
        Break;
      end;
end;

// GetFontBase
//

function TFontManager.GetFontBase(AName: string; FStyles: TFontStyles;
  FExtrusion: Single; allowedDeviation: Single; firstChar, lastChar: Integer;
  client: TObject): PFontEntry;
var
  NewEntry: PFontEntry;
  MemDC: HDC;
  AFont: TFont;
  nbLists: Integer;
  success: Boolean;
begin
  NewEntry := FindFont(AName, FStyles, FExtrusion, allowedDeviation, firstChar,
    lastChar);
  if Assigned(NewEntry) then
  begin
    Inc(NewEntry^.RefCount);
    if NewEntry^.FClients.IndexOf(client) < 0 then
      NewEntry^.FClients.Add(client);
    Result := NewEntry;
  end
  else
    Result := nil;
  if (Result = nil) or (Assigned(Result) and
    (Result^.FVirtualHandle.handle = 0)) then
  begin
    // no entry found, or entry was purged
    nbLists := lastChar - firstChar + 1;
    if not Assigned(NewEntry) then
    begin
      // no entry found, so create one
      New(NewEntry);
      NewEntry^.Name := AName;
      NewEntry^.FVirtualHandle := TVKVirtualHandleTransf.Create;
      NewEntry^.FVirtualHandle.OnAllocate := VirtualHandleAlloc;
      NewEntry^.FVirtualHandle.OnDestroy := VirtualHandleDestroy;
      NewEntry^.FVirtualHandle.Tag := nbLists;
      NewEntry^.Styles := FStyles;
      NewEntry^.Extrusion := FExtrusion;
      NewEntry^.RefCount := 1;
      NewEntry^.firstChar := firstChar;
      NewEntry^.lastChar := lastChar;
      SetLength(NewEntry^.glyphMetrics, nbLists);
      NewEntry^.allowedDeviation := allowedDeviation;
      NewEntry^.FClients := TList.Create;
      NewEntry^.FClients.Add(client);
      Add(NewEntry);
    end;
    // create a font to be used while display list creation
    AFont := TFont.Create;
    MemDC := CreateCompatibleDC(0);
    try
      AFont.Family := AName;
      AFont.Style := FStyles;
      { TODO : E2003 Undeclared identifier: 'handle' }
      (*SelectObject(MemDC, AFont.handle);*)
      FCurrentBase := glGenLists(nbLists);
      if FCurrentBase = 0 then
        raise Exception.Create('FontManager: no more display lists available');
      NewEntry^.FVirtualHandle.AllocateHandle;
      if lastChar < 256 then
      begin
        success := wglUseFontOutlinesA(MemDC, firstChar, nbLists, FCurrentBase,
          allowedDeviation, FExtrusion, WGL_FONT_POLYGONS,
          @NewEntry^.glyphMetrics[0]);
      end
      else
      begin
        success := wglUseFontOutlinesW(MemDC, firstChar, nbLists, FCurrentBase,
          allowedDeviation, FExtrusion, WGL_FONT_POLYGONS,
          @NewEntry^.glyphMetrics[0]);
      end;
      if not success then
        raise Exception.Create('FontManager: font creation failed');
    finally
      AFont.Free;
      DeleteDC(MemDC);
    end;
    Result := NewEntry;
  end;
end;

// Release
//

procedure TFontManager.Release(entry: PFontEntry; client: TObject);
var
  hMsg: TMessage;
begin
  if Assigned(entry) then
  begin
    Dec(entry^.RefCount);
    if Assigned(client) then
    begin
      hMsg.Msg := vFontManagerMsgID;
      client.DefaultHandler(hMsg);
    end;
    entry^.FClients.Remove(client);
    if entry^.RefCount = 0 then
    begin
      entry^.FVirtualHandle.Free;
      NotifyClients(entry^.FClients);
      entry^.FClients.Free;
      Remove(entry);
      Dispose(entry)
    end;
  end;
end;

// NotifyClients
//

procedure TFontManager.NotifyClients(Clients: TList);
var
  i: Integer;
  hMsg: TMessage;
begin
  hMsg.Msg := vFontManagerMsgID;
  for i := 0 to Clients.Count - 1 do
    TObject(Clients[i]).DefaultHandler(hMsg);
end;

// -------------------------------------------------------------
// -------------------------------------------------------------
// -------------------------------------------------------------
initialization

// -------------------------------------------------------------
// -------------------------------------------------------------
// -------------------------------------------------------------

vFontManagerMsgID := RegisterWindowMessage(cFontManagerMsg);
RegisterClass(TVKSpaceText);

finalization

ReleaseFontManager;

end.
