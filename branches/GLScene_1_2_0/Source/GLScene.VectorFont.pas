//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLScene.VectorFont<p>

  <b>History : </b><font size=-1><ul>
  <li>19/11/11 - Yar - Creation
  </ul></font>
}
unit GLScene.VectorFont;

interface

{$I GLScene.inc}

uses
  Classes,
  SysUtils,

  GLScene.Base.Classes,
  GLScene.Platform,
  GLScene.Base.Vector.Geometry,
  GLScene.Base.Vector.Types,
  GLScene.Base.GeometryBB,
  GLScene.DrawTechnique,
  GLScene.Core;

type

  TGLCustomVectorFont = class;

  // TVectorFontRange
  //
  TVectorFontRange = class(TCollectionItem)
  private
    { Private Declarations }
    function GetStartASCII: WideString;
    function GetStopASCII: WideString;
  protected
    { Protected Declarations }
    FStartASCII, FStopASCII: WideChar;
    FStartGlyphIdx, FStopGlyphIdx, FCharCount: Integer;
    procedure SetStartASCII(const val: WideString);
    procedure SetStopASCII(const val: WideString);
    procedure SetStartGlyphIdx(val: Integer);
    function GetDisplayName: string; override;

  public
    { Public Declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange;
  published
    { Published Declarations }
    property StartASCII: WideString read GetStartASCII write SetStartASCII;
    property StopASCII: WideString read GetStopASCII write SetStopASCII;
    property StartGlyphIdx: Integer read FStartGlyphIdx write SetStartGlyphIdx;
    property StopGlyphIdx: Integer read FStopGlyphIdx;
    property CharCount: Integer read FCharCount;
  end;

  // TVectorFontRanges
  //
  TVectorFontRanges = class(TCollection)
  private
    { Private Declarations }
    FCharCount: Integer;
  protected
    { Protected Declarations }
    FOwner: TComponent;

    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TVectorFontRange);
    function GetItems(index: Integer): TVectorFontRange;
    function CalcCharacterCount: Integer;
    procedure Update(Item: TCollectionItem); override;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    function Add: TVectorFontRange; overload;
    function Add(const StartASCII, StopASCII: WideChar)
      : TVectorFontRange; overload;
    function Add(const StartASCII, StopASCII: AnsiChar)
      : TVectorFontRange; overload;
    function FindItemID(ID: Integer): TVectorFontRange;
    property Items[index: Integer]: TVectorFontRange read GetItems
      write SetItems; default;

    { : Converts an ASCII character into a tile index.<p>
      Return -1 if character cannot be rendered. }
    function CharacterToTileIndex(aChar: WideChar): Integer;
    function TileIndexToChar(aIndex: Integer): WideChar;
    procedure NotifyChange;

    // : Total number of characters in the ranges; cached for performance
    property CharacterCount: Integer read FCharCount;
  end;

  // TGLCustomVectorFont
  //
  TGLCustomVectorFont = class(TGLUpdateAbleComponent)
  private
    { Private Declarations }
    FRanges: TVectorFontRanges;
    FUsers: TList;
    FExtrusion: Single;
    procedure SetExtrusion(Value: Single);
  protected
    { Protected Declarations }

    procedure SetRanges(const val: TVectorFontRanges);

    procedure InvalidateUsers;
    property Ranges: TVectorFontRanges read FRanges write SetRanges;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RegisterUser(anObject: TGLBaseSceneObject); virtual;
    procedure UnRegisterUser(anObject: TGLBaseSceneObject); virtual;
    procedure BuildString(ABatch: TDrawBatch;
      const aText: UnicodeString); virtual; abstract;
    function GetAABB(const aText: string): TAABB; virtual; abstract;
    { : Adjusts the 3D font extrusion.<p>
      If Extrusion=0, the characters will be flat (2D), values >0 will
      give them a third dimension. }
    property Extrusion: Single read FExtrusion write SetExtrusion;
  end;

  // TGLVectorFont
  //
  TGLVectorFont = class(TGLCustomVectorFont)
  public
    { Public Declarations }
    procedure BuildString(ABatch: TDrawBatch;
      const aText: UnicodeString); override;
  published
    { Published Declarations }
    property Ranges;
  end;

implementation

{$REGION 'TVectorFontRange'}
// ------------------
// ------------------ TVectorFontRange ------------------
// ------------------

procedure TVectorFontRange.Assign(Source: TPersistent);
begin
  if Source is TVectorFontRange then
  begin
    FStartASCII := TVectorFontRange(Source).FStartASCII;
    FStopASCII := TVectorFontRange(Source).FStopASCII;
    FStartGlyphIdx := TVectorFontRange(Source).FStartGlyphIdx;
    NotifyChange;
  end
  else
    inherited;
end;

constructor TVectorFontRange.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TVectorFontRange.Destroy;
begin
  inherited Destroy;
end;

function TVectorFontRange.GetDisplayName: string;
begin
  Result := Format('ASCII [#%d, #%d] -> Glyphs [%d, %d]',
                  [Integer(FStartASCII), Integer(FStopASCII), StartGlyphIdx, StopGlyphIdx]);
end;

function TVectorFontRange.GetStartASCII: WideString;
begin
  Result := FStartASCII;
end;

function TVectorFontRange.GetStopASCII: WideString;
begin
  Result := FStopASCII;
end;

procedure TVectorFontRange.NotifyChange;
begin
  FCharCount    := integer(FStopASCII) - integer(FStartASCII) + 1;
  FStopGlyphIdx := FStartGlyphIdx + FCharCount - 1;
  if Assigned(Collection) then
    (Collection as TVectorFontRanges).NotifyChange;
end;

procedure TVectorFontRange.SetStartASCII(const val: WideString);
begin
  if (Length(val)>0) and (val[1] <> FStartASCII) then
  begin
    FStartASCII := val[1];
    if FStartASCII > FStopASCII then
      FStopASCII := FStartASCII;
    NotifyChange;
  end;
end;

procedure TVectorFontRange.SetStartGlyphIdx(val: Integer);
begin
  val := MaxInteger(0, val);
  if val <> FStartGlyphIdx then
  begin
    FStartGlyphIdx := val;
    NotifyChange;
  end;
end;

procedure TVectorFontRange.SetStopASCII(const val: WideString);
begin
   if (Length(val)>0) and (FStopASCII <> val[1]) then
  begin
    FStopASCII := val[1];
    if FStopASCII < FStartASCII then
      FStartASCII := FStopASCII;
    NotifyChange;
  end;
end;
{$ENDREGION}
{$REGION 'TVectorFontRanges'}
// ------------------
// ------------------ TVectorFontRanges ------------------
// ------------------

function TVectorFontRanges.Add(const StartASCII,
  StopASCII: WideChar): TVectorFontRange;
begin
  Result := Add;
  Result.StartASCII := startASCII;
  Result.StopASCII := stopASCII;
end;

function TVectorFontRanges.Add(const StartASCII,
  StopASCII: AnsiChar): TVectorFontRange;
begin
  Result := Add(CharToWideChar(startASCII), CharToWideChar(stopASCII));
end;

function TVectorFontRanges.Add: TVectorFontRange;
begin
  Result := (inherited Add) as TVectorFontRange;
end;

function TVectorFontRanges.CalcCharacterCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    with Items[i] do
      Inc(Result, Integer(FStopASCII) - Integer(FStartASCII) + 1);
end;

function TVectorFontRanges.CharacterToTileIndex(aChar: WideChar): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    with Items[i] do
    begin
      if (aChar >= FStartASCII) and (aChar <= FStopASCII) then
      begin
        Result := StartGlyphIdx + Integer(aChar) - Integer(FStartASCII);
        Break;
      end;
    end;
end;

constructor TVectorFontRanges.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  inherited Create(TVectorFontRange);
end;

destructor TVectorFontRanges.Destroy;
begin
  inherited Destroy;
end;

function TVectorFontRanges.FindItemID(ID: Integer): TVectorFontRange;
begin
  Result := (inherited FindItemID(ID)) as TVectorFontRange;
end;

function TVectorFontRanges.GetItems(index: Integer): TVectorFontRange;
begin
  Result := TVectorFontRange(inherited Items[index]);
end;

function TVectorFontRanges.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TVectorFontRanges.NotifyChange;
begin
  FCharCount := CalcCharacterCount;

  if Assigned(FOwner) and (FOwner is TGLBaseSceneObject) then
    TGLBaseSceneObject(FOwner).StructureChanged;
end;

procedure TVectorFontRanges.SetItems(index: Integer;
  const val: TVectorFontRange);
begin
  inherited Items[index] := val;
end;

function TVectorFontRanges.TileIndexToChar(aIndex: Integer): WideChar;
var
  i: Integer;
begin
  Result := #0;
  for i := 0 to Count - 1 do
    with Items[i] do
    begin
      if (aIndex >= StartGlyphIdx) and (aIndex <= StopGlyphIdx) then
      begin
        Result := widechar(aIndex - StartGlyphIdx + Integer(FStartASCII));
        Break;
      end;
    end;
end;

procedure TVectorFontRanges.Update(Item: TCollectionItem);
begin
  inherited;
  NotifyChange;
end;
{$ENDREGION}
{$REGION 'TGLCustomVectorFont'}
// ------------------
// ------------------ TGLCustomVectorFont ------------------
// ------------------

constructor TGLCustomVectorFont.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRanges := TVectorFontRanges.Create(Self);
  FUsers := TList.Create;
end;

destructor TGLCustomVectorFont.Destroy;
begin
  inherited;
  FRanges.Free;
  Assert(FUsers.Count = 0);
  FUsers.Free;
end;

procedure TGLCustomVectorFont.InvalidateUsers;
var
  I: Integer;
begin
  for I := FUsers.Count - 1 downto 0 do
    TGLBaseSceneObject(FUsers[I]).NotifyChange(Self);
end;

procedure TGLCustomVectorFont.RegisterUser(anObject: TGLBaseSceneObject);
begin
  Assert(FUsers.IndexOf(anObject) < 0);
  FUsers.Add(anObject);
end;

procedure TGLCustomVectorFont.SetExtrusion(Value: Single);
begin
  Value := MaxFloat(Value, 0);
  if FExtrusion <> Value then
  begin
    FExtrusion := Value;
  end;
end;

procedure TGLCustomVectorFont.SetRanges(const val: TVectorFontRanges);
begin
  FRanges.Assign(val);
  InvalidateUsers;
end;

procedure TGLCustomVectorFont.UnRegisterUser(anObject: TGLBaseSceneObject);
begin
  FUsers.Remove(anObject);
end;

{$ENDREGION}
{$REGION 'TGLFreetypeVectorFont'}
// ------------------
// ------------------ TGLFreetypeVectorFont ------------------
// ------------------
procedure TGLVectorFont.BuildString(ABatch: TDrawBatch;
  const aText: UnicodeString);
begin

end;
{$ENDREGION}

initialization

RegisterClasses([TGLVectorFont]);

end.
