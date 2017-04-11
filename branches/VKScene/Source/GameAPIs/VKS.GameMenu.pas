//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Manages a basic game menu UI 
  
}
unit VKS.GameMenu;

interface

{$I VKScene.inc}

uses
  System.Classes, System.SysUtils,

  VKS.Scene, VKS.Material, VKS.BitmapFont, VKS.CrossPlatform, VKS.Color,
  VKS.RenderContextInfo, VKS.Canvas, Winapi.OpenGL, Winapi.OpenGLext,  VKS.Context;

type

  // TVKGameMenuScale
  //
  TVKGameMenuScale = (gmsNormal, gms1024x768);

  // TVKGameMenu
  //
  { Classic game menu interface made of several lines.  }
  TVKGameMenu = class(TVKSceneObject, IGLMaterialLibrarySupported)
  private
    { Private Properties }
    FItems: TStrings;
    FSelected: Integer;
    FFont: TVKCustomBitmapFont;
    FMarginVert, FMarginHorz, FSpacing: Integer;
    FMenuScale: TVKGameMenuScale;
    FBackColor: TVKColor;
    FInactiveColor, FActiveColor, FDisabledColor: TVKColor;
    FMaterialLibrary: TVKMaterialLibrary;
    FTitleMaterialName: TVKLibMaterialName;
    FTitleWidth, FTitleHeight: Integer;
    FOnSelectedChanged: TNotifyEvent;
    FBoxTop, FBoxBottom, FBoxLeft, FBoxRight: Integer;
    FMenuTop: integer;
    //implementing IGLMaterialLibrarySupported
    function GetMaterialLibrary: TVKAbstractMaterialLibrary;
  protected
    { Protected Properties }
    procedure SetMenuScale(AValue: TVKGameMenuScale);
    procedure SetMarginHorz(AValue: Integer);
    procedure SetMarginVert(AValue: Integer);
    procedure SetSpacing(AValue: Integer);
    procedure SetFont(AValue: TVKCustomBitmapFont);
    procedure SetBackColor(AValue: TVKColor);
    procedure SetInactiveColor(AValue: TVKColor);
    procedure SetActiveColor(AValue: TVKColor);
    procedure SetDisabledColor(AValue: TVKColor);
    function GetEnabled(AIndex: Integer): Boolean;
    procedure SetEnabled(AIndex: Integer; AValue: Boolean);
    procedure SetItems(AValue: TStrings);
    procedure SetSelected(AValue: Integer);
    function GetSelectedText: string;
    procedure SetMaterialLibrary(AValue: TVKMaterialLibrary);
    procedure SetTitleMaterialName(const AValue: string);
    procedure SetTitleWidth(AValue: Integer);
    procedure SetTitleHeight(AValue: Integer);

    procedure ItemsChanged(Sender: TObject);

  public
    { Public Properties }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure BuildList(var rci: TVKRenderContextInfo); override;

    property Enabled[AIndex: Integer]: Boolean read GetEnabled write SetEnabled;
    property SelectedText: string read GetSelectedText;

    procedure SelectNext;
    procedure SelectPrev;

    procedure MouseMenuSelect(const X, Y: integer);

  published
    { Published Properties }
    property MaterialLibrary: TVKMaterialLibrary read FMaterialLibrary write
      SetMaterialLibrary;

    property MenuScale: TVKGameMenuScale read FMenuScale write SetMenuScale
      default gmsNormal;
    property MarginHorz: Integer read FMarginHorz write SetMarginHorz default
      16;
    property MarginVert: Integer read FMarginVert write SetMarginVert default
      16;
    property Spacing: Integer read FSpacing write SetSpacing default 16;
    property Font: TVKCustomBitmapFont read FFont write SetFont;

    property TitleMaterialName: string read FTitleMaterialName write
      SetTitleMaterialName;
    property TitleWidth: Integer read FTitleWidth write SetTitleWidth default 0;
    property TitleHeight: Integer read FTitleHeight write SetTitleHeight default
      0;

    property BackColor: TVKColor read FBackColor write SetBackColor;
    property InactiveColor: TVKColor read FInactiveColor write SetInactiveColor;
    property ActiveColor: TVKColor read FActiveColor write SetActiveColor;
    property DisabledColor: TVKColor read FDisabledColor write SetDisabledColor;

    property Items: TStrings read FItems write SetItems;
    property Selected: Integer read FSelected write SetSelected default -1;
    property OnSelectedChanged: TNotifyEvent read FOnSelectedChanged write
      FOnSelectedChanged;

    // these are the extents of the menu
    property BoxTop: integer read FBoxTop;
    property BoxBottom: integer read FBoxBottom;
    property BoxLeft: integer read FBoxLeft;
    property BoxRight: integer read FBoxRight;
    // this is the top of the first menu item
    property MenuTop: integer read FMenuTop;

    //publish other stuff from TVKBaseSceneObject
    property ObjectsSorting;
    property VisibilityCulling;
    property Position;
    property Visible;
    property OnProgress;
    property Behaviours;
    property Effects;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TVKGameMenu ------------------
// ------------------

// Create
//

constructor TVKGameMenu.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChanged;
  FSelected := -1;
  FMarginHorz := 16;
  FMarginVert := 16;
  FSpacing := 16;
  FMenuScale := gmsNormal;
  FBackColor := TVKColor.CreateInitialized(Self, clrTransparent, NotifyChange);
  FInactiveColor := TVKColor.CreateInitialized(Self, clrGray75, NotifyChange);
  FActiveColor := TVKColor.CreateInitialized(Self, clrWhite, NotifyChange);
  FDisabledColor := TVKColor.CreateInitialized(Self, clrGray60, NotifyChange);
end;

// Destroy
//

destructor TVKGameMenu.Destroy;
begin
  inherited;
  FItems.Free;
  Font := nil;
  FBackColor.Free;
  FInactiveColor.Free;
  FActiveColor.Free;
  FDisabledColor.Free;
end;

// Notification
//

procedure TVKGameMenu.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = Font then
      Font := nil;
    if AComponent = MaterialLibrary then
      MaterialLibrary := nil;
  end;
end;

// BuildList
//

procedure TVKGameMenu.BuildList(var rci: TVKRenderContextInfo);
var
  canvas: TVKCanvas;
  buffer: TVKSceneBuffer;
  i, w, h, tw, y: Integer;
  color: TColorVector;
  libMat: TVKLibMaterial;
begin
  if Font = nil then
    Exit;
  case MenuScale of
    gmsNormal:
      begin
        buffer := TVKSceneBuffer(rci.buffer);
        canvas := TVKCanvas.Create(buffer.Width, buffer.Height);
      end;
    gms1024x768: canvas := TVKCanvas.Create(1024, 768);
  else
    canvas := nil;
    Assert(False);
  end;
  try
    // determine extents
    h := FItems.Count * (Font.CharHeight + Spacing) - Spacing + MarginVert * 2;
    if TitleHeight > 0 then
      h := h + TitleHeight + Spacing;
    w := TitleWidth;
    for i := 0 to FItems.Count - 1 do
    begin
      tw := Font.TextWidth(FItems[i]);
      if tw > w then
        w := tw;
    end;
    w := w + 2 * MarginHorz;

    // calculate boundaries for user
    FBoxLeft := Round(Position.X - w / 2);
    FBoxTop := Round(Position.Y - h / 2);
    FBoxRight := Round(Position.X + w / 2);
    FBoxBottom := Round(Position.Y + h / 2);

    // paint back
    if BackColor.Alpha > 0 then
    begin
      canvas.PenColor := BackColor.AsWinColor;
      canvas.PenAlpha := BackColor.Alpha;
      canvas.FillRect(FBoxLeft, FBoxTop, FBoxRight, FBoxBottom);
    end;

    canvas.StopPrimitive;

    // paint items
    y := Round(Position.Y - h / 2 + MarginVert);
    if TitleHeight > 0 then
    begin
      if (TitleMaterialName <> '') and (MaterialLibrary <> nil) and (TitleWidth
        > 0) then
      begin
        libMat := MaterialLibrary.LibMaterialByName(TitleMaterialName);
        if libMat <> nil then
        begin
          libMat.Apply(rci);
          repeat
            glBegin(GL_QUADS);
            glTexCoord2f(0, 0);
            glVertex2f(Position.X - TitleWidth div 2, y + TitleHeight);
            glTexCoord2f(1, 0);
            glVertex2f(Position.X + TitleWidth div 2, y + TitleHeight);
            glTexCoord2f(1, 1);
            glVertex2f(Position.X + TitleWidth div 2, y);
            glTexCoord2f(0, 1);
            glVertex2f(Position.X - TitleWidth div 2, y);
            glEnd;
          until (not libMat.UnApply(rci));
        end;
      end;
      y := y + TitleHeight + Spacing;
      FMenuTop := y;
    end
    else
      FMenuTop := y + Spacing;

    for i := 0 to FItems.Count - 1 do
    begin
      tw := Font.TextWidth(FItems[i]);
      if not Enabled[i] then
        color := DisabledColor.Color
      else if i = Selected then
        color := ActiveColor.Color
      else
        color := InactiveColor.Color;
      Font.TextOut(rci, Position.X - tw div 2, y, FItems[i], color);
      y := y + Font.CharHeight + Spacing;
    end;
  finally
    canvas.Free;
  end;
end;

// SelectNext
//

procedure TVKGameMenu.SelectNext;
var
  i: Integer;
begin
  i := Selected;
  repeat
    i := i + 1;
  until (i >= Items.Count) or Enabled[i];
  if (i < Items.Count) and (i <> Selected) then
    Selected := i;
end;

// SelectPrev
//

procedure TVKGameMenu.SelectPrev;
var
  i: Integer;
begin
  i := Selected;
  repeat
    i := i - 1;
  until (i < 0) or Enabled[i];
  if (i >= 0) and (i <> Selected) then
    Selected := i;
end;

// SetMenuScale
//

procedure TVKGameMenu.SetMenuScale(AValue: TVKGameMenuScale);
begin
  if FMenuScale <> AValue then
  begin
    FMenuScale := AValue;
    StructureChanged;
  end;
end;

// SetMarginHorz
//

procedure TVKGameMenu.SetMarginHorz(AValue: Integer);
begin
  if FMarginHorz <> AValue then
  begin
    FMarginHorz := AValue;
    StructureChanged;
  end;
end;

// SetMarginVert
//

procedure TVKGameMenu.SetMarginVert(AValue: Integer);
begin
  if FMarginVert <> AValue then
  begin
    FMarginVert := AValue;
    StructureChanged;
  end;
end;

// SetSpacing
//

procedure TVKGameMenu.SetSpacing(AValue: Integer);
begin
  if FSpacing <> AValue then
  begin
    FSpacing := AValue;
    StructureChanged;
  end;
end;

// SetFont
//

procedure TVKGameMenu.SetFont(AValue: TVKCustomBitmapFont);
begin
  if FFont <> nil then
    FFont.RemoveFreeNotification(Self);
  FFont := AValue;
  if FFont <> nil then
    FFont.FreeNotification(Self);
end;

// SetBackColor
//

procedure TVKGameMenu.SetBackColor(AValue: TVKColor);
begin
  FBackColor.Assign(AValue);
end;

// SetInactiveColor
//

procedure TVKGameMenu.SetInactiveColor(AValue: TVKColor);
begin
  FInactiveColor.Assign(AValue);
end;

// SetActiveColor
//

procedure TVKGameMenu.SetActiveColor(AValue: TVKColor);
begin
  FActiveColor.Assign(AValue);
end;

// SetDisabledColor
//

procedure TVKGameMenu.SetDisabledColor(AValue: TVKColor);
begin
  FDisabledColor.Assign(AValue);
end;

// GetEnabled
//

function TVKGameMenu.GetEnabled(AIndex: Integer): Boolean;
begin
  Result := not Boolean(PtrUint(FItems.Objects[AIndex]));
end;

// SetEnabled
//

procedure TVKGameMenu.SetEnabled(AIndex: Integer; AValue: Boolean);
begin
  FItems.Objects[AIndex] := TObject(pointer(PtrUInt(ord(not AValue))));
  StructureChanged;
end;

// SetItems
//

procedure TVKGameMenu.SetItems(AValue: TStrings);
begin
  FItems.Assign(AValue);
  SetSelected(Selected);
end;

// SetSelected
//

procedure TVKGameMenu.SetSelected(AValue: Integer);
begin
  if AValue < -1 then
    AValue := -1;
  if AValue >= FItems.Count then
    AValue := FItems.Count - 1;
  if AValue <> FSelected then
  begin
    FSelected := AValue;
    StructureChanged;
    if Assigned(FOnSelectedChanged) then
      FOnSelectedChanged(Self);
  end;
end;

// GetSelectedText
//

function TVKGameMenu.GetSelectedText: string;
begin
  if Cardinal(Selected) < Cardinal(FItems.Count) then
    Result := FItems[Selected]
  else
    Result := '';
end;

// SetMaterialLibrary
//

procedure TVKGameMenu.SetMaterialLibrary(AValue: TVKMaterialLibrary);
begin
  if FMaterialLibrary <> nil then
    FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := AValue;
  if FMaterialLibrary <> nil then
    FMaterialLibrary.FreeNotification(Self);
end;

// SetTitleMaterialName
//

procedure TVKGameMenu.SetTitleMaterialName(const AValue: string);
begin
  if FTitleMaterialName <> AValue then
  begin
    FTitleMaterialName := AValue;
    StructureChanged;
  end;
end;

// SetTitleWidth
//

procedure TVKGameMenu.SetTitleWidth(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FTitleWidth <> AValue then
  begin
    FTitleWidth := AValue;
    StructureChanged;
  end;
end;

// SetTitleHeight
//

procedure TVKGameMenu.SetTitleHeight(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FTitleHeight <> AValue then
  begin
    FTitleHeight := AValue;
    StructureChanged;
  end;
end;

// ItemsChanged
//

procedure TVKGameMenu.ItemsChanged(Sender: TObject);
begin
  SetSelected(FSelected);
  StructureChanged;
end;

// MouseMenuSelect
//

procedure TVKGameMenu.MouseMenuSelect(const X, Y: integer);
begin
  if (X >= BoxLeft) and (Y >= MenuTop) and
    (X <= BoxRight) and (Y <= BoxBottom) then
  begin
    Selected := (Y - FMenuTop) div (Font.CharHeight + FSpacing);
  end
  else
    Selected := -1;
end;

// GetMaterialLibrary
//

function TVKGameMenu.GetMaterialLibrary: TVKAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  RegisterClass(TVKGameMenu);

end.

