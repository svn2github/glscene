//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Manages a basic game menu UI 
  
}
unit VXS.GameMenu;

interface

{$I VXScene.inc}

uses
  System.Classes, System.SysUtils,

  VXS.Scene, VXS.Material, VXS.BitmapFont, VXS.CrossPlatform, VXS.Color,
  VXS.RenderContextInfo, VXS.Canvas, Winapi.OpenGL, Winapi.OpenGLext,  VXS.Context;

type

  // TVXGameMenuScale
  //
  TVXGameMenuScale = (gmsNormal, gms1024x768);

  // TVXGameMenu
  //
  { Classic game menu interface made of several lines.  }
  TVXGameMenu = class(TVXSceneObject, IGLMaterialLibrarySupported)
  private
    { Private Properties }
    FItems: TStrings;
    FSelected: Integer;
    FFont: TVXCustomBitmapFont;
    FMarginVert, FMarginHorz, FSpacing: Integer;
    FMenuScale: TVXGameMenuScale;
    FBackColor: TVXColor;
    FInactiveColor, FActiveColor, FDisabledColor: TVXColor;
    FMaterialLibrary: TVXMaterialLibrary;
    FTitleMaterialName: TVXLibMaterialName;
    FTitleWidth, FTitleHeight: Integer;
    FOnSelectedChanged: TNotifyEvent;
    FBoxTop, FBoxBottom, FBoxLeft, FBoxRight: Integer;
    FMenuTop: integer;
    //implementing IGLMaterialLibrarySupported
    function GetMaterialLibrary: TVXAbstractMaterialLibrary;
  protected
    { Protected Properties }
    procedure SetMenuScale(AValue: TVXGameMenuScale);
    procedure SetMarginHorz(AValue: Integer);
    procedure SetMarginVert(AValue: Integer);
    procedure SetSpacing(AValue: Integer);
    procedure SetFont(AValue: TVXCustomBitmapFont);
    procedure SetBackColor(AValue: TVXColor);
    procedure SetInactiveColor(AValue: TVXColor);
    procedure SetActiveColor(AValue: TVXColor);
    procedure SetDisabledColor(AValue: TVXColor);
    function GetEnabled(AIndex: Integer): Boolean;
    procedure SetEnabled(AIndex: Integer; AValue: Boolean);
    procedure SetItems(AValue: TStrings);
    procedure SetSelected(AValue: Integer);
    function GetSelectedText: string;
    procedure SetMaterialLibrary(AValue: TVXMaterialLibrary);
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
    procedure BuildList(var rci: TVXRenderContextInfo); override;

    property Enabled[AIndex: Integer]: Boolean read GetEnabled write SetEnabled;
    property SelectedText: string read GetSelectedText;

    procedure SelectNext;
    procedure SelectPrev;

    procedure MouseMenuSelect(const X, Y: integer);

  published
    { Published Properties }
    property MaterialLibrary: TVXMaterialLibrary read FMaterialLibrary write
      SetMaterialLibrary;

    property MenuScale: TVXGameMenuScale read FMenuScale write SetMenuScale
      default gmsNormal;
    property MarginHorz: Integer read FMarginHorz write SetMarginHorz default
      16;
    property MarginVert: Integer read FMarginVert write SetMarginVert default
      16;
    property Spacing: Integer read FSpacing write SetSpacing default 16;
    property Font: TVXCustomBitmapFont read FFont write SetFont;

    property TitleMaterialName: string read FTitleMaterialName write
      SetTitleMaterialName;
    property TitleWidth: Integer read FTitleWidth write SetTitleWidth default 0;
    property TitleHeight: Integer read FTitleHeight write SetTitleHeight default
      0;

    property BackColor: TVXColor read FBackColor write SetBackColor;
    property InactiveColor: TVXColor read FInactiveColor write SetInactiveColor;
    property ActiveColor: TVXColor read FActiveColor write SetActiveColor;
    property DisabledColor: TVXColor read FDisabledColor write SetDisabledColor;

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

    //publish other stuff from TVXBaseSceneObject
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
// ------------------ TVXGameMenu ------------------
// ------------------

// Create
//

constructor TVXGameMenu.Create(AOwner: TComponent);
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
  FBackColor := TVXColor.CreateInitialized(Self, clrTransparent, NotifyChange);
  FInactiveColor := TVXColor.CreateInitialized(Self, clrGray75, NotifyChange);
  FActiveColor := TVXColor.CreateInitialized(Self, clrWhite, NotifyChange);
  FDisabledColor := TVXColor.CreateInitialized(Self, clrGray60, NotifyChange);
end;

// Destroy
//

destructor TVXGameMenu.Destroy;
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

procedure TVXGameMenu.Notification(AComponent: TComponent; Operation:
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

procedure TVXGameMenu.BuildList(var rci: TVXRenderContextInfo);
var
  canvas: TVXCanvas;
  buffer: TVXSceneBuffer;
  i, w, h, tw, y: Integer;
  color: TColorVector;
  libMat: TVXLibMaterial;
begin
  if Font = nil then
    Exit;
  case MenuScale of
    gmsNormal:
      begin
        buffer := TVXSceneBuffer(rci.buffer);
        canvas := TVXCanvas.Create(buffer.Width, buffer.Height);
      end;
    gms1024x768: canvas := TVXCanvas.Create(1024, 768);
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

procedure TVXGameMenu.SelectNext;
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

procedure TVXGameMenu.SelectPrev;
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

procedure TVXGameMenu.SetMenuScale(AValue: TVXGameMenuScale);
begin
  if FMenuScale <> AValue then
  begin
    FMenuScale := AValue;
    StructureChanged;
  end;
end;

// SetMarginHorz
//

procedure TVXGameMenu.SetMarginHorz(AValue: Integer);
begin
  if FMarginHorz <> AValue then
  begin
    FMarginHorz := AValue;
    StructureChanged;
  end;
end;

// SetMarginVert
//

procedure TVXGameMenu.SetMarginVert(AValue: Integer);
begin
  if FMarginVert <> AValue then
  begin
    FMarginVert := AValue;
    StructureChanged;
  end;
end;

// SetSpacing
//

procedure TVXGameMenu.SetSpacing(AValue: Integer);
begin
  if FSpacing <> AValue then
  begin
    FSpacing := AValue;
    StructureChanged;
  end;
end;

// SetFont
//

procedure TVXGameMenu.SetFont(AValue: TVXCustomBitmapFont);
begin
  if FFont <> nil then
    FFont.RemoveFreeNotification(Self);
  FFont := AValue;
  if FFont <> nil then
    FFont.FreeNotification(Self);
end;

// SetBackColor
//

procedure TVXGameMenu.SetBackColor(AValue: TVXColor);
begin
  FBackColor.Assign(AValue);
end;

// SetInactiveColor
//

procedure TVXGameMenu.SetInactiveColor(AValue: TVXColor);
begin
  FInactiveColor.Assign(AValue);
end;

// SetActiveColor
//

procedure TVXGameMenu.SetActiveColor(AValue: TVXColor);
begin
  FActiveColor.Assign(AValue);
end;

// SetDisabledColor
//

procedure TVXGameMenu.SetDisabledColor(AValue: TVXColor);
begin
  FDisabledColor.Assign(AValue);
end;

// GetEnabled
//

function TVXGameMenu.GetEnabled(AIndex: Integer): Boolean;
begin
  Result := not Boolean(PtrUint(FItems.Objects[AIndex]));
end;

// SetEnabled
//

procedure TVXGameMenu.SetEnabled(AIndex: Integer; AValue: Boolean);
begin
  FItems.Objects[AIndex] := TObject(pointer(PtrUInt(ord(not AValue))));
  StructureChanged;
end;

// SetItems
//

procedure TVXGameMenu.SetItems(AValue: TStrings);
begin
  FItems.Assign(AValue);
  SetSelected(Selected);
end;

// SetSelected
//

procedure TVXGameMenu.SetSelected(AValue: Integer);
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

function TVXGameMenu.GetSelectedText: string;
begin
  if Cardinal(Selected) < Cardinal(FItems.Count) then
    Result := FItems[Selected]
  else
    Result := '';
end;

// SetMaterialLibrary
//

procedure TVXGameMenu.SetMaterialLibrary(AValue: TVXMaterialLibrary);
begin
  if FMaterialLibrary <> nil then
    FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := AValue;
  if FMaterialLibrary <> nil then
    FMaterialLibrary.FreeNotification(Self);
end;

// SetTitleMaterialName
//

procedure TVXGameMenu.SetTitleMaterialName(const AValue: string);
begin
  if FTitleMaterialName <> AValue then
  begin
    FTitleMaterialName := AValue;
    StructureChanged;
  end;
end;

// SetTitleWidth
//

procedure TVXGameMenu.SetTitleWidth(AValue: Integer);
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

procedure TVXGameMenu.SetTitleHeight(AValue: Integer);
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

procedure TVXGameMenu.ItemsChanged(Sender: TObject);
begin
  SetSelected(FSelected);
  StructureChanged;
end;

// MouseMenuSelect
//

procedure TVXGameMenu.MouseMenuSelect(const X, Y: integer);
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

function TVXGameMenu.GetMaterialLibrary: TVXAbstractMaterialLibrary;
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

  RegisterClass(TVXGameMenu);

end.

