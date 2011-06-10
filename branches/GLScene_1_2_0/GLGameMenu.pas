//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLGameMenu<p>

   Manages a basic game menu UI<p>

 <b>History : </b><font size=-1><ul>
      <li>10/06/11 - Yar - Transition to indirect rendering objects
      <li>16/03/11 - Yar - Fixes after emergence of GLMaterialEx
      <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>31/05/10 - Yar - Fixed for Linux x64
      <li>22/04/10 - Yar - Fixes after GLState revision
      <li>05/03/10 - DanB - More state added to TGLStateCache
      <li>04/09/07 - DaStr - Fixed memory leak in TGLGameMenu
                              (BugtrackerID = 1787617) (thanks Pierre Lemerle)
      <li>06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
      <li>30/03/07 - DaStr - Added $I GLScene.inc
      <li>28/03/07 - DaStr - Renamed parameters in some methods
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678658)
      <li>26/03/07 - DaveK - back to TGLSceneObject for Material support
      <li>16/02/07 - DaStr & DaveK - TGLGameMenu.MouseMenuSelect bugfixed (again)
                             Component made descendant of TGLBaseSceneObject
                             IGLMaterialLibrarySupported added
      <li>20/12/06 - DaStr - TGLGameMenu.MouseMenuSelect bugfixed (thanks to Predator)
      <li>03/27/06 - DaveK - added mouse selection support
      <li>03/03/05 - EG - Creation
   </ul></font>
}
unit GLGameMenu;

interface

{$I GLScene.inc}

uses
  Classes,
  GLScene,
  GLMaterial,
  GLBitmapFont,
  GLCrossPlatform,
  GLColor,
  GLRenderContextInfo,
  GLPipelineTransformation,
  GLS_Mesh,
  GLS_DrawTechnique;

type

  // TGLGameMenuScale
  //
  TGLGameMenuScale = (gmsNormal, gms1024x768);

  // TGLGameMenu
  //
  {: Classic game menu interface made of several lines.<p> }
  TGLGameMenu = class(TGLSceneObject, IGLMaterialLibrarySupported)
  private
    { Private Properties }
    FBackgroundBatch: TDrawBatch;
    FTitleBatch: TDrawBatch;
    FTextBatches: TDrawBatchArray;
    FTransformation: TTransformationRec;
    FItems: TStrings;
    FSelected: Integer;
    FFont: TGLCustomBitmapFont;
    FMarginVert, FMarginHorz, FSpacing: Integer;
    FMenuScale: TGLGameMenuScale;
    FBackColor: TGLColor;
    FInactiveColor, FActiveColor, FDisabledColor: TGLColor;
    FMaterialLibrary: TGLMaterialLibrary;
    FTitleMaterialName: TGLLibMaterialName;
    FTitleWidth, FTitleHeight: Integer;
    FOnSelectedChanged: TNotifyEvent;
    FBoxTop, FBoxBottom, FBoxLeft, FBoxRight: Integer;
    FMenuTop: integer;
    //implementing IGLMaterialLibrarySupported
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
    procedure OnColorChaged(Sender: TObject);
    function GetBoxBottom: integer;
    function GetBoxLeft: integer;
    function GetBoxRight: integer;
    function GetBoxTop: integer;
    function GetMenuTop: integer;
  protected
    { Protected Properties }
    procedure SetScene(const value: TGLScene); override;
    procedure SetMenuScale(AValue: TGLGameMenuScale);
    procedure SetMarginHorz(AValue: Integer);
    procedure SetMarginVert(AValue: Integer);
    procedure SetSpacing(AValue: Integer);
    procedure SetFont(AValue: TGLCustomBitmapFont);
    procedure SetBackColor(AValue: TGLColor);
    procedure SetInactiveColor(AValue: TGLColor);
    procedure SetActiveColor(AValue: TGLColor);
    procedure SetDisabledColor(AValue: TGLColor);
    function GetEnabled(AIndex: Integer): Boolean;
    procedure SetEnabled(AIndex: Integer; AValue: Boolean);
    procedure SetItems(AValue: TStrings);
    procedure SetSelected(AValue: Integer);
    function GetSelectedText: string;
    procedure SetMaterialLibrary(AValue: TGLMaterialLibrary);
    procedure SetTitleMaterialName(const AValue: string);
    procedure SetTitleWidth(AValue: Integer);
    procedure SetTitleHeight(AValue: Integer);

    procedure ItemsChanged(Sender: TObject);
    procedure BuildMeshes;
    procedure FreeBatches;
    procedure RegisterBatches;
  public
    { Public Properties }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

    procedure DoRender(var ARci: TRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;

    property Enabled[AIndex: Integer]: Boolean read GetEnabled write SetEnabled;
    property SelectedText: string read GetSelectedText;

    procedure SelectNext;
    procedure SelectPrev;

    procedure MouseMenuSelect(const X, Y: integer);

  published
    { Published Properties }
    property MaterialLibrary: TGLMaterialLibrary read FMaterialLibrary write
      SetMaterialLibrary;

    property MenuScale: TGLGameMenuScale read FMenuScale write SetMenuScale
      default gmsNormal;
    property MarginHorz: Integer read FMarginHorz write SetMarginHorz default
      16;
    property MarginVert: Integer read FMarginVert write SetMarginVert default
      16;
    property Spacing: Integer read FSpacing write SetSpacing default 16;
    property Font: TGLCustomBitmapFont read FFont write SetFont;

    property TitleMaterialName: string read FTitleMaterialName write
      SetTitleMaterialName;
    property TitleWidth: Integer read FTitleWidth write SetTitleWidth default 0;
    property TitleHeight: Integer read FTitleHeight write SetTitleHeight default
      0;

    property BackColor: TGLColor read FBackColor write SetBackColor;
    property InactiveColor: TGLColor read FInactiveColor write SetInactiveColor;
    property ActiveColor: TGLColor read FActiveColor write SetActiveColor;
    property DisabledColor: TGLColor read FDisabledColor write SetDisabledColor;

    property Items: TStrings read FItems write SetItems;
    property Selected: Integer read FSelected write SetSelected default -1;
    property OnSelectedChanged: TNotifyEvent read FOnSelectedChanged write
      FOnSelectedChanged;

    // these are the extents of the menu
    property BoxTop: integer read GetBoxTop;
    property BoxBottom: integer read GetBoxBottom;
    property BoxLeft: integer read GetBoxLeft;
    property BoxRight: integer read GetBoxRight;
    // this is the top of the first menu item
    property MenuTop: integer read GetMenuTop;

    //publish other stuff from TGLBaseSceneObject
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

uses
  SysUtils,
  GLContext,
  GLS_ShaderParameter,
  GLS_Material,
  GLState,
  VectorTypes,
  VectorGeometry;

// ------------------
// ------------------ TGLGameMenu ------------------
// ------------------

// Create
//

constructor TGLGameMenu.Create(AOwner: TComponent);
const
  cGameMenuMaterialName = 'GLScene_GameMenu_Material';
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDeferredDraw, osStreamDraw];
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChanged;
  FSelected := -1;
  FMarginHorz := 16;
  FMarginVert := 16;
  FSpacing := 16;
  FMenuScale := gmsNormal;
  FBackColor := TGLColor.CreateInitialized(Self, clrTransparent, NotifyChange);
  FInactiveColor := TGLColor.CreateInitialized(Self, clrGray75, NotifyChange);
  FActiveColor := TGLColor.CreateInitialized(Self, clrWhite, NotifyChange);
  FDisabledColor := TGLColor.CreateInitialized(Self, clrGray60, NotifyChange);
  FBackColor.OnNotifyChange := OnColorChaged;
  FInactiveColor.OnNotifyChange := OnColorChaged;
  FActiveColor.OnNotifyChange := OnColorChaged;
  FDisabledColor.OnNotifyChange := OnColorChaged;

  FBackgroundBatch.Mesh := TMeshAtom.Create;
  FBackgroundBatch.Mesh.TagName := Format('%s_Background', [ClassName]);
  FBackgroundBatch.Transformation := @FTransformation;
  FTitleBatch.Mesh := TMeshAtom.Create;
  FTitleBatch.Mesh.TagName := Format('%s_Title', [ClassName]);
  FTitleBatch.Transformation := @FTransformation;

  FBackgroundBatch.Material := GetInternalMaterialLibrary.Materials.GetLibMaterialByName(cGameMenuMaterialName);
  if FBackgroundBatch.Material = nil then
  begin
    FBackgroundBatch.Material := GetInternalMaterialLibrary.Materials.Add;
    with TGLLibMaterialEx(FBackgroundBatch.Material) do
    begin
      Name := cGameMenuMaterialName;
      FixedFunction.MaterialOptions := [moNoLighting, moIgnoreFog];
      FixedFunction.BlendingMode := bmTransparency;
    end;
  end;
end;

// Destroy
//

destructor TGLGameMenu.Destroy;
begin
  inherited;
  FItems.Free;
  Font := nil;
  FBackColor.Free;
  FInactiveColor.Free;
  FActiveColor.Free;
  FDisabledColor.Free;
  FBackgroundBatch.Mesh.Destroy;
  FTitleBatch.Mesh.Destroy;
  FreeBatches;
end;

procedure TGLGameMenu.FreeBatches;
var
  I: Integer;
begin
  if Assigned(Scene) then
    for I := 0 to High(FTextBatches) do
      Scene.RenderManager.UnRegisterBatch(FTextBatches[I]);

  for I := 0 to High(FTextBatches) do
    FTextBatches[I].Mesh.Free;
  if Length(FTextBatches) > 0 then
    FTextBatches[0].InstancesChain.Free;
  SetLength(FTextBatches, 0);
end;

// Notification
//

procedure TGLGameMenu.Notification(AComponent: TComponent; Operation:
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

procedure TGLGameMenu.OnColorChaged(Sender: TObject);
begin
  StructureChanged;
end;

procedure TGLGameMenu.RegisterBatches;
var
  I: Integer;
begin
  if Assigned(Scene) then
  begin
    for I := 0 to High(FTextBatches) do
    begin
      Scene.RenderManager.RegisterBatch(FTextBatches[I]);
      FTextBatches[I].Mesh.TagName := Format('%s_text_part%d', [ClassName, I]);
      FTextBatches[I].Transformation := @FTransformation;
    end;
  end;
end;

procedure TGLGameMenu.BuildMeshes;
var
  i, w, h, tw, y: Integer;
  color, v: TVector;
  lens: array of Integer;
begin
  // determine extents
  h := FItems.Count * (Font.CharHeight + Spacing) - Spacing + MarginVert * 2;
  if TitleHeight > 0 then
    h := h + TitleHeight + Spacing;
  w := TitleWidth;
  SetLength(lens, FItems.Count);
  for i := 0 to FItems.Count - 1 do
  begin
    tw := Font.TextWidth(FItems[i]);
    lens[i] := tw;
    if tw > w then
      w := tw;
  end;
  w := w + 2 * MarginHorz;

  // calculate boundaries for user
  FBoxLeft := Round(- w / 2);
  FBoxTop := Round(- h / 2);
  FBoxRight := Round( w / 2);
  FBoxBottom := Round(h / 2);

  with FBackgroundBatch.Mesh do
  begin
    Lock;
    try
      Clear;
      DeclareAttribute(attrPosition, GLSLType2f);
      DeclareAttribute(attrColor, GLSLType4f);

      BeginAssembly(mpTRIANGLE_STRIP);
      Attribute4f(attrColor, BackColor.Color);
      Attribute2f(attrPosition, FBoxLeft, FBoxTop);
      EmitVertex;
      Attribute2f(attrPosition, FBoxLeft, FBoxBottom);
      EmitVertex;
      Attribute2f(attrPosition, FBoxRight, FBoxTop);
      EmitVertex;
      Attribute2f(attrPosition, FBoxRight, FBoxBottom);
      EmitVertex;
      EndAssembly;
    finally
      UnLock;
    end;
  end;

  y := Round(- h / 2 + MarginVert);
  if TitleHeight > 0 then
  begin
    with FTitleBatch.Mesh do
    begin
      Lock;
      try
        Clear;
        DeclareAttribute(attrPosition, GLSLType2f);
        DeclareAttribute(attrTexCoord0, GLSLType2f);

        BeginAssembly(mpTRIANGLE_STRIP);
        Attribute2f(attrPosition, - TitleWidth div 2, y + TitleHeight);
        Attribute2f(attrTexCoord0, 0, 0);
        EmitVertex;
        Attribute2f(attrPosition, - TitleWidth div 2, y);
        Attribute2f(attrTexCoord0, 0, 1);
        EmitVertex;
        Attribute2f(attrPosition, TitleWidth div 2, y + TitleHeight);
        Attribute2f(attrTexCoord0, 1, 0);
        EmitVertex;
        Attribute2f(attrPosition, TitleWidth div 2, y);
        Attribute2f(attrTexCoord0, 1, 1);
        EmitVertex;
        EndAssembly;
      finally
        UnLock;
      end;
    end;
    y := y + TitleHeight + Spacing;
    FMenuTop := y;
  end
  else
    FMenuTop := y + Spacing;

  FreeBatches;
  v[2] := 0;
  v[3] := 1;
  for i := 0 to FItems.Count - 1 do
  begin
    if not Enabled[i] then
      color := DisabledColor.Color
    else if i = Selected then
      color := ActiveColor.Color
    else
      color := InactiveColor.Color;
    v[0] := - lens[i] div 2;
    v[1] := y;
    Font.BuildString(FTextBatches, FItems[i], taLeftJustify, tlTop, color, @v, True);
    y := y + Font.CharHeight + Spacing;
  end;
  RegisterBatches;

  ClearStructureChanged;
end;


procedure TGLGameMenu.DoRender(var ARci: TRenderContextInfo; ARenderSelf, ARenderChildren: Boolean);
var
  I: Integer;
begin
  if (ocStructure in Changes) and Assigned(Font) then
  begin
    BuildMeshes;
  end;

  if ARenderSelf then
  begin

    ARci.PipelineTransformation.Push;
    ARci.PipelineTransformation.ModelViewMatrix := IdentityHmgMatrix;
    case MenuScale of
      gmsNormal:
        ARci.PipelineTransformation.ProjectionMatrix := CreateOrthoMatrix(
          -Position.X, - Position.X + ARci.viewPortSize.cx,
          -Position.Y + ARci.viewPortSize.cy, - Position.Y, -1, 1);
      gms1024x768:
        ARci.PipelineTransformation.ProjectionMatrix := CreateOrthoMatrix(
          -Position.X, - Position.X + 1024,
          -Position.Y + 768, - Position.Y, -1, 1);
    end;
    FTransformation := ARci.PipelineTransformation.StackTop;
    ARci.PipelineTransformation.Pop;

    if BackColor.Alpha > 0 then
    begin
      FBackgroundBatch.Order := ARci.orderCounter;
      Inc(ARci.orderCounter);
    end;

    if Assigned(FTitleBatch.Material)
      and (TitleWidth > 0) and (TitleHeight > 0) then
    begin
      FTitleBatch.Order := ARci.orderCounter;
      Inc(ARci.orderCounter);
    end;

    for I := High(FTextBatches) downto 0 do
      FTextBatches[I].Order := ARci.orderCounter;
  end;

  if ARenderChildren then
    RenderChildren(0, Count - 1, ARci);
end;

// SelectNext
//

procedure TGLGameMenu.SelectNext;
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

procedure TGLGameMenu.SelectPrev;
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

procedure TGLGameMenu.SetMenuScale(AValue: TGLGameMenuScale);
begin
  if FMenuScale <> AValue then
  begin
    FMenuScale := AValue;
    StructureChanged;
  end;
end;

// SetMarginHorz
//

procedure TGLGameMenu.SetMarginHorz(AValue: Integer);
begin
  if FMarginHorz <> AValue then
  begin
    FMarginHorz := AValue;
    StructureChanged;
  end;
end;

// SetMarginVert
//

procedure TGLGameMenu.SetMarginVert(AValue: Integer);
begin
  if FMarginVert <> AValue then
  begin
    FMarginVert := AValue;
    StructureChanged;
  end;
end;

// SetSpacing
//

procedure TGLGameMenu.SetSpacing(AValue: Integer);
begin
  if FSpacing <> AValue then
  begin
    FSpacing := AValue;
    StructureChanged;
  end;
end;

// SetFont
//

procedure TGLGameMenu.SetFont(AValue: TGLCustomBitmapFont);
begin
  if FFont <> nil then
    FFont.RemoveFreeNotification(Self);
  FFont := AValue;
  if FFont <> nil then
    FFont.FreeNotification(Self);
  StructureChanged;
end;

// SetBackColor
//

procedure TGLGameMenu.SetBackColor(AValue: TGLColor);
begin
  FBackColor.Assign(AValue);
  StructureChanged;
end;

// SetInactiveColor
//

procedure TGLGameMenu.SetInactiveColor(AValue: TGLColor);
begin
  FInactiveColor.Assign(AValue);
  StructureChanged;
end;

// SetActiveColor
//

procedure TGLGameMenu.SetActiveColor(AValue: TGLColor);
begin
  FActiveColor.Assign(AValue);
  StructureChanged;
end;

// SetDisabledColor
//

procedure TGLGameMenu.SetDisabledColor(AValue: TGLColor);
begin
  FDisabledColor.Assign(AValue);
  StructureChanged;
end;

function TGLGameMenu.GetBoxBottom: integer;
begin
  Result := FBoxBottom + Round(Position.Y);
end;

function TGLGameMenu.GetBoxLeft: integer;
begin
  Result := FBoxLeft + Round(Position.X);
end;

function TGLGameMenu.GetBoxRight: integer;
begin
  Result := FBoxRight + Round(Position.X);
end;

function TGLGameMenu.GetBoxTop: integer;
begin
  Result := FBoxTop + Round(Position.Y);
end;

// GetEnabled
//

function TGLGameMenu.GetEnabled(AIndex: Integer): Boolean;
begin
  Result := not Boolean(PtrUint(FItems.Objects[AIndex]));
end;

// SetEnabled
//

procedure TGLGameMenu.SetEnabled(AIndex: Integer; AValue: Boolean);
begin
  FItems.Objects[AIndex] := TObject(pointer(PtrUInt(ord(not AValue))));
  StructureChanged;
end;

// SetItems
//

procedure TGLGameMenu.SetItems(AValue: TStrings);
begin
  FItems.Assign(AValue);
  SetSelected(Selected);
end;

// SetSelected
//

procedure TGLGameMenu.SetScene(const value: TGLScene);
begin
  if value <> Scene then
  begin
    if Assigned(Scene) then
    begin
      Scene.RenderManager.UnRegisterBatch(FBackgroundBatch);
      Scene.RenderManager.UnRegisterBatch(FTitleBatch);
    end;
    if Assigned(value) then
    begin
      value.RenderManager.RegisterBatch(FBackgroundBatch);
      value.RenderManager.RegisterBatch(FTitleBatch);
    end;
    inherited;
  end;
end;

procedure TGLGameMenu.SetSelected(AValue: Integer);
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

function TGLGameMenu.GetSelectedText: string;
begin
  if Cardinal(Selected) < Cardinal(FItems.Count) then
    Result := FItems[Selected]
  else
    Result := '';
end;

// SetMaterialLibrary
//

procedure TGLGameMenu.SetMaterialLibrary(AValue: TGLMaterialLibrary);
begin
  if FMaterialLibrary <> nil then
    FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := AValue;
  if FMaterialLibrary <> nil then
  begin
    FMaterialLibrary.FreeNotification(Self);
    FTitleBatch.Material := FMaterialLibrary.LibMaterialByName(FTitleMaterialName);
  end
  else
    FTitleBatch.Material := nil;
end;

// SetTitleMaterialName
//

procedure TGLGameMenu.SetTitleMaterialName(const AValue: string);
begin
  if FTitleMaterialName <> AValue then
  begin
    FTitleMaterialName := AValue;
    if Assigned(FMaterialLibrary) then
      FTitleBatch.Material := FMaterialLibrary.LibMaterialByName(AValue);
    NotifyChange(Self);
  end;
end;

// SetTitleWidth
//

procedure TGLGameMenu.SetTitleWidth(AValue: Integer);
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

procedure TGLGameMenu.SetTitleHeight(AValue: Integer);
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

procedure TGLGameMenu.ItemsChanged(Sender: TObject);
begin
  SetSelected(FSelected);
end;

// MouseMenuSelect
//

procedure TGLGameMenu.MouseMenuSelect(const X, Y: integer);
var
  OldValue: Integer;
begin
  OldValue := Selected;
  if (X >= BoxLeft) and (Y >= MenuTop) and
    (X <= BoxRight) and (Y <= BoxBottom) then
  begin
    Selected := (Y - MenuTop) div (Font.CharHeight + FSpacing);
  end
  else
    Selected := -1;
  if OldValue <> Selected then
    StructureChanged;
end;

// GetMaterialLibrary
//

function TGLGameMenu.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

function TGLGameMenu.GetMenuTop: integer;
begin
  Result := FMenuTop + Round(Position.Y);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  RegisterClass(TGLGameMenu);

end.

