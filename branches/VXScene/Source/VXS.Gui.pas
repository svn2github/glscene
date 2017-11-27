//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  In GL windows management classes and structures 
 
}

unit VXS.Gui;

interface

{$I VXScene.inc}

uses
  System.Classes, System.SysUtils,
     
  VXS.Scene, VXS.BitmapFont, VXS.Material, VXS.CrossPlatform, Winapi.OpenGL, Winapi.OpenGLext,  VXS.Context,
  VXS.PersistentClasses, VXS.VectorGeometry, VXS.Coordinates, VXS.BaseClasses;

type

  TVXBaseGuiObject = class(TVXBaseSceneObject)
  private
    FRecursiveVisible: Boolean;
    FWidth: Single;
    FHeight: Single;

  protected
    // self notification on hide. Also notifies children.
    procedure NotifyHide; virtual;
    // child notification on show. Also notifies children.
    procedure NotifyShow; virtual;

    procedure SetLeft(const Value: GLfloat);
    function GetLeft: GLfloat;
    procedure SetTop(const Value: GLfloat);
    function GetTop: GLfloat;
    procedure SetWidth(const val: Single);
    procedure SetHeight(const val: Single);
    procedure SetVisible(aValue: Boolean); override;

  public
    constructor Create(AOwner: TComponent); override;

    procedure AddChild(AChild: TVXBaseSceneObject); override;
    procedure Insert(aIndex: Integer; aChild: TVXBaseSceneObject); override;

    { GuiComponent Width in 3D world units. }
    property Width: Single read FWidth write SetWidth;
    { GuiComponent Height in 3D world units. }
    property Height: Single read FHeight write SetHeight;
    { GuiComponent Left in 3D world units. }
    property Left: GLfloat read GetLeft write SetLeft;
    { GuiComponent Top in 3D world units. }
    property Top: GLfloat read GetTop write SetTop;

    property RecursiveVisible: Boolean read FRecursiveVisible;
  end;

  TGUIAlignments = (GLAlTopLeft, GLAlTop, GLAlTopRight, GLAlLeft, GLAlCenter,
    GLAlRight, GLAlBottomLeft, GLAlBottom, GLAlBottomRight, GLAlBorder);
  TGUIRect = record
    X1: GLfloat;
    Y1: GLfloat;
    X2: GLfloat;
    Y2: GLfloat;
    XTiles: GLfloat;
    YTiles: GLfloat;
  end;
  TGUIDrawResult = array[TGUIAlignments] of TGUIRect;

  TVXGuiElementName = string;
  TVXGuiElement = class(TCollectionItem)
  private
    FTopLeft: TVXCoordinates2;
    FBottomRight: TVXCoordinates2;
    FScale: TVXCoordinates2;
    FAlign: TGUIAlignments;
    FName: TVXGuiElementName;
  protected
    function GetDisplayName: string; override;
    procedure SetName(const val: TVXGuiElementName);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property TopLeft: TVXCoordinates2 read FTopLeft write FTopLeft;
    property BottomRight: TVXCoordinates2 read FBottomRight write FBottomRight;
    property Scale: TVXCoordinates2 read FScale write FScale;
    property Align: TGUIAlignments read FAlign write FAlign;
    property Name: TVXGuiElementName read FName write SetName;
  end;

  TVXGuiComponent = class;

  TVXGuiElementList = class(TOwnedCollection)
  private
    FGuiComponent: TVXGuiComponent;
  protected
    procedure SetItems(index: Integer; const val: TVXGuiElement);
    function GetItems(index: Integer): TVXGuiElement;
  public
    constructor Create(AOwner: TVXGuiComponent);
    procedure AssignTo(Dest: TPersistent); override;

    function GetOwner: TPersistent; override;
    function IndexOf(const Item: TVXGuiElement): Integer;
    property Items[index: Integer]: TVXGuiElement read GetItems write SetItems;
      default;
  end;

  TVXGuiComponentName = string;

  TVXGuiComponentList = class;
  TVXGuiComponent = class(TCollectionItem)
  private
    FElements: TVXGuiElementList;
    FName: TVXGuiComponentName;
  protected
    function GetDisplayName: string; override;
    procedure SetName(const val: TVXGuiComponentName);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure RenderToArea(X1, Y1, X2, Y2: GLfloat; var Res: TGUIDrawResult;
      Refresh: Boolean = True; Scale: GLfloat = 1);
    function GetOwnerList: TVXGuiComponentList;
    property Owner: TVXGuiComponentList read GetOwnerList;
  published
    property Elements: TVXGuiElementList read FElements write FElements;
    property Name: TVXGuiComponentName read FName write SetName;
  end;

  TVXGuiLayout = class;
  TVXGuiComponentList = class(TOwnedCollection)
  private
    FLayout: TVXGuiLayout;
  protected
    procedure SetItems(index: Integer; const val: TVXGuiComponent);
    function GetItems(index: Integer): TVXGuiComponent;
  public
    constructor Create(AOwner: TVXGuiLayout);

    function GetOwner: TPersistent; override;
    function FindItem(name: TVXGuiComponentName): TVXGuiComponent;
    property Items[index: Integer]: TVXGuiComponent read GetItems write
      SetItems; default;
  end;

  TVXGuiLayout = class(TVXUpdateableComponent)
  private
    FBitmapFont: TVXCustomBitmapFont;
    FMaterial: TVXMaterial;
    FGuiComponents: TVXGuiComponentList;
    FFileName: string;
    FGuiComponentList: TList;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

    procedure SetFileName(newName: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(FN: string);

    procedure Clear;

    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(FN: string);
    procedure AddGuiComponent(Component: TVXUpdateableComponent);
    procedure RemoveGuiComponent(Component: TVXUpdateableComponent);

    procedure NotifyChange(Sender: TObject); override;
  published
    property BitmapFont: TVXCustomBitmapFont read FBitmapFont write FBitmapFont;
    property Material: TVXMaterial read FMaterial write FMaterial;
    property GuiComponents: TVXGuiComponentList read FGuiComponents write
      FGuiComponents;
    property FileName: string read FFileName write SetFileName;
  end;

const
  GuiNullRect: TGUIRect = (X1: 0.0; Y1: 0.0; X2: 0.0; Y2: 0.0; XTiles: 0.0;
    YTiles: 0.0);

function IsInRect(const R: TGUIRect; X, Y: Single): Boolean;

implementation

function IsInRect(const R: TGUIRect; X, Y: Single): Boolean;

begin
  Result := (R.X1 <= X) and (R.X2 >= X) and (R.Y1 <= Y) and (R.Y2 >= Y);
end;

// ------------------
// ------------------ TVXBaseGuiObject ------------------
// ------------------

// Create
//

constructor TVXBaseGuiObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRecursiveVisible := Visible;
end;

// SetLeft
//

procedure TVXBaseGuiObject.SetLeft(const Value: GLfloat);
var
  NewPosX: GLfloat;
  i: integer;
  Diff: GLfloat;
begin
  if Assigned(Parent) and (Parent is TVXBaseGuiObject) then
    NewPosX := (Parent as TVXBaseGuiObject).Position.X + Value
  else
    NewPosX := Value;

  if Position.X <> NewPosX then
  begin
    Diff := NewPosX - Position.X;
    Position.X := NewPosX;

    for i := 0 to Count - 1 do
      if Children[i] is TVXBaseGuiObject then
      begin
        (Children[i] as TVXBaseGuiObject).Left := (Children[i] as
          TVXBaseGuiObject).Left + Diff;
      end;
  end;
end;

// GetLeft
//

function TVXBaseGuiObject.GetLeft: GLfloat;
begin
  if Assigned(Parent) and (Parent is TVXBaseGuiObject) then
    Result := Position.X - (Parent as TVXBaseGuiObject).Position.X
  else
    Result := Position.X;
end;

// SetTop
//

procedure TVXBaseGuiObject.SetTop(const Value: GLfloat);
var
  NewPosY: GLfloat;
  i: integer;
  Diff: GLfloat;
begin
  if Assigned(Parent) and (Parent is TVXBaseGuiObject) then
    NewPosY := (Parent as TVXBaseGuiObject).Position.Y + Value
  else
    NewPosY := Value;

  if Position.Y <> NewPosY then
  begin
    Diff := NewPosY - Position.Y;
    Position.Y := NewPosY;

    for i := 0 to Count - 1 do
      if Children[i] is TVXBaseGuiObject then
      begin
        (Children[i] as TVXBaseGuiObject).Top := (Children[i] as
          TVXBaseGuiObject).Top + Diff;
      end;
  end;
end;

// GetTop
//

function TVXBaseGuiObject.GetTop: GLfloat;
begin
  if Assigned(Parent) and (Parent is TVXBaseGuiObject) then
    Result := Position.Y - (Parent as TVXBaseGuiObject).Position.Y
  else
    Result := Position.Y;
end;

// SetWidth
//

procedure TVXBaseGuiObject.SetWidth(const val: GLfloat);
begin
  if FWidth <> val then
  begin
    FWidth := val;
    NotifyChange(Self);
  end;
end;

// SetHeight
//

procedure TVXBaseGuiObject.SetHeight(const val: GLfloat);
begin
  if FHeight <> val then
  begin
    FHeight := val;
    NotifyChange(Self);
  end;
end;

// NotifyHide
//

procedure TVXBaseGuiObject.NotifyHide;
var
  child: TVXBaseSceneObject;
  xc: Integer;
begin
  if RecursiveVisible then
  begin
    FRecursiveVisible := False;
    for xc := 0 to Count - 1 do
    begin
      child := Children[xc];
      if child is TVXBaseGuiObject then
        TVXBaseGuiObject(child).NotifyHide;
    end;
  end;
end;

// NotifyShow
//

procedure TVXBaseGuiObject.NotifyShow;
var
  child: TVXBaseSceneObject;
  xc: Integer;
begin
  if not RecursiveVisible then
  begin
    FRecursiveVisible := True;
    for xc := 0 to Count - 1 do
    begin
      child := Children[xc];
      if child is TVXBaseGuiObject then
        TVXBaseGuiObject(child).NotifyShow;
    end;
  end;
end;

// AddChild
//

procedure TVXBaseGuiObject.AddChild(aChild: TVXBaseSceneObject);
begin
  inherited;
  if AChild is TVXBaseGuiObject then
  begin
    if RecursiveVisible then
      TVXBaseGuiObject(AChild).NotifyShow
    else
      TVXBaseGuiObject(AChild).NotifyHide;
  end;
end;

// Insert
//

procedure TVXBaseGuiObject.Insert(aIndex: Integer; aChild: TVXBaseSceneObject);
begin
  inherited;
  if AChild is TVXBaseGuiObject then
  begin
    if RecursiveVisible then
      TVXBaseGuiObject(AChild).NotifyShow
    else
      TVXBaseGuiObject(AChild).NotifyHide;
  end;
end;

// SetVisible
//

procedure TVXBaseGuiObject.SetVisible(aValue: Boolean);
begin
  if Visible <> aValue then
  begin
    inherited SetVisible(aValue);
    if aValue then
    begin
      if Parent <> nil then
        if Parent is TVXBaseGuiObject then
        begin
          if TVXBaseGuiObject(Parent).RecursiveVisible then
            NotifyShow;
        end
        else
        begin
          if Parent.Visible then
            NotifyShow;
        end;
    end
    else
    begin
      if RecursiveVisible then
        NotifyHide;
    end;
  end;
end;

constructor TVXGuiLayout.Create(AOwner: TComponent);
begin
  FGuiComponentList := TList.Create;
  inherited;
  FGuiComponents := TVXGuiComponentList.Create(Self);
  FMaterial := TVXMaterial.Create(Self);
end;

destructor TVXGuiLayout.Destroy;
begin
  Clear;
  FMaterial.Free;
  FGuiComponents.Free;
  inherited;
  FGuiComponentList.Free;
end;

procedure TVXGuiLayout.SetFileName(newName: string);
begin
  if newName <> FFileName then
  begin
    FFileName := newName;
    if FileExists(FFileName) then
    begin
      Clear;
      loadFromFile(FFileName);
    end;
  end;
end;

procedure TVXGuiLayout.LoadFromFile(FN: string);
var
  Stream: TMemoryStream;

begin
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(FN);
    LoadFromStream(stream);
    FFileName := FN;
  finally
    stream.Free;
  end;
end;

procedure TVXGuiLayout.SaveToFile(FN: string);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    SaveToStream(Stream);
    Stream.SaveToFile(FN);
    FFileName := FN;
  finally
    Stream.Free;
  end;
end;

procedure TVXGuiLayout.AddGuiComponent(Component: TVXUpdateableComponent);
begin
  if FGuiComponentList.IndexOf(Component) < 0 then
  begin
    FreeNotification(Component);
    FGuiComponentList.Add(Component);
  end;
end;

procedure TVXGuiLayout.RemoveGuiComponent(Component: TVXUpdateableComponent);
begin
  FGuiComponentList.Remove(Component);
  RemoveFreeNotification(Component);
end;

procedure TVXGuiLayout.Assign(Source: TPersistent);
var
  LLayout: TVXGuiLayout;
  LComponent: TVXGuiComponent;
  I: Integer;
begin
  if Source is TVXGuiLayout then
  begin
    LLayout := TVXGuiLayout(Source);
    FBitmapFont := LLayout.FBitmapFont;
    FMaterial.Assign(LLayout.Material);
    FFileName := LLayout.FFileName;
    Clear;
    for I := 0 to LLayout.FGuiComponents.Count - 1 do
    begin
      LComponent := TVXGuiComponent(FGuiComponents.Add);
      LLayout.FGuiComponents[I].AssignTo(LComponent);
      LComponent.Name := LLayout.FGuiComponents[I].Name;
    end;
    for I := 0 to FGuiComponentList.Count - 1 do
      TVXUpdateAbleComponent(FGuiComponentList[I]).RemoveFreeNotification(Self);
    FGuiComponentList.Assign(LLayout.FGuiComponentList);
    for I := 0 to FGuiComponentList.Count - 1 do
      TVXUpdateAbleComponent(FGuiComponentList[I]).FreeNotification(Self);
  end
  else
    inherited; // Assign Error
end;

procedure TVXGuiLayout.Clear;
var
  XC: Integer;

begin
  for XC := FGuiComponents.Count - 1 downto 0 do
  begin
    FGuiComponents.Delete(XC);
  end;
  NotifyChange(Self);
end;

procedure TVXGuiLayout.NotifyChange(Sender: TObject);
var
  XC: Integer;
begin
  inherited;

  for XC := FGuiComponentList.Count - 1 downto 0 do
    TVXUpdateAbleComponent(FGuiComponentList[XC]).NotifyChange(Self);
end;

procedure TVXGuiLayout.LoadFromStream(Stream: TStream);

var
  TmpComponent: TVXGuiComponent;
  XC, YC, ZC: Integer;
  TmpElement: TVXGuiElement;
  TmpAlignment: TGUIAlignments;
  Version: Integer;
  Data: TBinaryReader;

begin
  Data := TBinaryReader.Create(Stream);
  try

    Version := Data.ReadInteger;
    if Version <> 1 then
      Exit;

    for XC := 0 to Data.ReadInteger - 1 do
    begin
      TmpComponent := FGuiComponents.Add as TVXGuiComponent;
      TmpComponent.FName := Data.ReadString;
      for YC := 0 to Data.ReadInteger - 1 do
      begin
        TmpElement := TmpComponent.FElements.add as TVXGuiElement;

        TmpElement.FName := Data.ReadString;

        TmpElement.FTopLeft.X := Data.ReadFloat;
        TmpElement.FTopLeft.Y := Data.ReadFloat;
        TmpElement.FTopLeft.Z := Data.ReadFloat;

        TmpElement.FBottomRight.X := Data.ReadFloat;
        TmpElement.FBottomRight.Y := Data.ReadFloat;
        TmpElement.FBottomRight.Z := Data.ReadFloat;

        TmpElement.FScale.X := Data.ReadFloat;
        TmpElement.FScale.Y := Data.ReadFloat;
        TmpElement.FScale.Z := Data.ReadFloat;

        for ZC := 0 to Data.ReadInteger - 1 do
        begin
          TmpAlignment := TGUIAlignments(Data.ReadInteger);
          TmpElement.FAlign := TmpAlignment;
        end;
      end;
    end;
  finally
    Data.Free;
  end;
  NotifyChange(Self);
end;

procedure TVXGuiLayout.SaveToStream(stream: TStream);
var
  TmpComponent: TVXGuiComponent;
  Alignments, XC, YC: Integer;
  TmpElement: TVXGuiElement;
  TmpAlignment: TGUIAlignments;
  Data: TBinaryWriter;

begin
  Data := TBinaryWriter.Create(Stream);
  try
    Data.WriteInteger(1);
    Data.WriteInteger(FGuiComponents.Count);
    for XC := 0 to FGuiComponents.Count - 1 do
    begin
      TmpComponent := FGuiComponents.Items[XC];
      Data.WriteString(TmpComponent.FName);

      Data.WriteInteger(TmpComponent.FElements.Count);

      for YC := 0 to TmpComponent.FElements.Count - 1 do
      begin
        TmpElement := TmpComponent.FElements.Items[YC];

        Data.WriteString(TmpElement.FName);

        Data.WriteFloat(TmpElement.FTopLeft.X);
        Data.WriteFloat(TmpElement.FTopLeft.Y);
        Data.WriteFloat(TmpElement.FTopLeft.Z);

        Data.WriteFloat(TmpElement.FBottomRight.X);
        Data.WriteFloat(TmpElement.FBottomRight.Y);
        Data.WriteFloat(TmpElement.FBottomRight.Z);

        Data.WriteFloat(TmpElement.FScale.X);
        Data.WriteFloat(TmpElement.FScale.Y);
        Data.WriteFloat(TmpElement.FScale.Z);

        Alignments := 0;
        for TmpAlignMent := GLAlTopLeft to GLAlBorder do
        begin
          if TmpAlignMent = TmpElement.FAlign then
            inc(Alignments);
        end;

        Data.WriteInteger(Alignments);

        for TmpAlignMent := GLAlTopLeft to GLAlBorder do
        begin
          if TmpAlignMent = TmpElement.FAlign then
            Data.WriteInteger(Integer(TmpAlignMent));
        end;
      end;
    end;
  finally
    Data.Free;
  end;
end;

constructor TVXGuiComponentList.Create(AOwner: TVXGuiLayout);
begin
  inherited Create(AOwner, TVXGuiComponent);
  FLayout := AOwner;
end;

function TVXGuiComponentList.GetOwner: TPersistent;
begin
  Result := FLayout;
end;

procedure TVXGuiComponentList.SetItems(index: Integer; const val:
  TVXGuiComponent);
begin
  inherited Items[index] := val;
end;

function TVXGuiComponentList.FindItem(name: TVXGuiComponentName):
  TVXGuiComponent;
var
  XC: Integer;
  gc: TVXGuiComponent;
begin
  Result := nil;
  if Name = '' then
    Exit;
  for XC := 0 to Count - 1 do
  begin
    gc := Items[xc];
    if gc.FName = Name then
    begin
      Result := gc;
      Break;
    end;
  end;
end;

function TVXGuiComponentList.GetItems(index: Integer): TVXGuiComponent;
begin
  Result := TVXGuiComponent(inherited Items[index]);
end;

procedure TVXGuiComponent.RenderToArea(X1, Y1, X2, Y2: GLfloat; var Res:
  TGUIDrawResult; Refresh: Boolean = True; Scale: GLfloat = 1);
var
  XC: Integer;
  ThisElement: TVXGuiElement;
  W, H: GLfloat;
  Len1, Len2: GLfloat;
  Layout: TVXGuiLayout;
  LibMaterial: TVXLibMaterial;
  Material: TVXMaterial;
  TexWidth,
    TexHeight: GLfloat;
  AlignCount: TGUIAlignments;

  procedure Prepare;
  begin
    Len1 := (ThisElement.FTopLeft.x - ThisElement.FBottomRight.x) *
      ThisElement.Scale.X * Scale;
    Len2 := (ThisElement.FTopLeft.y - ThisElement.FBottomRight.y) *
      ThisElement.Scale.Y * Scale;
    if Len1 < 0 then
    begin
      if Len2 < 0 then
      begin
        W := -Len1;
        H := -Len2;
      end
      else
      begin
        W := -Len1;
        H := Len2;
      end;
    end
    else
    begin
      if Len2 < 0 then
      begin
        W := Len1;
        H := -Len2;
      end
      else
      begin
        W := Len1;
        H := Len2;
      end;
    end;
  end;

  procedure RenderIt(var ARect: TGuiRect; AElement: TVXGuiElement);
  var
    XC: GLfloat;
    YC: GLfloat;
    XPos, X2Pos: GLfloat;
    YPos, y2Pos: GLfloat;
    tx1, ty1, tx2, ty2: GLfloat;
    XTileSize, YTileSize: GLfloat;
    tx3, ty3: GLfloat;
    tx, ty: GLfloat;

  begin
    if (ARect.XTiles = 1) and (ARect.YTiles = 1) then
    begin
      glTexCoord2f(AElement.TopLeft.X / TexWidth, -AElement.TopLeft.Y /
        TexHeight);
      glVertex2f(ARect.X1, -ARect.Y1);

      glTexCoord2f(AElement.TopLeft.X / TexWidth, -AElement.BottomRight.Y /
        TexHeight);
      glVertex2f(ARect.X1, -ARect.Y2);

      glTexCoord2f(AElement.BottomRight.X / TexWidth, -AElement.BottomRight.Y /
        TexHeight);
      glVertex2f(ARect.X2, -ARect.Y2);

      glTexCoord2f(AElement.BottomRight.X / TexWidth, -AElement.TopLeft.Y /
        TexHeight);
      glVertex2f(ARect.X2, -ARect.Y1);
    end
    else
    begin
      XTileSize := (ARect.X2 - ARect.X1) / ARect.XTiles;
      YTileSize := (ARect.Y2 - ARect.Y1) / ARect.YTiles;
      tx1 := AElement.TopLeft.X / TexWidth;
      ty1 := -AElement.TopLeft.Y / TexHeight;
      tx2 := AElement.BottomRight.X / TexWidth;
      ty2 := -AElement.BottomRight.Y / TexHeight;
      tx3 := (AElement.TopLeft.X + (AElement.BottomRight.X - AElement.TopLeft.X)
        * Frac(ARect.XTiles)) / TexWidth;
      ty3 := -(AElement.TopLeft.y + (AElement.BottomRight.y - AElement.TopLeft.y)
        * Frac(ARect.yTiles)) / TexHeight;

      XC := ARect.XTiles;
      XPos := ARect.X1;
      tx := tx2;
      while XC > 0 do
      begin
        YC := ARect.YTiles;
        YPos := ARect.Y1;
        ty := ty2;

        if XC >= 1 then
          X2Pos := XPos + XTileSize
        else
        begin
          X2Pos := ARect.X2;
          tx := tx3;
        end;

        while YC > 0 do
        begin
          if YC >= 1 then
            Y2Pos := YPos + YTileSize
          else
          begin
            Y2Pos := ARect.Y2;
            ty := ty3;
          end;

          glTexCoord2f(tx1, ty1);
          glVertex2f(XPos, -YPos);

          glTexCoord2f(tx1, ty);
          glVertex2f(XPos, -Y2Pos);

          glTexCoord2f(tx, ty);
          glVertex2f(X2Pos, -Y2Pos);

          glTexCoord2f(tx, ty1);
          glVertex2f(X2Pos, -YPos);
          yc := yc - 1.0;
          ypos := Y2Pos;
        end;
        xc := xc - 1.0;
        xpos := X2Pos;
      end;
    end;
  end;

  procedure RenderBorder(AElement: TVXGuiElement);
  var
    TmpElement: TVXGuiElement;

  begin
    TmpElement := TVXGuiElement.Create(nil);
    TmpElement.FTopLeft.X := ThisElement.FTopLeft.X;
    TmpElement.FTopLeft.Y := ThisElement.FTopLeft.Y;
    TmpElement.FBottomRight.X := ThisElement.FTopLeft.X + ThisElement.Scale.X;
    TmpElement.FBottomRight.Y := ThisElement.FTopLeft.Y + ThisElement.Scale.Y;
    TmpElement.Scale.SetPoint2D(1, 1);
    RenderIt(Res[GLALTopLeft], TmpElement);

    TmpElement.FTopLeft.X := ThisElement.FTopLeft.X + ThisElement.Scale.X;
    TmpElement.FBottomRight.X := ThisElement.FBottomRight.X -
      ThisElement.Scale.X;
    RenderIt(Res[GLALTop], TmpElement);

    TmpElement.FTopLeft.X := ThisElement.FBottomRight.X - ThisElement.Scale.X;
    TmpElement.FBottomRight.X := ThisElement.FBottomRight.X;
    RenderIt(Res[GLALTopRight], TmpElement);

    TmpElement.FTopLeft.Y := ThisElement.FTopLeft.Y + ThisElement.Scale.Y;
    TmpElement.FBottomRight.Y := ThisElement.FBottomRight.Y -
      ThisElement.Scale.Y;
    RenderIt(Res[GLALRight], TmpElement);

    TmpElement.FTopLeft.X := ThisElement.FBottomRight.X - ThisElement.Scale.X;
    TmpElement.FTopLeft.Y := ThisElement.FBottomRight.Y - ThisElement.Scale.Y;
    TmpElement.FBottomRight.X := ThisElement.FBottomRight.X;
    TmpElement.FBottomRight.Y := ThisElement.FBottomRight.Y;
    RenderIt(Res[GLALBottomRight], TmpElement);

    TmpElement.FTopLeft.X := ThisElement.FTopLeft.X + ThisElement.Scale.X;
    TmpElement.FTopLeft.Y := ThisElement.FBottomRight.Y - ThisElement.Scale.Y;
    TmpElement.FBottomRight.X := ThisElement.FBottomRight.X -
      ThisElement.Scale.X;
    TmpElement.FBottomRight.Y := ThisElement.FBottomRight.Y;
    RenderIt(Res[GLALBottom], TmpElement);

    TmpElement.FTopLeft.X := ThisElement.FTopLeft.X;
    TmpElement.FTopLeft.Y := ThisElement.FBottomRight.Y - ThisElement.Scale.Y;
    TmpElement.FBottomRight.X := ThisElement.FTopLeft.X + ThisElement.Scale.X;
    TmpElement.FBottomRight.Y := ThisElement.FBottomRight.Y;
    RenderIt(Res[GLALBottomLeft], TmpElement);

    TmpElement.FTopLeft.X := ThisElement.FTopLeft.X;
    TmpElement.FTopLeft.Y := ThisElement.FTopLeft.Y + ThisElement.Scale.Y;
    TmpElement.FBottomRight.X := ThisElement.FTopLeft.X + ThisElement.Scale.X;
    TmpElement.FBottomRight.Y := ThisElement.FBottomRight.Y -
      ThisElement.Scale.Y;
    RenderIt(Res[GLALLeft], TmpElement);

    TmpElement.FTopLeft.X := ThisElement.FTopLeft.X + ThisElement.Scale.X;
    TmpElement.FTopLeft.Y := ThisElement.FTopLeft.Y + ThisElement.Scale.Y;
    TmpElement.FBottomRight.X := ThisElement.FBottomRight.X -
      ThisElement.Scale.X;
    TmpElement.FBottomRight.Y := ThisElement.FBottomRight.Y -
      ThisElement.Scale.Y;
    RenderIt(Res[GLALCenter], TmpElement);
  end;

begin
  Layout := ((GetOwner as TVXGuiComponentList).GetOwner as TVXGuiLayout);
  Material := nil;
  if Assigned(Layout.Material.MaterialLibrary)
    and (Layout.Material.MaterialLibrary is TVXMaterialLibrary)
    and (Layout.Material.LibMaterialName <> '') then
  begin
    LibMaterial :=
      TVXMaterialLibrary(Layout.Material.MaterialLibrary).Materials.GetLibMaterialByName(Layout.Material.LibMaterialName);
    if Assigned(LibMaterial) then
      Material := LibMaterial.Material;
  end;
  if not Assigned(Material) then
  begin
    Material := Layout.Material;
  end;

  if Refresh then
  begin
    Res[GLALtopLeft].X1 := X1;
    Res[GLALtopLeft].Y1 := Y1;
    Res[GLALtopLeft].X2 := X1;
    Res[GLALtopLeft].Y2 := Y1;

    Res[GLALtopRight].X1 := X2;
    Res[GLALtopRight].Y1 := Y1;
    Res[GLALtopRight].X2 := X2;
    Res[GLALtopRight].Y2 := Y1;

    Res[GLALBottomLeft].X1 := X1;
    Res[GLALBottomLeft].Y1 := Y2;
    Res[GLALBottomLeft].X2 := X1;
    Res[GLALBottomLeft].Y2 := Y2;

    Res[GLALBottomRight].X1 := X2;
    Res[GLALBottomRight].Y1 := Y2;
    Res[GLALBottomRight].X2 := X2;
    Res[GLALBottomRight].Y2 := Y2;

    for XC := 0 to FElements.Count - 1 do
    begin
      ThisElement := FElements[XC];
      if GLAlBorder = ThisElement.Align then
      begin
        Res[GLALtopLeft].X1 := X1;
        Res[GLALtopLeft].Y1 := Y1;
        Res[GLALtopLeft].X2 := X1 + ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALtopLeft].Y2 := Y1 + ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;

        Res[GLALtop].X1 := X1 + ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALtop].Y1 := Y1;
        Res[GLALtop].X2 := X2 - ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALtop].Y2 := Y1 + ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;

        Res[GLALtopRight].X1 := X2 - ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALtopRight].Y1 := Y1;
        Res[GLALtopRight].X2 := X2;
        Res[GLALtopRight].Y2 := Y1 + ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;

        Res[GLALRight].X1 := X2 - ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALRight].Y1 := Y1 + ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;
        Res[GLALRight].X2 := X2;
        Res[GLALRight].Y2 := Y2 - ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;

        Res[GLALBottomRight].X1 := X2 - ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALBottomRight].Y1 := Y2 - ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;
        Res[GLALBottomRight].X2 := X2;
        Res[GLALBottomRight].Y2 := Y2;

        Res[GLALBottom].X1 := X1 + ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALBottom].Y1 := Y2 - ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;
        Res[GLALBottom].X2 := X2 - ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALBottom].Y2 := Y2;

        Res[GLALBottomLeft].X1 := X1;
        Res[GLALBottomLeft].Y1 := Y2 - ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;
        Res[GLALBottomLeft].X2 := X1 + ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALBottomLeft].Y2 := Y2;

        Res[GLALLeft].X1 := X1;
        Res[GLALLeft].Y1 := Y1 + ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;
        Res[GLALLeft].X2 := X1 + ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALLeft].Y2 := Y2 - ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;

        Res[GLALCenter].X1 := X1 + ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALCenter].Y1 := Y1 + ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;
        Res[GLALCenter].X2 := X2 - ThisElement.Scale.X * Scale *
          ThisElement.Scale.Z;
        Res[GLALCenter].Y2 := Y2 - ThisElement.Scale.Y * Scale *
          ThisElement.Scale.Z;
      end;

      if GLALtopLeft = ThisElement.Align then
      begin
        Prepare;
        Res[GLALtopLeft].X1 := X1;
        Res[GLALtopLeft].Y1 := Y1;
        Res[GLALtopLeft].X2 := X1 + W;
        Res[GLALtopLeft].Y2 := Y1 + H;
      end;
      if GLALtopRight = ThisElement.Align then
      begin
        Prepare;
        Res[GLALtopRight].X1 := X2 - W;
        Res[GLALtopRight].Y1 := Y1;
        Res[GLALtopRight].X2 := X2;
        Res[GLALtopRight].Y2 := Y1 + H;
      end;
      if GLALBottomLeft = ThisElement.Align then
      begin
        Prepare;
        Res[GLALBottomLeft].X1 := X1;
        Res[GLALBottomLeft].Y1 := Y2 - H;
        Res[GLALBottomLeft].X2 := X1 + W;
        Res[GLALBottomLeft].Y2 := Y2;
      end;
      if GLALBottomRight = ThisElement.Align then
      begin
        Prepare;
        Res[GLALBottomRight].X1 := X2 - W;
        Res[GLALBottomRight].Y1 := Y2 - H;
        Res[GLALBottomRight].X2 := X2;
        Res[GLALBottomRight].Y2 := Y2;
      end;
    end;

    Res[GLALtop].X1 := Res[GLALtopLeft].X2;
    Res[GLALtop].Y1 := Res[GLALtopRight].Y1;
    Res[GLALtop].X2 := Res[GLALtopRight].X1;
    Res[GLALtop].Y2 := Res[GLALtopLeft].Y2;

    Res[GLALBottom].X1 := Res[GLALBottomLeft].X2;
    Res[GLALBottom].Y1 := Res[GLALBottomLeft].Y1;
    Res[GLALBottom].X2 := Res[GLALBottomRight].X1;
    Res[GLALBottom].Y2 := Res[GLALBottomRight].Y2;

    Res[GLALLeft].X1 := Res[GLALtopLeft].X1;
    Res[GLALLeft].Y1 := Res[GLALtopLeft].Y2;
    Res[GLALLeft].X2 := Res[GLALBottomLeft].X2;
    Res[GLALLeft].Y2 := Res[GLALBottomLeft].Y1;

    Res[GLALRight].X1 := Res[GLALtopRight].X1;
    Res[GLALRight].Y1 := Res[GLALtopRight].Y2;
    Res[GLALRight].X2 := Res[GLALBottomRight].X2;
    Res[GLALRight].Y2 := Res[GLALBottomRight].Y1;

    for XC := 0 to FElements.Count - 1 do
    begin
      ThisElement := FElements[XC];
      if GLALtop = ThisElement.Align then
      begin
        Prepare;
        Res[GLALtop].Y1 := Y1;
        Res[GLALtop].Y2 := Y1 + H;
      end;
      if GLALBottom = ThisElement.Align then
      begin
        Prepare;
        Res[GLALBottom].Y1 := Y2 - H;
        Res[GLALBottom].Y2 := Y2;
      end;
      if GLALLeft = ThisElement.Align then
      begin
        Prepare;
        Res[GLALLeft].X1 := X1;
        Res[GLALLeft].X2 := X1 + W;
      end;
      if GLALRight = ThisElement.Align then
      begin
        Prepare;
        Res[GLALRight].X1 := X2 - W;
        Res[GLALRight].X2 := X2;
      end;
    end;

    Res[GLALCenter].X1 := Res[GLALLeft].X2;
    Res[GLALCenter].Y1 := Res[GLALtop].Y2;
    Res[GLALCenter].X2 := Res[GLALRight].X1;
    Res[GLALCenter].Y2 := Res[GLALBottom].Y1;
  end;

  TexWidth := Material.Texture.TexWidth;
  if TexWidth = 0 then
    TexWidth := Material.Texture.Image.Width;

  TexHeight := Material.Texture.TexHeight;
  if TexHeight = 0 then
    TexHeight := Material.Texture.Image.Height;

  glBegin(GL_QUADS);

  for XC := 0 to FElements.Count - 1 do
  begin
    ThisElement := FElements[XC];
    for AlignCount := GLAlTopLeft to GLAlBottomRight do
      if (AlignCount = ThisElement.Align) then
      begin
        if Refresh then
        begin
          Res[AlignCount].XTiles := ((Res[AlignCount].X2 - Res[AlignCount].X1) /
            (ThisElement.FBottomRight.X - ThisElement.FTopLeft.X)) /
            ThisElement.Scale.X;
          Res[AlignCount].YTiles := ((Res[AlignCount].Y2 - Res[AlignCount].Y1) /
            (ThisElement.FBottomRight.Y - ThisElement.FTopLeft.Y)) /
            ThisElement.Scale.Y;
        end;
        RenderIt(Res[AlignCount], ThisElement);
      end;
    if (GLALBorder = ThisElement.Align) then
    begin
      RenderBorder(ThisElement);
    end;

  end;
  glEnd;
end;

function TVXGuiComponent.GetOwnerList: TVXGuiComponentList;
begin
  Result := GetOwner as TVXGuiComponentList;
end;

function TVXGuiComponent.GetDisplayName: string;
begin
  Result := FName;
end;

procedure TVXGuiComponent.SetName(const val: TVXGuiComponentName);
begin
  FName := Val;
end;

constructor TVXGuiComponent.Create(Collection: TCollection);
begin
  inherited;
  FElements := TVXGuiElementList.Create(Self);
end;

destructor TVXGuiComponent.Destroy;
begin
  FElements.Free;
  inherited;
end;

constructor TVXGuiElementList.Create(AOwner: TVXGuiComponent);
begin
  inherited Create(AOwner, TVXGuiElement);
  FGuiComponent := AOwner;
end;

function TVXGuiElementList.GetOwner: TPersistent;
begin
  Result := FGuiComponent;
end;

procedure TVXGuiElementList.SetItems(index: Integer; const val: TVXGuiElement);
begin
  inherited Items[index] := val;
end;

function TVXGuiElementList.IndexOf(const Item: TVXGuiElement): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Count <> 0 then
    for I := 0 to Count - 1 do
      if GetItems(I) = Item then
      begin
        Result := I;
        Exit;
      end;
end;

function TVXGuiElementList.GetItems(index: Integer): TVXGuiElement;
begin
  Result := TVXGuiElement(inherited Items[index]);
end;

function TVXGuiElement.GetDisplayName: string;
begin
  Result := FName;
end;

procedure TVXGuiElement.SetName(const val: TVXGuiElementName);
begin
  FName := Val;
end;

constructor TVXGuiElement.Create(Collection: TCollection);
begin
  inherited;
  FTopLeft := TVXCoordinates2.CreateInitialized(Self, NullHmgVector, csPoint2D);
  FBottomRight := TVXCoordinates2.CreateInitialized(Self, NullHmgVector,
    csPoint2D);
  FScale := TVXCoordinates2.CreateInitialized(Self, XYHmgVector, csPoint2D);
end;

destructor TVXGuiElement.Destroy;
begin
  FTopLeft.Free;
  FBottomRight.Free;
  FScale.Free;
  inherited;
end;

procedure TVXGuiLayout.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FBitmapFont then
      BitmapFont := nil
    else
      FGuiComponentList.Remove(AComponent);
  end;
  NotifyChange(Self); // EG : looks suspicious...
  inherited;
end;

procedure TVXGuiComponent.AssignTo(Dest: TPersistent);
begin
  if Dest is TVXGuiComponent then
  begin
    TVXGuiComponent(Dest).Elements.Assign(Elements);
  end
  else
    inherited;
end;

procedure TVXGuiElementList.AssignTo(Dest: TPersistent);
var
  i: Integer;
begin
  if Dest is TVXGuiElementList then
  begin
    for i := 0 to Count - 1 do
    begin
      TVXGuiElementList(Dest).Add.Assign(Items[i]);
    end;
  end
  else
    inherited;
end;

procedure TVXGuiElement.AssignTo(Dest: TPersistent);
var
  element: TVXGuiElement;
begin
  if Dest is TVXGuiElement then
  begin
    element := TVXGuiElement(Dest);

    element.TopLeft.Assign(TopLeft);
    element.BottomRight.Assign(BottomRight);
    element.Scale.Assign(Scale);
    element.Align := Align;
    element.Name := Name;
  end
  else
    inherited;
end;

end.

