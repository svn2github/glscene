//
// GLScene project based on GLScene library, http://glscene.sourceforge.net
//
{ 
  Invisible component for helping to Move, Rotate and Scale an Object
  under GLScene (usefull for an Editor).
}
unit GLS.Gizmo;

interface

{$I GLScene.inc}

uses
  System.Classes, System.SysUtils,
  //GLS
  GLS.Scene, GLS.Color, GLS.Objects, GLS.VectorGeometry, GLS.Material, GLS.Strings,
  GLS.GeomObjects, GLS.BitmapFont, GLS.SceneViewer, GLS.VectorFileObjects,
  GLS.CrossPlatform, GLS.Coordinates, GLS.RenderContextInfo, GLS.State,
  GLS.Selection, GLS.VectorTypes;

type
  TVKGizmoUndoCollection = class;
  TVKGizmo = class;

  TVKGizmoUndoItem = class(TCollectionItem)
  private
    FOldLibMaterialName: string;
    FOldAutoScaling: TVKCoordinates;
    FEffectedObject: TVKCustomSceneObject;
    FOldMatr: TMatrix;
    FOldMatrix: TMatrix;
    procedure SetEffectedObject(const Value: TVKCustomSceneObject);
    procedure SetOldAutoScaling(const Value: TVKCoordinates);
    procedure SetOldMatrix(const Value: TMatrix);
  protected
    procedure DoUndo; virtual;
    function GetParent: TVKGizmoUndoCollection;
    function GetGizmo: TVKGizmo;
  public
    constructor Create(AOwner: TCollection); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); virtual;
    procedure AssignFromObject(const AObject: TVKCustomSceneObject);

    // TODO: create a special type for Matrix.
    property OldMatrix: TMatrix read FOldMatrix write SetOldMatrix;
  published
    property EffectedObject: TVKCustomSceneObject read FEffectedObject
      write SetEffectedObject;
    property OldAutoScaling: TVKCoordinates read FOldAutoScaling
      write SetOldAutoScaling;
    property OldLibMaterialName: string read FOldLibMaterialName
      write FOldLibMaterialName;
  end;

  TVKGizmoUndoCollection = class(TOwnedCollection)
  private
    function GetItems(const Index: Integer): TVKGizmoUndoItem;
    procedure SetItems(const Index: Integer; const Value: TVKGizmoUndoItem);
  protected
    function GetParent: TVKGizmo;
  public
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    procedure RemoveByObject(const AObject: TVKCustomSceneObject);
    function Add: TVKGizmoUndoItem;
    property Items[const Index: Integer]: TVKGizmoUndoItem read GetItems
      write SetItems; default;
  end;

  TVKGizmoElement = (geMove, geRotate, geScale, geAxisLabel, geObjectInfos,
    geBoundingBox);
  TVKGizmoElements = set of TVKGizmoElement;

  TVKGizmoVisibleInfoLabel = (vliName, vliOperation, vliCoords);
  TVKGizmoVisibleInfoLabels = set of TVKGizmoVisibleInfoLabel;

  TVKGizmoAxis = (gaNone, gaX, gaY, gaZ, gaXY, gaXZ, gaYZ);

  TVKGizmoOperation = (gopMove, gopRotate, gopScale, gopNone, gpMoveGizmo,
    gpRotateGizmo);

  TVKGizmoAcceptEvent = procedure(Sender: TObject; var Obj: TVKBaseSceneObject;
    var Accept: Boolean; var Dimensions: TVector) of object;
  TVKGizmoUpdateEvent = procedure(Sender: TObject; Obj: TVKBaseSceneObject;
    Axis: TVKGizmoAxis; Operation: TVKGizmoOperation; var Vector: TVector)
    of object;

  TVKGizmoPickMode = (pmGetPickedObjects, pmRayCast);

  TVKGizmoRayCastHitData = class(TPersistent)
  public
    Obj: TVKBaseSceneObject;
    Point: TVector;
  end;

  TVKGizmoPickCube = class(TVKCube)
  end;

  TVKGizmoPickTorus = class(TVKTorus)
  end;

  TVKGizmo = class(TComponent)
  private
    _GZObaseGizmo: TVKBaseSceneObject;

    _GZOBoundingcube: TVKCube;

    _GZOrootHelpers: TVKBaseSceneObject;
    _GZOrootLines: TVKBaseSceneObject;
    _GZOrootTorus: TVKBaseSceneObject;
    _GZOrootCubes: TVKBaseSceneObject;
    _GZORootAxisLabel: TVKBaseSceneObject;
    _GZORootVisibleInfoLabels: TVKBaseSceneObject;

    _GZOlineX, _GZOlineY, _GZOlineZ, _GZOplaneXY, _GZOplaneXZ,
      _GZOplaneYZ: TVKLines; // For Move
    _GZOTorusX, _GZOTorusY, _GZOTorusZ: TVKGizmoPickTorus; // For Rotate
    _GZOCubeX, _GZOCubeY, _GZOCubeZ: TVKGizmoPickCube; // For Scale

    _GZOAxisLabelX, _GZOAxisLabelY, _GZOAxisLabelZ: TVKFlatText;
    _GZOVisibleInfoLabels: TVKFlatText;

    FRootGizmo: TVKBaseSceneObject;
    FSelectedObj: TVKBaseSceneObject;
    // FLastOperation,
    FOperation: TVKGizmoOperation;
    FSelAxis: TVKGizmoAxis;

    FBoundingBoxColor: TVKColor;
    FSelectedColor: TVKColor;
    FVisibleInfoLabelsColor: TVKColor;

    FBoundingBoxColorChanged: Boolean;
    FVisibleInfoLabelsColorChanged: Boolean;

    FForceOperation: Boolean;
    FForceAxis: Boolean;
    FForceUniformScale: Boolean;
    FAutoZoom: Boolean;
    FExcludeObjects: Boolean;
    FNoZWrite: Boolean;
    FEnabled: Boolean;

    FAutoZoomFactor: Single;
    FZoomFactor: Single;
    FMoveCoef: Single;
    FRotationCoef: Single;

    FViewer: TVKSceneViewer;

    FGizmoElements: TVKGizmoElements;
    FVisibleVisibleInfoLabels: TVKGizmoVisibleInfoLabels;

    FExcludeObjectsList: TStrings;

    Moving: Boolean;
    Mx, My: Integer;
    Rx, Ry: Integer;

    dglEnable, dglDisable, dgtEnable, dgtDisable, dgcEnable, dgcDisable,
      dglaEnable, dglaDisable, dgliEnable, dgliDisable: TVKDirectOpenGL;

    LastMousePos: TVector;
    ObjDimensions: TVector;

    FOnBeforeSelect: TVKGizmoAcceptEvent;
    FOnBeforeUpdate: TVKGizmoUpdateEvent;
    FOnSelectionLost: TNotifyEvent;
    FScaleCoef: Single;
    FGizmoThickness: Single;
    FPickMode: TVKGizmoPickMode;
    FInternalRaycastHitData: TList;

    FUndoHistory:  TVKGizmoUndoCollection;
    FLabelFont: TVKCustomBitmapFont;

    procedure SetRootGizmo(const AValue: TVKBaseSceneObject);

    procedure SetGizmoElements(const AValue: TVKGizmoElements);
    procedure SetVKGizmoVisibleInfoLabels(const AValue
      : TVKGizmoVisibleInfoLabels);
    procedure SetBoundingBoxColor(const AValue: TVKColor);
    procedure SetSelectedColor(const AValue: TVKColor);
    procedure SetVisibleInfoLabelsColor(const AValue: TVKColor);

    procedure SetExcludeObjectsList(const AValue: TStrings);

    procedure DirectGlDisable(Sender: TObject; var Rci: TVKRenderContextInfo);
    procedure DirectGlEnable(Sender: TObject; var Rci: TVKRenderContextInfo);

    function MouseWorldPos(const X, Y: Integer): TVector;
    function CheckObjectInExcludeList(const Obj: TVKBaseSceneObject): Boolean;
    procedure UpdateVisibleInfoLabels;
    procedure SetVKGizmoThickness(const Value: Single);

    function InternalGetPickedObjects(const X1, Y1, X2, Y2: Integer;
      const GuessCount: Integer = 8): TVKPickList;
    procedure ClearInternalRaycastHitData;
    procedure SetViewer(const Value: TVKSceneViewer);
    procedure SetLabelFont(const Value: TVKCustomBitmapFont);
    procedure SetSelectedObj(const Value: TVKBaseSceneObject);
  public
    PickableObjectsWithRayCast: TList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    procedure ViewerMouseMove(const X, Y: Integer);
    procedure ViewerMouseDown(const X, Y: Integer);
    procedure ViewerMouseUp(const X, Y: Integer);

    procedure UpdateGizmo; overload;
    procedure UpdateGizmo(const NewDimensions: TVector); overload;
    procedure SetVisible(const AValue: Boolean);
    function GetPickedObjectPoint(const Obj: TVKBaseSceneObject): TVector;

    procedure LooseSelection; virtual;

    procedure UndoAdd(const AObject: TVKCustomSceneObject);
    property RootGizmo: TVKBaseSceneObject read FRootGizmo write SetRootGizmo;

    // --------------------------------------------------------------------
  published

    property Viewer: TVKSceneViewer read FViewer write SetViewer;

    property GizmoElements: TVKGizmoElements read FGizmoElements
      write SetGizmoElements;

    property BoundingBoxColor: TVKColor read FBoundingBoxColor
      write SetBoundingBoxColor;
    property SelectedColor: TVKColor read FSelectedColor write SetSelectedColor;

    property SelAxis: TVKGizmoAxis read FSelAxis write FSelAxis;
    property ForceAxis: Boolean read FForceAxis write FForceAxis;

    property SelectedObj: TVKBaseSceneObject read FSelectedObj
      write SetSelectedObj;

    property Operation: TVKGizmoOperation read FOperation write FOperation;
    property ForceOperation: Boolean read FForceOperation write FForceoperation;
    property ForceUniformScale: Boolean read FForceUniformScale
      write FForceUniformScale;

    property ExcludeObjects: Boolean read FExcludeObjects write FExcludeObjects;
    property ExcludeObjectsList: TStrings read FExcludeObjectsList
      write SetExcludeObjectsList;

    property VisibleInfoLabels: TVKGizmoVisibleInfoLabels
      read FVisibleVisibleInfoLabels write SeTVKGizmoVisibleInfoLabels;
    property VisibleInfoLabelsColor: TVKColor read FVisibleInfoLabelsColor
      write SetVisibleInfoLabelsColor;

    property AutoZoom: Boolean read FAutoZoom write FAutoZoom;
    property AutoZoomFactor: Single read FAutoZoomFactor write FAutoZoomFactor;
    property ZoomFactor: Single read FZoomFactor write FZoomFactor;

    property MoveCoef: Single read FMoveCoef write FMoveCoef;
    property RotationCoef: Single read FRotationCoef write FRotationCoef;
    property ScaleCoef: Single read FScaleCoef write FScaleCoef;
    property NoZWrite: Boolean read FNoZWrite write FNoZWrite;

    property GizmoThickness: Single read FGizmoThickness
      write SetVKGizmoThickness;

    { Indicates whether the gizmo is enabled or not.
      WARNING: When loading/editing (possibly whenever a structureChanged
      call is made) a model, sometimes the gizmo will trigger a
      bug if the mouse is inside the glscene Viewer. To prevent that,
      remember to disable the gizmo before loading, then process windows
      messages (i.e. application.processMessage) and then enable the gizmo
      again. }

    { Warning Enable is ReadOnly property if you set to False, Gizmo is not Hidden
      use Visible instead if you want to Hide, if you want to Hide but keep enabled
      see the VisibleGizmo property }
    property Enabled: Boolean read FEnabled write FEnabled default False;

    property LabelFont: TVKCustomBitmapFont read FLabelFont write SetLabelFont
      default nil;

    property OnBeforeSelect: TVKGizmoAcceptEvent read FOnBeforeSelect
      write FOnBeforeSelect;
    property OnSelectionLost: TNotifyEvent read FOnSelectionLost
      write FOnSelectionLost;

    { Called before an Update is applied. The "vector" parameter is the difference
      that will be applied to the object, according to the axis and
      operation selected. }
    property OnBeforeUpdate: TVKGizmoUpdateEvent read FOnBeforeUpdate
      write FOnBeforeUpdate;
    property PickMode: TVKGizmoPickMode read FPickMode write FPickMode
      default PmGetPickedObjects;
  end;
//-----------------------------------------------------------------------
implementation
//-----------------------------------------------------------------------
procedure RotateAroundArbitraryAxis(const AnObject: TVKBaseSceneObject;
  const Axis, Origin: TAffineVector; const Angle: Single);
var
  M, M1, M2, M3: TMatrix;
begin
  M1 := CreateTranslationMatrix(VectorNegate(Origin));
  M2 := CreateRotationMatrix(Axis, Angle * PI / 180);
  M3 := CreateTranslationMatrix(Origin);
  M := MatrixMultiply(M1, M2);
  M := MatrixMultiply(M, M3);
  AnObject.Matrix := MatrixMultiply(AnObject.Matrix, M);

  // Just a workarround to Update angles...
  AnObject.Roll(0);
  AnObject.Pitch(0);
  AnObject.Turn(0);
end;

// ------------------------------------------------------------------------------

procedure TVKGizmo.ClearInternalRaycastHitData;
var
  T: Integer;
begin
  for T := FInternalRaycastHitData.Count - 1 downto 0 do
  begin
    TVKGizmoRayCastHitData(FInternalRaycastHitData[T]).Free;
  end;
  FInternalRaycastHitData.Clear;
end;

constructor TVKGizmo.Create(AOwner: TComponent);
var
  Cub: TVKCube;
begin
  inherited Create(AOwner);
  FUndoHistory := TVKGizmoUndoCollection.Create(Self, TVKGizmoUndoItem);
  FPickMode := PmGetPickedObjects;
  PickableObjectsWithRayCast := TList.Create;
  FRotationCoef := 1;
  FMoveCoef := 0.1;
  FScaleCoef := 0.1;
  FGizmoThickness := 1;

  FInternalRaycastHitData := TList.Create;
  FBoundingBoxColor := TVKColor.Create(Self);
  FBoundingBoxColor.Color := ClrWhite;
  FBoundingBoxColorChanged := False;
  FSelectedColor := TVKColor.Create(Self);
  FSelectedColor.Color := ClrYellow;
  FVisibleInfoLabelsColor := TVKColor.Create(Self);
  FVisibleInfoLabelsColor.Color := ClrYellow;
  FVisibleInfoLabelsColorChanged := False;

  _GZObaseGizmo := TVKDummyCube.Create(Self);
  _GZORootHelpers := TVKDummyCube(_GZObaseGizmo.AddNewChild(TVKDummyCube));
  _GZOBoundingcube := TVKCube(_GZORootHelpers.AddNewChild(TVKCube));

  _GZORootLines := _GZORootHelpers.AddNewChild(TVKDummyCube);
  _GZORootTorus := _GZORootHelpers.AddNewChild(TVKDummyCube);
  _GZORootCubes := _GZORootHelpers.AddNewChild(TVKDummyCube);
  _GZORootAxisLabel := _GZORootHelpers.AddNewChild(TVKDummyCube);
  _GZORootVisibleInfoLabels := _GZORootHelpers.AddNewChild(TVKDummyCube);

  DglDisable := TVKDirectOpenGL(_GZORootLines.AddNewChild(TVKDirectOpenGL));
  DglDisable.OnRender := DirectGlDisable;
  DgtDisable := TVKDirectOpenGL(_GZORootTorus.AddNewChild(TVKDirectOpenGL));
  DgtDisable.OnRender := DirectGlDisable;
  DgcDisable := TVKDirectOpenGL(_GZORootCubes.AddNewChild(TVKDirectOpenGL));
  DgcDisable.OnRender := DirectGlDisable;
  DglaDisable := TVKDirectOpenGL
    (_GZORootAxisLabel.AddNewChild(TVKDirectOpenGL));
  DglaDisable.OnRender := DirectGlDisable;
  DgliDisable := TVKDirectOpenGL(_GZORootVisibleInfoLabels.AddNewChild
    (TVKDirectOpenGL));
  DgliDisable.OnRender := DirectGlDisable;

  with _GZOBoundingcube.Material do
  begin
    FaceCulling := FcNoCull;
    PolygonMode := PmLines;
    with FrontProperties do
    begin
      Diffuse.Color := FBoundingBoxColor.Color;
      Ambient.Color := FBoundingBoxColor.Color;
      Emission.Color := FBoundingBoxColor.Color;
    end;
    with BackProperties do
    begin
      Diffuse.Color := FBoundingBoxColor.Color;
      Ambient.Color := FBoundingBoxColor.Color;
      Emission.Color := FBoundingBoxColor.Color;
    end;
  end;

  _GZOlinex := TVKLines(_GZORootLines.AddnewChild(TVKLines));
  with _GZOlinex do
  begin
    LineColor.Color := clrRed;
    LineWidth := 3;
    NodesAspect := LnaInvisible;
    AddNode(0, 0, 0);
    AddNode(1, 0, 0);
    AddNode(0.9, 0, -0.1);
    AddNode(1, 0, 0);
    AddNode(0.9, 0, 0.1);
    // Raycast pickable object
    Cub := TVKGizmoPickCube(AddNewChild(TVKGizmoPickCube));
    Cub.Up.SetVector(1, 0, 0);
    Cub.CubeWidth := 0.1;
    Cub.CubeHeight := 1;
    Cub.CubeDepth := 0.1;
    Cub.Position.SetPoint(0.5, 0, 0);
    Cub.Visible := False;
  end;

  _GZOliney := TVKLines(_GZORootLines.AddnewChild(TVKLines));
  with _GZOliney do
  begin
    LineColor.Color := clrLime;
    LineWidth := 3;
    NodesAspect := LnaInvisible;
    AddNode(0, 0, 0);
    AddNode(0, 1, 0);
    AddNode(0.1, 0.9, 0);
    AddNode(0, 1, 0);
    AddNode(-0.1, 0.9, 0);
    // Raycast pickable object
    Cub := TVKGizmoPickCube(AddNewChild(TVKGizmoPickCube));
    Cub.Up.SetVector(0, 1, 0);
    Cub.CubeWidth := 0.1;
    Cub.CubeHeight := 1;
    Cub.CubeDepth := 0.1;
    Cub.Position.SetPoint(0, 0.5, 0);
    Cub.Visible := False;
  end;

  _GZOlinez := TVKLines(_GZORootLines.AddnewChild(TVKLines));
  with _GZOlinez do
  begin
    LineColor.Color := clrBlue;
    LineWidth := 3;
    NodesAspect := LnaInvisible;
    AddNode(0, 0, 0);
    AddNode(0, 0, 1);
    AddNode(0.1, 0, 0.9);
    AddNode(0, 0, 1);
    AddNode(-0.1, 0, 0.9);
    // Raycast pickable object
    Cub := TVKGizmoPickCube(AddNewChild(TVKGizmoPickCube));
    Cub.Up.SetVector(0, 0, 1);
    Cub.CubeWidth := 0.1;
    Cub.CubeHeight := 1;
    Cub.CubeDepth := 0.1;
    Cub.Position.SetPoint(0, 0, 0.5);
    Cub.Visible := False;
  end;

  _GZOplaneXY := TVKLines(_GZORootLines.AddnewChild(TVKLines));
  with _GZOplaneXY do
  begin
    LineWidth := 3;
    Options := [LoUseNodeColorForLines];
    NodesAspect := LnaInvisible;
    SplineMode := LsmSegments;
    AddNode(0.8, 1, 0);
    TVKLinesNode(Nodes[0]).Color.Color := clrRed;
    AddNode(1, 1, 0);
    TVKLinesNode(Nodes[1]).Color.Color := clrRed;
    AddNode(1, 1, 0);
    TVKLinesNode(Nodes[2]).Color.Color := clrLime;
    AddNode(1, 0.8, 0);
    TVKLinesNode(Nodes[3]).Color.Color := clrLime;
    // Raycast pickable object
    Cub := TVKGizmoPickCube(AddNewChild(TVKGizmoPickCube));
    Cub.Up.SetVector(1, 0, 0);
    Cub.CubeWidth := 0.2;
    Cub.CubeHeight := 0.2;
    Cub.CubeDepth := 0.1;
    Cub.Position.SetPoint(0.9, 0.9, 0);
    Cub.Visible := False;
  end;

  _GZOplaneXZ := TVKLines(_GZORootLines.AddnewChild(TVKLines));
  with _GZOplaneXZ do
  begin
    LineWidth := 3;
    Options := [LoUseNodeColorForLines];
    NodesAspect := LnaInvisible;
    SplineMode := LsmSegments;
    AddNode(1, 0, 0.8);
    TVKLinesNode(Nodes[0]).Color.Color := clrBlue;
    AddNode(1, 0, 1);
    TVKLinesNode(Nodes[1]).Color.Color := clrBlue;
    AddNode(1, 0, 1);
    TVKLinesNode(Nodes[2]).Color.Color := clrRed;
    AddNode(0.8, 0, 1);
    TVKLinesNode(Nodes[3]).Color.Color := clrRed;
    // Raycast pickable object
    Cub := TVKGizmoPickCube(AddNewChild(TVKGizmoPickCube));
    Cub.Up.SetVector(1, 0, 0);
    Cub.CubeWidth := 0.1;
    Cub.CubeHeight := 0.2;
    Cub.CubeDepth := 0.2;
    Cub.Position.SetPoint(0.9, 0, 0.9);
    Cub.Visible := False;
  end;

  _GZOplaneYZ := TVKLines(_GZORootLines.AddnewChild(TVKLines));
  with _GZOplaneYZ do
  begin
    LineWidth := 3;
    Options := [LoUseNodeColorForLines];
    NodesAspect := LnaInvisible;
    SplineMode := LsmSegments;
    AddNode(0, 0.8, 1);
    TVKLinesNode(Nodes[0]).Color.Color := clrLime;
    AddNode(0, 1, 1);
    TVKLinesNode(Nodes[1]).Color.Color := clrLime;
    AddNode(0, 1, 1);
    TVKLinesNode(Nodes[2]).Color.Color := clrBlue;
    AddNode(0, 1, 0.8);
    TVKLinesNode(Nodes[3]).Color.Color := clrBlue;
    // Raycast pickable object
    Cub := TVKGizmoPickCube(AddNewChild(TVKGizmoPickCube));
    Cub.Up.SetVector(0, 0, 1);
    Cub.CubeWidth := 0.2;
    Cub.CubeHeight := 0.2;
    Cub.CubeDepth := 0.1;
    Cub.Position.SetPoint(0, 0.9, 0.9);
    Cub.Visible := False;
  end;

  _GZOTorusX := TVKGizmoPickTorus(_GZORootTorus.AddnewChild(TVKGizmoPickTorus));
  with _GZOTorusX do
  begin
    Rings := 16;
    Sides := 4;
    MajorRadius := 0.6;
    MinorRadius := 0.03;
    PitchAngle := 90;
    TurnAngle := 90;
    with Material do
    begin
      // FaceCulling:= fcNoCull;
      PolygonMode := PmFill;
      // BackProperties.PolygonMode:= pmFill;
      FrontProperties.Emission.Color := clrBlue;
    end;
  end;

  _GZOTorusY := TVKGizmoPickTorus(_GZORootTorus.AddnewChild(TVKGizmoPickTorus));
  with _GZOTorusY do
  begin
    Rings := 16;
    Sides := 4;
    MajorRadius := 0.6;
    MinorRadius := 0.03;
    PitchAngle := 90;
    with Material do
    begin
      // FaceCulling:= fcNoCull;
      PolygonMode := PmFill;
      // BackProperties.PolygonMode:= pmFill;
      FrontProperties.Emission.Color := clrRed;
    end;
  end;

  _GZOTorusZ := TVKGizmoPickTorus(_GZORootTorus.AddnewChild(TVKGizmoPickTorus));
  with _GZOTorusZ do
  begin
    Rings := 16;
    Sides := 4;
    MajorRadius := 0.6;
    MinorRadius := 0.03;
    with Material do
    begin
      // FaceCulling:= fcNoCull;
      PolygonMode := PmFill;
      // BackProperties.PolygonMode:= pmFill;
      FrontProperties.Emission.Color := clrLime;
    end;
  end;

  _GZOCubeX := TVKGizmoPickCube(_GZORootCubes.AddnewChild(TVKGizmoPickCube));
  with _GZOCubeX do
  begin
    CubeDepth := 0.1;
    CubeHeight := 0.1;
    CubeWidth := 0.1;
    Position.X := 1.15;
    with Material do
    begin
      FaceCulling := FcNoCull;
      PolygonMode := PmFill;
      FrontProperties.Emission.Color := clrRed;
    end;
  end;

  _GZOCubeY := TVKGizmoPickCube(_GZORootCubes.AddnewChild(TVKGizmoPickCube));
  with _GZOCubeY do
  begin
    CubeDepth := 0.1;
    CubeHeight := 0.1;
    CubeWidth := 0.1;
    Position.Y := 1.15;
    with Material do
    begin
      FaceCulling := FcNoCull;
      PolygonMode := PmFill;
      FrontProperties.Emission.Color := clrLime;
    end;
  end;

  _GZOCubeZ := TVKGizmoPickCube(_GZORootCubes.AddnewChild(TVKGizmoPickCube));
  with _GZOCubeZ do
  begin
    CubeDepth := 0.1;
    CubeHeight := 0.1;
    CubeWidth := 0.1;
    Position.Z := 1.15;
    with Material do
    begin
      FaceCulling := FcNoCull;
      PolygonMode := PmFill;
      FrontProperties.Emission.Color := clrBlue;
    end;
  end;

  _GZOAxisLabelX := TVKFlatText(_GZORootAxisLabel.AddNewChild(TVKFlatText));
  with _GZOAxisLabelX do
  begin
    ModulateColor.Color := ClrRed;
    Alignment := TaCenter;
    Layout := TlCenter;
    Options := Options + [FtoTwoSided];
    Position.X := 1.5;
    Scale.X := 0.02;
    Scale.Y := 0.02;
    Text := 'X';
  end;

  _GZOAxisLabelY := TVKFlatText(_GZORootAxisLabel.AddNewChild(TVKFlatText));
  with _GZOAxisLabelY do
  begin
    ModulateColor.Color := clrLime;
    Alignment := TaCenter;
    Layout := TlCenter;
    Options := Options + [FtoTwoSided];
    Position.Y := 1.5;
    Scale.X := 0.02;
    Scale.Y := 0.02;
    Text := 'Y';
  end;

  _GZOAxisLabelZ := TVKFlatText(_GZORootAxisLabel.AddNewChild(TVKFlatText));
  with _GZOAxisLabelZ do
  begin
    ModulateColor.Color := ClrBlue;
    Alignment := TaCenter;
    Layout := TlCenter;
    Options := Options + [FtoTwoSided];
    Position.Z := 1.5;
    Scale.X := 0.02;
    Scale.Y := 0.02;
    Text := 'Z';
  end;

  _GZOVisibleInfoLabels :=
    TVKFlatText(_GZORootVisibleInfoLabels.AddNewChild(TVKFlatText));
  with _GZOVisibleInfoLabels do
  begin
    ModulateColor.Color := clrYellow;
    Alignment := TaCenter;
    Layout := TlCenter;
    Options := Options + [FtoTwoSided];
    Position.Y := 1.8;
    Position.X := 0;
    Scale.X := 0.01;
    Scale.Y := 0.01;
    Text := '';
  end;

  DglEnable := TVKDirectOpenGL(_GZORootLines.AddNewChild(TVKDirectOpenGL));
  DglEnable.OnRender := DirectGlEnable;
  DgtEnable := TVKDirectOpenGL(_GZORootTorus.AddNewChild(TVKDirectOpenGL));
  DgtEnable.OnRender := DirectGlEnable;
  DgcEnable := TVKDirectOpenGL(_GZORootCubes.AddNewChild(TVKDirectOpenGL));
  DgcEnable.OnRender := DirectGlEnable;
  DglaEnable := TVKDirectOpenGL(_GZORootAxisLabel.AddNewChild(TVKDirectOpenGL));
  DglaEnable.OnRender := DirectGlEnable;
  DgliEnable := TVKDirectOpenGL(_GZORootVisibleInfoLabels.AddNewChild
    (TVKDirectOpenGL));
  DgliEnable.OnRender := DirectGlEnable;

  _GZObaseGizmo.Visible := False;
  FGizmoElements := FGizmoElements + [GeMove, GeRotate, GeScale, GeAxisLabel,
    GeObjectInfos, GeBoundingBox];
  FVisibleVisibleInfoLabels := FVisibleVisibleInfoLabels +
    [VliName, VliOperation, VliCoords];
  AutoZoom := True;
  AutoZoomFactor := 5.0;
  ZoomFactor := 0.35;
  ForceOperation := False;
  ForceAxis := False;
  ForceUniformScale := False;
  Enabled := True;
  FNoZWrite := True;
  FExcludeObjectsList := TStringList.Create;
end;

destructor TVKGizmo.Destroy;
begin
  if Assigned(FRootGizmo) then
    FRootGizmo.DeleteChildren
  else
  begin
    _GZOBaseGizmo.DeleteChildren;
    _GZOBaseGizmo.Free;
  end;

  FBoundingBoxColor.Free;
  FSelectedColor.Free;
  FVisibleInfoLabelsColor.Free;
  PickableObjectsWithRayCast.Free;
  FExcludeObjectsList.Free;
  ClearInternalRaycastHitData;
  FInternalRaycastHitData.Free;

  // FUndoHistory has to be nil before Notification() is called.
  FreeAndNil(FUndoHistory);
  inherited Destroy;
end;

procedure TVKGizmo.SetVisible(const AValue: Boolean);
begin
  _GZObaseGizmo.Visible := AValue;
end;

procedure TVKGizmo.SetGizmoElements(const AValue: TVKGizmoElements);
begin
  if AValue <> FGizmoElements then
  begin
    FGizmoElements := AValue;
    _GZORootLines.Visible := GeMove in FGizmoElements;
    _GZORootTorus.Visible := GeRotate in FGizmoElements;
    _GZORootCubes.Visible := GeScale in FGizmoElements;
    _GZORootAxisLabel.Visible := GeAxisLabel in FGizmoElements;
    _GZORootVisibleInfoLabels.Visible := GeObjectInfos in FGizmoElements;
    _GZOBoundingcube.Visible := GeBoundingBox in FGizmoElements;
  end;
end;

procedure TVKGizmo.SetBoundingBoxColor(const AValue: TVKColor);
begin
  // Bug Here New Color is not Updated
  if AValue <> FBoundingBoxColor then
  begin
    FBoundingBoxColor.Color := AValue.Color;
    with _GZOBoundingcube.Material do
    begin
      with FrontProperties do
      begin
        Diffuse.Color := FBoundingBoxColor.Color;
        Ambient.Color := FBoundingBoxColor.Color;
        Emission.Color := FBoundingBoxColor.Color;
      end;
      with BackProperties do
      begin
        Diffuse.Color := FBoundingBoxColor.Color;
        Ambient.Color := FBoundingBoxColor.Color;
        Emission.Color := FBoundingBoxColor.Color;
      end;
    end;
    FBoundingBoxColorChanged := True;
  end;
end;

procedure TVKGizmo.SetSelectedColor(const AValue: TVKColor);
begin
  if AValue <> FSelectedColor then
  begin
    FSelectedColor.Color := AValue.Color;
  end;
end;

procedure TVKGizmo.SetVisibleInfoLabelsColor(const AValue: TVKColor);
begin
  // Bug Here New Color is not Updated
  if AValue <> FSelectedColor then
  begin
    FVisibleInfoLabelsColor.Color := AValue.Color;
    _GZOVisibleInfoLabels.ModulateColor.Color := AValue.Color;
    FVisibleInfoLabelsColorChanged := True;
  end;
end;

procedure TVKGizmo.SeTVKGizmoVisibleInfoLabels(const AValue
  : TVKGizmoVisibleInfoLabels);
begin
  if AValue <> FVisibleVisibleInfoLabels then
  begin
    FVisibleVisibleInfoLabels := AValue;
    if not(CsDesigning in ComponentState) then
      UpdateGizmo;
  end;
end;

procedure TVKGizmo.UndoAdd(const AObject: TVKCustomSceneObject);
begin
  if AObject <> nil then
  begin
    FUndoHistory.Add.AssignFromObject(AObject)
  end;
end;

procedure TVKGizmo.SetRootGizmo(const AValue: TVKBaseSceneObject);
begin
  if FRootGizmo <> AValue then
  begin
    if FRootGizmo <> nil then
      FRootGizmo.RemoveFreeNotification(Self);
    FRootGizmo := AValue;
    if FRootGizmo <> nil then
      FRootGizmo.FreeNotification(Self);
    _GZObaseGizmo.MoveTo(AValue);
  end;
end;

procedure TVKGizmo.SetExcludeObjectsList(const AValue: TStrings);
begin
  FExcludeObjectsList.Clear;
  FExcludeObjectsList.AddStrings(AValue);
end;

procedure TVKGizmo.SetVKGizmoThickness(const Value: Single);
var
  Thk: Single;
begin
  if FGizmoThickness <> Value then
  begin
    Thk := MaxInteger(1, Round(3 * Value));
    _GZOlinex.LineWidth := Thk;
    _GZOliney.LineWidth := Thk;
    _GZOlinez.LineWidth := Thk;
    _GZOplaneXY.LineWidth := Thk;
    _GZOplaneXZ.LineWidth := Thk;
    _GZOplaneYZ.LineWidth := Thk;

    _GZOTorusX.MinorRadius := 0.03 * Value;
    _GZOTorusY.MinorRadius := 0.03 * Value;
    _GZOTorusZ.MinorRadius := 0.03 * Value;

    with _GZOCubeX do
    begin
      CubeDepth := 0.1 * Value;
      CubeHeight := 0.1 * Value;
      CubeWidth := 0.1 * Value;
    end;
    with _GZOCubeY do
    begin
      CubeDepth := 0.1 * Value;
      CubeHeight := 0.1 * Value;
      CubeWidth := 0.1 * Value;
    end;
    with _GZOCubeZ do
    begin
      CubeDepth := 0.1 * Value;
      CubeHeight := 0.1 * Value;
      CubeWidth := 0.1 * Value;
    end;

    FGizmoThickness := Value;
  end;
end;

// ------------------------------------------------------------------------------

procedure TVKGizmo.DirectGlDisable(Sender: TObject;
  var Rci: TVKRenderContextInfo);
begin
  if FNoZWrite then
    Rci.GLStates.Disable(StDepthTest);
end;

procedure TVKGizmo.SetLabelFont(const Value: TVKCustomBitmapFont);
begin
  if FLabelFont <> Value then
  begin
    if FLabelFont <> nil then
      FLabelFont.RemoveFreeNotification(Self);
    FLabelFont := Value;
    if FLabelFont <> nil then
      FLabelFont.FreeNotification(Self);

    _GZOAxisLabelX.BitmapFont := Value;
    _GZOAxisLabelY.BitmapFont := Value;
    _GZOAxisLabelZ.BitmapFont := Value;
    _GZOVisibleInfoLabels.BitmapFont := Value;
  end;
end;

procedure TVKGizmo.DirectGlEnable(Sender: TObject; var Rci: TVKRenderContextInfo);
begin
  if FNoZWrite then
    Rci.GLStates.Enable(StDepthTest);
end;

function TVKGizmo.GetPickedObjectPoint(const Obj: TVKBaseSceneObject): TVector;
var
  T: Integer;
  R: TVKGizmoRayCastHitData;
begin
  for T := 0 to FInternalRaycastHitData.Count - 1 do
  begin
    R := TVKGizmoRayCastHitData(FInternalRaycastHitData[T]);
    if R.Obj = Obj then
    begin
      Result := R.Point;
      Break;
    end;
  end;
end;

function TVKGizmo.InternalGetPickedObjects(const X1, Y1, X2, Y2: Integer;
  const GuessCount: Integer): TVKPickList;
var
  T: Integer;
  RayStart, RayVector, IPoint, INormal: TVector;
  O: TVKBaseSceneObject;
  Dist: Single;
  HitData: TVKGizmoRayCastHitData;

  procedure AddGizmosToPicklListRecurse(const Root: TVKBaseSceneObject);
  var
    U: Integer;
  begin
    for U := 0 to Root.Count - 1 do
    begin
      if ((Root[U] is TVKGizmoPickTorus) or (Root[U] is TVKGizmoPickCube)) then
        PickableObjectsWithRayCast.Add(Root[U]);
      AddGizmosToPicklListRecurse(Root[U]);
    end;
  end;

begin
  case FPickMode of
    PmGetPickedObjects:
      begin
        Result := Viewer.Buffer.GetPickedObjects(Rect(X1, Y1, X2, Y2),
          GuessCount);
      end;

    PmRayCast:
      begin
        Result := TVKPickList.Create(PsMinDepth);
        ClearInternalRaycastHitData;
        SetVector(RayStart, Viewer.Camera.AbsolutePosition);
        SetVector(RayVector, Viewer.Buffer.ScreenToVector
          (AffineVectorMake((X1 + X2) * 0.5,
          Viewer.Height - ((Y1 + Y2) * 0.5), 0)));
        NormalizeVector(RayVector);
        // Add gizmos
        if (RootGizmo <> nil) and (SelectedObj <> nil) then
          AddGizmosToPicklListRecurse(RootGizmo);
        // pick
        for T := 0 to PickableObjectsWithRayCast.Count - 1 do
        begin
          O := TVKBaseSceneObject(PickableObjectsWithRayCast[T]);
          if (O.RayCastIntersect(RayStart, RayVector, @IPoint, @INormal)) and
            (VectorDotProduct(RayVector, INormal) < 0) then
          begin
            try
              Dist := VectorLength(VectorSubtract(IPoint, RayStart));
              Result.AddHit(O, nil, Dist, 0);
              HitData := TVKGizmoRayCastHitData.Create;
              HitData.Obj := O;
              MakeVector(HitData.Point, IPoint);
              FInternalRaycastHitData.Add(HitData);
            except
              //
            end;
          end;
        end;
      end;

  else
    begin
      Result := nil;
      Assert(False, glsErrorEx + glsUnknownType);
    end;

  end;
end;

procedure TVKGizmo.Loaded;
begin
  inherited;
  SeTVKGizmoThickness(GizmoThickness);
end;

// ------------------------------------------------------------------------------
procedure TVKGizmo.UpdateVisibleInfoLabels;
var
  T: string;
  X, Y, Z: Single;
begin
  T := '';
  if not(Assigned(SelectedObj)) then
    Exit;
  if VliName in FVisibleVisibleInfoLabels then
    T := SelectedObj.Name;

  if VliOperation in FVisibleVisibleInfoLabels then
  begin
    if (Operation <> GopNone) then
    begin
      if Length(T) > 0 then
        T := T + ' - ';
      case Operation of
        GopMove:
          T := T + 'Move';
        GopRotate:
          T := T + 'Rotate';
        GopScale:
          T := T + 'Scale';
      end;
    end;
  end;

  if VliCoords in FVisibleVisibleInfoLabels then
  begin
    if (Operation <> GopNone) then
    begin
      if Length(T) > 0 then
        T := T + ' - ';
      case Operation of
        GopMove:
          begin
            X := SelectedObj.Position.X;
            Y := SelectedObj.Position.Y;
            Z := SelectedObj.Position.Z;
            T := T + 'X : ' + Format('%2.3f', [X]);
            T := T + ' Y : ' + Format('%2.3f', [Y]);
            T := T + ' Z : ' + Format('%2.3f', [Z]);
          end;
        GopRotate:
          begin
            X := SelectedObj.Rotation.X;
            Y := SelectedObj.Rotation.Y;
            Z := SelectedObj.Rotation.Z;
            T := T + 'X : ' + Format('%2.3f', [X]);
            T := T + ' Y : ' + Format('%2.3f', [Y]);
            T := T + ' Z : ' + Format('%2.3f', [Z]);
          end;
        GopScale:
          begin
            X := SelectedObj.Scale.X;
            Y := SelectedObj.Scale.Y;
            Z := SelectedObj.Scale.Z;
            T := T + 'X : ' + Format('%2.3f', [X]);
            T := T + ' Y : ' + Format('%2.3f', [Y]);
            T := T + ' Z : ' + Format('%2.3f', [Z]);
          end;
      end;
    end;
  end;

  _GZOVisibleInfoLabels.Text := T;
  _GZOVisibleInfoLabels.StructureChanged;
end;

// ------------------------------------------------------------------------------

function TVKGizmo.CheckObjectInExcludeList
  (const Obj: TVKBaseSceneObject): Boolean;
var
  I: Integer;
begin
  Result := False;
  if FExcludeObjects then
  begin
    for I := 0 to FExcludeObjectsList.Count - 1 do
    begin
      if UpperCase(Obj.Name) = UpperCase(FExcludeObjectsList[I]) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TVKGizmo.MouseWorldPos(const X, Y: Integer): TVector;
var
  V: TVector;
  InvertedY: Integer;
begin
  InvertedY := Round(Viewer.Height) - Y;
  if Assigned(SelectedObj) then
  begin
    SetVector(V, X, InvertedY, 0);

    case SelAxis of
      GaX:
        if not Viewer.Buffer.ScreenVectorIntersectWithPlaneXZ(V,
          SelectedObj.AbsolutePosition.V[1], Result) then
          MakeVector(Result, X / 5, 0, 0);

      GaY:
        if not Viewer.Buffer.ScreenVectorIntersectWithPlaneYZ(V,
          SelectedObj.AbsolutePosition.V[0], Result) then
          MakeVector(Result, 0, InvertedY / 5, 0);

      GaZ:
        if not Viewer.Buffer.ScreenVectorIntersectWithPlaneYZ(V,
          SelectedObj.AbsolutePosition.V[0], Result) then
          MakeVector(Result, 0, 0, -InvertedY / 5);

      GaXY:
        begin
          Viewer.Buffer.ScreenVectorIntersectWithPlaneXY(V,
            SelectedObj.AbsolutePosition.V[2], Result);
        end;
      GaXZ:
        begin
          Viewer.Buffer.ScreenVectorIntersectWithPlaneXZ(V,
            SelectedObj.AbsolutePosition.V[1], Result);
        end;
      GaYZ:
        begin
          Viewer.Buffer.ScreenVectorIntersectWithPlaneYZ(V,
            SelectedObj.AbsolutePosition.V[0], Result);
        end;
    end;

  end
  else
    SetVector(Result, NullVector);
end;

procedure TVKGizmo.ViewerMouseMove(const X, Y: Integer);
var
  PickList: TVKPickList;
  MousePos: TVector;

  function IndexOf(Obj: TVKBaseSceneObject): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to PickList.Count - 1 do
      if PickList.Hit[I] = Obj then
      begin
        Result := I;
        Break;
      end;
  end;

  function LightLine(const Line: TVKLines; const Dark: TVector;
    const Axis: TVKGizmoAxis; AlterStyle: Boolean = False): Boolean;
  var
    PickObj: TVKBaseSceneObject;
  begin
    case FPickMode of
      PmGetPickedObjects:
        PickObj := Line;
      PmRayCast:
        PickObj := Line;
    else
      begin
        PickObj := nil;
        Assert(False, glsErrorEx + glsUnknownType);
      end;
    end;

    if IndexOf(PickObj) > -1 then
    begin
      Line.LineColor.Color := FSelectedColor.Color;
      if not(FForceOperation) then
        if Operation <> GopMove then
          Operation := GopMove;
      Line.Options := [];
      if not(FForceAxis) then
        SelAxis := Axis;
      Result := True;
    end
    else
    begin
      Line.LineColor.Color := Dark;
      if not(FForceOperation) then
        Operation := GopNone;
      if AlterStyle then
        Line.Options := [LoUseNodeColorForLines];
      if not(FForceAxis) then
        if SelAxis = Axis then
          SelAxis := GaNone;
      Result := False;
    end;
  end;

  function LightTorus(const Torus: TVKGizmoPickTorus; const Dark: TVector;
    const Axis: TVKGizmoAxis; AlterStyle: Boolean = False): Boolean;
  begin
    if IndexOf(Torus) > -1 then
    begin
      Torus.Material.FrontProperties.Emission.Color := FSelectedColor.Color;
      if not(FForceOperation) then
        if Operation <> GopRotate then
          Operation := GopRotate;
      if not(FForceAxis) then
        SelAxis := Axis;
      Result := True;
    end
    else
    begin
      Torus.Material.FrontProperties.Emission.Color := Dark;
      if not(FForceOperation) then
        Operation := GopNone;
      if not(FForceAxis) then
        if SelAxis = Axis then
          SelAxis := GaNone;
      Result := False;
    end;
  end;

  function LightCube(const Cube: TVKCube; const Dark: TVector;
    const Axis: TVKGizmoAxis; AlterStyle: Boolean = False): Boolean;
  begin
    if IndexOf(Cube) > -1 then
    begin
      Cube.Material.FrontProperties.Emission.Color := FSelectedColor.Color;
      if not(FForceOperation) then
        if Operation <> GopScale then
          Operation := GopScale;
      if not(FForceAxis) then
        SelAxis := Axis;
      Result := True;
    end
    else
    begin
      Cube.Material.FrontProperties.Emission.Color := Dark;
      if not(FForceOperation) then
        Operation := GopNone;
      if not(FForceAxis) then
        if SelAxis = Axis then
          SelAxis := GaNone;
      Result := False;
    end;
  end;

  procedure OpeMove(MousePos: TVector);
  var
    Vec1, Vec2: TVector;
    QuantizedMousePos, QuantizedMousePos2: TVector;
    T: Integer;
  begin
    for T := 0 to 3 do
    begin
      QuantizedMousePos.V[T] := (Round(MousePos.V[T] / MoveCoef)) * MoveCoef;
      QuantizedMousePos2.V[T] := (Round(LastMousePos.V[T] / MoveCoef)) * MoveCoef;
    end;
    case SelAxis of
      GaX:
        begin
          MakeVector(Vec1, QuantizedMousePos.V[0], 0, 0);
          MakeVector(Vec2, QuantizedMousePos2.V[0], 0, 0);
        end;
      GaY:
        begin
          MakeVector(Vec1, 0, QuantizedMousePos.V[1], 0);
          MakeVector(Vec2, 0, QuantizedMousePos2.V[1], 0);
        end;
      GaZ:
        begin
          MakeVector(Vec1, 0, 0, QuantizedMousePos.V[2]);
          MakeVector(Vec2, 0, 0, QuantizedMousePos2.V[2]);
        end;
    else
      begin
        Vec1 := QuantizedMousePos;
        Vec2 := QuantizedMousePos2;
      end;
    end;
    SubtractVector(Vec1, Vec2);
    if Assigned(OnBeforeUpdate) then
      OnBeforeUpdate(Self, SelectedObj, SelAxis, Operation, Vec1);
    Vec1 := SelectedObj.Parent.AbsoluteToLocal(Vec1);
    if (VectorLength(Vec1) > 0) then // prevents NAN problems
    begin
      SelectedObj.Position.Translate(Vec1);
    end;
  end;

  procedure OpeRotate(const X, Y: Integer);
  var
    Vec1: TVector;
    RotV: TAffineVector;
    Pmat: TMatrix;

  begin
    Vec1.V[0] := 0;
    Vec1.V[1] := 0;
    if Abs(X - Rx) >= RotationCoef then
    begin
      if RotationCoef > 1 then
        Vec1.V[0] := RotationCoef * (Round((X - Rx) / (RotationCoef)))
      else
        Vec1.V[0] := RotationCoef * (X - Rx);
      Rx := X;
    end;
    if Abs(Y - Ry) >= RotationCoef then
    begin
      if RotationCoef > 1 then
        Vec1.V[1] := RotationCoef * (Round((Y - Ry) / (RotationCoef)))
      else
        Vec1.V[1] := RotationCoef * (Y - Ry);
      Ry := Y;
    end;

    Vec1.V[2] := 0;
    Vec1.V[3] := 0;
    if Assigned(OnBeforeUpdate) then
      OnBeforeUpdate(Self, SelectedObj, SelAxis, Operation, Vec1);

    Pmat := SelectedObj.Parent.InvAbsoluteMatrix;
    SetVector(Pmat.V[3], NullHmgPoint);
    case SelAxis of
      GaX:
        begin
          RotV := VectorTransform(XVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.V[1]);
        end;
      GaY:
        begin
          RotV := VectorTransform(YVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.V[0]);
        end;
      GaZ:
        begin
          RotV := VectorTransform(ZVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.V[1]);
        end;
      GaXY:
        begin
          RotV := VectorTransform(XVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.V[1]);
          RotV := VectorTransform(YVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.V[0]);
        end;
      GaXZ:
        begin
          RotV := VectorTransform(XVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.V[1]);
          RotV := VectorTransform(ZVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.V[0]);
        end;
      GaYZ:
        begin
          RotV := VectorTransform(YVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.V[1]);
          RotV := VectorTransform(ZVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.V[0]);
        end;
    end;
  end;

  procedure OpeScale(const MousePos: TVector);
  var
    Vec1, Vec2: TVector;
    QuantizedMousePos, QuantizedMousePos2: TVector;
    T: Integer;
  begin
    for T := 0 to 3 do
    begin
      QuantizedMousePos.V[T] := (Round(MousePos.V[T] / ScaleCoef)) * FScaleCoef;
      QuantizedMousePos2.V[T] := (Round(LastMousePos.V[T] / FScaleCoef)) *
        FScaleCoef;
    end;
    case SelAxis of
      GaX:
        begin
          if FForceUniformScale then
          begin
            MakeVector(Vec1, QuantizedMousePos.V[0], QuantizedMousePos.V[0],
              QuantizedMousePos.V[0]);
            MakeVector(Vec2, QuantizedMousePos2.V[0], QuantizedMousePos2.V[0],
              QuantizedMousePos2.V[0]);
          end
          else
          begin
            MakeVector(Vec1, QuantizedMousePos.V[0], 0, 0);
            MakeVector(Vec2, QuantizedMousePos2.V[0], 0, 0);
          end;

        end;

      GaY:
        begin
          if FForceUniformScale then
          begin
            MakeVector(Vec1, QuantizedMousePos.V[1], QuantizedMousePos.V[1],
              QuantizedMousePos.V[1]);
            MakeVector(Vec2, QuantizedMousePos2.V[1], QuantizedMousePos2.V[1],
              QuantizedMousePos2.V[1]);
          end
          else
          begin
            MakeVector(Vec1, 0, QuantizedMousePos.V[1], 0);
            MakeVector(Vec2, 0, QuantizedMousePos2.V[1], 0);
          end;
        end;

      GaZ:
        begin
          if FForceUniformScale then
          begin
            MakeVector(Vec1, QuantizedMousePos.V[2], QuantizedMousePos.V[2],
              QuantizedMousePos.V[2]);
            MakeVector(Vec2, QuantizedMousePos2.V[2], QuantizedMousePos2.V[2],
              QuantizedMousePos2.V[2]);
          end
          else
          begin
            MakeVector(Vec1, 0, 0, QuantizedMousePos.V[2]);
            MakeVector(Vec2, 0, 0, QuantizedMousePos2.V[2]);
          end;
        end;
    else
      begin
        Vec1 := QuantizedMousePos;
        Vec2 := QuantizedMousePos2;
      end;
    end;
    SubtractVector(Vec1, Vec2);
    if Assigned(OnBeforeUpdate) then
      OnBeforeUpdate(Self, SelectedObj, SelAxis, Operation, Vec1);
    SelectedObj.Scale.Translate(Vec1);
    UpdateGizmo;
  end;

begin
  if not Enabled then
    Exit;

  if Assigned(SelectedObj) and (SelAxis <> GaNone) and Moving then
  begin
    MousePos := MouseWorldPos(X, Y);

    // moving object...
    if Operation = GopMove then
    begin
      // FLastOperation = gopMove;
      OpeMove(MousePos);
    end
    else if Operation = GopRotate then
    begin
      // FLastOperation = gopRotate;
      OpeRotate(X, Y);
    end
    else if Operation = GopScale then
    begin
      // FLastOperation = gopScale;
      OpeScale(MousePos);
    end;

    UpdateGizmo;
    Mx := X;
    My := Y;
    LastMousePos := MousePos;
    Exit;
  end;

  Assert(FViewer <> nil, 'Viewer not Assigned to gizmo');
  Picklist := InternalGetPickedObjects(X - 1, Y - 1, X + 1, Y + 1, 8);
  // Viewer.buffer.GetPickedObjects(rect(x-1, y-1, x+1, y+1), 8);

  if not LightLine(_GZOlinex, ClrRed, GaX) and not LightLine(_GZOliney, ClrLime,
    GaY) and not LightLine(_GZOlinez, ClrBlue, GaZ) and
    not LightTorus(_GZOTorusX, ClrRed, GaX) and
    not LightTorus(_GZOTorusY, ClrLime, GaY) and
    not LightTorus(_GZOTorusz, ClrBlue, GaZ) and
    not LightCube(_GZOCubeX, ClrRed, GaX) and not LightCube(_GZOCubeY, ClrLime,
    GaY) and not LightCube(_GZOCubeZ, ClrBlue, GaZ) and
    not LightLine(_GZOplaneXY, ClrWhite, GaXY, True) and
    not LightLine(_GZOplaneXZ, ClrWhite, GaXZ, True) and
    not LightLine(_GZOplaneYZ, ClrWhite, GaYZ, True) then
  begin
    if not(FForceAxis) then
      SelAxis := GaNone;
    if not(FForceOperation) then
      Operation := GopNone;
  end;

  Picklist.Free;

  Mx := X;
  My := Y;
end;

procedure TVKGizmo.ViewerMouseDown(const X, Y: Integer);
var
  Pick: TVKPickList;
  I: Integer;
  Accept: Boolean;
  Dimensions: TVector;
  GotPick: Boolean;
  PickedObj: TVKBaseSceneObject;
begin
  Mx := X;
  My := Y;
  Rx := X;
  Ry := Y;

  if not Enabled then
    Exit;

  Pick := InternalGetPickedObjects(X - 1, Y - 1, X + 1, Y + 1);
  // Viewer.Buffer.GetPickedObjects(rect(x-1, y-1, x+1, y+1));
  GotPick := False;
  Accept := False;

  case FPickMode of
    PmGetPickedObjects:
      begin
        // primeiro, ver se é uma das linhas/planos
        for I := 0 to Pick.Count - 1 do
          if (_GZOrootLines.IndexOfChild(TVKBaseSceneObject(Pick.Hit[I])) > -1)
            or (_GZOrootTorus.IndexOfChild(TVKBaseSceneObject(Pick.Hit[I])) >
            -1) or (_GZOrootCubes.IndexOfChild(TVKBaseSceneObject(Pick.Hit[I]))
            > -1) then
            GotPick := True;
      end;

    PmRayCast:
      begin
        for I := 0 to Pick.Count - 1 do
        begin
          if (Pick.Hit[I] is TVKGizmoPickCube) or
            (Pick.Hit[I] is TVKGizmoPickTorus) then
            GotPick := True;
        end;
      end;
  else
    begin
      Assert(False, glsErrorEx + glsUnknownType);
    end;

  end;

  if not GotPick then
  begin
    for I := 0 to Pick.Count - 1 do

      if (Pick.Hit[I] <> _GZOBoundingcube) and (Pick.Hit[I] <> _GZOAxisLabelX)
        and (Pick.Hit[I] <> _GZOAxisLabelY) and (Pick.Hit[I] <> _GZOAxisLabelZ)
        and (Pick.Hit[I] <> _GZOVisibleInfoLabels) and
        not(CheckObjectInExcludeList(TVKBaseSceneObject(Pick.Hit[I]))) then
      begin
        Accept := True;
        PickedObj := TVKBaseSceneObject(Pick.Hit[I]);
        Dimensions := PickedObj.AxisAlignedDimensions;
        if Assigned(OnBeforeSelect) then
          OnBeforeSelect(Self, PickedObj, Accept, Dimensions);

        Break;
      end;

    if Accept then
      SetSelectedObj(PickedObj)
    else
      SetSelectedObj(nil);
  end
  else
    UpdateVisibleInfoLabels();

  Pick.Free;

  Moving := True;
  LastMousePos := MouseWorldPos(X, Y);
end;

procedure TVKGizmo.ViewerMouseUp(const X, Y: Integer);
begin
  Moving := False;
end;

// ------------------------------------------------------------------------------

procedure TVKGizmo.UpdateGizmo;
var
  D: Single;
begin
  if SelectedObj = nil then
  begin
    _GZObaseGizmo.Visible := False;
    Exit;
  end;

  _GZObaseGizmo.Position.AsVector := SelectedObj.AbsolutePosition;
  if GeObjectInfos in FGizmoElements then
    UpdateVisibleInfoLabels;

  _GZOBoundingcube.Matrix := SelectedObj.AbsoluteMatrix;
  _GZOBoundingcube.Position.SetPoint(0, 0, 0);

  // We must Update Color Of the BoundingBox And VisibleInfoLabels Here
  // If not Color is not Updated;

  // if FBoundingBoxColorChanged then
  // Begin
  with _GZOBoundingcube.Material do
  begin
    with FrontProperties do
    begin
      Diffuse.Color := FBoundingBoxColor.Color;
      Ambient.Color := FBoundingBoxColor.Color;
      Emission.Color := FBoundingBoxColor.Color;
    end;
    with BackProperties do
    begin
      Diffuse.Color := FBoundingBoxColor.Color;
      Ambient.Color := FBoundingBoxColor.Color;
      Emission.Color := FBoundingBoxColor.Color;
    end;
  end;
  // FBoundingBoxColorChanged:=False;
  // End;
  // If FVisibleInfoLabelsColorChanged then
  // Begin
  _GZOVisibleInfoLabels.ModulateColor.Color := FVisibleInfoLabelsColor.Color;
  // FVisibleInfoLabelsColorChanged:=False;
  // End;

  ObjDimensions := SelectedObj.AxisAlignedDimensions;
  _GZOBoundingcube.Scale.AsVector := VectorScale(ObjDimensions, 2);

  Assert(Viewer <> nil, 'Viewer not Assigned to gizmo');

  _GZOAxisLabelX.PointTo(Viewer.Camera.Position.AsVector,
    Viewer.Camera.Up.AsVector);
  _GZOAxisLabelX.StructureChanged;
  _GZOAxisLabelY.PointTo(Viewer.Camera.Position.AsVector,
    Viewer.Camera.Up.AsVector);
  _GZOAxisLabelY.StructureChanged;
  _GZOAxisLabelZ.PointTo(Viewer.Camera.Position.AsVector,
    Viewer.Camera.Up.AsVector);
  _GZOAxisLabelZ.StructureChanged;
  _GZOVisibleInfoLabels.PointTo(Viewer.Camera.Position.AsVector,
    Viewer.Camera.Up.AsVector);
  _GZOVisibleInfoLabels.StructureChanged;
  if FAutoZoom then
    D := Viewer.Camera.DistanceTo(SelectedObj) / FAutoZoomFactor
  else
    D := FZoomFactor;
  _GZOrootLines.Scale.AsVector := VectorMake(D, D, D);
  _GZOrootTorus.Scale.AsVector := VectorMake(D, D, D);
  _GZOrootCubes.Scale.AsVector := VectorMake(D, D, D);
  _GZOrootAxisLabel.Scale.AsVector := VectorMake(D, D, D);
  _GZOrootVisibleInfoLabels.Scale.AsVector := VectorMake(D, D, D);
end;

procedure TVKGizmo.UpdateGizmo(const NewDimensions: TVector);
begin
  ObjDimensions := NewDimensions;
  UpdateGizmo;
end;

procedure TVKGizmo.LooseSelection;
begin
  SelectedObj := nil;
  UpdateGizmo;
  if Assigned(OnSelectionLost) then
    OnSelectionLost(Self);
end;

procedure TVKGizmo.SetViewer(const Value: TVKSceneViewer);
begin
  if FViewer <> Value then
  begin
    if FViewer <> nil then
      FViewer.RemoveFreeNotification(Self);
    FViewer := Value;
    if FViewer <> nil then
      FViewer.FreeNotification(Self);
  end;
end;

procedure TVKGizmo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = OpRemove then
  begin
    if AComponent = FViewer then
      FViewer := nil;
    if AComponent = FRootGizmo then
      FRootGizmo := nil;
  end;

  if FUndoHistory <> nil then
    FUndoHistory.Notification(AComponent, Operation);
end;

procedure TVKGizmoUndoItem.AssignFromObject(const AObject
  : TVKCustomSceneObject);
begin
  SetEffectedObject(AObject);
  SetOldMatrix(AObject.Matrix);
  if AObject is TVKFreeForm then
  begin
    FOldAutoScaling.Assign(TVKFreeForm(AObject).AutoScaling);
  end;
  FOldLibMaterialName := AObject.Material.LibMaterialName;
end;

constructor TVKGizmoUndoItem.Create(AOwner: TCollection);
begin
  inherited;
  FOldAutoScaling := TVKCoordinates.CreateInitialized(Self,
    NullHmgVector, CsPoint);
end;

destructor TVKGizmoUndoItem.Destroy;
begin
  FOldAutoScaling.Free;
  inherited;
end;

procedure TVKGizmoUndoItem.DoUndo;
begin
  FEffectedObject.Matrix := FOldMatr;
  if FEffectedObject is TVKFreeForm then
    TVKFreeForm(FEffectedObject).AutoScaling.Assign(FOldAutoScaling);
  FEffectedObject.Material.LibMaterialName := FOldLibMaterialName;
end;

function TVKGizmoUndoItem.GetGizmo: TVKGizmo;
begin
  if GetParent <> nil then
    Result := GetPArent.GetParent
  else
    Result := nil;
end;

function TVKGizmoUndoItem.GetParent: TVKGizmoUndoCollection;
begin
  Result := TVKGizmoUndoCollection(GetOwner);
end;

procedure TVKGizmoUndoItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = OpRemove then
  begin
    if AComponent = FEffectedObject then
      FEffectedObject := nil;
  end;
end;

procedure TVKGizmoUndoItem.SetEffectedObject(const Value: TVKCustomSceneObject);
begin
  if FEffectedObject <> nil then
    FEffectedObject.RemoveFreeNotification(GetGizmo);
  FEffectedObject := Value;
  if FEffectedObject <> nil then
    FEffectedObject.FreeNotification(GetGizmo);
end;

procedure TVKGizmoUndoItem.SetOldAutoScaling(const Value: TVKCoordinates);
begin
  FOldAutoScaling.Assign(Value);
end;

procedure TVKGizmoUndoItem.SetOldMatrix(const Value: TMatrix);
begin
  FOldMatrix := Value;
end;

{ TVKGizmoUndoCollection }

function TVKGizmoUndoCollection.Add: TVKGizmoUndoItem;
begin
  Result := TVKGizmoUndoItem(inherited Add);
end;

function TVKGizmoUndoCollection.GetItems(const Index: Integer)
  : TVKGizmoUndoItem;
begin
  Result := TVKGizmoUndoItem(inherited GetItem(Index));
end;

function TVKGizmoUndoCollection.GetParent: TVKGizmo;
begin
  Result := TVKGizmo(GetOwner);
end;

procedure TVKGizmoUndoCollection.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  if Count <> 0 then
    for I := 0 to Count - 1 do
      GetItems(I).Notification(AComponent, Operation);
end;

procedure TVKGizmoUndoCollection.RemoveByObject(const AObject
  : TVKCustomSceneObject);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if GetItems(I).FEffectedObject = AObject then
      GetItems(I).Free;
end;

procedure TVKGizmoUndoCollection.SetItems(const Index: Integer;
  const Value: TVKGizmoUndoItem);
begin
  GetItems(Index).Assign(Value);
end;

procedure TVKGizmo.SetSelectedObj(const Value: TVKBaseSceneObject);
begin
  if FSelectedObj <> Value then
  begin
    FSelectedObj := Value;

    if Value <> nil then
    begin
      SetVisible(True);
      UpdateVisibleInfoLabels();
      UpdateGizmo();
    end
    else
    begin
      LooseSelection();
      SetVisible(False);
    end;
  end;
end;

end.
