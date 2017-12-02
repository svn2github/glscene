//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{ 
  Invisible component for helping to Move, Rotate and Scale an Object
  under GLScene (usefull for an Editor).
}
unit VXS.Gizmo;

interface

{$I VXScene.inc}

uses
  System.Classes,
  System.SysUtils,
  
  VXS.PersistentClasses,
  VXS.Scene,
  VXS.Color,
  VXS.Objects,
  VXS.VectorGeometry,
  VXS.Material,
  VXS.Strings,
  VXS.GeomObjects,
  VXS.BitmapFont,
  VXS.Win64Viewer,
  VXS.VectorFileObjects,
  VXS.CrossPlatform,
  VXS.Coordinates,
  VXS.RenderContextInfo,
  VXS.State,
  VXS.Selection,
  VXS.VectorTypes;

type
  TVXGizmoUndoCollection = class;
  TVXGizmo = class;

  TVXGizmoUndoItem = class(TCollectionItem)
  private
    FOldLibMaterialName: string;
    FOldAutoScaling: TVXCoordinates;
    FEffectedObject: TVXCustomSceneObject;
    FOldMatr: TMatrix;
    FOldMatrix: TMatrix;
    procedure SetEffectedObject(const Value: TVXCustomSceneObject);
    procedure SetOldAutoScaling(const Value: TVXCoordinates);
    procedure SetOldMatrix(const Value: TMatrix);
  protected
    procedure DoUndo; virtual;
    function GetParent: TVXGizmoUndoCollection;
    function GetGizmo: TVXGizmo;
  public
    constructor Create(AOwner: TCollection); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); virtual;
    procedure AssignFromObject(const AObject: TVXCustomSceneObject);
    // TODO: create a special type for Matrix.
    property OldMatrix: TMatrix read FOldMatrix write SetOldMatrix;
  published
    property EffectedObject: TVXCustomSceneObject read FEffectedObject
      write SetEffectedObject;
    property OldAutoScaling: TVXCoordinates read FOldAutoScaling
      write SetOldAutoScaling;
    property OldLibMaterialName: string read FOldLibMaterialName
      write FOldLibMaterialName;
  end;

  TVXGizmoUndoCollection = class(TOwnedCollection)
  private
    function GetItems(const Index: Integer): TVXGizmoUndoItem;
    procedure SetItems(const Index: Integer; const Value: TVXGizmoUndoItem);
  protected
    function GetParent: TVXGizmo;
  public
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    procedure RemoveByObject(const AObject: TVXCustomSceneObject);
    function Add: TVXGizmoUndoItem;
    property Items[const Index: Integer]: TVXGizmoUndoItem read GetItems
      write SetItems; default;
  end;

  TVXGizmoElement = (geMove, geRotate, geScale, geAxisLabel, geObjectInfos,
    geBoundingBox);
  TVXGizmoElements = set of TVXGizmoElement;

  TVXGizmoVisibleInfoLabel = (vliName, vliOperation, vliCoords);
  TVXGizmoVisibleInfoLabels = set of TVXGizmoVisibleInfoLabel;

  TVXGizmoAxis = (gaNone, gaX, gaY, gaZ, gaXY, gaXZ, gaYZ);

  TVXGizmoOperation = (gopMove, gopRotate, gopScale, gopNone, gpMoveGizmo,
    gpRotateGizmo);

  TVXGizmoAcceptEvent = procedure(Sender: TObject; var Obj: TVXBaseSceneObject;
    var Accept: Boolean; var Dimensions: TVector) of object;
  TVXGizmoUpdateEvent = procedure(Sender: TObject; Obj: TVXBaseSceneObject;
    Axis: TVXGizmoAxis; Operation: TVXGizmoOperation; var Vector: TVector)
    of object;

  TVXGizmoPickMode = (pmGetPickedObjects, pmRayCast);

  TVXGizmoRayCastHitData = class(TPersistent)
  public
    Obj: TVXBaseSceneObject;
    Point: TVector;
  end;

  TVXGizmoPickCube = class(TVXCube)
  end;

  TVXGizmoPickTorus = class(TVXTorus)
  end;

  TVXGizmo = class(TComponent)
  private
    _GZObaseGizmo: TVXBaseSceneObject;
    _GZOBoundingcube: TVXCube;
    _GZOrootHelpers: TVXBaseSceneObject;
    _GZOrootLines: TVXBaseSceneObject;
    _GZOrootTorus: TVXBaseSceneObject;
    _GZOrootCubes: TVXBaseSceneObject;
    _GZORootAxisLabel: TVXBaseSceneObject;
    _GZORootVisibleInfoLabels: TVXBaseSceneObject;
    _GZOlineX, _GZOlineY, _GZOlineZ, _GZOplaneXY, _GZOplaneXZ,
      _GZOplaneYZ: TVXLines; // For Move
    _GZOTorusX, _GZOTorusY, _GZOTorusZ: TVXGizmoPickTorus; // For Rotate
    _GZOCubeX, _GZOCubeY, _GZOCubeZ: TVXGizmoPickCube; // For Scale
    _GZOAxisLabelX, _GZOAxisLabelY, _GZOAxisLabelZ: TVXFlatText;
    _GZOVisibleInfoLabels: TVXFlatText;
    FRootGizmo: TVXBaseSceneObject;
    FSelectedObj: TVXBaseSceneObject;
    // FLastOperation,
    FOperation: TVXGizmoOperation;
    FSelAxis: TVXGizmoAxis;
    FBoundingBoxColor: TVXColor;
    FSelectedColor: TVXColor;
    FVisibleInfoLabelsColor: TVXColor;
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
    FViewer: TVXSceneViewer;
    FGizmoElements: TVXGizmoElements;
    FVisibleVisibleInfoLabels: TVXGizmoVisibleInfoLabels;
    FExcludeObjectsList: TStrings;
    Moving: Boolean;
    Mx, My: Integer;
    Rx, Ry: Integer;
    dglEnable, dglDisable, dgtEnable, dgtDisable, dgcEnable, dgcDisable,
      dglaEnable, dglaDisable, dgliEnable, dgliDisable: TVXDirectOpenVX;
    LastMousePos: TVector;
    ObjDimensions: TVector;
    FOnBeforeSelect: TVXGizmoAcceptEvent;
    FOnBeforeUpdate: TVXGizmoUpdateEvent;
    FOnSelectionLost: TNotifyEvent;
    FScaleCoef: Single;
    FGizmoThickness: Single;
    FPickMode: TVXGizmoPickMode;
    FInternalRaycastHitData: TList;
    FUndoHistory:  TVXGizmoUndoCollection;
    FLabelFont: TVXCustomBitmapFont;
    procedure SetRootGizmo(const AValue: TVXBaseSceneObject);
    procedure SetGizmoElements(const AValue: TVXGizmoElements);
    procedure SetGizmoVisibleInfoLabels(const AValue
      : TVXGizmoVisibleInfoLabels);
    procedure SetBoundingBoxColor(const AValue: TVXColor);
    procedure SetSelectedColor(const AValue: TVXColor);
    procedure SetVisibleInfoLabelsColor(const AValue: TVXColor);
    procedure SetExcludeObjectsList(const AValue: TStrings);
    procedure DirectGLDisable(Sender: TObject; var Rci: TVXRenderContextInfo);
    procedure DirectGLEnable(Sender: TObject; var Rci: TVXRenderContextInfo);
    function MouseWorldPos(const X, Y: Integer): TVector;
    function CheckObjectInExcludeList(const Obj: TVXBaseSceneObject): Boolean;
    procedure UpdateVisibleInfoLabels;
    procedure SetGizmoThickness(const Value: Single);
    function InternalGetPickedObjects(const X1, Y1, X2, Y2: Integer;
      const GuessCount: Integer = 8): TVXPickList;
    procedure ClearInternalRaycastHitData;
    procedure SetViewer(const Value: TVXSceneViewer);
    procedure SetLabelFont(const Value: TVXCustomBitmapFont);
    procedure SetSelectedObj(const Value: TVXBaseSceneObject);
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
    function GetPickedObjectPoint(const Obj: TVXBaseSceneObject): TVector;
    procedure LooseSelection; virtual;
    procedure UndoAdd(const AObject: TVXCustomSceneObject);
    property RootGizmo: TVXBaseSceneObject read FRootGizmo write SetRootGizmo;
    // --------------------------------------------------------------------
  published
    property Viewer: TVXSceneViewer read FViewer write SetViewer;
    property GizmoElements: TVXGizmoElements read FGizmoElements
      write SetGizmoElements;
    property BoundingBoxColor: TVXColor read FBoundingBoxColor
      write SetBoundingBoxColor;
    property SelectedColor: TVXColor read FSelectedColor write SetSelectedColor;
    property SelAxis: TVXGizmoAxis read FSelAxis write FSelAxis;
    property ForceAxis: Boolean read FForceAxis write FForceAxis;
    property SelectedObj: TVXBaseSceneObject read FSelectedObj
      write SetSelectedObj;
    property Operation: TVXGizmoOperation read FOperation write FOperation;
    property ForceOperation: Boolean read FForceOperation write FForceoperation;
    property ForceUniformScale: Boolean read FForceUniformScale
      write FForceUniformScale;
    property ExcludeObjects: Boolean read FExcludeObjects write FExcludeObjects;
    property ExcludeObjectsList: TStrings read FExcludeObjectsList
      write SetExcludeObjectsList;
    property VisibleInfoLabels: TVXGizmoVisibleInfoLabels
      read FVisibleVisibleInfoLabels write SetGizmoVisibleInfoLabels;
    property VisibleInfoLabelsColor: TVXColor read FVisibleInfoLabelsColor
      write SetVisibleInfoLabelsColor;
    property AutoZoom: Boolean read FAutoZoom write FAutoZoom;
    property AutoZoomFactor: Single read FAutoZoomFactor write FAutoZoomFactor;
    property ZoomFactor: Single read FZoomFactor write FZoomFactor;
    property MoveCoef: Single read FMoveCoef write FMoveCoef;
    property RotationCoef: Single read FRotationCoef write FRotationCoef;
    property ScaleCoef: Single read FScaleCoef write FScaleCoef;
    property NoZWrite: Boolean read FNoZWrite write FNoZWrite;
    property GizmoThickness: Single read FGizmoThickness
      write SetGizmoThickness;
    { Indicates whether the gizmo is enabled or not.
      WARNING: When loading/editing (possibly whenever a structureChanged
      call is made) a model, sometimes the gizmo will trigger a
      bug if the mouse is inside the Viewer. To prevent that,
      remember to disable the gizmo before loading, then process windows
      messages (i.e. application.processMessage) and then enable the gizmo
      again. }
    { Warning Enable is ReadOnly property if you set to False, Gizmo is not Hidden
      use Visible instead if you want to Hide, if you want to Hide but keep enabled
      see the VisibleGizmo property }
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property LabelFont: TVXCustomBitmapFont read FLabelFont write SetLabelFont
      default nil;
    property OnBeforeSelect: TVXGizmoAcceptEvent read FOnBeforeSelect
      write FOnBeforeSelect;
    property OnSelectionLost: TNotifyEvent read FOnSelectionLost
      write FOnSelectionLost;
    { Called before an Update is applied. The "vector" parameter is the difference
      that will be applied to the object, according to the axis and
      operation selected. }
    property OnBeforeUpdate: TVXGizmoUpdateEvent read FOnBeforeUpdate
      write FOnBeforeUpdate;
    property PickMode: TVXGizmoPickMode read FPickMode write FPickMode
      default PmGetPickedObjects;
  end;

//=========================================================
implementation
//=========================================================

procedure RotateAroundArbitraryAxis(const AnObject: TVXBaseSceneObject;
  const Axis, Origin: TAffineVector; const Angle: Single);
var
  M, M1, M2, M3: TMatrix;
begin
  M1 := CreateTranslationMatrix(VectorNegate(Origin));
  M2 := CreateRotationMatrix(Axis, Angle * PI / 180);
  M3 := CreateTranslationMatrix(Origin);
  M := MatrixMultiply(M1, M2);
  M := MatrixMultiply(M, M3);
  AnObject.SetMatrix(MatrixMultiply(AnObject.Matrix^, M));

  // Just a workarround to Update angles...
  AnObject.Roll(0);
  AnObject.Pitch(0);
  AnObject.Turn(0);
end;

// ------------------------------------------------------------------------------

procedure TVXGizmo.ClearInternalRaycastHitData;
var
  T: Integer;
begin
  for T := FInternalRaycastHitData.Count - 1 downto 0 do
  begin
    TVXGizmoRayCastHitData(FInternalRaycastHitData[T]).Free;
  end;
  FInternalRaycastHitData.Clear;
end;

constructor TVXGizmo.Create(AOwner: TComponent);
var
  Cub: TVXCube;
begin
  inherited Create(AOwner);
  FUndoHistory := TVXGizmoUndoCollection.Create(Self, TVXGizmoUndoItem);
  FPickMode := PmGetPickedObjects;
  PickableObjectsWithRayCast := TList.Create;
  FRotationCoef := 1;
  FMoveCoef := 0.1;
  FScaleCoef := 0.1;
  FGizmoThickness := 1;

  FInternalRaycastHitData := TList.Create;
  FBoundingBoxColor := TVXColor.Create(Self);
  FBoundingBoxColor.Color := ClrWhite;
  FBoundingBoxColorChanged := False;
  FSelectedColor := TVXColor.Create(Self);
  FSelectedColor.Color := ClrYellow;
  FVisibleInfoLabelsColor := TVXColor.Create(Self);
  FVisibleInfoLabelsColor.Color := ClrYellow;
  FVisibleInfoLabelsColorChanged := False;

  _GZObaseGizmo := TVXDummyCube.Create(Self);
  _GZORootHelpers := TVXDummyCube(_GZObaseGizmo.AddNewChild(TVXDummyCube));
  _GZOBoundingcube := TVXCube(_GZORootHelpers.AddNewChild(TVXCube));

  _GZORootLines := _GZORootHelpers.AddNewChild(TVXDummyCube);
  _GZORootTorus := _GZORootHelpers.AddNewChild(TVXDummyCube);
  _GZORootCubes := _GZORootHelpers.AddNewChild(TVXDummyCube);
  _GZORootAxisLabel := _GZORootHelpers.AddNewChild(TVXDummyCube);
  _GZORootVisibleInfoLabels := _GZORootHelpers.AddNewChild(TVXDummyCube);

  DglDisable := TVXDirectOpenVX(_GZORootLines.AddNewChild(TVXDirectOpenVX));
  DglDisable.OnRender := DirectGlDisable;
  DgtDisable := TVXDirectOpenVX(_GZORootTorus.AddNewChild(TVXDirectOpenVX));
  DgtDisable.OnRender := DirectGlDisable;
  DgcDisable := TVXDirectOpenVX(_GZORootCubes.AddNewChild(TVXDirectOpenVX));
  DgcDisable.OnRender := DirectGlDisable;
  DglaDisable := TVXDirectOpenVX
    (_GZORootAxisLabel.AddNewChild(TVXDirectOpenVX));
  DglaDisable.OnRender := DirectGlDisable;
  DgliDisable := TVXDirectOpenVX(_GZORootVisibleInfoLabels.AddNewChild
    (TVXDirectOpenVX));
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

  _GZOlinex := TVXLines(_GZORootLines.AddnewChild(TVXLines));
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
    Cub := TVXGizmoPickCube(AddNewChild(TVXGizmoPickCube));
    Cub.Up.SetVector(1, 0, 0);
    Cub.CubeWidth := 0.1;
    Cub.CubeHeight := 1;
    Cub.CubeDepth := 0.1;
    Cub.Position.SetPoint(0.5, 0, 0);
    Cub.Visible := False;
  end;

  _GZOliney := TVXLines(_GZORootLines.AddnewChild(TVXLines));
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
    Cub := TVXGizmoPickCube(AddNewChild(TVXGizmoPickCube));
    Cub.Up.SetVector(0, 1, 0);
    Cub.CubeWidth := 0.1;
    Cub.CubeHeight := 1;
    Cub.CubeDepth := 0.1;
    Cub.Position.SetPoint(0, 0.5, 0);
    Cub.Visible := False;
  end;

  _GZOlinez := TVXLines(_GZORootLines.AddnewChild(TVXLines));
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
    Cub := TVXGizmoPickCube(AddNewChild(TVXGizmoPickCube));
    Cub.Up.SetVector(0, 0, 1);
    Cub.CubeWidth := 0.1;
    Cub.CubeHeight := 1;
    Cub.CubeDepth := 0.1;
    Cub.Position.SetPoint(0, 0, 0.5);
    Cub.Visible := False;
  end;

  _GZOplaneXY := TVXLines(_GZORootLines.AddnewChild(TVXLines));
  with _GZOplaneXY do
  begin
    LineWidth := 3;
    Options := [LoUseNodeColorForLines];
    NodesAspect := LnaInvisible;
    SplineMode := LsmSegments;
    AddNode(0.8, 1, 0);
    TVXLinesNode(Nodes[0]).Color.Color := clrRed;
    AddNode(1, 1, 0);
    TVXLinesNode(Nodes[1]).Color.Color := clrRed;
    AddNode(1, 1, 0);
    TVXLinesNode(Nodes[2]).Color.Color := clrLime;
    AddNode(1, 0.8, 0);
    TVXLinesNode(Nodes[3]).Color.Color := clrLime;
    // Raycast pickable object
    Cub := TVXGizmoPickCube(AddNewChild(TVXGizmoPickCube));
    Cub.Up.SetVector(1, 0, 0);
    Cub.CubeWidth := 0.2;
    Cub.CubeHeight := 0.2;
    Cub.CubeDepth := 0.1;
    Cub.Position.SetPoint(0.9, 0.9, 0);
    Cub.Visible := False;
  end;

  _GZOplaneXZ := TVXLines(_GZORootLines.AddnewChild(TVXLines));
  with _GZOplaneXZ do
  begin
    LineWidth := 3;
    Options := [LoUseNodeColorForLines];
    NodesAspect := LnaInvisible;
    SplineMode := LsmSegments;
    AddNode(1, 0, 0.8);
    TVXLinesNode(Nodes[0]).Color.Color := clrBlue;
    AddNode(1, 0, 1);
    TVXLinesNode(Nodes[1]).Color.Color := clrBlue;
    AddNode(1, 0, 1);
    TVXLinesNode(Nodes[2]).Color.Color := clrRed;
    AddNode(0.8, 0, 1);
    TVXLinesNode(Nodes[3]).Color.Color := clrRed;
    // Raycast pickable object
    Cub := TVXGizmoPickCube(AddNewChild(TVXGizmoPickCube));
    Cub.Up.SetVector(1, 0, 0);
    Cub.CubeWidth := 0.1;
    Cub.CubeHeight := 0.2;
    Cub.CubeDepth := 0.2;
    Cub.Position.SetPoint(0.9, 0, 0.9);
    Cub.Visible := False;
  end;

  _GZOplaneYZ := TVXLines(_GZORootLines.AddnewChild(TVXLines));
  with _GZOplaneYZ do
  begin
    LineWidth := 3;
    Options := [LoUseNodeColorForLines];
    NodesAspect := LnaInvisible;
    SplineMode := LsmSegments;
    AddNode(0, 0.8, 1);
    TVXLinesNode(Nodes[0]).Color.Color := clrLime;
    AddNode(0, 1, 1);
    TVXLinesNode(Nodes[1]).Color.Color := clrLime;
    AddNode(0, 1, 1);
    TVXLinesNode(Nodes[2]).Color.Color := clrBlue;
    AddNode(0, 1, 0.8);
    TVXLinesNode(Nodes[3]).Color.Color := clrBlue;
    // Raycast pickable object
    Cub := TVXGizmoPickCube(AddNewChild(TVXGizmoPickCube));
    Cub.Up.SetVector(0, 0, 1);
    Cub.CubeWidth := 0.2;
    Cub.CubeHeight := 0.2;
    Cub.CubeDepth := 0.1;
    Cub.Position.SetPoint(0, 0.9, 0.9);
    Cub.Visible := False;
  end;

  _GZOTorusX := TVXGizmoPickTorus(_GZORootTorus.AddnewChild(TVXGizmoPickTorus));
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

  _GZOTorusY := TVXGizmoPickTorus(_GZORootTorus.AddnewChild(TVXGizmoPickTorus));
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

  _GZOTorusZ := TVXGizmoPickTorus(_GZORootTorus.AddnewChild(TVXGizmoPickTorus));
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

  _GZOCubeX := TVXGizmoPickCube(_GZORootCubes.AddnewChild(TVXGizmoPickCube));
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

  _GZOCubeY := TVXGizmoPickCube(_GZORootCubes.AddnewChild(TVXGizmoPickCube));
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

  _GZOCubeZ := TVXGizmoPickCube(_GZORootCubes.AddnewChild(TVXGizmoPickCube));
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

  _GZOAxisLabelX := TVXFlatText(_GZORootAxisLabel.AddNewChild(TVXFlatText));
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

  _GZOAxisLabelY := TVXFlatText(_GZORootAxisLabel.AddNewChild(TVXFlatText));
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

  _GZOAxisLabelZ := TVXFlatText(_GZORootAxisLabel.AddNewChild(TVXFlatText));
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
    TVXFlatText(_GZORootVisibleInfoLabels.AddNewChild(TVXFlatText));
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

  DglEnable := TVXDirectOpenVX(_GZORootLines.AddNewChild(TVXDirectOpenVX));
  DglEnable.OnRender := DirectGlEnable;
  DgtEnable := TVXDirectOpenVX(_GZORootTorus.AddNewChild(TVXDirectOpenVX));
  DgtEnable.OnRender := DirectGlEnable;
  DgcEnable := TVXDirectOpenVX(_GZORootCubes.AddNewChild(TVXDirectOpenVX));
  DgcEnable.OnRender := DirectGlEnable;
  DglaEnable := TVXDirectOpenVX(_GZORootAxisLabel.AddNewChild(TVXDirectOpenVX));
  DglaEnable.OnRender := DirectGlEnable;
  DgliEnable := TVXDirectOpenVX(_GZORootVisibleInfoLabels.AddNewChild
    (TVXDirectOpenVX));
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

destructor TVXGizmo.Destroy;
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

procedure TVXGizmo.SetVisible(const AValue: Boolean);
begin
  _GZObaseGizmo.Visible := AValue;
end;

procedure TVXGizmo.SetGizmoElements(const AValue: TVXGizmoElements);
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

procedure TVXGizmo.SetBoundingBoxColor(const AValue: TVXColor);
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

procedure TVXGizmo.SetSelectedColor(const AValue: TVXColor);
begin
  if AValue <> FSelectedColor then
  begin
    FSelectedColor.Color := AValue.Color;
  end;
end;

procedure TVXGizmo.SetVisibleInfoLabelsColor(const AValue: TVXColor);
begin
  // Bug Here New Color is not Updated
  if AValue <> FSelectedColor then
  begin
    FVisibleInfoLabelsColor.Color := AValue.Color;
    _GZOVisibleInfoLabels.ModulateColor.Color := AValue.Color;
    FVisibleInfoLabelsColorChanged := True;
  end;
end;

procedure TVXGizmo.SetGizmoVisibleInfoLabels(const AValue
  : TVXGizmoVisibleInfoLabels);
begin
  if AValue <> FVisibleVisibleInfoLabels then
  begin
    FVisibleVisibleInfoLabels := AValue;
    if not(CsDesigning in ComponentState) then
      UpdateGizmo;
  end;
end;

procedure TVXGizmo.UndoAdd(const AObject: TVXCustomSceneObject);
begin
  if AObject <> nil then
  begin
    FUndoHistory.Add.AssignFromObject(AObject)
  end;
end;

procedure TVXGizmo.SetRootGizmo(const AValue: TVXBaseSceneObject);
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

procedure TVXGizmo.SetExcludeObjectsList(const AValue: TStrings);
begin
  FExcludeObjectsList.Clear;
  FExcludeObjectsList.AddStrings(AValue);
end;

procedure TVXGizmo.SetGizmoThickness(const Value: Single);
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

procedure TVXGizmo.DirectGlDisable(Sender: TObject;
  var Rci: TVXRenderContextInfo);
begin
  if FNoZWrite then
    Rci.VXStates.Disable(StDepthTest);
end;

procedure TVXGizmo.SetLabelFont(const Value: TVXCustomBitmapFont);
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

procedure TVXGizmo.DirectGlEnable(Sender: TObject; var Rci: TVXRenderContextInfo);
begin
  if FNoZWrite then
    Rci.VXStates.Enable(StDepthTest);
end;

function TVXGizmo.GetPickedObjectPoint(const Obj: TVXBaseSceneObject): TVector;
var
  T: Integer;
  R: TVXGizmoRayCastHitData;
begin
  for T := 0 to FInternalRaycastHitData.Count - 1 do
  begin
    R := TVXGizmoRayCastHitData(FInternalRaycastHitData[T]);
    if R.Obj = Obj then
    begin
      Result := R.Point;
      Break;
    end;
  end;
end;

function TVXGizmo.InternalGetPickedObjects(const X1, Y1, X2, Y2: Integer;
  const GuessCount: Integer): TVXPickList;
var
  T: Integer;
  RayStart, RayVector, IPoint, INormal: TVector;
  O: TVXBaseSceneObject;
  Dist: Single;
  HitData: TVXGizmoRayCastHitData;

  procedure AddGizmosToPicklListRecurse(const Root: TVXBaseSceneObject);
  var
    U: Integer;
  begin
    for U := 0 to Root.Count - 1 do
    begin
      if ((Root[U] is TVXGizmoPickTorus) or (Root[U] is TVXGizmoPickCube)) then
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
        Result := TVXPickList.Create(PsMinDepth);
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
          O := TVXBaseSceneObject(PickableObjectsWithRayCast[T]);
          if (O.RayCastIntersect(RayStart, RayVector, @IPoint, @INormal)) and
            (VectorDotProduct(RayVector, INormal) < 0) then
          begin
            try
              Dist := VectorLength(VectorSubtract(IPoint, RayStart));
              Result.AddHit(O, nil, Dist, 0);
              HitData := TVXGizmoRayCastHitData.Create;
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
      Assert(False, strErrorEx + strUnknownType);
    end;

  end;
end;

procedure TVXGizmo.Loaded;
begin
  inherited;
  SetGizmoThickness(GizmoThickness);
end;

// ------------------------------------------------------------------------------
procedure TVXGizmo.UpdateVisibleInfoLabels;
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

function TVXGizmo.CheckObjectInExcludeList
  (const Obj: TVXBaseSceneObject): Boolean;
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

function TVXGizmo.MouseWorldPos(const X, Y: Integer): TVector;
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
          SelectedObj.AbsolutePosition.Y, Result) then
          MakeVector(Result, X / 5, 0, 0);

      GaY:
        if not Viewer.Buffer.ScreenVectorIntersectWithPlaneYZ(V,
          SelectedObj.AbsolutePosition.X, Result) then
          MakeVector(Result, 0, InvertedY / 5, 0);

      GaZ:
        if not Viewer.Buffer.ScreenVectorIntersectWithPlaneYZ(V,
          SelectedObj.AbsolutePosition.X, Result) then
          MakeVector(Result, 0, 0, -InvertedY / 5);

      GaXY:
        begin
          Viewer.Buffer.ScreenVectorIntersectWithPlaneXY(V,
            SelectedObj.AbsolutePosition.Z, Result);
        end;
      GaXZ:
        begin
          Viewer.Buffer.ScreenVectorIntersectWithPlaneXZ(V,
            SelectedObj.AbsolutePosition.Y, Result);
        end;
      GaYZ:
        begin
          Viewer.Buffer.ScreenVectorIntersectWithPlaneYZ(V,
            SelectedObj.AbsolutePosition.X, Result);
        end;
    end;

  end
  else
    SetVector(Result, NullVector);
end;

procedure TVXGizmo.ViewerMouseMove(const X, Y: Integer);
var
  PickList: TVXPickList;
  MousePos: TVector;

  function IndexOf(Obj: TVXBaseSceneObject): Integer;
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

  function LightLine(const Line: TVXLines; const Dark: TVector;
    const Axis: TVXGizmoAxis; AlterStyle: Boolean = False): Boolean;
  var
    PickObj: TVXBaseSceneObject;
  begin
    case FPickMode of
      PmGetPickedObjects:
        PickObj := Line;
      PmRayCast:
        PickObj := Line;
    else
      begin
        PickObj := nil;
        Assert(False, strErrorEx + strUnknownType);
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

  function LightTorus(const Torus: TVXGizmoPickTorus; const Dark: TVector;
    const Axis: TVXGizmoAxis; AlterStyle: Boolean = False): Boolean;
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

  function LightCube(const Cube: TVXCube; const Dark: TVector;
    const Axis: TVXGizmoAxis; AlterStyle: Boolean = False): Boolean;
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
          MakeVector(Vec1, QuantizedMousePos.X, 0, 0);
          MakeVector(Vec2, QuantizedMousePos2.X, 0, 0);
        end;
      GaY:
        begin
          MakeVector(Vec1, 0, QuantizedMousePos.Y, 0);
          MakeVector(Vec2, 0, QuantizedMousePos2.Y, 0);
        end;
      GaZ:
        begin
          MakeVector(Vec1, 0, 0, QuantizedMousePos.Z);
          MakeVector(Vec2, 0, 0, QuantizedMousePos2.Z);
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
    Vec1.X := 0;
    Vec1.Y := 0;
    if Abs(X - Rx) >= RotationCoef then
    begin
      if RotationCoef > 1 then
        Vec1.X := RotationCoef * (Round((X - Rx) / (RotationCoef)))
      else
        Vec1.X := RotationCoef * (X - Rx);
      Rx := X;
    end;
    if Abs(Y - Ry) >= RotationCoef then
    begin
      if RotationCoef > 1 then
        Vec1.Y := RotationCoef * (Round((Y - Ry) / (RotationCoef)))
      else
        Vec1.Y := RotationCoef * (Y - Ry);
      Ry := Y;
    end;

    Vec1.Z := 0;
    Vec1.W := 0;
    if Assigned(OnBeforeUpdate) then
      OnBeforeUpdate(Self, SelectedObj, SelAxis, Operation, Vec1);

    Pmat := SelectedObj.Parent.InvAbsoluteMatrix;
    SetVector(Pmat.W, NullHmgPoint);
    case SelAxis of
      GaX:
        begin
          RotV := VectorTransform(XVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.Y);
        end;
      GaY:
        begin
          RotV := VectorTransform(YVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.X);
        end;
      GaZ:
        begin
          RotV := VectorTransform(ZVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.Y);
        end;
      GaXY:
        begin
          RotV := VectorTransform(XVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.Y);
          RotV := VectorTransform(YVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.X);
        end;
      GaXZ:
        begin
          RotV := VectorTransform(XVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.Y);
          RotV := VectorTransform(ZVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.X);
        end;
      GaYZ:
        begin
          RotV := VectorTransform(YVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.Y);
          RotV := VectorTransform(ZVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.X);
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
            MakeVector(Vec1, QuantizedMousePos.X, QuantizedMousePos.X,
              QuantizedMousePos.X);
            MakeVector(Vec2, QuantizedMousePos2.X, QuantizedMousePos2.X,
              QuantizedMousePos2.X);
          end
          else
          begin
            MakeVector(Vec1, QuantizedMousePos.X, 0, 0);
            MakeVector(Vec2, QuantizedMousePos2.X, 0, 0);
          end;

        end;

      GaY:
        begin
          if FForceUniformScale then
          begin
            MakeVector(Vec1, QuantizedMousePos.Y, QuantizedMousePos.Y,
              QuantizedMousePos.Y);
            MakeVector(Vec2, QuantizedMousePos2.Y, QuantizedMousePos2.Y,
              QuantizedMousePos2.Y);
          end
          else
          begin
            MakeVector(Vec1, 0, QuantizedMousePos.Y, 0);
            MakeVector(Vec2, 0, QuantizedMousePos2.Y, 0);
          end;
        end;

      GaZ:
        begin
          if FForceUniformScale then
          begin
            MakeVector(Vec1, QuantizedMousePos.Z, QuantizedMousePos.Z,
              QuantizedMousePos.Z);
            MakeVector(Vec2, QuantizedMousePos2.Z, QuantizedMousePos2.Z,
              QuantizedMousePos2.Z);
          end
          else
          begin
            MakeVector(Vec1, 0, 0, QuantizedMousePos.Z);
            MakeVector(Vec2, 0, 0, QuantizedMousePos2.Z);
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

procedure TVXGizmo.ViewerMouseDown(const X, Y: Integer);
var
  Pick: TVXPickList;
  I: Integer;
  Accept: Boolean;
  Dimensions: TVector;
  GotPick: Boolean;
  PickedObj: TVXBaseSceneObject;
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
          if (_GZOrootLines.IndexOfChild(TVXBaseSceneObject(Pick.Hit[I])) > -1)
            or (_GZOrootTorus.IndexOfChild(TVXBaseSceneObject(Pick.Hit[I])) >
            -1) or (_GZOrootCubes.IndexOfChild(TVXBaseSceneObject(Pick.Hit[I]))
            > -1) then
            GotPick := True;
      end;

    PmRayCast:
      begin
        for I := 0 to Pick.Count - 1 do
        begin
          if (Pick.Hit[I] is TVXGizmoPickCube) or
            (Pick.Hit[I] is TVXGizmoPickTorus) then
            GotPick := True;
        end;
      end;
  else
    begin
      Assert(False, strErrorEx + strUnknownType);
    end;

  end;

  if not GotPick then
  begin
    for I := 0 to Pick.Count - 1 do

      if (Pick.Hit[I] <> _GZOBoundingcube) and (Pick.Hit[I] <> _GZOAxisLabelX)
        and (Pick.Hit[I] <> _GZOAxisLabelY) and (Pick.Hit[I] <> _GZOAxisLabelZ)
        and (Pick.Hit[I] <> _GZOVisibleInfoLabels) and
        not(CheckObjectInExcludeList(TVXBaseSceneObject(Pick.Hit[I]))) then
      begin
        Accept := True;
        PickedObj := TVXBaseSceneObject(Pick.Hit[I]);
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

procedure TVXGizmo.ViewerMouseUp(const X, Y: Integer);
begin
  Moving := False;
end;

// ------------------------------------------------------------------------------

procedure TVXGizmo.UpdateGizmo;
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

  _GZOBoundingcube.SetMatrix(SelectedObj.AbsoluteMatrix);
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

procedure TVXGizmo.UpdateGizmo(const NewDimensions: TVector);
begin
  ObjDimensions := NewDimensions;
  UpdateGizmo;
end;

procedure TVXGizmo.LooseSelection;
begin
  SelectedObj := nil;
  UpdateGizmo;
  if Assigned(OnSelectionLost) then
    OnSelectionLost(Self);
end;

procedure TVXGizmo.SetViewer(const Value: TVXSceneViewer);
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

procedure TVXGizmo.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TVXGizmoUndoItem.AssignFromObject(const AObject
  : TVXCustomSceneObject);
begin
  SetEffectedObject(AObject);
  SetOldMatrix(AObject.Matrix^);
  if AObject is TVXFreeForm then
  begin
    FOldAutoScaling.Assign(TVXFreeForm(AObject).AutoScaling);
  end;
  FOldLibMaterialName := AObject.Material.LibMaterialName;
end;

constructor TVXGizmoUndoItem.Create(AOwner: TCollection);
begin
  inherited;
  FOldAutoScaling := TVXCoordinates.CreateInitialized(Self,
    NullHmgVector, CsPoint);
end;

destructor TVXGizmoUndoItem.Destroy;
begin
  FOldAutoScaling.Free;
  inherited;
end;

procedure TVXGizmoUndoItem.DoUndo;
begin
  FEffectedObject.SetMatrix(FOldMatr);
  if FEffectedObject is TVXFreeForm then
    TVXFreeForm(FEffectedObject).AutoScaling.Assign(FOldAutoScaling);
  FEffectedObject.Material.LibMaterialName := FOldLibMaterialName;
end;

function TVXGizmoUndoItem.GetGizmo: TVXGizmo;
begin
  if GetParent <> nil then
    Result := GetPArent.GetParent
  else
    Result := nil;
end;

function TVXGizmoUndoItem.GetParent: TVXGizmoUndoCollection;
begin
  Result := TVXGizmoUndoCollection(GetOwner);
end;

procedure TVXGizmoUndoItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = OpRemove then
  begin
    if AComponent = FEffectedObject then
      FEffectedObject := nil;
  end;
end;

procedure TVXGizmoUndoItem.SetEffectedObject(const Value: TVXCustomSceneObject);
begin
  if FEffectedObject <> nil then
    FEffectedObject.RemoveFreeNotification(GetGizmo);
  FEffectedObject := Value;
  if FEffectedObject <> nil then
    FEffectedObject.FreeNotification(GetGizmo);
end;

procedure TVXGizmoUndoItem.SetOldAutoScaling(const Value: TVXCoordinates);
begin
  FOldAutoScaling.Assign(Value);
end;

procedure TVXGizmoUndoItem.SetOldMatrix(const Value: TMatrix);
begin
  FOldMatrix := Value;
end;

{ TVXGizmoUndoCollection }

function TVXGizmoUndoCollection.Add: TVXGizmoUndoItem;
begin
  Result := TVXGizmoUndoItem(inherited Add);
end;

function TVXGizmoUndoCollection.GetItems(const Index: Integer)
  : TVXGizmoUndoItem;
begin
  Result := TVXGizmoUndoItem(inherited GetItem(Index));
end;

function TVXGizmoUndoCollection.GetParent: TVXGizmo;
begin
  Result := TVXGizmo(GetOwner);
end;

procedure TVXGizmoUndoCollection.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  if Count <> 0 then
    for I := 0 to Count - 1 do
      GetItems(I).Notification(AComponent, Operation);
end;

procedure TVXGizmoUndoCollection.RemoveByObject(const AObject
  : TVXCustomSceneObject);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if GetItems(I).FEffectedObject = AObject then
      GetItems(I).Free;
end;

procedure TVXGizmoUndoCollection.SetItems(const Index: Integer;
  const Value: TVXGizmoUndoItem);
begin
  GetItems(Index).Assign(Value);
end;

procedure TVXGizmo.SetSelectedObj(const Value: TVXBaseSceneObject);
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
