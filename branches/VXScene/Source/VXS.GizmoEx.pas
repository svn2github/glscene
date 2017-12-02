//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
   Invisible component for helping to Move, Rotate and Scale an Object
   (usefull for an Editor).

   This is an enhanced version of TVXGizmo, which was originally created and
   modified by Adirex, J.Delauney, Degiovani, Marcus Oblak and Da Stranger
   Rustam Asmandiarov (aka Predator) re-wrote TVXGizmo from scratch and
   contributed to GLScene. This is how TVXGizmoEx was born.
     
}
unit VXS.GizmoEx;

interface

{$I VXScene.inc}

uses
  Winapi.Windows,
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  System.Math,

  VXS.Scene,
  VXS.Color,
  VXS.Objects,
  VXS.VectorGeometry,
  VXS.Material,
  VXS.Strings,
  VXS.Win64Viewer,
  VXS.GeomObjects,
  VXS.BitmapFont,
  VXS.VectorFileObjects,
  VXS.CrossPlatform,
  VXS.Coordinates,
  VXS.RenderContextInfo,
  VXS.GeometryBB,
  VXS.VectorTypes,
  VXS.Canvas,
  VXS.PersistentClasses,
  VXS.Screen,
  VXS.State,
  VXS.PipelineTransformation,
  VXS.Selection,
  VXS.Context;


type
  TVXGizmoExObjectCollection = class;
  TVXGizmoEx = class;

  TVXGizmoExObjectItem = class(TCollectionItem)
  private
    FOldAutoScaling: TVector;
    FEffectedObject: TVXBaseSceneObject;
    FParentOldObject: TVXBaseSceneObject;
    FIndexOldObject: Integer;
    FNameOldObject: string;
    FReturnObject: Boolean;
    FOldMatrix: TMatrix;
    FGizmoTmpRoot: TVXBaseSceneObject;
    procedure SetEffectedObject(const Value: TVXBaseSceneObject);
    procedure SetOldMatrix(const Value: TMatrix);
  protected
    procedure DoUndo;
    function GetParent: TVXGizmoExObjectCollection;
    function GetGizmo: TVXGizmoEx;
  public
    property GizmoTmpRoot: TVXBaseSceneObject read FGizmoTmpRoot write FGizmoTmpRoot;
    constructor Create(AOwner: TCollection); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); virtual;
    procedure AssignFromObject(const AObject: TVXBaseSceneObject; AssignAndRemoveObj: Boolean = False);
    // TODO: create a special type for Matrix.
    property OldMatrix: TMatrix read FOldMatrix write SetOldMatrix;
  published
    property EffectedObject: TVXBaseSceneobject read FEffectedObject write SetEffectedObject;
  end;

  TVXGizmoExObjectCollection = class(TOwnedCollection)
  private
    function GetItems(const Index: Integer): TVXGizmoExObjectItem;
    procedure SetItems(const Index: Integer; const Value: TVXGizmoExObjectItem);
  protected
    function GetParent: TVXGizmoEx;
    procedure DoUndo;
  public
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    procedure RemoveByObject(const AObject: TVXCustomSceneObject);
    function Add: TVXGizmoExObjectItem;
    property Items[const Index: Integer]: TVXGizmoExObjectItem read GetItems write SetItems; default;
  end;


  TVXGizmoExActionHistoryItem = class(TCollectionItem)
  private
    FObject: TObject;
    FGizmoObjectCollection: TVXGizmoExObjectCollection;
    procedure SetObject(aValue: TObject);
    procedure SetGizmoObjectCollection(aValue: TVXGizmoExObjectCollection);
  public
    constructor Create(AOwner: TCollection); override;
    destructor Destroy; override;
    property BaseObject: TObject read FObject write SetObject;
    property GizmoObjectCollection: TVXGizmoExObjectCollection read FGizmoObjectCollection write SetGizmoObjectCollection;
  end;

  TVXGizmoExActionHistoryCollection = class(TOwnedCollection)
  private
    FItemIndex: Integer;
    FItemsMaxCount: Integer;
    FGizmoTmpRoot: TVXBaseSceneObject;
    function GetItems(const Index: Integer): TVXGizmoExActionHistoryItem;
    procedure SetItems(const Index: Integer; const Value: TVXGizmoExActionHistoryItem);
    function Add: TVXGizmoExActionHistoryItem;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    property ItemIndex: Integer read FItemIndex write FItemIndex;
    function Undo: TVXGizmoExActionHistoryItem;
    function Redo: TVXGizmoExActionHistoryItem;
    procedure AddObjects(objs: TVXPickList);
    procedure AddObject(obj: TObject);
    procedure RemoveObjects(objs: TVXPickList);
    property MaxCount: Integer read FItemsMaxCount write FItemsMaxCount;
    property Items[const Index: Integer]: TVXGizmoExActionHistoryItem read GetItems write SetItems; default;
    property GizmoTmpRoot: TVXBaseSceneObject read FGizmoTmpRoot write FGizmoTmpRoot;
  end;

  TVXGizmoExVisibleInfoLabel = (vliName, vliOperation, vliCoords);
  TVXGizmoExVisibleInfoLabels = set of TVXGizmoExVisibleInfoLabel;
  TInfoLabelCoordType = (ilcChanging, ilcChangeRate);

  TVXGizmoExAxis = (gaNone, gaX, gaY, gaZ, gaXY, gaXZ, gaYZ, gaXYZ);

  TVXGizmoExSelectionRegion = (gsrRectangular, gsrCircular, gsrFence,
    gsrLasso);

  TVXGizmoExReferenceCoordinateSystem = (rcsView, rcsLocal);

  TVXGizmoExSelRec = array of TPoint;

  TVXGizmoExOperation = (gopMove, gopRotate, gopScale, gopNone);
  TVXGizmoExOperationMode = (gomNone, gomSelect, gomMove, gomRotate, gomScale);


  TVXGizmoExAcceptEvent = procedure(Sender: TObject; var objs: TVXPickList) of object;
  TVXGizmoExAxisSelected = procedure(Sender: TObject; var Axis: TVXGizmoExAxis) of object;
  TVXGizmoExPickMode = (pmGetPickedObjects, pmRayCast);

  //Gizmo objects

  TVXGizmoExUIFrustrum = class(TVXFrustrum)
  private
    FNoZWrite: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    property NoZWrite: Boolean read FNoZWrite write FNoZWrite;
  end;

  TVXGizmoExUISphere = class(TVXSphere)
  private
    FNoZWrite: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    property NoZWrite: Boolean read FNoZWrite write FNoZWrite;
  end;

  TVXGizmoExUIDisk = class(TVXDisk)
  private
    FNoZWrite: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    property NoZWrite: Boolean read FNoZWrite write FNoZWrite;
  end;

  TVXGizmoExUITorus = class(TVXTorus)
  private
    FNoZWrite: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    property NoZWrite: Boolean read FNoZWrite write FNoZWrite;
  end;

  TVXGizmoExUIPolygon = class(TVXPolygon)
  private
    FNoZWrite: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    property NoZWrite: Boolean read FNoZWrite write FNoZWrite;
  end;

  TVXGizmoExUIArrowLine = class(TVXArrowLine)
  private
    FNoZWrite: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    property NoZWrite: Boolean read FNoZWrite write FNoZWrite;
  end;

  TVXGizmoExUILines = class(TVXLines)
  private
    FNoZWrite: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    property NoZWrite: Boolean read FNoZWrite write FNoZWrite;
  end;

  TVXGizmoExUIFlatText = class(TVXFlatText)
  private
    FNoZWrite: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    property NoZWrite: Boolean read FNoZWrite write FNoZWrite;
  end;

  TVXGizmoEx = class(TComponent)
  private
    FUIBaseGizmo: TVXBaseSceneObject;

    FUIRootHelpers: TVXBaseSceneObject;

    FUIRootSelect: TVXBaseSceneObject; // for None
    FUIRootMovement: TVXBaseSceneObject; // for Move
    FUIRootRotate: TVXBaseSceneObject; //for Rotate
    FUIRootRotateAxisLabel: TVXBaseSceneObject;
    FUIRootScale: TVXBaseSceneObject; // for Scale
    FUIRootAxisLabel: TVXBaseSceneObject;
    FUIRootVisibleInfoLabels: TVXBaseSceneObject;
    FInterfaceRender: TVXDirectOpenVX;
    FInternalRender: TVXDirectOpenVX;

    FUISelectLineX, FUISelectLineY, FUISelectLineZ: TVXGizmoExUILines;  //  For None (Select)

    //IC- Invisible Control
    //For Move
    FUIICMovementLineX, FUIICMovementLineY, FUIICMovementLineZ, FUIICMovementLineXY, FUIICMovementLineXZ, FUIICMovementLineYZ: TVXGizmoExUIFrustrum;
    FUIMovementArrowX, FUIMovementArrowY, FUIMovementArrowZ: TVXGizmoExUIArrowLine; // For Move
    FUIMovementLineX, FUIMovementLineY, FUIMovementLineZ, FUIMovementLineXY, FUIMovementLineXZ, FUIMovementLineYZ: TVXGizmoExUILines; // For Move
    FUIMovementPlaneXY, FUIMovementPlaneXZ, FUIMovementPlaneYZ: TVXGizmoExUIPolyGon; // For Move

    //ForRotate
    FUIRotateLineX, FUIRotateLineY, FUIRotateLineZ, FUIRotateLineXY, FUIRotateLineXZ: TVXGizmoExUILines;
    FUIICRotateTorusX, FUIICRotateTorusY, FUIICRotateTorusZ, FUIICRotateTorusXZ: TVXGizmoExUITorus; // For Rotate
    FUIRotateDiskXY, FUIRotateDiskX, FUIRotateDiskX2, FUIRotateDiskY, FUIRotateDiskY2, FUIRotateDiskZ, FUIRotateDiskZ2: TVXGizmoExUIDisk;
    FUIRotateLineArrowX, FUIRotateLineArrowY, FUIRotateLineArrowZ: TVXGizmoExUILines;
    FUIICRotateSphereXY: TVXGizmoExUISphere;
    FUIRotateAxisLabelX, FUIRotateAxisLabelY, FUIRotateAxisLabelZ: TVXGizmoExUIFlatText;

    //ForScale
    FUIScaleArrowX, FUIScaleArrowY, FUIScaleArrowZ: TVXGizmoExUISphere; // For Scale

    FUIScaleLineX, FUIScaleLineY, FUIScaleLineZ, FUIScaleLineXY, FUIScaleLineYZ, FUIScaleLineXZ: TVXGizmoExUILines;

    FUIICScaleLineX, FUIICScaleLineY, FUIICScaleLineZ, FUIICScaleLineXY, FUIICScaleLineXZ, FUIICScaleLineYZ, FUIICScaleLineXYZ: TVXGizmoExUIFrustrum;
    FUIScalePlaneXY, FUIScalePlaneXZ, FUIScalePlaneYZ, FUIScalePlaneXYZ: TVXGizmoExUIPolyGon; // For Move

    FUIAxisLabelX, FUIAxisLabelY, FUIAxisLabelZ: TVXGizmoExUIFlatText;
    FUIVisibleInfoLabels: TVXGizmoExUIFlatText;

    FRootGizmo: TVXBaseSceneObject;
    FRootObjects: TVXBaseSceneObject;
    FGizmoTmpRoot: TVXBaseSceneObject;
    FSelectedObj: TVXBaseSceneObject;

    FOperation: TVXGizmoExOperation;
    FOperationMode: TVXGizmoExOperationMode;
    FSelAxis: TVXGizmoExAxis;
    fInfoLabelCoordType: TInfoLabelCoordType;
    FReferenceCoordSystem: TVXGizmoExReferenceCoordinateSystem;

    FBoundingBoxColor: TVXColor;
    FSelectedColor: TVXColor;
    FVisibleInfoLabelsColor: TVXColor;
    FSelectionRegionColor: TVXColor;

    FVisibleInfoLabelsColorChanged: Boolean;

    FAutoZoom: Boolean;
    FExcludeObjects: Boolean;
    FExcludeClassname: Boolean;
    FNoZWrite: Boolean;
    FEnabled: Boolean;

    FAutoZoomFactor: Single;
    FZoomFactor: Single;
    FMoveCoef: Single;
    FRotationCoef: Single;

    FViewer: TVXSceneViewer;

    FVisibleVisibleInfoLabels: TVXGizmoExVisibleInfoLabels;

    FExcludeObjectsList: TStrings;
    FExcludeClassNameList: TStrings;

    FSelectionRegion: TVXGizmoExSelectionRegion;
    FEnableMultiSelection: Boolean;
    FShowMultiSelecting: Boolean;
    FSelectionRec: TVXGizmoExSelRec;
    FCanAddObjToSelectionList: Boolean;
    FCanRemoveObjFromSelectionList: Boolean;
    FSelectedObjects: TVXPickList;
    FAntiAliasedLines: Boolean;

    FShowAxisLabel: Boolean;
    FShowObjectInfos: Boolean;
    FShowBoundingBox: Boolean;

    FCanChangeWithChildren: Boolean;


    moving: Boolean;
    mx, my: Integer;

    fCursorPos: TVXPoint;
    fLastCursorPos: TVXPoint;
    fChangeRate: TAffineVector;   //total rotate angle
    FEnableLoopCursorMoving: Boolean;

    lastMousePos: TVector;

    FOnUpdate: TNotifyEvent;
    FOnSelect: TVXGizmoExAcceptEvent;
    FOnOperationChange: TNotifyEvent;
    FOnOperationModeChange: TNotifyEvent;
    FOnSelectionLost: TNotifyEvent;
    FOnAxisSelected: TVXGizmoExAxisSelected;
    FScaleCoef: Single;
    FGizmoThickness: Single;
    FPickMode: TVXGizmoExPickMode;

    FEnableHistory: Boolean;
    FHistory: TVXGizmoExActionHistoryCollection;
    FHistoryStepsCount: Integer;
    FLabelFont: TVXCustomBitmapFont;

    procedure SetRootGizmo(const AValue: TVXBaseSceneObject);
    procedure SetRootObjects(const AValue: TVXBaseSceneObject);
    procedure SetGizmoTmpRoot(const AValue: TVXBaseSceneObject);

    procedure SetGizmoExVisibleInfoLabels(const AValue: TVXGizmoExVisibleInfoLabels);
    procedure SetBoundingBoxColor(const AValue: TVXColor);
    procedure SetSelectedColor(const AValue: TVXColor);
    procedure SetVisibleInfoLabelsColor(const AValue: TVXColor);
    procedure SetSelectionRegionColor(const AValue: TVXColor);
    procedure SetCanChangeWithChildren(AValue: Boolean);
    procedure SetAALines(aValue: Boolean);
    procedure SetInfoLabelCoordType(aValue: TInfoLabelCoordType);
    procedure SetReferenceCoordSystem(aValue: TVXGizmoExReferenceCoordinateSystem);
    procedure SetHistoryStepsCount(aValue: Integer);

    procedure SetExcludeObjectsList(const AValue: TStrings);
    procedure SetExcludeClassNameList(const AValue: TStrings);

    function MouseWorldPos(const X, Y: Integer): TVector;
    function CheckObjectInExcludeList(const Obj: TVXBaseSceneObject): Boolean;
    function CheckClassNameInExcludeList(const Obj: TVXBaseSceneObject): Boolean;
    procedure UpdateVisibleInfoLabels;
    procedure SetGizmoExThickness(const Value: Single);

    procedure ActivatingElements(PickList: TVXPickList);
    procedure InterfaceRender(Sender: TObject; var rci: TVXRenderContextInfo);
    procedure InternalRender(Sender: TObject; var rci: TVXRenderContextInfo);
    function InternalGetPickedObjects(const x1, y1, x2, y2: Integer; const guessCount: Integer = 8): TVXPickList;
    procedure SetViewer(const Value: TVXSceneViewer);
    procedure SetLabelFont(const Value: TVXCustomBitmapFont);
    procedure SetSelectedObj(const Value: TVXBaseSceneObject);
    function GetSelectedObj: TVXBaseSceneObject;
    procedure SetNoZWrite(const Value: Boolean);
    procedure SetOperation(const Value: TVXGizmoExOperation);
    procedure SetOperationMode(const Value: TVXGizmoExOperationMode);
    procedure SetAngleDisk(aAngle: Single);
    procedure SetEnableLoopCursorMoving(const AValue: Boolean);
    procedure SetEnableMultiSelection(const AValue: Boolean);
    procedure SetSelectionRegion(const AValue: TVXGizmoExSelectionRegion);
    procedure SetShowAxisLabel(const AValue: Boolean);
    procedure SetShowObjectInfos(const AValue: Boolean);
    procedure SetShowBoundingBox(const AValue: Boolean);
    procedure SetAutoZoomFactor(const AValue: Single);
    procedure SetZoomFactor(const AValue: Single);
    procedure SetSelAxis(aValue: TVXGizmoExAxis);
    procedure SetPickMode(APickMode: TVXGizmoExPickMode);

    procedure AssignPickList(aList: TVXPickList; RemoveObj: Boolean = False);
    procedure AddObjToSelectionList(Obj: TVXBaseSceneObject);
    procedure RemoveObjFromSelectionList(Obj: TVXBaseSceneObject);
    procedure MultiSelMouseDown(X, Y: Integer);
    procedure MultiSelMouseUp(X, Y: Integer);
    procedure MultiSelMouseMove(X, Y: Integer);

    function GetPickList: TVXPickList;
    procedure SetPickList(aValue: TVXPickList);
    property SelAxis: TVXGizmoExAxis read FSelAxis write SetSelAxis;
    property Operation: TVXGizmoExOperation read FOperation write SetOperation;
    procedure ClearSelection;
    procedure SetVisible(const AValue: Boolean);
    function GetVisible: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure ViewerMouseMove(const X, Y: Integer);
    procedure ViewerMouseDown(const X, Y: Integer);
    procedure ViewerMouseUp(const X, Y: Integer);

    procedure UpdateGizmo; overload;

    procedure LooseSelection; virtual;

    procedure UndoAdd(const AObject: TObject);
    procedure RemoveSelectedObjects;

    function Undo: TVXGizmoExActionHistoryItem;
    function Redo: TVXGizmoExActionHistoryItem;

    property CanAddObjToSelectionList: Boolean read FCanAddObjToSelectionList write FCanAddObjToSelectionList;
    property CanRemoveObjFromSelectionList: Boolean read FCanRemoveObjFromSelectionList write FCanRemoveObjFromSelectionList;

    procedure LooseCursorSelection;
    property CursorSelectingRegion: Boolean read FShowMultiSelecting;

    property RootObjects: TVXBaseSceneObject read FRootObjects write SetRootObjects;
    property RootGizmo: TVXBaseSceneObject read FRootGizmo write SetRootGizmo;
    property GizmoTmpRoot: TVXBaseSceneObject read FGizmoTmpRoot write SetGizmoTmpRoot;
    //--------------------------------------------------------------------
  published
    property Viewer: TVXSceneViewer read FViewer write SetViewer;

    property BoundingBoxColor: TVXColor read FBoundingBoxColor write SetBoundingBoxColor;
    property SelectedColor: TVXColor read FSelectedColor write SetSelectedColor;
    property SelectionRegionColor: TVXColor read FSelectionRegionColor write SetSelectionRegionColor;

    property SelectedObj: TVXBaseSceneObject read GetSelectedObj write SetSelectedObj;
    property SelectedObjects: TVXPickList read GetPickList write SetPickList;

    property OperationMode: TVXGizmoExOperationMode read FOperationMode write SetOperationMode default gomSelect;

    property ExcludeObjects: Boolean read FExcludeObjects write FExcludeObjects;
    property ExcludeObjectsList: TStrings read FExcludeObjectsList write SetExcludeObjectsList;

    property ExcludeClassname: Boolean read FExcludeClassname write FExcludeClassname;
    property ExcludeClassnameList: TStrings read FExcludeClassnameList write SetExcludeClassnameList;

    property VisibleInfoLabels: TVXGizmoExVisibleInfoLabels read FVisibleVisibleInfoLabels write SetGizmoExVisibleInfoLabels;
    property VisibleInfoLabelsColor: TVXColor read FVisibleInfoLabelsColor write SetVisibleInfoLabelsColor;

    property AutoZoom: Boolean read FAutoZoom write FAutoZoom default True;
    property AutoZoomFactor: Single read FAutoZoomFactor write SetAutoZoomFactor;
    property ZoomFactor: Single read FZoomFactor write SetZoomFactor;

    property MoveCoef: Single read FMoveCoef write FMoveCoef;
    property RotationCoef: Single read FRotationCoef write FRotationCoef;
    property ScaleCoef: Single read FScaleCoef write FScaleCoef;
    property NoZWrite: Boolean read FNoZWrite write SetNoZWrite default True;

    property GizmoThickness: Single read FGizmoThickness write SetGizmoExThickness;

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

    { Use the property OperationMode=gomNone to unactivate gizmo and make it invisible}
    property Enabled: Boolean read FEnabled write FEnabled default True;

    property LabelFont: TVXCustomBitmapFont read FLabelFont write SetLabelFont default nil;

    property OnSelectionLost: TNotifyEvent read FOnSelectionLost write FOnSelectionLost;
    property OnOperationChange: TNotifyEvent read FOnOperationChange write FOnOperationChange;
    property OnOperationModeChange: TNotifyEvent read FOnOperationModeChange write FOnOperationModeChange;
    property OnSelect: TVXGizmoExAcceptEvent read FOnSelect write FOnSelect;
    property OnAxisSelected: TVXGizmoExAxisSelected read FOnAxisSelected write FOnAxisSelected;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
    property PickMode: TVXGizmoExPickMode read FPickMode write SetPickMode default pmGetPickedObjects;

    property EnableActionHistory: Boolean read FEnableHistory write FEnableHistory default True;
    property HistoryStepsCount: Integer read FHistoryStepsCount write SetHistoryStepsCount;

    property EnableLoopCursorMoving: Boolean read FEnableLoopCursorMoving write SetEnableLoopCursorMoving default True;
    property EnableMultiSelection: Boolean read FEnableMultiSelection write SetEnableMultiSelection default True;
    property CanChangeWithChildren: Boolean read FCanChangeWithChildren write SetCanChangeWithChildren;

    property AntiAliasedLines: Boolean read FAntiAliasedLines write SetAALines default True;
    property InfoLabelCoordType: TInfoLabelCoordType read fInfoLabelCoordType write SetInfoLabelCoordType default ilcChangeRate;

    property SelectionRegion: TVXGizmoExSelectionRegion read FSelectionRegion write SetSelectionRegion default gsrRectangular;

    property ShowAxisLabel: Boolean read FShowAxisLabel write SetShowAxisLabel default True;
    property ShowObjectInfos: Boolean read FShowObjectInfos write SetShowObjectInfos default True;
    property ShowBoundingBox: Boolean read FShowBoundingBox write SetShowBoundingBox default True;
    property ReferenceCoordSystem: TVXGizmoExReferenceCoordinateSystem read FReferenceCoordSystem write SetReferenceCoordSystem default rcsView;
    property Visible: Boolean read GetVisible write SetVisible;
  end;


implementation

procedure RotateAroundArbitraryAxis(const anObject: TVXBaseSceneObject; const Axis, Origin: TAffineVector; const angle: Single);
var
  M, M1, M2, M3: TMatrix;
begin
  M1 := CreateTranslationMatrix(VectorNegate(Origin));
  M2 := CreateRotationMatrix(Axis, Angle * PI / 180);
  M3 := CreateTranslationMatrix(Origin);
  M := MatrixMultiply(M1, M2);
  M := MatrixMultiply(M, M3);
  anObject.AbsoluteMatrix := MatrixMultiply(anObject.AbsoluteMatrix, M);

  //Just a workarround to Update angles...
  anObject.Roll(0);
  anObject.Pitch(0);
  anObject.Turn(0);
end;

//-------------------------------------------------------------------
//                 Mathematical functions for canvas
//-------------------------------------------------------------------
function Det(const a, b, c, d: real): real;
begin
  Det := a * d - b * c;
end;

//Distance between two points
function Dist(const P1, P2: TPoint): real;
begin
  Result := Sqrt(Sqr(P1.X - P2.X) + Sqr(P1.Y - P2.Y));
end;

function CrossingPointLine(p: TPoint; p1, p2: TPoint): Boolean;
begin
  Result := (abs(p1.X - p.X) + abs(p2.X - p.X) = abs(p2.X - p1.X)) and
    (abs(p1.Y - p.Y) + abs(p2.Y - p.Y) = abs(p2.Y - p1.Y));
end;

//Intersection between two lines, return true or false
//converted from http://doc-for-prog.narod.ru/topics/math/crossing.html
function IsLineIntLine(p11, p12, p21, p22: TPoint; var p: TPoint): Boolean;  // координаты второго отрезка
var
  Z, ca, cb, ua, ub: Single;
begin
  // demominator
  Z := (p12.Y - p11.Y) * (p21.X - p22.X) - (p21.Y - p22.Y) * (p12.X - p11.X);
  // numerator 1
  Ca := (p12.Y - p11.Y) * (p21.X - p11.X) - (p21.Y - p11.Y) * (p12.X - p11.X);
  // numerator 2
  Cb := (p21.Y - p11.Y) * (p21.X - p22.X) - (p21.Y - p22.Y) * (p21.X - p11.X);

  // if  numerator and demominator = 0, then coincide lines
  if (Z = 0) and (Ca = 0) and (Cb = 0) then
  begin
    Result := False;
    Exit;
  end
  else
  // if demominator = 0, then parallel lines
  if Z = 0 then
  begin
    Result := False;
    Exit;
  end;

  Ua := Ca / Z;
  Ub := Cb / Z;

  // if 0<=Ua<=1 and 0<=Ub<=1, then the intersection point is inside intervals
  if (0 <= Ua) and (Ua <= 1) and (0 <= Ub) and (Ub <= 1) then
  begin
    p.X := round(p11.X + (p12.X - p11.X) * Ub);
    p.Y := round(p11.Y + (p12.Y - p11.Y) * Ub);
    Result := True;
  end
  // otherwise the intersection point is outside intervals
  else
    Result := False;
end;

//Intersection of line and circle
function IsLineIntCirlce(CR: Single; CC: TPoint; LP1, LP2: TPoint; var PIL1, PIL2: TPoint): Smallint;
var
  d, K, b: Single;
begin

  K := (LP1.Y - LP2.Y) / (LP1.X - LP2.X);
  b := LP1.Y - K * LP1.X;
  //determine decrement of quadratic equation

  d := (power((2 * K * b - 2 * CC.X - 2 * CC.Y * K), 2) - (4 + 4 * K * K) * (b * b - cr * cr + CC.X * CC.X + CC.Y * CC.Y - 2 * CC.Y * b));
  //if decrement = 0, then no decision and line and circle do not intersect
  if (d < 0) then
  begin
    Result := -1;
    PIL1 := point(0, 0);
    PIL2 := point(0, 0);
    Exit;
  end;
  //otherwise find roots of quadratic equation

  PIL1.X := round((-(2 * K * b - 2 * CC.X - 2 * CC.Y * K) - sqrt(d)) / (2 + 2 * K * K));
  PIL2.X := round((-(2 * K * b - 2 * CC.X - 2 * CC.Y * K) + sqrt(d)) / (2 + 2 * K * K));
  //if abscissas of points are coinside, then the intersection is only in one point
  //and line and circle have a point of contact
  if (PIL1.X = PIL2.X) then
  begin
    Result := 0;
    PIL1.Y := round(K * PIL1.X + b);
    PIL2 := PIL1;

    Exit;
  end;
  //otherwise find ordinates of intersection points
  PIL1.Y := round(K * PIL1.X + b);
  PIL2.Y := round(K * PIL2.X + b);
  Result := 1;
end;

constructor TVXGizmoExUIArrowLine.Create(AOwner: TComponent);
begin
  FNoZWrite := True;
  inherited;
end;

procedure TVXGizmoExUIArrowLine.BuildList(var rci: TVXRenderContextInfo);
begin
  if FNoZWrite then
    rci.VXStates.Disable(stDepthTest)
  else
    rci.VXStates.Enable(stDepthTest);
  inherited;
end;


constructor TVXGizmoExUIDisk.Create(AOwner: TComponent);
begin
  FNoZWrite := True;
  inherited;
end;

procedure TVXGizmoExUIDisk.BuildList(var rci: TVXRenderContextInfo);
begin
  if FNoZWrite then
    rci.VXStates.Disable(stDepthTest)
  else
    rci.VXStates.Enable(stDepthTest);
  inherited;
end;

constructor TVXGizmoExUISphere.Create(AOwner: TComponent);
begin
  FNoZWrite := True;
  inherited;
end;

procedure TVXGizmoExUISphere.BuildList(var rci: TVXRenderContextInfo);
begin
  if FNoZWrite then
    rci.VXStates.Disable(stDepthTest)
  else
    rci.VXStates.Enable(stDepthTest);
  inherited;
end;

constructor TVXGizmoExUIPolyGon.Create(AOwner: TComponent);
begin
  FNoZWrite := True;
  inherited;
end;

procedure TVXGizmoExUIPolyGon.BuildList(var rci: TVXRenderContextInfo);
begin
  if FNoZWrite then
    rci.VXStates.Disable(stDepthTest)
  else
    rci.VXStates.Enable(stDepthTest);
  inherited;
end;

constructor TVXGizmoExUIFrustrum.Create(AOwner: TComponent);
begin
  FNoZWrite := True;
  inherited;
end;

procedure TVXGizmoExUIFrustrum.BuildList(var rci: TVXRenderContextInfo);
begin
  if FNoZWrite then
    rci.VXStates.Disable(stDepthTest)
  else
    rci.VXStates.Enable(stDepthTest);
  inherited;
end;

constructor TVXGizmoExUITorus.Create(AOwner: TComponent);
begin
  FNoZWrite := True;
  inherited;
end;

procedure TVXGizmoExUITorus.BuildList(var rci: TVXRenderContextInfo);
begin
  if FNoZWrite then
    rci.VXStates.Disable(stDepthTest)
  else
    rci.VXStates.Enable(stDepthTest);
  inherited;
end;

constructor TVXGizmoExUILines.Create(AOwner: TComponent);
begin
  FNoZWrite := True;
  inherited;
end;

procedure TVXGizmoExUILines.BuildList(var rci: TVXRenderContextInfo);
begin
  if FNoZWrite then
    rci.VXStates.Disable(stDepthTest)
  else
    rci.VXStates.Enable(stDepthTest);
  inherited;
end;

constructor TVXGizmoExUIFlatText.Create(AOwner: TComponent);
begin
  FNoZWrite := True;
  inherited;
end;

procedure TVXGizmoExUIFlatText.BuildList(var rci: TVXRenderContextInfo);
begin
  if FNoZWrite then
    rci.VXStates.Disable(stDepthTest)
  else
    rci.VXStates.Enable(stDepthTest);
  inherited;
end;

//------------------------------------------------------------------------------

constructor TVXGizmoEx.Create(aOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(aOwner);
  FHistory := TVXGizmoExActionHistoryCollection.Create(Self, TVXGizmoExActionHistoryItem);
  FPickMode := pmGetPickedObjects;
  FRotationCoef := 1;
  FMoveCoef := 0.1;
  FScaleCoef := 0.1;

  FBoundingBoxColor := TVXColor.Create(Self);
  FBoundingBoxColor.Color := clrWhite;
  FSelectionRegionColor := TVXColor.Create(Self);
  SelectionRegionColor.Color := clrBlack;
  FSelectedColor := TVXColor.Create(Self);
  FSelectedColor.Color := clrYellow;
  FVisibleInfoLabelsColor := TVXColor.Create(Self);
  FVisibleInfoLabelsColor.Color := clrYellow;
  FVisibleInfoLabelsColorChanged := False;

  FUIBaseGizmo := TVXDummyCube.Create(Self);

  //BoundingBoxes...
  FInternalRender := TVXDirectOpenVX(FUIBaseGizmo.AddNewChild(TVXDirectOpenVX));
  FInternalRender.OnRender := InternalRender;

  FUIRootHelpers := TVXDummyCube(FUIBaseGizmo.AddNewChild(TVXDummyCube));

  //Canvas...
  FInterfaceRender := TVXDirectOpenVX(FUIBaseGizmo.AddNewChild(TVXDirectOpenVX));
  FInterfaceRender.OnRender := InterfaceRender;

  FSelectedObjects := TVXPickList.Create(psMinDepth);

  //For None
  FUIRootSelect := FUIRootHelpers.AddNewChild(TVXDummyCube); // for None
  FUIRootMovement := FUIRootHelpers.AddNewChild(TVXDummyCube);
  FUIRootRotate := FUIRootHelpers.AddNewChild(TVXDummyCube);
  FUIRootScale := FUIRootHelpers.AddNewChild(TVXDummyCube);
  FUIRootAxisLabel := FUIRootHelpers.AddNewChild(TVXDummyCube);
  FUIRootVisibleInfoLabels := FUIRootHelpers.AddNewChild(TVXDummyCube);

  FUISelectLineX := TVXGizmoExUILines(FUIRootSelect.addnewChild(TVXGizmoExUILines));
  with FUISelectLineX do
  begin
    LineColor.Color := clrRed;
    LineWidth := 1;
    NodesAspect := lnaInvisible;
    AddNode(0, 0, 0);
    AddNode(1, 0, 0);
    AddNode(0.9, 0, -0.1);
    addNode(1, 0, 0);
    addNode(0.9, 0, 0.1);
  end;

  FUISelectLineY := TVXGizmoExUILines(FUIRootSelect.addnewChild(TVXGizmoExUILines));
  with FUISelectLineY do
  begin
    LineColor.Color := clrLime;
    LineWidth := 1;
    NodesAspect := lnaInvisible;
    AddNode(0, 0, 0);
    AddNode(0, 1, 0);
    AddNode(0.1, 0.9, 0);
    addNode(0, 1, 0);
    addNode(-0.1, 0.9, 0);
  end;

  FUISelectLineZ := TVXGizmoExUILines(FUIRootSelect.addnewChild(TVXGizmoExUILines));
  with FUISelectLineZ do
  begin
    LineColor.Color := clrBlue;
    LineWidth := 1;
    NodesAspect := lnaInvisible;
    AddNode(0, 0, 0);
    AddNode(0, 0, 1);
    AddNode(0.1, 0, 0.9);
    addNode(0, 0, 1);
    addNode(-0.1, 0, 0.9);
  end;


  //For movement


  FUIMovementLineX := TVXGizmoExUILines(FUIRootMovement.addnewChild(TVXGizmoExUILines));
  with FUIMovementLineX do
  begin
    LineColor.Color := clrRed;
    LineWidth := 1;
    NodesAspect := lnaInvisible;
    AddNode(0.2, 0, 0);
    AddNode(1, 0, 0);
    // Raycast pickable object
    FUIICMovementLineX := TVXGizmoExUIFrustrum(AddNewChild(TVXGizmoExUIFrustrum));
    with FUIICMovementLineX do
    begin
      Material.MaterialOptions := [moNoLighting];
      Material.BlendingMode := bmTransparency;
      Material.FrontProperties.Diffuse.Color := clrYellow;
      Material.FrontProperties.Diffuse.Alpha := 0;
      Up.SetVector(1, 0, 0);
      Height := 0.8;
      ApexHeight := 8;
      BaseDepth := 0.15;
      BaseWidth := 0.15;
      position.SetPoint(0.6, 0, 0);
    end;
    FUIMovementArrowX := TVXGizmoExUIArrowLine(addnewChild(TVXGizmoExUIArrowLine));
    with FUIMovementArrowX do
    begin
      Material.MaterialOptions := [moNoLighting];
      Material.BlendingMode := bmTransparency;
      Material.FrontProperties.Diffuse.Color := clrRed;

      TurnAngle := 90;
      Height := 0.3;
      Position.X := 1;
      Slices := 8;
      Stacks := 2;

      TopRadius := 0;
      TopArrowHeadHeight := 0;
      TopArrowHeadRadius := 0;
      BottomArrowHeadHeight := 0.1;
      BottomRadius := 0.06;
    end;
  end;

  FUIMovementLineY := TVXGizmoExUILines(FUIRootMovement.addnewChild(TVXGizmoExUILines));
  with FUIMovementLineY do
  begin
    LineColor.Color := clrLime;
    LineWidth := 1;
    NodesAspect := lnaInvisible;
    AddNode(0, 0.2, 0);
    AddNode(0, 1, 0);
    FUIMovementArrowY := TVXGizmoExUIArrowLine(addnewChild(TVXGizmoExUIArrowLine));
    with FUIMovementArrowY do
    begin
      Material.MaterialOptions := [moNoLighting];
      Material.BlendingMode := bmTransparency;
      Material.FrontProperties.Diffuse.Color := clrLime;

      PitchAngle := 90;
      Height := 0.3;
      Position.Y := 1;
      Slices := 8;
      Stacks := 2;

      TopRadius := 0;
      TopArrowHeadHeight := 0;
      TopArrowHeadRadius := 0;
      BottomArrowHeadHeight := 0.1;
      BottomRadius := 0.06;
    end;
    // Raycast pickable object
    FUIICMovementLineY := TVXGizmoExUIFrustrum(AddNewChild(TVXGizmoExUIFrustrum));
    with FUIICMovementLineY do
    begin
      Material.MaterialOptions := [moNoLighting];
      Material.BlendingMode := bmTransparency;
      Material.FrontProperties.Diffuse.Alpha := 0;
      Up.SetVector(0, 1, 0);
      Height := 0.8;
      ApexHeight := 8;
      BaseDepth := 0.15;
      BaseWidth := 0.15;
      position.SetPoint(0, 0.6, 0);
    end;
  end;

  FUIMovementLineZ := TVXGizmoExUILines(FUIRootMovement.addnewChild(TVXGizmoExUILines));
  with FUIMovementLineZ do
  begin
    LineColor.Color := clrBlue;
    LineWidth := 1;
    NodesAspect := lnaInvisible;
    AddNode(0, 0, 0.2);
    AddNode(0, 0, 1);
    FUIMovementArrowZ := TVXGizmoExUIArrowLine(addnewChild(TVXGizmoExUIArrowLine));
    with FUIMovementArrowZ do
    begin
      Material.MaterialOptions := [moNoLighting];
      Material.BlendingMode := bmTransparency;
      Material.FrontProperties.Diffuse.Color := clrBlue;

      RollAngle := 90;
      Height := 0.3;
      Position.Z := 1;
      Slices := 8;
      Stacks := 2;

      TopRadius := 0;
      TopArrowHeadHeight := 0;
      TopArrowHeadRadius := 0;
      BottomArrowHeadHeight := 0.1;
      BottomRadius := 0.06;
    end;
    // Raycast pickable object
    FUIICMovementLineZ := TVXGizmoExUIFrustrum(AddNewChild(TVXGizmoExUIFrustrum));
    with FUIICMovementLineZ do
    begin
      Material.MaterialOptions := [moNoLighting];
      Material.BlendingMode := bmTransparency;
      Material.FrontProperties.Diffuse.Alpha := 0;
      Up.SetVector(0, 0, 1);
      Height := 0.8;
      ApexHeight := 8;
      BaseDepth := 0.15;
      BaseWidth := 0.15;
      position.SetPoint(0, 0, 0.6);
    end;
  end;

  FUIMovementLineXY := TVXGizmoExUILines(FUIRootMovement.addnewChild(TVXGizmoExUILines));
  with FUIMovementLineXY do
  begin
    LineWidth := 1;
    Options := [loUseNodeColorForLines];
    NodesAspect := lnaInvisible;
    SplineMode := lsmSegments;
    addNode(0, 0.4, 0);
    TVXLinesNode(Nodes[0]).Color.color := clrLime;
    addNode(0.4, 0.4, 0);
    TVXLinesNode(Nodes[1]).Color.color := clrLime;
    addNode(0.4, 0.4, 0);
    TVXLinesNode(Nodes[2]).Color.color := clrRed;
    addNode(0.4, 0, 0);
    TVXLinesNode(Nodes[3]).Color.color := clrRed;
    // Raycast pickable object
    FUIMovementPlaneXY := TVXGizmoExUIPolyGon(addnewChild(TVXGizmoExUIPolyGon));
    with FUIMovementPlaneXY do
    begin
      Material.MaterialOptions := [moNoLighting];
      Material.BlendingMode := bmTransparency;
      Material.FrontProperties.Diffuse.Color := clrYellow;
      Material.FrontProperties.Diffuse.Alpha := 0.01;
      addNode(0.01, 0.39, 0);
      addNode(0.39, 0.39, 0);
      addNode(0.39, 0.01, 0);
      addNode(0.01, 0.01, 0);
    end;

    FUIICMovementLineXY := TVXGizmoExUIFrustrum(AddNewChild(TVXGizmoExUIFrustrum));
    with FUIICMovementLineXY do
    begin
      Material.MaterialOptions := [moNoLighting];
      Material.BlendingMode := bmTransparency;
      Material.FrontProperties.Diffuse.Alpha := 0;
      Up.SetVector(1, 0, 0);
      Height := 0.35;
      ApexHeight := 8;
      BaseDepth := 0.1;
      BaseWidth := 0.35;
      position.SetPoint(0.25, 0.25, 0);
    end;
  end;

  FUIMovementLineXZ := TVXGizmoExUILines(FUIRootMovement.addnewChild(TVXGizmoExUILines));
  with FUIMovementLineXZ do
  begin
    LineWidth := 1;
    Options := [loUseNodeColorForLines];
    NodesAspect := lnaInvisible;
    SplineMode := lsmSegments;
    addNode(0.4, 0, 0);
    TVXLinesNode(Nodes[0]).Color.color := clrRed;
    addNode(0.4, 0, 0.4);
    TVXLinesNode(Nodes[1]).Color.color := clrRed;
    addNode(0.4, 0, 0.4);
    TVXLinesNode(Nodes[2]).Color.color := clrBlue;
    addNode(0, 0, 0.4);
    TVXLinesNode(Nodes[3]).Color.color := clrBlue;
    FUIMovementPlaneXZ := TVXGizmoExUIPolyGon(addnewChild(TVXGizmoExUIPolyGon));
    with FUIMovementPlaneXZ do
    begin
      Material.MaterialOptions := [moNoLighting];
      Material.BlendingMode := bmTransparency;
      Material.FrontProperties.Diffuse.Color := clrYellow;
      Material.FrontProperties.Diffuse.Alpha := 0.01;
      addNode(0.39, 0, 0.01);
      addNode(0.39, 0, 0.39);
      addNode(0.01, 0, 0.39);
      addNode(0, 0, 0.01);
    end;
    // Raycast pickable object
    FUIICMovementLineXZ := TVXGizmoExUIFrustrum(AddNewChild(TVXGizmoExUIFrustrum));
    with FUIICMovementLineXZ do
    begin
      Material.MaterialOptions := [moNoLighting];
      Material.BlendingMode := bmTransparency;
      Material.FrontProperties.Diffuse.Alpha := 0;
      pitchAngle := 90;
      Height := 0.35;
      ApexHeight := 8;
      BaseDepth := 0.1;
      BaseWidth := 0.35;
      position.SetPoint(0.25, 0, 0.25);
    end;
  end;

  FUIMovementLineYZ := TVXGizmoExUILines(FUIRootMovement.addnewChild(TVXGizmoExUILines));
  with FUIMovementLineYZ do
  begin
    LineWidth := 1;
    Options := [loUseNodeColorForLines];
    NodesAspect := lnaInvisible;
    SplineMode := lsmSegments;
    addNode(0, 0, 0.4);
    TVXLinesNode(Nodes[0]).Color.color := clrBlue;
    addNode(0, 0.4, 0.4);
    TVXLinesNode(Nodes[1]).Color.color := clrBlue;
    addNode(0, 0.4, 0.4);
    TVXLinesNode(Nodes[2]).Color.color := clrLime;
    addNode(0, 0.4, 0);
    TVXLinesNode(Nodes[3]).Color.color := clrLime;
    FUIMovementPlaneYZ := TVXGizmoExUIPolyGon(addnewChild(TVXGizmoExUIPolyGon));
    with FUIMovementPlaneYZ do
    begin
      Material.MaterialOptions := [moNoLighting];
      Material.BlendingMode := bmTransparency;
      Material.FrontProperties.Diffuse.Color := clrYellow;
      Material.FrontProperties.Diffuse.Alpha := 0.01;
      addNode(0, 0.01, 0.39);
      addNode(0, 0.39, 0.39);
      addNode(0, 0.39, 0);
      addNode(0, 0.01, 0);
    end;
    // Raycast pickable object
    FUIICMovementLineYZ := TVXGizmoExUIFrustrum(AddNewChild(TVXGizmoExUIFrustrum));
    with FUIICMovementLineYZ do
    begin
      Material.MaterialOptions := [moNoLighting];
      Material.BlendingMode := bmTransparency;
      Material.FrontProperties.Diffuse.Alpha := 0;
      Up.SetVector(0, 0, 1);
      Height := 0.35;
      ApexHeight := 8;
      BaseDepth := 0.1;
      BaseWidth := 0.35;
      position.SetPoint(0, 0.25, 0.25);
    end;
  end;

  //Rotate


  FUIRotateLineXY := TVXGizmoExUILines(FUIRootRotate.addnewChild(TVXGizmoExUILines));
  with FUIRotateLineXY do
  begin
    LineColor.Color := clrGray50;
    SplineMode := lsmCubicSpline;
    NodesAspect := lnaInvisible;
    LineWidth := 1;
    Nodes.AddXYArc(1, 1, 0, 360, 24, AffineVectorMake(0, 0, 0));
    FUIRotateDiskXY := TVXGizmoExUIDisk(addnewChild(TVXGizmoExUIDisk));
    with FUIRotateDiskXY do
    begin
      OuterRadius := 1;
      Slices := 18;
      with Material do
      begin
        MaterialOptions := [moNoLighting];
        BlendingMode := bmTransparency;
        FrontProperties.Diffuse.Color := clrGray50;
        FrontProperties.Diffuse.Alpha := 0;
      end;
    end;
    FUIICRotateSphereXY := TVXGizmoExUISphere(addnewChild(TVXGizmoExUISphere));
    with FUIICRotateSphereXY do
    begin
      Radius := 1;
      Stop := 180;
      Slices := 18;
      TurnAngle := -90;
      with Material do
      begin
        MaterialOptions := [moNoLighting];
        BlendingMode := bmTransparency;
        FrontProperties.Diffuse.Color := clryellow;
        FrontProperties.Diffuse.Alpha := 0;
      end;
    end;
  end;

  FUIRotateLineXZ := TVXGizmoExUILines(FUIRootRotate.addnewChild(TVXGizmoExUILines));
  with FUIRotateLineXZ do
  begin
    LineColor.Color := clrGray75;
    SplineMode := lsmCubicSpline;
    NodesAspect := lnaInvisible;
    LineWidth := 1;
    Nodes.AddXYArc(1.3, 1.3, 0, 360, 24, AffineVectorMake(0, 0, 0));
    FUIICRotateTorusXZ := TVXGizmoExUITorus(addnewChild(TVXGizmoExUITorus));
    with FUIICRotateTorusXZ do
    begin
      Rings := 18;
      Sides := 0;
      MajorRadius := 1.3;
      MinorRadius := 0.07;
      with material do
      begin
        FaceCulling := fcNoCull;
        MaterialOptions := [moNoLighting];
        BlendingMode := bmTransparency;
        FrontProperties.Diffuse.Color := clrYellow;
        FrontProperties.Diffuse.Alpha := 0;
      end;
    end;
  end;

  FUIRotateLineX := TVXGizmoExUILines(FUIRootRotate.addnewChild(TVXGizmoExUILines));
  with FUIRotateLineX do
  begin
    Options := [loUseNodeColorForLines];
    //ƒл€ исправлени€ проблем с прозрачностью
    lineColor.Alpha := 0.1;
    Nodecolor.Color := clrred;
    Nodecolor.Alpha := 0.1;
    TurnAngle := 90;
    SplineMode := lsmCubicSpline;
    NodesAspect := lnaInvisible;
    LineWidth := 1;
    Nodes.AddXYArc(1, 1, 0, 360, 24, AffineVectorMake(0, 0, 0));
    for I := 0 to 24 do
    begin
      TVXLinesNode(Nodes[I]).Color.color := clrred;
    end;
  end;

  FUIRotateLineArrowX := TVXGizmoExUILines(FUIRootRotate.addnewChild(TVXGizmoExUILines));
  with FUIRotateLineArrowX do
  begin
    LineColor.Color := clrRed;
    LineWidth := 1;
    NodesAspect := lnaInvisible;
    AddNode(0, 0, 0);
    AddNode(0.4, 0, 0);
  end;

  FUIRotateDiskX := TVXGizmoExUIDisk(FUIRootRotate.addnewChild(TVXGizmoExUIDisk));
  with FUIRotateDiskX do
  begin
    OuterRadius := 1.01;
    Slices := 18;
    sweepangle := 10;
    StartAngle := 0;
    TurnAngle := 90;
    with Material do
    begin
      FaceCulling := fcNoCull;
      MaterialOptions := [moNoLighting];
      BlendingMode := bmTransparency;
      FrontProperties.Diffuse.Color := clrred;
      FrontProperties.Diffuse.Alpha := 0;
    end;
  end;

  FUIRotateDiskX2 := TVXGizmoExUIDisk(FUIRootRotate.addnewChild(TVXGizmoExUIDisk));
  with FUIRotateDiskX2 do
  begin
    OuterRadius := 1.01;
    Slices := 18;
    sweepangle := 10;
    StartAngle := 0;
    TurnAngle := 90;
    with Material do
    begin
      FaceCulling := fcNoCull;
      MaterialOptions := [moNoLighting];
      BlendingMode := bmTransparency;
      FrontProperties.Diffuse.Color := clrred;
      FrontProperties.Diffuse.Alpha := 0;
    end;
  end;

  FUIICRotateTorusX := TVXGizmoExUITorus(FUIRootRotate.addnewChild(TVXGizmoExUITorus));
  with FUIICRotateTorusX do
  begin
    Rings := 18;
    Sides := 0;
    MajorRadius := 1;
    MinorRadius := 0.07;
    TurnAngle := 90;
    with material do
    begin
      FaceCulling := fcNoCull;
      MaterialOptions := [moNoLighting];
      BlendingMode := bmTransparency;
      FrontProperties.Diffuse.Color := clrYellow;
      FrontProperties.Diffuse.Alpha := 0;
    end;
  end;

  FUIRotateLineY := TVXGizmoExUILines(FUIRootRotate.addnewChild(TVXGizmoExUILines));
  with FUIRotateLineY do
  begin
    Options := [loUseNodeColorForLines];
    //ƒл€ исправлени€ проблем с прозрачностью
    lineColor.Alpha := 0.1;
    Nodecolor.Color := clrLime;
    Nodecolor.Alpha := 0.1;

    SplineMode := lsmCubicSpline;
    NodesAspect := lnaInvisible;
    LineWidth := 1;
    Nodes.AddXYArc(1, 1, 0, 360, 24, AffineVectorMake(0, 0, 0));
    PitchAngle := 90;
    for I := 0 to 24 do
    begin
      TVXLinesNode(Nodes[I]).Color.color := clrLime;
    end;
  end;

  FUIRotateLineArrowY := TVXGizmoExUILines(FUIRootRotate.addnewChild(TVXGizmoExUILines));
  with FUIRotateLineArrowY do
  begin
    LineColor.Color := clrLime;
    LineWidth := 1;
    NodesAspect := lnaInvisible;
    AddNode(0, 0, 0);
    AddNode(0, 0.4, 0);
  end;

  FUIRotateDiskY := TVXGizmoExUIDisk(FUIRootRotate.addnewChild(TVXGizmoExUIDisk));
  with FUIRotateDiskY do
  begin
    OuterRadius := 1;
    Slices := 18;
    sweepangle := 20;
    startangle := 0;
    PitchAngle := 90;
    with Material do
    begin
      FaceCulling := fcNoCull;
      MaterialOptions := [moNoLighting];
      BlendingMode := bmTransparency;
      FrontProperties.Diffuse.Color := clrLime;
      FrontProperties.Diffuse.Alpha := 0;
    end;
  end;

  FUIRotateDiskY2 := TVXGizmoExUIDisk(FUIRootRotate.addnewChild(TVXGizmoExUIDisk));
  with FUIRotateDiskY2 do
  begin
    OuterRadius := 1;
    Slices := 18;
    sweepangle := 20;
    startangle := 0;
    PitchAngle := 90;
    with Material do
    begin
      FaceCulling := fcNoCull;
      MaterialOptions := [moNoLighting];
      BlendingMode := bmTransparency;
      FrontProperties.Diffuse.Color := clrLime;
      FrontProperties.Diffuse.Alpha := 0;
    end;
  end;

  FUIICRotateTorusY := TVXGizmoExUITorus(FUIRootRotate.addnewChild(TVXGizmoExUITorus));
  with FUIICRotateTorusY do
  begin
    Rings := 18;
    Sides := 0;
    MajorRadius := 1;
    MinorRadius := 0.07;
    PitchAngle := 90;
    with material do
    begin
      FaceCulling := fcNoCull;
      MaterialOptions := [moNoLighting];
      BlendingMode := bmTransparency;
      FrontProperties.Diffuse.Color := clrYellow;
      FrontProperties.Diffuse.Alpha := 0;
    end;
  end;

  FUIRotateLineZ := TVXGizmoExUILines(FUIRootRotate.addnewChild(TVXGizmoExUILines));
  with FUIRotateLineZ do
  begin
    Options := [loUseNodeColorForLines];
    //to correct transparency problem
    lineColor.Alpha := 0.1;
    Nodecolor.Color := clrBlue;
    Nodecolor.Alpha := 0.1;

    SplineMode := lsmCubicSpline;
    NodesAspect := lnaInvisible;
    LineWidth := 1;
    Nodes.AddXYArc(1, 1, 0, 360, 24, AffineVectorMake(0, 0, 0));
    for I := 0 to 24 do
    begin
      TVXLinesNode(Nodes[I]).Color.color := clrBlue;
    end;
  end;

  FUIRotateLineArrowZ := TVXGizmoExUILines(FUIRootRotate.addnewChild(TVXGizmoExUILines));
  with FUIRotateLineArrowZ do
  begin
    LineColor.Color := clrBlue;
    LineWidth := 1;
    NodesAspect := lnaInvisible;
    AddNode(0, 0, 0);
    AddNode(0, 0, 0.4);
  end;

  FUIRotateDiskZ := TVXGizmoExUIDisk(FUIRootRotate.addnewChild(TVXGizmoExUIDisk));
  with FUIRotateDiskZ do
  begin
    OuterRadius := 1;
    Slices := 18;
    SweepAngle := 10;
    StartAngle := 0;
    with Material do
    begin
      FaceCulling := fcNoCull;
      MaterialOptions := [moNoLighting];
      BlendingMode := bmTransparency;
      FrontProperties.Diffuse.Color := clrBlue;
      BackProperties.Diffuse.Color := clrBlue;
      FrontProperties.Diffuse.Alpha := 0;
    end;
  end;

  FUIRotateDiskZ2 := TVXGizmoExUIDisk(FUIRootRotate.addnewChild(TVXGizmoExUIDisk));
  with FUIRotateDiskZ2 do
  begin
    OuterRadius := 1;
    Slices := 18;
    SweepAngle := 10;
    StartAngle := 0;
    with Material do
    begin
      FaceCulling := fcNoCull;
      MaterialOptions := [moNoLighting];
      BlendingMode := bmTransparency;
      FrontProperties.Diffuse.Color := clrBlue;
      FrontProperties.Diffuse.Alpha := 0;
    end;
  end;

  FUIICRotateTorusZ := TVXGizmoExUITorus(FUIRootRotate.addnewChild(TVXGizmoExUITorus));
  with FUIICRotateTorusZ do
  begin
    Rings := 18;
    Sides := 0;
    MajorRadius := 1;
    MinorRadius := 0.07;
    with material do
    begin
      FaceCulling := fcNoCull;
      MaterialOptions := [moNoLighting];
      BlendingMode := bmTransparency;
      FrontProperties.Diffuse.Color := clrYellow;
      FrontProperties.Diffuse.Alpha := 0;
    end;
  end;

  FUIRootRotateAxisLabel := FUIRootRotate.AddNewChild(TVXDummyCube);

  FUIRotateAxisLabelX := TVXGizmoExUIFlatText(FUIRootRotateAxisLabel.AddNewChild(TVXGizmoExUIFlatText));
  with FUIRotateAxisLabelX do
  begin
    ModulateColor.Color := clrRed;
    Alignment := taCenter;
    Layout := tlCenter;
    Options := Options + [ftoTwoSided];
    Position.X := 0.5;

    Scale.X := 0.010;
    Scale.Y := 0.010;
    Text := 'X';
  end;

  FUIRotateAxisLabelY := TVXGizmoExUIFlatText(FUIRootRotateAxisLabel.AddNewChild(TVXGizmoExUIFlatText));
  with FUIRotateAxisLabelY do
  begin
    ModulateColor.Color := clrLime;
    Alignment := taCenter;
    Layout := tlCenter;
    Options := Options + [ftoTwoSided];
    Position.Y := 0.5;
    Scale.X := 0.010;
    Scale.Y := 0.010;
    Text := 'Y';
  end;

  FUIRotateAxisLabelZ := TVXGizmoExUIFlatText(FUIRootRotateAxisLabel.AddNewChild(TVXGizmoExUIFlatText));
  with FUIRotateAxisLabelZ do
  begin
    ModulateColor.Color := clrBlue;
    Alignment := taCenter;
    Layout := tlCenter;
    Options := Options + [ftoTwoSided];
    Position.Z := 0.5;
    Scale.X := 0.010;
    Scale.Y := 0.010;
    Text := 'Z';
  end;

  //for Scale

  FUIScaleLineX := TVXGizmoExUILines(FUIRootScale.addnewChild(TVXGizmoExUILines));
  with FUIScaleLineX do
  begin
    LineColor.Color := clrRed;
    LineWidth := 1;
    NodesAspect := lnaInvisible;
    AddNode(0, 0, 0);
    AddNode(1, 0, 0);
    // Raycast pickable object
    FUIICScaleLineX := TVXGizmoExUIFrustrum(AddNewChild(TVXGizmoExUIFrustrum));
    with FUIICScaleLineX do
    begin
      Material.MaterialOptions := [moNoLighting];
      Material.BlendingMode := bmTransparency;
      Material.FrontProperties.Diffuse.Color := clrYellow;
      Material.FrontProperties.Diffuse.Alpha := 0;
      Up.SetVector(1, 0, 0);
      Height := 0.5;
      ApexHeight := 8;
      BaseDepth := 0.15;
      BaseWidth := 0.15;
      position.SetPoint(0.8, 0, 0);
    end;
  end;

  FUIScaleLineY := TVXGizmoExUILines(FUIRootScale.addnewChild(TVXGizmoExUILines));
  with FUIScaleLineY do
  begin
    LineColor.Color := clrLime;
    LineWidth := 1;
    NodesAspect := lnaInvisible;
    AddNode(0, 0, 0);
    AddNode(0, 1, 0);
    // Raycast pickable object
    FUIICScaleLineY := TVXGizmoExUIFrustrum(AddNewChild(TVXGizmoExUIFrustrum));
    with FUIICScaleLineY do
    begin
      Material.MaterialOptions := [moNoLighting];
      Material.BlendingMode := bmTransparency;
      Material.FrontProperties.Diffuse.Color := clrYellow;
      Material.FrontProperties.Diffuse.Alpha := 0;
      Up.SetVector(0, 1, 0);
      Height := 0.5;
      ApexHeight := 8;
      BaseDepth := 0.15;
      BaseWidth := 0.15;
      position.SetPoint(0, 0.8, 0);
    end;
  end;

  FUIScaleLineZ := TVXGizmoExUILines(FUIRootScale.addnewChild(TVXGizmoExUILines));
  with FUIScaleLineZ do
  begin
    LineColor.Color := clrBlue;
    LineWidth := 1;
    NodesAspect := lnaInvisible;
    AddNode(0, 0, 0);
    AddNode(0, 0, 1);
    // Raycast pickable object
    FUIICScaleLineZ := TVXGizmoExUIFrustrum(AddNewChild(TVXGizmoExUIFrustrum));
    with FUIICScaleLineZ do
    begin
      Material.MaterialOptions := [moNoLighting];
      Material.BlendingMode := bmTransparency;
      Material.FrontProperties.Diffuse.Color := clrYellow;
      Material.FrontProperties.Diffuse.Alpha := 0;
      Up.SetVector(0, 0, 1);
      Height := 0.5;
      ApexHeight := 8;
      BaseDepth := 0.1;
      BaseWidth := 0.1;
      position.SetPoint(0, 0, 0.8);
    end;
  end;

  FUIScaleLineXY := TVXGizmoExUILines(FUIRootScale.addnewChild(TVXGizmoExUILines));
  with FUIScaleLineXY do
  begin
    Options := [loUseNodeColorForLines];
    SplineMode := lsmSegments;
    LineColor.Color := clrRed;
    LineWidth := 1;
    NodesAspect := lnaInvisible;
    AddNode(0, 0.7, 0);
    AddNode(0.35, 0.35, 0);
    TVXLinesNode(Nodes[0]).Color.color := clrLime;
    TVXLinesNode(Nodes[1]).Color.color := clrLime;
    AddNode(0.35, 0.35, 0);
    AddNode(0.7, 0, 0);
    TVXLinesNode(Nodes[2]).Color.color := clrRed;
    TVXLinesNode(Nodes[3]).Color.color := clrRed;
    AddNode(0.5, 0, 0);
    AddNode(0.25, 0.25, 0);
    TVXLinesNode(Nodes[4]).Color.color := clrRed;
    TVXLinesNode(Nodes[5]).Color.color := clrRed;
    AddNode(0.25, 0.25, 0);
    AddNode(0, 0.5, 0);
    TVXLinesNode(Nodes[6]).Color.color := clrLime;
    TVXLinesNode(Nodes[7]).Color.color := clrLime;
    FUIScalePlaneXY := TVXGizmoExUIPolyGon(addnewChild(TVXGizmoExUIPolyGon));
    with FUIScalePlaneXY do
    begin
      with Material do
      begin
        MaterialOptions := [moNoLighting];
        BlendingMode := bmTransparency;
        FrontProperties.Diffuse.Color := clrYellow;
        FrontProperties.Diffuse.Alpha := 0.01;
      end;
      AddNode(0, 0.7, 0);
      AddNode(0.35, 0.35, 0);
      AddNode(0.7, 0, 0);
      AddNode(0.5, 0, 0);
      AddNode(0.25, 0.25, 0);
      AddNode(0, 0.5, 0);
    end;
    // Raycast pickable object
    FUIICScaleLineXY := TVXGizmoExUIFrustrum(AddNewChild(TVXGizmoExUIFrustrum));
    with FUIICScaleLineXY do
    begin
      Material.MaterialOptions := [moNoLighting];
      Material.BlendingMode := bmTransparency;
      Material.FrontProperties.Diffuse.Color := clrYellow;
      Material.FrontProperties.Diffuse.Alpha := 0;
      rollAngle := 45;
      turnAngle := 45;
      Height := 0.8;
      ApexHeight := 8;
      BaseDepth := 0.1;
      BaseWidth := 0.1;
      position.SetPoint(0.3, 0.3, 0);
    end;
  end;

  FUIScaleLineXZ := TVXGizmoExUILines(FUIRootScale.addnewChild(TVXGizmoExUILines));
  with FUIScaleLineXZ do
  begin
    Options := [loUseNodeColorForLines];
    SplineMode := lsmSegments;
    LineColor.Color := clrRed;
    LineWidth := 1;
    NodesAspect := lnaInvisible;
    AddNode(0.7, 0, 0);
    AddNode(0.35, 0, 0.35);
    TVXLinesNode(Nodes[0]).Color.color := clrRed;
    TVXLinesNode(Nodes[1]).Color.color := clrRed;
    AddNode(0.35, 0, 0.35);
    AddNode(0, 0, 0.7);
    TVXLinesNode(Nodes[2]).Color.color := clrBlue;
    TVXLinesNode(Nodes[3]).Color.color := clrBlue;
    AddNode(0, 0, 0.5);
    AddNode(0.25, 0, 0.25);
    TVXLinesNode(Nodes[4]).Color.color := clrBlue;
    TVXLinesNode(Nodes[5]).Color.color := clrBlue;
    AddNode(0.25, 0, 0.25);
    AddNode(0.5, 0, 0);
    TVXLinesNode(Nodes[6]).Color.color := clrRed;
    TVXLinesNode(Nodes[7]).Color.color := clrRed;
    FUIScalePlaneXZ := TVXGizmoExUIPolyGon(addnewChild(TVXGizmoExUIPolyGon));
    with FUIScalePlaneXZ do
    begin
      with Material do
      begin
        MaterialOptions := [moNoLighting];
        BlendingMode := bmTransparency;
        FrontProperties.Diffuse.Color := clrYellow;
        FrontProperties.Diffuse.Alpha := 0;
      end;
      AddNode(0.7, 0, 0);
      AddNode(0.35, 0, 0.35);
      AddNode(0, 0, 0.7);
      AddNode(0, 0, 0.5);
      AddNode(0.25, 0, 0.25);
      AddNode(0.5, 0, 0);
    end;
    // Raycast pickable object
    FUIICScaleLineXZ := TVXGizmoExUIFrustrum(AddNewChild(TVXGizmoExUIFrustrum));
    with FUIICScaleLineXZ do
    begin
      Material.MaterialOptions := [moNoLighting];
      Material.BlendingMode := bmTransparency;
      Material.FrontProperties.Diffuse.Color := clrYellow;
      Material.FrontProperties.Diffuse.Alpha := 0;
      turnAngle := -45;
      pitchAngle := 90;
      Height := 0.8;
      ApexHeight := 8;
      BaseDepth := 0.1;
      BaseWidth := 0.1;
      position.SetPoint(0.3, 0, 0.3);
    end;
  end;

  FUIScaleLineYZ := TVXGizmoExUILines(FUIRootScale.addnewChild(TVXGizmoExUILines));
  with FUIScaleLineYZ do
  begin
    Options := [loUseNodeColorForLines];
    SplineMode := lsmSegments;
    LineColor.Color := clrRed;
    LineWidth := 1;
    NodesAspect := lnaInvisible;
    AddNode(0, 0.7, 0);
    AddNode(0, 0.35, 0.35);
    TVXLinesNode(Nodes[0]).Color.color := clrLime;
    TVXLinesNode(Nodes[1]).Color.color := clrLime;
    AddNode(0, 0.35, 0.35);
    AddNode(0, 0, 0.7);
    TVXLinesNode(Nodes[2]).Color.color := clrBlue;
    TVXLinesNode(Nodes[3]).Color.color := clrBlue;
    AddNode(0, 0, 0.5);
    AddNode(0, 0.25, 0.25);
    TVXLinesNode(Nodes[4]).Color.color := clrBlue;
    TVXLinesNode(Nodes[5]).Color.color := clrBlue;
    AddNode(0, 0.25, 0.25);
    AddNode(0, 0.5, 0);
    TVXLinesNode(Nodes[6]).Color.color := clrLime;
    TVXLinesNode(Nodes[7]).Color.color := clrLime;

    FUIScalePlaneYZ := TVXGizmoExUIPolyGon(addnewChild(TVXGizmoExUIPolyGon));
    with FUIScalePlaneYZ do
    begin
      with Material do
      begin
        MaterialOptions := [moNoLighting];
        BlendingMode := bmTransparency;
        FrontProperties.Diffuse.Color := clrYellow;
        FrontProperties.Diffuse.Alpha := 0;
      end;
      AddNode(0, 0.7, 0);
      AddNode(0, 0.35, 0.35);
      AddNode(0, 0, 0.7);
      AddNode(0, 0, 0.5);
      AddNode(0, 0.25, 0.25);
      AddNode(0, 0.5, 0);
    end;
    // Raycast pickable object
    FUIICScaleLineYZ := TVXGizmoExUIFrustrum(AddNewChild(TVXGizmoExUIFrustrum));
    with FUIICScaleLineYZ do
    begin
      Material.MaterialOptions := [moNoLighting];
      Material.BlendingMode := bmTransparency;
      Material.FrontProperties.Diffuse.Color := clrYellow;
      Material.FrontProperties.Diffuse.Alpha := 0;
      pitchAngle := 45;
      Height := 0.8;
      ApexHeight := 8;
      BaseDepth := 0.1;
      BaseWidth := 0.1;
      position.SetPoint(0, 0.3, 0.3);
    end;
  end;

  FUIScalePlaneXYZ := TVXGizmoExUIPolyGon(FUIRootScale.addnewChild(TVXGizmoExUIPolyGon));
  with FUIScalePlaneXYZ do
  begin
    with Material do
    begin
      MaterialOptions := [moNoLighting];
      BlendingMode := bmTransparency;
      FrontProperties.Diffuse.Color := clrYellow;
      FrontProperties.Diffuse.Alpha := 0;
    end;
    AddNode(0.5, 0, 0);
    AddNode(0, 0.5, 0);
    AddNode(0, 0, 0.5);
    AddNode(0.5, 0, 0);
    // Raycast pickable object
    FUIICScaleLineXYZ := TVXGizmoExUIFrustrum(FUIRootScale.AddNewChild(TVXGizmoExUIFrustrum));
    with FUIICScaleLineXYZ do
    begin
      Material.MaterialOptions := [moNoLighting];
      Material.BlendingMode := bmTransparency;
      Material.FrontProperties.Diffuse.Color := clrYellow;
      Material.FrontProperties.Diffuse.Alpha := 0;
      turnAngle := -45;
      rollAngle := 35;
      Height := 0.5;
      ApexHeight := 0.6;
      BaseDepth := 0.6;
      BaseWidth := 0.05;
      position.SetPoint(0.15, 0.2, 0.15);
    end;
  end;

  FUIScaleArrowX := TVXGizmoExUISphere(FUIRootScale.addnewChild(TVXGizmoExUISphere));
  with FUIScaleArrowX do
  begin
    Slices := 8;
    Stacks := 2;
    Radius := 0.04;
    Position.X := 1;
    with material do
    begin
      MaterialOptions := [moNoLighting];
      FrontProperties.Diffuse.Color := clrRed;
    end;
  end;

  FUIScaleArrowY := TVXGizmoExUISphere(FUIRootScale.addnewChild(TVXGizmoExUISphere));
  with FUIScaleArrowY do
  begin
    Slices := 8;
    Stacks := 2;
    Radius := 0.04;
    Position.Y := 1;
    with material do
    begin
      //FaceCulling := fcNoCull;
      // FrontProperties.PolygonMode := pmFill;
      // BackProperties.PolygonMode := pmFill;
      MaterialOptions := [moNoLighting];
      FrontProperties.Diffuse.Color := clrLime;
      //FrontProperties.Emission.Color := clrLime;
    end;
  end;

  FUIScaleArrowZ := TVXGizmoExUISphere(FUIRootScale.addnewChild(TVXGizmoExUISphere));
  with FUIScaleArrowZ do
  begin
    Slices := 8;
    Stacks := 2;
    Radius := 0.04;
    Position.Z := 1;
    with material do
    begin
      // FaceCulling := fcNoCull;
      //FrontProperties.PolygonMode := pmFill;
      //BackProperties.PolygonMode := pmFill;
      MaterialOptions := [moNoLighting];
      FrontProperties.Diffuse.Color := clrBlue;
      //FrontProperties.Emission.Color := clrBlue;
    end;
  end;

  //For Axis

  FUIAxisLabelX := TVXGizmoExUIFlatText(FUIRootAxisLabel.AddNewChild(TVXGizmoExUIFlatText));
  with FUIAxisLabelX do
  begin
    ModulateColor.Color := clrRed;
    Alignment := taCenter;
    Layout := tlCenter;
    Options := Options + [ftoTwoSided];
    Position.X := 1.3;

    Scale.X := 0.015;
    Scale.Y := 0.015;
    Text := 'X';
  end;

  FUIAxisLabelY := TVXGizmoExUIFlatText(FUIRootAxisLabel.AddNewChild(TVXGizmoExUIFlatText));
  with FUIAxisLabelY do
  begin
    ModulateColor.Color := clrLime;
    Alignment := taCenter;
    Layout := tlCenter;
    Options := Options + [ftoTwoSided];
    Position.Y := 1.3;
    Scale.X := 0.015;
    Scale.Y := 0.015;
    Text := 'Y';
  end;

  FUIAxisLabelZ := TVXGizmoExUIFlatText(FUIRootAxisLabel.AddNewChild(TVXGizmoExUIFlatText));
  with FUIAxisLabelZ do
  begin
    ModulateColor.Color := clrBlue;
    Alignment := taCenter;
    Layout := tlCenter;
    Options := Options + [ftoTwoSided];
    Position.Z := 1.3;
    Scale.X := 0.015;
    Scale.Y := 0.015;
    Text := 'Z';
  end;

  FUIVisibleInfoLabels := TVXGizmoExUIFlatText(FUIRootVisibleInfoLabels.AddNewChild(TVXGizmoExUIFlatText));
  with FUIVisibleInfoLabels do
  begin
    ModulateColor.Color := clrYellow;
    Alignment := taCenter;
    Layout := tlCenter;
    Options := Options + [ftoTwoSided];
    Position.Y := 1.8;
    Position.X := 0;
    Scale.X := 0.01;
    Scale.Y := 0.01;
    Text := '';
  end;

  HistoryStepsCount := 30;
  BoundingBoxColor.Color := clrWhite;
  VisibleInfoLabelsColor.Color := clrYellow;
  SelectedColor.Color := clrYellow;
  SelectionRegionColor.Color := clrBlack;

  ShowAxisLabel := True;
  ShowObjectInfos := True;
  ShowBoundingBox := True;
  FReferenceCoordSystem := rcsView;
  FEnableHistory := True;
  FinfoLabelCoordType := ilcChangeRate;
  AntiAliasedLines := True;
  FOperation := gopNone;
  FSelAxis := gaNone;
  EnableMultiSelection := True;
  FSelectionRegion := gsrRectangular;
  EnableLoopCursorMoving := True;
  FUIRootHelpers.Visible := False;
  OperationMode := gomSelect;
  FVisibleVisibleInfoLabels := FVisibleVisibleInfoLabels + [vliName, vliOperation, vliCoords];
  GizmoThickness := 1;
  AutoZoom := True;
  AutoZoomFactor := 5.0;
  ZoomFactor := 0.35;
  Enabled := True;
  FNoZWrite := True;
  FExcludeObjectsList := TStringList.Create;
  FExcludeClassNameList := TStringList.Create;
end;

destructor TVXGizmoEx.Destroy;
begin
  if Assigned(FRootGizmo) then
    FRootGizmo.DeleteChildren
  else
  begin
    FUIBaseGizmo.DeleteChildren;
    FUIBaseGizmo.Free;
  end;

  FRootObjects := nil;
  FGizmoTmpRoot := nil;

  FBoundingBoxColor.Free;
  SelectionRegionColor.Free;
  FSelectedObjects.Free;
  FSelectedColor.Free;
  FVisibleInfoLabelsColor.Free;
  FExcludeObjectsList.Free;
  FExcludeClassNameList.Free;

  // FUndoHistory has to be nil before Notification() is called.
  FreeAndNil(FHistory);
  inherited Destroy;
end;

procedure TVXGizmoEx.SetVisible(const AValue: Boolean);
begin
  FUIBaseGizmo.Visible := AValue;
end;

function TVXGizmoEx.GetVisible: Boolean;
begin
  Result := FUIBaseGizmo.Visible;
end;

procedure TVXGizmoEx.SetSelectionRegion(const AValue: TVXGizmoExSelectionRegion);
begin
  if FSelectionRegion <> AValue then
    FSelectionRegion := AValue;
end;

procedure TVXGizmoEx.SetShowAxisLabel(const AValue: Boolean);
begin
  if FShowAxisLabel <> AValue then
  begin
    FShowAxisLabel := AValue;
    FUIRootRotateAxisLabel.Visible := AValue;
    if FOperationMode <> gomRotate then
      FUIRootAxisLabel.Visible := AValue;
  end;
end;

procedure TVXGizmoEx.SetSelAxis(aValue: TVXGizmoExAxis);
begin
  if FSelAxis <> aValue then
  begin
    FSelAxis := aValue;
    if Assigned(OnAxisSelected) then
      OnAxisSelected(self, FSelAxis);
  end;
end;

procedure TVXGizmoEx.SetPickMode(APickMode: TVXGizmoExPickMode);
begin
  if APickMode <> FPickMode then
    FPickMode := APickMode;
end;

procedure TVXGizmoEx.SetAutoZoomFactor(const AValue: Single);
begin
  if (FAutoZoomFactor <> AValue) and (AValue > 0) then
  begin
    FAutoZoomFactor := AValue;
    UpdateGizmo;
  end;
end;

procedure TVXGizmoEx.SetZoomFactor(const AValue: Single);
begin
  if (FZoomFactor <> AValue) and (AValue > 0) then
  begin
    FZoomFactor := AValue;
    UpdateGizmo;
  end;
end;

procedure TVXGizmoEx.SetShowObjectInfos(const AValue: Boolean);
begin
  if FShowObjectInfos <> AValue then
  begin
    FShowObjectInfos := AValue;
    FUIRootVisibleInfoLabels.Visible := FShowObjectInfos;
  end;
end;

procedure TVXGizmoEx.SetShowBoundingBox(const AValue: Boolean);
begin
  if FShowBoundingBox <> AValue then
    FShowBoundingBox := AValue;
end;

function TVXGizmoEx.GetPickList: TVXPickList;
begin
  Result := FSelectedObjects;
end;

procedure TVXGizmoEx.SetPickList(aValue: TVXPickList);
var
  I: Integer;
begin
  if FSelectedObjects <> aValue then
    if aValue.Count - 1 >= 0 then
    begin
      FSelectedObjects.Clear;
      for I := 0 to aValue.Count - 1 do
        with aValue do
          FSelectedObjects.AddHit(hit[I], SubObjects[I], NearDistance[I], FarDistance[I]);
      UpdateGizmo();
    end
    else
      LooseSelection();
end;

procedure TVXGizmoEx.SetSelectedObj(const Value: TVXBaseSceneObject);
begin
  if FSelectedObjects.FindObject(Value) <> -1 then
    Exit;
  if (FSelectedObjects.Count - 1 >= 0) or (Value = nil) then
    ClearSelection;
  if Value <> nil then
    FSelectedObjects.AddHit(Value, nil, 0, 0);
  UpdateGizmo();
end;

function TVXGizmoEx.GetSelectedObj: TVXBaseSceneObject;
begin
  Result := nil;
  if FSelectedObjects.Count - 1 = -1 then
    Result := nil
  else
  if FSelectedObjects.Count - 1 >= 0 then
    Result := TVXBaseSceneObject(FSelectedObjects.Hit[0]);
end;

procedure TVXGizmoEx.AddObjToSelectionList(Obj: TVXBaseSceneObject);
begin
  if (Obj <> nil) and (FSelectedObjects.FindObject(Obj) = -1) then
    FSelectedObjects.AddHit(Obj, nil, 0, 0);
end;

procedure TVXGizmoEx.RemoveObjFromSelectionList(Obj: TVXBaseSceneObject);
var
  I: Integer;
begin
  I := FSelectedObjects.FindObject(Obj);
  if I <> -1 then
    FSelectedObjects.Delete(I);
end;

procedure TVXGizmoEx.AssignPickList(aList: TVXPickList; RemoveObj: Boolean = False);

  function WithOutGizmoElements(obj: TVXBasesceneobject): Boolean;
  begin
    if (obj <> FInterfaceRender) and
      (obj <> FInternalRender) and not (obj is TVXGizmoExUISphere) and not (obj is TVXGizmoExUIPolyGon) and not (obj is TVXGizmoExUITorus) and not (obj is TVXGizmoExUIFrustrum) and not (obj is TVXGizmoExUIArrowLine) and not (obj is TVXGizmoExUILines) and not (obj is TVXGizmoExUIDisk) and not (obj is TVXGizmoExUIFlatText) and not (CheckObjectInExcludeList(obj)) and not (CheckClassNameInExcludeList(obj)) then
      Result := True
    else
      Result := False;
  end;

var
  I: Integer;
begin
  for I := 0 to aList.Count - 1 do
    with aList do
      if WithOutGizmoElements(TVXBaseSceneObject(Hit[I])) then
        if not RemoveObj then
        begin
          if (Hit[I] <> nil) and (FSelectedObjects.FindObject(Hit[I]) = -1) then
            FSelectedObjects.AddHit(Hit[I], SubObjects[I], NearDistance[I], FarDistance[I]);
        end
        else
        if (Hit[I] <> nil) and (FSelectedObjects.FindObject(Hit[I]) <> -1) then
          FSelectedObjects.Delete(FSelectedObjects.FindObject(Hit[I]));
end;



procedure TVXGizmoEx.InterfaceRender(Sender: TObject; var rci: TVXRenderContextInfo);

  procedure cLine(glc: TVXCanvas; p1, p2: TPoint);
  begin
    glc.Line(p1.X, p1.Y, p2.X, p2.Y);
  end;

var
  glc: TVXCanvas;
  I:   Integer;
  LastCurPosX, LastCurPosY, CurPosX, CurPosY: Single;
begin

  if (not Enabled) or (RootGizmo = nil) or (RootObjects = nil) then
    Exit;

  //here takes place rendering of lines and circles on canvas
  //according to modes, it's a pity that canvas has restrictions
  if FShowMultiSelecting then
  begin
    glc := TVXCanvas.Create(Round(Viewer.Width), Round(Viewer.Height));
    glc.PenColor := FSelectionRegionColor.AsWinColor;
    glc.PenWidth := 1;
    LastCurPosX := fLastCursorPos.X;
    LastCurPosY := fLastCursorPos.Y;
    CurPosX := fCursorPos.X;
    CurPosY := fCursorPos.Y;
    with glc do
      case FSelectionRegion of
        gsrRectangular: FrameRect(LastCurPosX, LastCurPosY, CurPosX, CurPosY);
        gsrCircular: Ellipse(LastCurPosX, LastCurPosY,
            MaxFloat(abs(CurPosX - LastCurPosX),
            abs(CurPosY - LastCurPosY)));
        gsrFence:
        begin
          for I := Low(FSelectionRec) to High(FSelectionRec) do
            if I <> High(FSelectionRec) then
              cLine(glc, FSelectionRec[I], FSelectionRec[I + 1])
            else
              cLine(glc, FSelectionRec[I], fcursorPos);

          //glc.PenWidth thickness of rectangle
          //it's necessary to show that the begining and the end
          // of a figure are joining and when cursor is near begining of array
          // then appears square, that show to user that he picked right object
          if High(FSelectionRec) > 0 then
            with FSelectionRec[Low(FSelectionRec)] do
              if IsInRange(CurPosX, X + 2, X - 2) and IsInRange(CurPosY, Y + 2, Y - 2) then
                FillRect(CurPosX - PenWidth - 2, CurPosY - PenWidth - 2,
                  CurPosX + PenWidth + 2, CurPosY + PenWidth + 2);
        end;
        gsrLasso:
        begin
          //here showing arrays of lines
          //when additional line formed by begining and and of array
          for I := Low(FSelectionRec) to High(FSelectionRec) do
            if I <> High(FSelectionRec) then
              cLine(glc, FSelectionRec[I], FSelectionRec[I + 1])
            else
              cLine(glc, FSelectionRec[I], FSelectionRec[Low(FSelectionRec)]);
        end;
      end;
    glc.Destroy;
  end;
end;

procedure TVXGizmoEx.InternalRender(Sender: TObject; var rci: TVXRenderContextInfo);

  procedure ShowBoundingBox(aObject: TVXBaseSceneObject);
  const
    ACorners: array [0..7, 0..2] of Byte = ((1, 3, 4),
      (0, 2, 5),
      (1, 6, 3),
      (0, 2, 7),
      (0, 5, 7),
      (1, 4, 6),
      (2, 5, 7),
      (3, 4, 6));
  var
    I, J:    Byte;
    BB:      THmgBoundingBox;
    AVector: TVector;
  begin
    if aObject = nil then
      Exit;

    BB := aObject.BoundingBoxAbsolute(False);
    for I := 0 to 7 do
    begin
      for J := 0 to 2 do
      begin
        AVector := VectorSubtract(BB.BBox[ACorners[I][J]], BB.BBox[I]);
        AVector := VectorScale(AVector, 0.25);
        AVector := VectorAdd(AVector, BB.BBox[I]);

        glBegin(GL_LINES);
        glVertex3f(BB.BBox[I].X, BB.BBox[I].Y, BB.BBox[I].Z);
        glVertex3f(AVector.X, AVector.Y, AVector.Z);
        glEnd;
      end;
    end;
  end;

  //test#12 result is positive, but only for 2d
  //
  procedure ShowText(const Text: UnicodeString; Position: Tvector; Scale: TVector; Color: Tvector);
  var
    FLayout: TVXTextLayout;
    FAlignment: TAlignment;
    wm:   TMatrix;
    I, J: Integer;
  begin
    if not Assigned(FLabelFont) and (Text = '') then
      Exit;
    rci.VXStates.Enable(stDepthTest);
    FLayout := VXS.CrossPlatform.tlCenter;
    FAlignment := taCenter;

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
    wm := rci.PipelineTransformation.ViewMatrix^;

    TransposeMatrix(wm);

    for I := 0 to 2 do
      for J := 0 to 2 do
        if I = J then
          wm.V[I].C[J] := 1
        else
          wm.V[I].C[J] := 0;
    glLoadMatrixf(@wm);

    rci.VXStates.PolygonMode := pmFill;
    glScalef(Scale.X, Scale.Y, Scale.Z);
    glTranslatef(Position.X, Position.Y, Position.Z);


    if Color.W <> 1 then
    begin
      rci.VXStates.Enable(stBlend);
      rci.VXStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
    end;
    rci.VXStates.Disable(stDepthTest);
    rci.VXStates.Disable(stCullFace);

    FLabelFont.RenderString(rci, Text, FAlignment, FLayout, Color);
    glPopMatrix;

  end;

var
  I: Integer;
begin
  if (not Enabled) or (RootGizmo = nil) or (RootObjects = nil) then
    Exit;

  if FShowBoundingBox and (FSelectedObjects.Count - 1 >= 0) then
  begin
    rci.VXStates.Disable(stLighting);
    if FAntiAliasedLines then
      rci.VXStates.Enable(stLineSmooth);

    if (FGizmoThickness >= 0.5) and (FGizmoThickness <= 7) then
      rci.VXStates.LineWidth := FGizmoThickness
    else
      rci.VXStates.LineWidth := 1;

    glColorMaterial(GL_FRONT, GL_EMISSION);
    rci.VXStates.Enable(stColorMaterial);

    glColor4fv(@FBoundingBoxColor.Color);

    for I := 0 to FSelectedObjects.Count - 1 do
      ShowBoundingBox(TVXBaseSceneObject(FSelectedObjects.Hit[I]));

  end;
  rci.VXStates.Disable(stColorMaterial);
end;

procedure TVXGizmoEx.SetReferenceCoordSystem(aValue: TVXGizmoExReferenceCoordinateSystem);
begin
  if FReferenceCoordSystem <> aValue then
  begin
    FReferenceCoordSystem := aValue;
    UpdateGizmo;
  end;
end;

procedure TVXGizmoEx.SetHistoryStepsCount(aValue: Integer);
begin
  if (FHistoryStepsCount <> aValue) and (aValue > 5) then
  begin
    FHistoryStepsCount := aValue;
    FHistory.FItemsMaxCount := aValue;
  end;
end;

procedure TVXGizmoEx.SetCanChangeWithChildren(AValue: Boolean);
begin
  if FCanChangeWithChildren <> AValue then
    FCanChangeWithChildren := AValue;
end;

procedure TVXGizmoEx.SetAALines(aValue: Boolean);
begin
  if FAntiAliasedLines <> aValue then
  begin
    FAntiAliasedLines := aValue;
    FUISelectLineX.AntiAliased := aValue;
    FUISelectLineY.AntiAliased := aValue;
    FUISelectLineZ.AntiAliased := aValue;

    FUIMovementLineX.AntiAliased := aValue;
    FUIMovementLineY.AntiAliased := aValue;
    FUIMovementLineZ.AntiAliased := aValue;
    FUIMovementLineXY.AntiAliased := aValue;
    FUIMovementLineXZ.AntiAliased := aValue;
    FUIMovementLineYZ.AntiAliased := aValue;

    FUIRotateLineX.AntiAliased := aValue;
    FUIRotateLineY.AntiAliased := aValue;
    FUIRotateLineZ.AntiAliased := aValue;
    FUIrotateLineXY.AntiAliased := aValue;
    FUIRotateLineXZ.AntiAliased := aValue;
    FUIRotateLineArrowX.AntiAliased := aValue;
    FUIRotateLineArrowY.AntiAliased := aValue;
    FUIRotateLineArrowZ.AntiAliased := aValue;

    FUIScaleLineX.AntiAliased := aValue;
    FUIScaleLineY.AntiAliased := aValue;
    FUIScaleLineZ.AntiAliased := aValue;
    FUIScaleLineXY.AntiAliased := aValue;
    FUIScaleLineXZ.AntiAliased := aValue;
    FUIScaleLineYZ.AntiAliased := aValue;
  end;
end;

procedure TVXGizmoEx.SetInfoLabelCoordType(aValue: TInfoLabelCoordType);
begin
  if fInfoLabelCoordType <> aValue then
  begin
    fInfoLabelCoordType := aValue;
    UpdateVisibleInfoLabels;
  end;
end;

procedure TVXGizmoEx.SetAngleDisk(aAngle: Single);
var
  Disk1alpha, Disk2alpha, Disk1Angle, Disk2Angle: Single;
begin
  Disk1alpha := 0;
  Disk2alpha := 0;
  Disk1Angle := 0;
  Disk2Angle := 0;
  if aAngle = 0 then
  begin
    fchangerate := NullVector;
    FUIRotateDiskX.SweepAngle := 0;
    FUIRotateDiskY.SweepAngle := 0;
    FUIRotateDiskZ.SweepAngle := 0;
    FUIRotateDiskX.Material.FrontProperties.Diffuse.Alpha := 0;
    FUIRotateDiskY.Material.FrontProperties.Diffuse.Alpha := 0;
    FUIRotateDiskZ.Material.FrontProperties.Diffuse.Alpha := 0;
    FUIRotateDiskX2.SweepAngle := 0;
    FUIRotateDiskY2.SweepAngle := 0;
    FUIRotateDiskZ2.SweepAngle := 0;
    FUIRotateDiskX2.Material.FrontProperties.Diffuse.Alpha := 0;
    FUIRotateDiskY2.Material.FrontProperties.Diffuse.Alpha := 0;
    FUIRotateDiskZ2.Material.FrontProperties.Diffuse.Alpha := 0;
  end
  else
  if (abs(aAngle) > 0) and (abs(aAngle) <= 360) then
  begin
    Disk1alpha := 0.3;
    Disk2alpha := 0;
    Disk1Angle := aAngle;
    Disk2Angle := 0;
  end
  else
  if (abs(aAngle) > 360) and (abs(aAngle) <= 720) then
  begin
    Disk1alpha := 0.3;
    Disk2alpha := 0.3;
    Disk1Angle := 360;
    if aAngle > 0 then
      Disk2Angle := aAngle - 360
    else
      Disk2Angle := aAngle + 360;
  end
  else
  if (abs(aAngle) > 720) and (abs(aAngle) <= 1080) then
  begin
    Disk1alpha := 0.5;
    Disk2alpha := 0.3;
    Disk1Angle := 360;
    if aAngle > 0 then
      Disk2Angle := aAngle - 720
    else
      Disk2Angle := aAngle + 720;
  end
  else
  if (abs(aAngle) > 1080) and (abs(aAngle) <= 1440) then
  begin
    Disk1alpha := 0.6;
    Disk2alpha := 0.3;
    Disk1Angle := 360;
    if aAngle > 0 then
      Disk2Angle := aAngle - 1080
    else
      Disk2Angle := aAngle + 1080;
  end
  else
  if (abs(aAngle) > 1440) then
  begin
    Disk1alpha := 0.6;
    Disk2alpha := 0.3;
    Disk1Angle := 360;
    Disk2Angle := 360;
  end;

  case SelAxis of
    gaX:
    begin
      FUIRotateDiskX.SweepAngle := Disk1Angle;
      FUIRotateDiskX.Material.FrontProperties.Diffuse.Alpha := Disk1alpha;
      FUIRotateDiskX2.SweepAngle := Disk2Angle;
      FUIRotateDiskX2.Material.FrontProperties.Diffuse.Alpha := Disk2alpha;
    end;
    gaY:
    begin
      FUIRotateDiskY.SweepAngle := Disk1Angle;
      FUIRotateDiskY.Material.FrontProperties.Diffuse.Alpha := Disk1alpha;
      FUIRotateDiskY2.SweepAngle := Disk2Angle;
      FUIRotateDiskY2.Material.FrontProperties.Diffuse.Alpha := Disk2alpha;
    end;
    gaZ:
    begin
      FUIRotateDiskZ.SweepAngle := Disk1Angle;
      FUIRotateDiskZ.Material.FrontProperties.Diffuse.Alpha := Disk1alpha;
      FUIRotateDiskZ2.SweepAngle := Disk2Angle;
      FUIRotateDiskZ2.Material.FrontProperties.Diffuse.Alpha := Disk2alpha;
    end;
  end;

end;

procedure TVXGizmoEx.SetBoundingBoxColor(const AValue: TVXColor);
begin
  if AValue <> FBoundingBoxColor then
  begin
    FBoundingBoxColor.Color := AValue.Color;
    UpdateGizmo;
  end;
end;

procedure TVXGizmoEx.SetSelectedColor(const AValue: TVXColor);
begin
  if AValue <> FSelectedColor then
  begin
    FSelectedColor.Color := AValue.Color;
    UpdateGizmo;
  end;
end;

procedure TVXGizmoEx.SetOperation(const Value: TVXGizmoExOperation);
begin
  if FOperation <> Value then
  begin
    FOperation := Value;
    if Assigned(OnOperationChange) then
      OnOperationChange(self);
  end;
end;

procedure TVXGizmoEx.SetOperationMode(const Value: TVXGizmoExOperationMode);
begin
  if FOperationMode <> Value then
  begin
    FOperationMode := Value;

    if Value = gomNone then
    begin
      Visible := False;
      Enabled := False;
    end
    else
    begin
      Visible := True;
      Enabled := True;
    end;

    if Value = gomSelect then
      FUIRootSelect.Visible := True
    else
      FUIRootSelect.Visible := False;

    if Value = gomMove then
      FUIRootMovement.Visible := True
    else
      FUIRootMovement.Visible := False;

    if Value = gomrotate then
    begin
      FUIRootAxisLabel.Visible := False;
      FUIRootRotate.Visible := True;
    end
    else
    begin
      FUIRootRotate.Visible := False;
      FUIRootAxisLabel.Visible := ShowAxisLabel;
    end;

    if Value = gomscale then
      FUIRootScale.Visible := True
    else
      FUIRootScale.Visible := False;

    if Assigned(OnOperationModeChange) then
      OnOperationModeChange(self);
    UpdateGizmo;
  end;
end;

procedure TVXGizmoEx.SetNoZWrite(const Value: Boolean);
begin
  if fNoZWrite <> Value then
  begin
    fNoZWrite := Value;

    //For Select
    FUISelectLineX.NoZWrite := Value;
    FUISelectLineY.NoZWrite := Value;
    FUISelectLineZ.NoZWrite := Value;

    //For Move
    FUIMovementLineX.NoZWrite := Value;
    FUIMovementLineY.NoZWrite := Value;
    FUIMovementLineZ.NoZWrite := Value;
    FUIMovementLineXY.NoZWrite := Value;
    FUIMovementLineXZ.NoZWrite := Value;
    FUIMovementLineYZ.NoZWrite := Value;
    FUIMovementArrowX.NoZWrite := Value;
    FUIMovementArrowY.NoZWrite := Value;
    FUIMovementArrowZ.NoZWrite := Value;
    FUIMovementPlaneXY.NoZWrite := Value;
    FUIMovementPlaneXZ.NoZWrite := Value;
    FUIMovementPlaneYZ.NoZWrite := Value;
    FUIICMovementLineX.NoZWrite := Value;
    FUIICMovementLineY.NoZWrite := Value;
    FUIICMovementLineZ.NoZWrite := Value;
    FUIICMovementLineXY.NoZWrite := Value;
    FUIICMovementLineXZ.NoZWrite := Value;
    FUIICMovementLineYZ.NoZWrite := Value;

    //ForRotate
    FUIRotateLineX.NoZWrite := Value;
    FUIRotateLineY.NoZWrite := Value;
    FUIRotateLineZ.NoZWrite := Value;
    FUIRotateLineXY.NoZWrite := Value;
    FUIRotateLineXZ.NoZWrite := Value;
    FUIICRotateTorusX.NoZWrite := Value;
    FUIICRotateTorusY.NoZWrite := Value;
    FUIICRotateTorusZ.NoZWrite := Value;
    FUIICRotateTorusXZ.NoZWrite := Value;
    FUIRotateDiskXY.NoZWrite := Value;
    FUIRotateDiskX.NoZWrite := Value;
    FUIRotateDiskY.NoZWrite := Value;
    FUIRotateDiskZ.NoZWrite := Value;
    FUIRotateDiskX2.NoZWrite := Value;
    FUIRotateDiskY2.NoZWrite := Value;
    FUIRotateDiskZ2.NoZWrite := Value;
    FUIICRotateSphereXY.NoZWrite := Value;
    FUIRotateLineArrowX.NoZWrite := Value;
    FUIRotateLineArrowY.NoZWrite := Value;
    FUIRotateLineArrowZ.NoZWrite := Value;
    FUIRotateAxisLabelX.NoZWrite := Value;
    FUIRotateAxisLabelY.NoZWrite := Value;
    FUIRotateAxisLabelZ.NoZWrite := Value;

    //ForScale
    FUIScaleArrowX.NoZWrite := Value;
    FUIScaleArrowY.NoZWrite := Value;
    FUIScaleArrowZ.NoZWrite := Value;
    FUIScaleLineX.NoZWrite := Value;
    FUIScaleLineY.NoZWrite := Value;
    FUIScaleLineZ.NoZWrite := Value;
    FUIScaleLineXY.NoZWrite := Value;
    FUIScaleLineYZ.NoZWrite := Value;
    FUIScaleLineXZ.NoZWrite := Value;
    FUIICScaleLineX.NoZWrite := Value;
    FUIICScaleLineY.NoZWrite := Value;
    FUIICScaleLineZ.NoZWrite := Value;
    FUIICScaleLineXY.NoZWrite := Value;
    FUIICScaleLineXZ.NoZWrite := Value;
    FUIICScaleLineYZ.NoZWrite := Value;
    FUIICScaleLineXYZ.NoZWrite := Value;
    FUIScalePlaneXY.NoZWrite := Value;
    FUIScalePlaneXZ.NoZWrite := Value;
    FUIScalePlaneYZ.NoZWrite := Value;
    FUIScalePlaneXYZ.NoZWrite := Value;


    FUIAxisLabelX.NoZWrite := Value;
    FUIAxisLabelY.NoZWrite := Value;
    FUIAxisLabelZ.NoZWrite := Value;
    FUIVisibleInfoLabels.NoZWrite := Value;
  end;
end;

procedure TVXGizmoEx.SetEnableLoopCursorMoving(const AValue: Boolean);
begin
  if FEnableLoopCursorMoving <> AValue then
    FEnableLoopCursorMoving := AValue;
end;

procedure TVXGizmoEx.SetEnableMultiSelection(const AValue: Boolean);
begin
  if FEnableMultiSelection <> AValue then
  begin
    FEnableMultiSelection := AValue;
    FInterfaceRender.Visible := AValue;
  end;
end;

procedure TVXGizmoEx.MultiSelMouseDown(X, Y: Integer);
begin
  flastcursorPos := Point(X, Y);
  fcursorPos := point(X, Y);

  if (fSelectionRegion = gsrFence) and FShowMultiSelecting then
  begin
    SetLength(FSelectionRec, Length(FSelectionRec) + 1);
    FSelectionRec[high(FSelectionRec)] := point(X, Y);
  end;
end;

procedure TVXGizmoEx.MultiSelMouseMove(X, Y: Integer);
begin
  //calculation starts when the mouse button is down
  //ans distance from pick pisition is not more then 10
  if Moving and not FShowMultiSelecting and
    (Dist(point(X, Y), flastcursorPos) > 10) then
  begin
    FShowMultiSelecting := True;
    if fSelectionRegion = gsrFence then
    begin
      SetLength(FSelectionRec, Length(FSelectionRec) + 1);
      FSelectionRec[high(FSelectionRec)] := point(X, Y);
    end;
  end;

  if FShowMultiSelecting then
  begin
    fcursorPos := point(X, Y);
    //creating lines when moving mouse
    if (fSelectionRegion = gsrLasso) and
      //clculate distance between two points to view as in 3D Max
      (Dist(point(X, Y), flastcursorPos) > 20) then
    begin
      flastcursorPos := point(X, Y);
      SetLength(FSelectionRec, Length(FSelectionRec) + 1);
      FSelectionRec[high(FSelectionRec)] := point(X, Y);
    end;
  end;
end;

procedure TVXGizmoEx.MultiSelMouseUp(X, Y: Integer);

  procedure SelectAssignMode(pick: TVXPickList);
  begin
    if not FCanRemoveObjFromSelectionList and not FCanAddObjtoSelectionList then
      AssignPickList(pick)
    else
    if FCanRemoveObjFromSelectionList then
      AssignPickList(pick, FCanRemoveObjFromSelectionList)
    else
    if FCanAddObjtoSelectionList then
      AssignPickList(pick);
  end;

var
  I, J:   Integer;
  pick:   TVXPickList;
  p1, p2: TVXPoint;
  Line:   TVXGizmoExSelRec;
  LastCurPosX, LastCurPosY, CurPosX, CurPosY: Single;
begin

  LastCurPosX := flastcursorPos.X;
  LastCurPosY := flastcursorPos.Y;
  CurPosX := fcursorPos.X;
  CurPosY := fcursorPos.Y;

  if (fSelectionRegion = gsrRectangular) then
  begin

    pick := InternalGetPickedObjects(X - 1, Y - 1, flastcursorPos.X + 1, flastcursorPos.Y + 1, 8);
    if not FCanRemoveObjFromSelectionList and not FCanAddObjtoSelectionList then
      FSelectedObjects.Clear;
    SelectAssignMode(pick);
    pick.Free;
    if Assigned(onSelect) then
      onSelect(self, FSelectedObjects);

    FShowMultiSelecting := False;
  end;

  if (fSelectionRegion = gsrCircular) then
  begin
    FShowMultiSelecting := False;
    if not FCanRemoveObjFromSelectionList and not FCanAddObjtoSelectionList then
      FSelectedObjects.Clear;

    for I := 0 to Round(viewer.Height) - 1 do
      if IsLineIntCirlce(Maxfloat(abs(CurPosX - LastCurPosX),
        abs(CurPosY - LastCurPosY)),
        flastcursorPos, point(0, I), point(Round(viewer.Width), I), p1, p2) >= 0 then
        if (I mod 2 = 0) then
        begin
          pick := InternalGetPickedObjects(p2.X - 1, p2.Y - 1, p1.X + 1, p1.Y + 1, 8);
          SelectAssignMode(pick);
          pick.Free;
        end;
    if Assigned(onSelect) then
      onSelect(self, FSelectedObjects);
  end;


  if (fSelectionRegion = gsrFence) and (high(FSelectionRec) > 0) then
    with FSelectionRec[Low(FSelectionRec)] do
      //verify if a pick is near point, not to seek a centre
      if IsInRange(CurPosX, X + 2, X - 2) and IsInRange(CurPosY, Y + 2, Y - 2) then
      begin
        FShowMultiSelecting := False;
        //connect the begining and end
        SetLength(FSelectionRec, Length(FSelectionRec) + 1);
        FSelectionRec[high(FSelectionRec)] := point(X, Y);

        if not FCanRemoveObjFromSelectionList and not FCanAddObjtoSelectionList then
          FSelectedObjects.Clear;
        for J := 0 to Round(viewer.Height) - 1 do
          for I := 0 to Round(viewer.Width) - 1 do
          begin
            if IsPointInPolygon(FSelectionRec, point(I, J)) then
            begin
              if not IsPointInPolygon(FSelectionRec, point(I + 1, J)) then
              begin
                SetLength(line, Length(line) + 1);
                line[high(line)] := point(I, J);
              end;
            end
            else
            if IsPointInPolygon(FSelectionRec, point(I + 1, J)) then
            begin
              SetLength(line, Length(line) + 1);
              line[high(line)] := point(I, J);
            end;
          end;
        for I := 0 to High(line) do
          if (I mod 2 = 0) then
          begin
            pick := InternalGetPickedObjects(line[I].X - 1, line[I].Y - 1, line[I + 1].X + 1, line[I + 1].Y + 1, 8);
            SelectAssignMode(pick);
            pick.Free;
          end;
        if Assigned(onSelect) then
          onSelect(self, FSelectedObjects);
        //nulling of array
        SetLength(line, 0);
        SetLength(FSelectionRec, 0);
      end;

  if (fSelectionRegion = gsrLasso) then
  begin
    FShowMultiSelecting := False;

    SetLength(FSelectionRec, Length(FSelectionRec) + 1);
    FSelectionRec[high(FSelectionRec)] := FSelectionRec[Low(FSelectionRec)];

    if not FCanRemoveObjFromSelectionList and not FCanAddObjtoSelectionList then
      FSelectedObjects.Clear;
    for J := 0 to Round(viewer.Height) - 1 do
      for I := 0 to Round(viewer.Width) - 1 do
      begin
        if IsPointInPolygon(FSelectionRec, point(I, J)) then
        begin
          if not IsPointInPolygon(FSelectionRec, point(I + 1, J)) then
          begin
            SetLength(line, Length(line) + 1);
            line[high(line)] := point(I, J);
          end;
        end
        else
        if IsPointInPolygon(FSelectionRec, point(I + 1, J)) then
        begin
          SetLength(line, Length(line) + 1);
          line[high(line)] := point(I, J);
        end;
      end;

    for I := 0 to High(line) do
      if (I mod 2 = 0) then
      begin
        pick := InternalGetPickedObjects(line[I].X - 1, line[I].Y - 1, line[I + 1].X + 1, line[I + 1].Y + 1, 8);
        SelectAssignMode(pick);
        pick.Free;
      end;
    SetLength(line, 0);
    SetLength(FSelectionRec, 0);
    if Assigned(onSelect) then
      onSelect(self, FSelectedObjects);
  end;
end;


procedure TVXGizmoEx.SetVisibleInfoLabelsColor(const AValue: TVXColor);
begin
  if AValue <> FSelectedColor then
  begin
    FVisibleInfoLabelsColor.Color := AValue.Color;
    FUIVisibleInfoLabels.ModulateColor.Color := AValue.Color;
    FVisibleInfoLabelsColorChanged := True;
    UpdateGizmo;
  end;
end;

procedure TVXGizmoEx.SetSelectionRegionColor(const AValue: TVXColor);
begin
  if AValue <> FSelectionRegionColor then
  begin
    FSelectionRegionColor.Color := AValue.Color;
  end;
end;

procedure TVXGizmoEx.SetGizmoExVisibleInfoLabels(const AValue: TVXGizmoExVisibleInfoLabels);
begin
  if AValue <> FVisibleVisibleInfoLabels then
  begin
    FVisibleVisibleInfoLabels := AValue;
    if not (csDesigning in ComponentState) then
      UpdateGizmo;
  end;
end;

procedure TVXGizmoEx.UndoAdd(const AObject: TObject);
begin
  if AObject <> nil then
    FHistory.AddObject(AObject);
end;

procedure TVXGizmoEx.RemoveSelectedObjects;
begin
  if not Assigned(FHistory.FGizmoTmpRoot) then
    Exit;
  FHistory.RemoveObjects(SelectedObjects);
  FHistory.AddObjects(SelectedObjects);
  UpdateGizmo();
end;

procedure TVXGizmoEx.SetRootGizmo(const AValue: TVXBaseSceneObject);
begin
  if FRootGizmo <> AValue then
  begin
    if FRootGizmo <> nil then
      FRootGizmo.RemoveFreeNotification(Self);
    FRootGizmo := AValue;
    if FRootGizmo <> nil then
      FRootGizmo.FreeNotification(Self);
    FUIBaseGizmo.MoveTo(AValue);
  end;
end;

procedure TVXGizmoEx.SetGizmoTmpRoot(const AValue: TVXBaseSceneObject);
begin
  if FGizmoTmpRoot <> AValue then
  begin
    if FGizmoTmpRoot <> nil then
      FGizmoTmpRoot.RemoveFreeNotification(Self);
    FGizmoTmpRoot := AValue;
    FGizmoTmpRoot.Visible := False;
    FHistory.GizmoTmpRoot := FGizmoTmpRoot;
  end;
end;

procedure TVXGizmoEx.SetRootObjects(const AValue: TVXBaseSceneObject);
begin
  if fRootObjects <> AValue then
  begin
    if fRootObjects <> nil then
      fRootObjects.RemoveFreeNotification(Self);
    fRootObjects := AValue;
  end;
end;

procedure TVXGizmoEx.SetExcludeObjectsList(const AValue: TStrings);
begin
  FExcludeObjectsList.Clear;
  FExcludeObjectsList.AddStrings(AValue);
end;

procedure TVXGizmoEx.SetExcludeClassNameList(const AValue: TStrings);
begin
  FExcludeClassNameList.Clear;
  FExcludeClassNameList.AddStrings(AValue);
end;

procedure TVXGizmoEx.SetGizmoExThickness(const Value: Single);
begin
  if (FGizmoThickness <> Value) and (Value > 0.2) then
  begin
    FGizmoThickness := Value;
    FUISelectLineX.LineWidth := 1 * Value;
    FUISelectLineY.LineWidth := 1 * Value;
    FUISelectLineZ.LineWidth := 1 * Value;

    FUIMovementLineX.LineWidth := 1 * Value;
    FUIMovementLineY.LineWidth := 1 * Value;
    FUIMovementLineZ.LineWidth := 1 * Value;
    FUIMovementLineXY.LineWidth := 1 * Value;
    FUIMovementLineXZ.LineWidth := 1 * Value;
    FUIMovementLineYZ.LineWidth := 1 * Value;

    FUIRotateLineX.LineWidth := 1 * Value;
    FUIRotateLineY.LineWidth := 1 * Value;
    FUIRotateLineZ.LineWidth := 1 * Value;
    FUIrotateLineXY.LineWidth := 1 * Value;
    FUIRotateLineXZ.LineWidth := 1 * Value;
    FUIRotateLineArrowX.LineWidth := 1 * Value;
    FUIRotateLineArrowY.LineWidth := 1 * Value;
    FUIRotateLineArrowZ.LineWidth := 1 * Value;

    FUIScaleLineX.LineWidth := 1 * Value;
    FUIScaleLineY.LineWidth := 1 * Value;
    FUIScaleLineZ.LineWidth := 1 * Value;
    FUIScaleLineXY.LineWidth := 1 * Value;
    FUIScaleLineXZ.LineWidth := 1 * Value;
    FUIScaleLineYZ.LineWidth := 1 * Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TVXGizmoEx.SetLabelFont(const Value: TVXCustomBitmapFont);
begin
  if FLabelFont <> Value then
  begin
    if FLabelFont <> nil then
      FLabelFont.RemoveFreeNotification(Self);
    FLabelFont := Value;
    if FLabelFont <> nil then
      FLabelFont.FreeNotification(Self);

    FUIAxisLabelX.BitmapFont := Value;
    FUIAxisLabelY.BitmapFont := Value;
    FUIAxisLabelZ.BitmapFont := Value;
    FUIRotateAxisLabelX.BitmapFont := Value;
    FUIRotateAxisLabelY.BitmapFont := Value;
    FUIRotateAxisLabelZ.BitmapFont := Value;
    FUIVisibleInfoLabels.BitmapFont := Value;
  end;
end;

function TVXGizmoEx.InternalGetPickedObjects(const x1, y1, x2, y2: Integer; const guessCount: Integer): TVXPickList;

  procedure AddObjectToPicklList(const root: TVXBaseSceneObject; PickList: TVXPickList; X, Y: Integer);
  var
    t:    Integer;
    dist: Single;
    rayStart, rayVector, iPoint, iNormal: TVector;
  begin
    SetVector(rayStart, Viewer.Camera.AbsolutePosition);
    SetVector(rayVector, Viewer.Buffer.ScreenToVector(AffineVectorMake(X, Viewer.Height - Y, 0)));
    NormalizeVector(rayVector);
    for t := 0 to root.Count - 1 do
      if root[t].Visible then
      begin
        if (root[t].RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal)) and
          (VectorDotProduct(rayVector, iNormal) < 0) then
          if PickList.FindObject(root[t]) = -1 then
          begin
            dist := VectorLength(VectorSubtract(iPoint, rayStart));
            PickList.AddHit(root[t], nil, dist, 0);
          end;
        AddObjectToPicklList(root[t], PickList, X, Y);
      end;
  end;

var
  I, J: Integer;
  minx, miny, maxx, maxy: Integer;
begin
  case FPickMode of
    pmGetPickedObjects:
    begin
      Result := Viewer.Buffer.GetPickedObjects(rect(x1, y1, x2, y2), guessCount);
    end;

    pmRayCast:
    begin
      Result := TVXPickList.Create(psMinDepth);
      maxX := MaxInteger(x1, x2);
      maxY := MaxInteger(Y1, Y2);
      minX := MinInteger(x1, x2);
      minY := MinInteger(Y1, Y2);
      for J := minY to maxY do
        for I := minX to maxX do
          //uploading to exclude hanging of application :)
          if (I mod 4 = 0) or (J mod 4 = 0) then
            AddObjectToPicklList(RootObjects, Result, I, J);
      AddObjectToPicklList(RootGizmo, Result, round((x1 + x2) * 0.5), round((y1 + y2) * 0.5));
    end;

  else
    begin
      Result := nil;
      Assert(False, strUnknownType);
    end;
  end;
end;


procedure TVXGizmoEx.Loaded;
begin
  inherited;
  SetGizmoExThickness(GizmoThickness);
end;

//------------------------------------------------------------------------------
procedure TVXGizmoEx.UpdateVisibleInfoLabels;
var
  T: string;
  X, Y, Z: Single;
  obj: TVXBaseSceneObject;
begin
  t := '';
  X := 0;
  Y := 0;
  Z := 0;
  if FSelectedObjects.Count - 1 < 0 then
    Exit;

  if (FSelectedObjects.Count - 1 = 0) and (vliName in FVisibleVisibleInfoLabels) then
    t := TVXBaseSceneObject(FSelectedObjects[0]).Name;

  if vliOperation in FVisibleVisibleInfoLabels then
  begin
    begin
      if Length(t) > 0 then
        T := T + ' - ';
      case Operation of
        gopNone: T := T + 'Selected';
        gopMove: T := T + 'Move';
        gopRotate: T := T + 'Rotate';
        gopScale: T := T + 'Scale';
      end;
    end;
  end;

  if vliCoords in FVisibleVisibleInfoLabels then
  begin
    if (Operation <> gopNone) then
    begin
      if Length(t) > 0 then
        T := T + ' - ';
      if FinfoLabelCoordType = ilcChanging then
      begin
        obj := TVXBaseSceneObject(FSelectedObjects[0]);
        case Operation of
          gopMove:
          begin
            X := obj.Position.X;
            Y := obj.Position.Y;
            Z := obj.Position.Z;
          end;
          gopRotate:
          begin
            X := obj.Rotation.X;
            Y := obj.Rotation.Y;
            Z := obj.Rotation.Z;
          end;
          gopScale:
          begin
            X := obj.Scale.X;
            Y := obj.Scale.Y;
            Z := obj.Scale.Z;
          end;
        end;
        T := T + '[' + Format('%2.2f', [X]);
        T := T + ' ' + Format('%2.2f', [Y]);
        T := T + ' ' + Format('%2.2f', [Z]) + ']';
      end
      else
      begin
        T := T + '[' + Format('%2.2f', [FChangeRate.X]);
        T := T + ' ' + Format('%2.2f', [FChangeRate.Y]);
        T := T + ' ' + Format('%2.2f', [FChangeRate.Z]) + ']';
      end;
    end;
  end;

  FUIVisibleInfoLabels.Text := T;
  FUIVisibleInfoLabels.StructureChanged;
end;

//------------------------------------------------------------------------------

function TVXGizmoEx.CheckObjectInExcludeList(const Obj: TVXBaseSceneObject): Boolean;
var
  I: Integer;
begin
  Result := False;
  if FExcludeObjects then
  begin
    for I := 0 to FExcludeObjectsList.Count - 1 do
    begin
      if UpperCase(obj.Name) = UpperCase(FExcludeObjectsList[I]) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

end;

function TVXGizmoEx.CheckClassNameInExcludeList(const Obj: TVXBaseSceneObject): Boolean;
var
  I: Integer;
begin
  Result := False;
  if FExcludeClassName then
  begin
    for I := 0 to FExcludeClassNameList.Count - 1 do
    begin
      if UpperCase(obj.ClassName) = UpperCase(FExcludeClassNameList[I]) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

end;

function TVXGizmoEx.MouseWorldPos(const X, Y: Integer): TVector;
var
  v: TVector;
  InvertedY: Integer;
begin

  InvertedY := Round(Viewer.Height) - Y;

  SetVector(v, X, InvertedY, 0);

  case selAxis of
    gaX: Viewer.Buffer.ScreenVectorIntersectWithPlaneXZ(v, FUIRootHelpers.AbsolutePosition.Y, Result);
    gaY: Viewer.Buffer.ScreenVectorIntersectWithPlaneYZ(v, FUIRootHelpers.AbsolutePosition.X, Result);
    gaZ: Viewer.Buffer.ScreenVectorIntersectWithPlaneYZ(v, FUIRootHelpers.AbsolutePosition.X, Result);
    gaXY: Viewer.Buffer.ScreenVectorIntersectWithPlaneXY(v, FUIRootHelpers.AbsolutePosition.Z, Result);
    gaYZ: Viewer.Buffer.ScreenVectorIntersectWithPlaneYZ(v, FUIRootHelpers.AbsolutePosition.X, Result);
    gaXZ: Viewer.Buffer.ScreenVectorIntersectWithPlaneXZ(v, FUIRootHelpers.AbsolutePosition.Y, Result);
    gaXYZ:
    begin
      Viewer.Buffer.ScreenVectorIntersectWithPlaneXZ(v, FUIRootHelpers.AbsolutePosition.Y, Result);
      MakeVector(Result, InvertedY / 25, InvertedY / 25, InvertedY / 25);
    end;
  end;
end;

procedure TVXGizmoEx.ActivatingElements(PickList: TVXPickList);

  procedure ActlightRotateLine(const line: TVXLines; const dark: TVector);
  var
    v: TVector4f;
    I: Integer;
  begin
    line.options := [loUseNodeColorForLines];
    for I := 0 to line.Nodes.Count - 1 do
    begin
      v := FUIRotateLineXY.AbsoluteToLocal((line.LocalToAbsolute(line.Nodes[I].AsVector)));
      if v.Z >= 0 then
      begin
        TVXLinesNode(line.Nodes[I]).Color.Color := FSelectedColor.Color;
        TVXLinesNode(line.Nodes[I]).Color.Alpha := 1;
      end
      else
      begin
        TVXLinesNode(line.Nodes[I]).Color.Color := dark;
        TVXLinesNode(line.Nodes[I]).Color.Alpha := 1;
      end;
    end;
  end;

  procedure DeActlightRotateLine(const line: TVXLines; const dark: TVector);
  var
    v: TVector4f;
    I: Integer;
  begin

    line.options := [loUseNodeColorForLines];
    for I := 0 to line.Nodes.Count - 1 do
    begin
      v := FUIRotateLineXY.AbsoluteToLocal((line.LocalToAbsolute(line.Nodes[I].AsVector)));
      if v.Z >= 0 then
      begin
        TVXLinesNode(line.Nodes[I]).Color.Color := dark;
        TVXLinesNode(line.Nodes[I]).Color.Alpha := 1;
      end
      else
      begin
        TVXLinesNode(line.Nodes[I]).Color.Color := dark;
        TVXLinesNode(line.Nodes[I]).Color.Alpha := 0;
      end;
    end;
  end;

  procedure ActlightLine(const line: TVXLines);
  begin
    line.LineColor.color := FSelectedColor.Color;
    line.Options := [];
  end;

  procedure DeActlightLine(const line: TVXLines; const dark: TVector; alterStyle: Boolean = False);
  begin
    with  line.LineColor do
      if (AsWinColor = FSelectedColor.AsWinColor) then
      begin
        color := dark;
        line.Options := [];
        if alterStyle then
          line.options := [loUseNodeColorForLines];
      end;
  end;

  procedure ActlightRotateArrowLine(const line: TVXLines; Color: TVector);
  begin
    line.LineColor.color := Color;
    line.Options := [];
  end;

  procedure DeActlightRotateArrowLine(const line: TVXLines; const dark: TVector);
  begin
    if not VectorEquals(line.LineColor.Color, dark) then
    begin
      line.LineColor.Color := dark;
      line.Options := [];
    end;
  end;

  procedure Actlightobject(const aObject: TVXCustomSceneObject);
  begin
    aObject.Material.FrontProperties.Diffuse.Alpha := 0.4;
    aObject.Visible := True;
  end;

  procedure DeActlightObject(const aObject: TVXCustomSceneObject);
  begin
    aObject.Visible := False;
  end;

  procedure ActlightText(const FlatText: TVXFlatText);
  begin
    FlatText.ModulateColor.Color := FSelectedColor.Color;
  end;

  procedure DeActlightText(const FlatText: TVXFlatText; const dark: TVector);
  begin
    with FlatText.ModulateColor do
      if AsWinColor = FSelectedColor.AsWinColor then
        Color := dark;
  end;

  procedure ActlightTextRotate(const FlatText: TVXFlatText; Color: TVector);
  begin
    FlatText.ModulateColor.Color := Color;
  end;

  procedure DeActlightTextRotate(const FlatText: TVXFlatText; const dark: TVector);
  begin
    with FlatText.ModulateColor do
      if not VectorEquals(Color, dark) then
        Color := dark;
  end;

  procedure AssingOpertion(const aOperation: TVXGizmoExOperation; const axis: TVXGizmoExAxis);
  begin
    if Operation <> aOperation then
      Operation := aOperation;
    if SelAxis <> axis then
      SelAxis := axis;
  end;

var
  I: Integer;
begin
  AssingOpertion(gopNone, gaNone);

  if FUIRootMovement.Visible then
  begin
    DeActlightObject(FUIMovementPlaneXY);
    DeActlightObject(FUIMovementPlaneXZ);
    DeActlightObject(FUIMovementPlaneYZ);
    DeActlightLine(FUIMovementLineX, clrRed);
    DeActlightLine(FUIMovementLineY, clrLime);
    DeActlightLine(FUIMovementLineZ, clrBlue);
    DeActlightLine(FUIMovementLineXY, clrWhite, True);
    DeActlightLine(FUIMovementLineXZ, clrWhite, True);
    DeActlightLine(FUIMovementLineYZ, clrWhite, True);
  end;

  if FUIRootRotate.Visible then
  begin
    DeActlightObject(FUIRotateDiskXY);
    DeActlightLine(FUIRotateLineXZ, clrgray70);
    DeActlightRotateArrowLine(FUIRotateLineArrowX, clrgray70);
    DeActlightRotateArrowLine(FUIRotateLineArrowY, clrgray70);
    DeActlightRotateArrowLine(FUIRotateLineArrowZ, clrgray70);
    DeActlightRotateLine(FUIRotateLineX, clrRed);
    DeActlightRotateLine(FUIRotateLineY, clrLime);
    DeActlightRotateLine(FUIRotateLineZ, clrBlue);
    DeActlightTextRotate(FUIRotateAxisLabelX, clrgray70);
    DeActlightTextRotate(FUIRotateAxisLabelY, clrgray70);
    DeActlightTextRotate(FUIRotateAxisLabelZ, clrgray70);
  end;

  if FUIRootScale.Visible then
  begin
    DeActlightLine(FUIScaleLineX, clrRed);
    DeActlightLine(FUIScaleLineY, clrLime);
    DeActlightLine(FUIScaleLineZ, clrBlue);
    DeActlightLine(FUIScaleLineXY, clrWhite, True);
    DeActlightLine(FUIScaleLineYZ, clrWhite, True);
    DeActlightLine(FUIScaleLineXZ, clrWhite, True);
    DeActlightObject(FUIScalePlaneXY);
    DeActlightObject(FUIScalePlaneXZ);
    DeActlightObject(FUIScalePlaneYZ);
    DeActlightObject(FUIScalePlaneXYZ);
  end;

  DeActlightText(FUIAxisLabelX, clrRed);
  DeActlightText(FUIAxisLabelY, clrLime);
  DeActlightText(FUIAxisLabelZ, clrBlue);

  for I := 0 to pickList.Count - 1 do
    with pickList do
    begin

      if FUIRootMovement.Visible then
      begin
        if hit[I] = FUIICMovementLineXY then
        begin
          AssingOpertion(gopMove, gaXY);
          ActlightObject(FUIMovementPlaneXY);
          ActlightLine(FUIMovementLineX);
          ActlightLine(FUIMovementLineY);
          ActlightLine(FUIMovementLineXY);
          ActlightText(FUIAxisLabelX);
          ActlightText(FUIAxisLabelY);
          Break;
        end;

        if hit[I] = FUIICMovementLineXZ then
        begin
          AssingOpertion(gopMove, gaXZ);
          Actlightobject(FUIMovementPlaneXZ);
          ActlightLine(FUIMovementLineX);
          ActlightLine(FUIMovementLineZ);
          ActlightLine(FUIMovementLineXZ);
          ActlightText(FUIAxisLabelX);
          ActlightText(FUIAxisLabelZ);
          Break;
        end;
        if hit[I] = FUIICMovementLineYZ then
        begin
          AssingOpertion(gopMove, gaYZ);
          Actlightobject(FUIMovementPlaneYZ);
          ActlightLine(FUIMovementLineY);
          ActlightLine(FUIMovementLineZ);
          ActlightLine(FUIMovementLineYZ);
          ActlightText(FUIAxisLabelY);
          ActlightText(FUIAxisLabelZ);
          Break;
        end;
        if hit[I] = FUIICMovementLineX then
        begin
          AssingOpertion(gopMove, gaX);
          ActlightLine(FUIMovementLineX);
          ActlightText(FUIAxisLabelX);
          Break;
        end;
        if hit[I] = FUIICMovementLineY then
        begin
          AssingOpertion(gopMove, gaY);
          ActlightLine(FUIMovementLineY);
          ActlightText(FUIAxisLabelY);
          Break;
        end;
        if hit[I] = FUIICMovementLineZ then
        begin
          AssingOpertion(gopMove, gaZ);
          ActlightLine(FUIMovementLineZ);
          ActlightText(FUIAxisLabelZ);
          Break;
        end;
      end;

      if FUIRootRotate.Visible then
      begin
        if hit[I] = FUIICRotateTorusX then
        begin
          AssingOpertion(gopRotate, gaX);
          ActlightRotateLine(FUIRotateLineX, clrgray50);
          ActlightRotateArrowLine(FUIRotateLineArrowX, clrRed);
          DeActlightTextRotate(FUIRotateAxisLabelX, clrRed);
          Break;
        end;
        if hit[I] = FUIICRotateTorusY then
        begin
          AssingOpertion(gopRotate, gaY);
          ActlightRotateLine(FUIRotateLineY, clrgray50);
          ActlightRotateArrowLine(FUIRotateLineArrowY, clrLime);
          DeActlightTextRotate(FUIRotateAxisLabelY, clrLime);
          Break;
        end;

        if hit[I] = FUIICRotateTorusZ then
        begin
          AssingOpertion(gopRotate, gaZ);
          ActlightRotateLine(FUIRotateLineZ, clrgray50);
          ActlightRotateArrowLine(FUIRotateLineArrowZ, clrBlue);
          DeActlightTextRotate(FUIRotateAxisLabelZ, clrBlue);
          Break;
        end;

        if hit[I] = FUIICRotateSphereXY then
        begin
          AssingOpertion(gopRotate, gaXY);
          ActlightObject(FUIRotateDiskXY);
          DeActlightTextRotate(FUIRotateAxisLabelX, clrRed);
          DeActlightTextRotate(FUIRotateAxisLabelY, clrLime);
          ActlightRotateArrowLine(FUIRotateLineArrowX, clrRed);
          ActlightRotateArrowLine(FUIRotateLineArrowY, clrLime);
          Break;
        end;

        if hit[I] = FUIICRotateTorusXZ then
        begin
          AssingOpertion(gopRotate, gaXZ);
          ActlightLine(FUIRotateLineXZ);
          DeActlightTextRotate(FUIRotateAxisLabelX, clrRed);
          DeActlightTextRotate(FUIRotateAxisLabelZ, clrBlue);
          ActlightRotateArrowLine(FUIRotateLineArrowX, clrRed);
          ActlightRotateArrowLine(FUIRotateLineArrowZ, clrBlue);
          Break;
        end;
      end;

      if FUIRootScale.Visible then
      begin
        if hit[I] = FUIICScaleLineX then
        begin
          AssingOpertion(gopScale, gaX);
          ActlightLine(FUIScaleLineX);
          ActlightText(FUIAxisLabelX);
          Break;
        end;
        if hit[I] = FUIICScaleLineY then
        begin
          AssingOpertion(gopScale, gaY);
          ActlightLine(FUIScaleLineY);
          ActlightText(FUIAxisLabelY);
          Break;
        end;
        if hit[I] = FUIICScaleLineZ then
        begin
          AssingOpertion(gopScale, gaZ);
          ActlightLine(FUIScaleLineZ);
          ActlightText(FUIAxisLabelZ);
          Break;
        end;
        if hit[I] = FUIICScaleLineXY then
        begin
          AssingOpertion(gopScale, gaXY);
          Actlightobject(FUIScalePlaneXY);
          ActlightLine(FUIScaleLineXY);
          ActlightText(FUIAxisLabelX);
          ActlightText(FUIAxisLabelY);
          Break;
        end;
        if hit[I] = FUIICScaleLineXZ then
        begin
          AssingOpertion(gopScale, gaXZ);
          Actlightobject(FUIScalePlaneXZ);
          ActlightLine(FUIScaleLineXZ);
          ActlightText(FUIAxisLabelX);
          ActlightText(FUIAxisLabelZ);
          Break;
        end;
        if hit[I] = FUIICScaleLineYZ then
        begin
          AssingOpertion(gopScale, gaYZ);
          Actlightobject(FUIScalePlaneYZ);
          ActlightLine(FUIScaleLineYZ);
          ActlightText(FUIAxisLabelY);
          ActlightText(FUIAxisLabelZ);
          Break;
        end;
        if hit[I] = FUIICScaleLineXYZ then
        begin
          AssingOpertion(gopScale, gaXYZ);
          Actlightobject(FUIScalePlaneXYZ);
          ActlightText(FUIAxisLabelX);
          ActlightText(FUIAxisLabelY);
          ActlightText(FUIAxisLabelZ);
          Actlightobject(FUIScalePlaneXY);
          ActlightLine(FUIScaleLineXY);
          Actlightobject(FUIScalePlaneYZ);
          ActlightLine(FUIScaleLineYZ);
          Actlightobject(FUIScalePlaneXZ);
          ActlightLine(FUIScaleLineXZ);
          Break;
        end;
      end;
    end;
end;

procedure TVXGizmoEx.ViewerMouseMove(const X, Y: Integer);
var
  pickList:  TVXPickList;
  mousePos:  TVector;
  includeCh: Boolean;

  function FindParent(parent: TVXBaseSceneObject): Boolean;
  begin
    Result := False;
    if assigned(parent) then
    begin
      if parent = rootobjects then
        Exit;
      Result := FSelectedObjects.FindObject(parent) = -1;
    end;
  end;

  procedure OpeMove(mousePos: TVector);
  var
    vec1, vec2: TVector;
    quantizedMousePos, quantizedMousePos2: TVector;
    I: Integer;
  begin
    if VectorNorm(lastMousePos) = 0 then
      Exit;
    for I := 0 to 3 do
    begin
      quantizedMousePos.V[I] := (Round(mousePos.V[I] / MoveCoef)) * MoveCoef;
      quantizedMousePos2.V[I] := (Round(lastMousePos.V[I] / MoveCoef)) * MoveCoef;
    end;

    case SelAxis of
      gaX:
      begin
        MakeVector(vec1, quantizedMousePos.X, 0, 0);
        makeVector(vec2, quantizedMousePos2.X, 0, 0);
      end;
      gaY:
      begin
        MakeVector(vec1, 0, quantizedMousePos.Y, 0);
        makeVector(vec2, 0, quantizedMousePos2.Y, 0);
      end;
      gaZ:
      begin
        MakeVector(vec1, 0, 0, quantizedMousePos.Z);
        makeVector(vec2, 0, 0, quantizedMousePos2.Z);
      end;
      else
      begin
        vec1 := quantizedMousePos;
        vec2 := quantizedMousePos2;
      end;
    end;
    SubtractVector(vec1, vec2);

    //Control of object flying to infinity
    if (VectorLength(Vec1) > 5) then
      Exit;// prevents NAN problems

    case SelAxis of
      gaX: fchangerate.X := fchangerate.X + vec1.X;
      gaY: fchangerate.Y := fchangerate.Y + vec1.Y;
      gaZ: fchangerate.Z := fchangerate.Z + vec1.Z;
      gaXY:
      begin
        fchangerate.X := fchangerate.X + vec1.X;
        fchangerate.Y := fchangerate.Y + vec1.Y;
      end;
      gaYZ:
      begin
        fchangerate.Z := fchangerate.Z + vec1.Z;
        fchangerate.Y := fchangerate.Y + vec1.Y;
      end;
      gaXZ:
      begin
        fchangerate.X := fchangerate.X + vec1.X;
        fchangerate.Z := fchangerate.Z + vec1.Z;
      end;
    end;

    for I := 0 to FSelectedObjects.Count - 1 do
      with TVXBaseSceneObject(FSelectedObjects.Hit[I]) do
      begin

        IncludeCh := True;

        if not CanChangeWithChildren and (parent <> RootObjects) and (FSelectedObjects.Count - 1 > 0) then
          IncludeCh := FindParent(parent);
        if IncludeCh then
          case Ord(ReferenceCoordSystem) of
            0: AbsolutePosition := VectorAdd(absoluteposition, vec1);
            1:
            begin
              vec1 := LocalToAbsolute(vec1);
              absoluteposition := VectorAdd(absoluteposition, vec1);
            end;
          end;

      end;
  end;

  procedure OpeRotate(const X, Y: Integer);
  var
    vec1: TVector;
    rotV: TAffineVector;
    pmat: TMatrix;
    I:    Integer;
    IncludeCh: Boolean;
    v:    TVector;
  begin

    vec1.X := 0;
    vec1.Y := 0;
    if abs(X - mx) >= RotationCoef then
    begin
      if RotationCoef > 1 then
        vec1.X := RotationCoef * (Round((X - mx) / (RotationCoef)))
      else
        vec1.X := RotationCoef * (X - mx);
      mx := X;
    end;
    if abs(Y - my) >= RotationCoef then
    begin
      if RotationCoef > 1 then
        vec1.Y := RotationCoef * (Round((Y - my) / (RotationCoef)))
      else
        vec1.Y := RotationCoef * (Y - my);
      my := Y;
    end;


    vec1.Z := 0;
    vec1.W := 0;

    case SelAxis of
      gaX: fchangerate.Y := fchangerate.Y + vec1.Y;
      gaY: fchangerate.X := fchangerate.X + vec1.X;
      gaZ: fchangerate.Y := fchangerate.Y + vec1.Y;
    end;

    for I := 0 to FSelectedObjects.Count - 1 do
      with FSelectedObjects do
      begin

        case Ord(FReferenceCoordSystem) of
          0: v := FUIRootHelpers.AbsolutePosition;
          1: v := TVXBaseSceneObject(Hit[I]).AbsolutePosition;
        end;

        IncludeCh := True;

        if not CanChangeWithChildren
          and (TVXBaseSceneObject(Hit[I]).parent <> RootObjects)
          and (FSelectedObjects.Count - 1 > 0) then
          IncludeCh := FindParent(TVXBaseSceneObject(Hit[I]).parent);

        pmat := TVXBaseSceneObject(Hit[I]).parent.InvAbsoluteMatrix;
        SetVector(pmat.W, NullHmgPoint);

        if IncludeCh then
          case SelAxis of
            gaX:
            begin
              rotV := VectorTransform(XVector, pmat);
              RotateAroundArbitraryAxis(TVXBaseSceneObject(Hit[I]), rotV, AffineVectorMake(v), vec1.Y);

            end;
            gaY:
            begin
              rotV := VectorTransform(YVector, pmat);
              RotateAroundArbitraryAxis(TVXBaseSceneObject(Hit[I]), rotV, AffineVectorMake(v), vec1.X);
            end;
            gaZ:
            begin
              rotV := VectorTransform(ZVector, pmat);
              RotateAroundArbitraryAxis(TVXBaseSceneObject(Hit[I]), rotV, AffineVectorMake(v), vec1.Y);
            end;
            gaXY:
            begin
              rotV := VectorTransform(XVector, pmat);
              RotateAroundArbitraryAxis(TVXBaseSceneObject(Hit[I]), rotV, AffineVectorMake(v), vec1.Y);
              rotV := VectorTransform(YVector, pmat);
              RotateAroundArbitraryAxis(TVXBaseSceneObject(Hit[I]), rotV, AffineVectorMake(v), vec1.X);
            end;
            gaXZ:
            begin
              rotV := VectorTransform(XVector, pmat);
              RotateAroundArbitraryAxis(TVXBaseSceneObject(Hit[I]), rotV, AffineVectorMake(v), vec1.Y);
              rotV := VectorTransform(ZVector, pmat);
              RotateAroundArbitraryAxis(TVXBaseSceneObject(Hit[I]), rotV, AffineVectorMake(v), vec1.X);
            end;
            gaYZ:
            begin
              rotV := VectorTransform(YVector, pmat);
              RotateAroundArbitraryAxis(TVXBaseSceneObject(Hit[I]), rotV, AffineVectorMake(v), vec1.Y);
              rotV := VectorTransform(ZVector, pmat);
              RotateAroundArbitraryAxis(TVXBaseSceneObject(Hit[I]), rotV, AffineVectorMake(v), vec1.X);
            end;
          end;
      end;
  end;

  procedure OpeScale(const mousePos: TVector);
  var
    vec1, vec2: TVector;
    quantizedMousePos, quantizedMousePos2: TVector;
    t: Integer;
  begin
    if VectorNorm(lastMousePos) = 0 then
      Exit;

    for t := 0 to 3 do
    begin
      quantizedMousePos.V[t] := (Round(mousePos.V[t] / ScaleCoef)) * FScaleCoef;
      quantizedMousePos2.V[t] := (Round(lastMousePos.V[t] / FScaleCoef)) * FScaleCoef;
    end;

    case SelAxis of
      gaX:
      begin
        MakeVector(vec1, quantizedMousePos.X, 0, 0);
        makeVector(vec2, quantizedMousePos2.X, 0, 0);
      end;
      gaY:
      begin
        MakeVector(vec1, 0, quantizedMousePos.Y, 0);
        makeVector(vec2, 0, quantizedMousePos2.Y, 0);
      end;
      gaZ:
      begin
        MakeVector(vec1, 0, 0, quantizedMousePos.Z);
        makeVector(vec2, 0, 0, quantizedMousePos2.Z);
      end;

      gaXY:
      begin
        MakeVector(vec1, quantizedMousePos.X, quantizedMousePos.Y, 0);
        makeVector(vec2, quantizedMousePos2.X, quantizedMousePos2.Y, 0);
      end;

      gaXYZ:
      begin
        MakeVector(vec1, quantizedMousePos.X, quantizedMousePos.Y, quantizedMousePos.Z);
        makeVector(vec2, quantizedMousePos2.X, quantizedMousePos2.Y, quantizedMousePos2.Z);
      end

      else
      begin
        vec1 := quantizedMousePos;
        vec2 := quantizedMousePos2;
      end;
    end;

    SubtractVector(vec1, vec2);

    if (VectorLength(Vec1) > 5) then
      Exit;// prevents NAN problems

    case SelAxis of
      gaX: fchangerate.X := fchangerate.X + vec1.X;
      gaY: fchangerate.Y := fchangerate.Y + vec1.Y;
      gaZ: fchangerate.Z := fchangerate.Z + vec1.Z;
      gaXY:
      begin
        fchangerate.X := fchangerate.X + vec1.X;
        fchangerate.Y := fchangerate.Y + vec1.Y;
      end;
      gaYZ:
      begin
        fchangerate.Z := fchangerate.Z + vec1.Z;
        fchangerate.Y := fchangerate.Y + vec1.Y;
      end;
      gaXZ:
      begin
        fchangerate.X := fchangerate.X + vec1.X;
        fchangerate.Z := fchangerate.Z + vec1.Z;
      end;
      gaXYZ:
        fchangerate := VectorAdd(fchangerate, AffineVectorMake(vec1));
    end;

    for t := 0 to FSelectedObjects.Count - 1 do
      with TVXBaseSceneObject(FSelectedObjects.Hit[t]) do
      begin
        IncludeCh := True;

        if not CanChangeWithChildren and (parent <> RootObjects) and (FSelectedObjects.Count - 1 > 0) then
          IncludeCh := FindParent(parent);

        FUIRootScale.Scale.Translate(vec1);

        if IncludeCh then
        begin
         { case ord(ReferenceCoordSystem) of
               0:begin
                vec1:=LocalToAbsolute(vec1);
                absoluteScale:=VectorAdd(absolutescale,vec1);
               end;
               1:Scale.Translate(vec1);
          end; }
          Scale.Translate(vec1);
        end;
      end;
  end;

  procedure LoopCursorMoving(isvector: Boolean = False);
  {$IFDEF MSWINDOWS}
  var
    R, vR: TVXRect;
    cp:    TVXpoint;
  {$ENDIF}
  begin
  {$IFDEF MSWINDOWS}
    //ѕроцедура дл€ перевода курсора из начала в конец
    //без потерь операций над обьектом
    GetWindowRect(GetDesktopWindow, R);
    { TODO : E2003 Undeclared identifier: 'Handle' }
    (*GetWindowRect(viewer.Handle, VR);*)
    GLGetCursorPos(cp);
    if cp.Y = R.Bottom - 1 then
    begin
      SetCursorPos(cp.X, R.Top + 3);
      if not isvector then
        my := r.Top - vr.Top
      else
      begin
        lastMousePos := MouseWorldPos(X, r.Top + 3 - vr.Top);
        //введено что бы обьект не дергалс€
        mousepos := lastMousePos;
      end;
    end;

    if cp.Y = R.Top then
    begin
      SetCursorPos(cp.X, R.Bottom - 3);
      if not isvector then
        my := R.Bottom - 1 - vr.top
      else
      begin
        lastMousePos := MouseWorldPos(X, R.Bottom - 1 - vr.top);
        mousepos := lastMousePos;
      end;
    end;

    if cp.X = R.Right - 1 then
    begin
      SetCursorPos(r.Left + 3, cp.Y);
      if not isvector then
        mx := r.Left - vr.Left
      else
      begin
        lastMousePos := MouseWorldPos(r.Left - vr.Left, Y);
        mousepos := lastMousePos;
      end;
    end;

    if cp.X = R.Left then
    begin
      SetCursorPos(r.Right - 3, cp.Y);
      if not isvector then
        mx := r.Right - 1 - vr.Left
      else
      begin
        lastMousePos := MouseWorldPos(r.Right - 1 - vr.Left, Y);
        mousepos := lastMousePos;
      end;
    end;
    {$ENDIF}
  end;

begin

  if (not Enabled) or (RootGizmo = nil) or (RootObjects = nil) then
    Exit;

  if not FShowMultiSelecting then
  begin

    if (FSelectedObjects.Count - 1 >= 0) and (SelAxis <> gaNone) and moving then
    begin
      mousePos := MouseWorldPos(X, Y);
      //moving object...
      if Operation = gopMove then
      begin
        OpeMove(MousePos);
      end
      else if Operation = gopRotate then
      begin
        if EnableLoopCursorMoving then
          LoopCursorMoving;
        OpeRotate(X, Y);
        if (SelAxis = gax) or (SelAxis = gaz) then
          SetAngleDisk(fchangerate.Y)
        else
        if SelAxis = gaY then
          SetAngleDisk(fchangerate.X);

      end
      else if Operation = gopScale then
      begin
        if EnableLoopCursorMoving then
          LoopCursorMoving(True);
        OpeScale(MousePos);
      end;

      UpdateGizmo;
      mx := X;
      my := Y;
      lastMousePos := mousePos;
      Exit;
    end;

    Assert(FViewer <> nil, 'Viewer not Assigned to gizmo');
    picklist := InternalGetPickedObjects(X - 1, Y - 1, X + 1, Y + 1, 8);//Viewer.buffer.GetPickedObjects(rect(x-1, y-1, x+1, y+1), 8);
    ActivatingElements(picklist);
    picklist.Free;
  end;

  if EnableMultiSelection and (Operation = gopNone) and (SelAxis = gaNone) then
    MultiSelMouseMove(X, Y);
  mx := X;
  my := Y;
end;

procedure TVXGizmoEx.ViewerMouseDown(const X, Y: Integer);

  function SetInitialDiskPostition(aObject, aObject2: TVXCustomSceneObject): TVector;
  var
    rayStart, rayVector, iPoint, iNormal: TVector;
  begin
    if (Viewer = nil) then
      Exit;
    if (Viewer.Camera = nil) then
      Exit;

    SetVector(rayStart, Viewer.Camera.AbsolutePosition);
    SetVector(rayVector, Viewer.Buffer.ScreenToVector(AffineVectorMake(X, Viewer.Height - Y, 0)));
    NormalizeVector(rayVector);

    if aObject.RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal) then

      aObject2.Up.Setvector(VectorNormalize(VectorSubtract(iPoint, FUIRootHelpers.Position.AsVector)));
    aObject2.StructureChanged;
    Result := iPoint;
  end;

var
  pick: TVXPickList;
  I:    Integer;
  gotPick: Boolean;
begin
  if not Enabled               or
     not Assigned(RootGizmo)   or
     not Assigned(RootObjects) or
     not Assigned(Viewer)      then
    Exit;

  mx := X;
  my := Y;

  pick := InternalGetPickedObjects(X - 1, Y - 1, X + 1, Y + 1);
  gotPick := False;

  for I := 0 to pick.Count - 1 do
    if (pick.Hit[I] is TVXGizmoExUIDisk) or
      (pick.Hit[I] is TVXGizmoExUISphere) or
      (pick.Hit[I] is TVXGizmoExUIPolyGon) or
      (pick.Hit[I] is TVXGizmoExuITorus) or
      (pick.Hit[I] is TVXGizmoExUIFrustrum) or
      (pick.Hit[I] is TVXGizmoExUIArrowLine) or
      (pick.Hit[I] is TVXGizmoExUIFlatText) or
      (pick.Hit[I] is TVXGizmoExUILines) then
    begin
      gotPick := True;
      case fOperation of
        gopRotate:
        begin
          if (pick.Hit[I] = FUIICRotateTorusX) then
          begin
            SetInitialDiskPostition(FUIICRotateTorusX, FUIRotateDiskx);
            SetInitialDiskPostition(FUIICRotateTorusX, FUIRotateDiskx2);
          end;
          if (pick.Hit[I] = FUIICRotateTorusY) then
          begin
            SetInitialDiskPostition(FUIICRotateTorusY, FUIRotateDiskY);
            SetInitialDiskPostition(FUIICRotateTorusY, FUIRotateDiskY2);
          end;
          if (pick.Hit[I] = FUIICRotateTorusZ) then
          begin
            SetInitialDiskPostition(FUIICRotateTorusZ, FUIRotateDiskZ);
            SetInitialDiskPostition(FUIICRotateTorusZ, FUIRotateDiskZ2);
          end;
        end;
      end;
    end;

  if not FShowMultiSelecting and not gotPick then
  begin
    for I := 0 to pick.Count - 1 do

      if (pick.Hit[I] <> FInterfaceRender) and
        (pick.Hit[I] <> FInternalRender) and not (pick.Hit[I] is TVXGizmoExUISphere)
        and not (pick.Hit[I] is TVXGizmoExUIPolyGon)
        and not (pick.Hit[I] is TVXGizmoExuITorus)
        and not (pick.Hit[I] is TVXGizmoExUIFrustrum)
        and not (pick.Hit[I] is TVXGizmoExUIArrowLine)
        and not (pick.Hit[I] is TVXGizmoExUILines)
        and not (pick.Hit[I] is TVXGizmoExUIFlatText)
        and not (CheckObjectInExcludeList(TVXBaseSceneObject(pick.hit[I])))
        and not (CheckClassNameInExcludeList(TVXBaseSceneObject(pick.hit[I]))) then
      begin

        //Clear list
        if not EnableMultiSelection then
          ClearSelection
        else
        if (pick.Count - 1 >= 0) and
          (FSelectedObjects.FindObject(pick.Hit[I]) = -1) then
          if not FCanAddObjToSelectionList and not FCanRemoveObjFromSelectionList then
            ClearSelection;

        if not FCanRemoveObjFromSelectionList then
          AddObjToSelectionList(TVXBaseSceneObject(pick.Hit[I]))
        else
          RemoveObjFromSelectionList(TVXBaseSceneObject(pick.Hit[I]));

        if Assigned(onSelect) then
          onSelect(self, FSelectedObjects);

        UpdateGizmo();
        Break;
      end;

  end
  else
    UpdateVisibleInfoLabels();

  pick.Free;

  moving := True;
  lastMousePos := MouseWorldPos(X, Y);

  if EnableMultiSelection then
    MultiSelMouseDown(X, Y);

end;

procedure TVXGizmoEx.ViewerMouseUp(const X, Y: Integer);
var
  pick: TVXPickList;
begin

  if (not Enabled) or (RootGizmo = nil) or (RootObjects = nil) then
    Exit;

  moving := False;

  case fOperation of
    gopRotate: SetAngleDisk(0);
  end;
  fchangerate := NullVector;

  //MassSelection+\-add mass selected obj
  if operation = gopNone then
  begin
    pick := InternalGetPickedObjects(X - 1, Y - 1, X + 1, Y + 1, 8);
    //очистка списка если кликнули в пустоту
    if not FCanAddObjToSelectionList and not FCanRemoveObjFromSelectionList and (pick.Count = 0) then
      ClearSelection;

    pick.Free;
  end;

  if EnableMultiSelection and FShowMultiSelecting then
    MultiSelMouseUp(X, Y);


  if not FShowMultiSelecting and EnableActionHistory then
    FHistory.AddObjects(FSelectedObjects);
  Updategizmo;
end;

//------------------------------------------------------------------------------

procedure TVXGizmoEx.UpdateGizmo;
var
  d: Single;
  v: TVector;
  I: Integer;
begin
  if not Assigned(RootGizmo)   or
     not Assigned(RootObjects) or
     not Assigned(Viewer)      then
    Exit;
    
  if FSelectedObjects.Count - 1 < 0 then
  begin
    FUIRootHelpers.Visible := False;
    Exit;
  end
  else
  begin
    FUIRootHelpers.Visible := True;

    if Assigned(onUpdate) then
      OnUpdate(self);

    v := VectorMake(0, 0, 0);
    //устанавливаем гизмо в нужную позицию!
    for  I := 0 to FSelectedObjects.Count - 1 do
      VectorAdd(v, TVXBaseSceneObject(FSelectedObjects.Hit[I]).AbsolutePosition, v);

    if FSelectedObjects.Count = 1 then
      I := 1
    else
      I := FSelectedObjects.Count;
    FUIRootHelpers.Position.AsVector := VectorDivide(v, VectorMake(I, I, I));
  end;

  case Ord(ReferenceCoordSystem) of
    0:
    begin
      FUIRootHelpers.Direction := FUIBaseGizmo.Direction;
      FUIRootHelpers.Up := FUIBaseGizmo.Up;
    end;

    1:
    begin
      FUIRootHelpers.AbsoluteDirection := TVXBaseSceneObject(FSelectedObjects.Hit[0]).AbsoluteDirection;
      FUIRootHelpers.AbsoluteUp := TVXBaseSceneObject(FSelectedObjects.Hit[0]).AbsoluteUp;
    end;
  end;

  Assert(Viewer <> nil, 'Viewer not Assigned to gizmo');

  if FAutoZoom then
    d := Viewer.Camera.distanceTo(FUIRootHelpers) / FAutoZoomFactor
  else
    d := FZoomFactor;

  if FUIRootAxisLabel.Visible then
  begin
    FUIAxisLabelX.PointTo(Viewer.Camera.Position.AsVector, Viewer.Camera.Up.AsVector);
    FUIAxisLabelX.StructureChanged;
    FUIAxisLabelY.PointTo(Viewer.Camera.Position.AsVector, Viewer.Camera.Up.AsVector);
    FUIAxisLabelY.StructureChanged;
    FUIAxisLabelZ.PointTo(Viewer.Camera.Position.AsVector, Viewer.Camera.Up.AsVector);
    FUIAxisLabelZ.StructureChanged;
    FUIRootAxisLabel.Scale.AsVector := VectorMake(d, d, d);
  end;

  if FUIRootSelect.Visible then
    FUIRootSelect.Scale.AsVector := VectorMake(d, d, d);

  if FUIRootMovement.Visible then
    FUIRootMovement.Scale.AsVector := VectorMake(d, d, d);

  if FUIRootRotate.Visible then
  begin
    FUIRotateLineXY.PointTo(Viewer.Camera.Position.AsVector, Viewer.Camera.Up.AsVector);
    FUIRotateLineXY.StructureChanged;
    FUIRotateLineXZ.PointTo(Viewer.Camera.Position.AsVector, Viewer.Camera.Up.AsVector);
    FUIRotateLineXZ.StructureChanged;

    FUIRotateAxisLabelX.PointTo(Viewer.Camera.Position.AsVector, Viewer.Camera.Up.AsVector);
    FUIRotateAxisLabelX.StructureChanged;
    FUIRotateAxisLabelY.PointTo(Viewer.Camera.Position.AsVector, Viewer.Camera.Up.AsVector);
    FUIRotateAxisLabelY.StructureChanged;
    FUIRotateAxisLabelZ.PointTo(Viewer.Camera.Position.AsVector, Viewer.Camera.Up.AsVector);
    FUIRotateAxisLabelZ.StructureChanged;

    FUIRootRotate.Scale.AsVector := VectorMake(d, d, d);
  end;

  if not moving and FUIRootScale.Visible then
    FUIRootScale.Scale.AsVector := VectorMake(d, d, d);

  if FUIRootVisibleInfoLabels.Visible then
  begin
    UpdateVisibleInfoLabels;

    FUIRootVisibleInfoLabels.AbsoluteDirection := FUIBaseGizmo.AbsoluteDirection;
    FUIRootVisibleInfoLabels.AbsoluteUp := FUIBaseGizmo.AbsoluteUp;

    FUIVisibleInfoLabels.ModulateColor.Color := FVisibleInfoLabelsColor.Color;
    FUIVisibleInfoLabels.PointTo(Viewer.Camera.Position.AsVector, Viewer.Camera.Up.AsVector);
    FUIVisibleInfoLabels.StructureChanged;
    FUIRootVisibleInfoLabels.Scale.AsVector := VectorMake(d, d, d);
  end;

end;

procedure TVXGizmoEx.LooseSelection;
begin
  ClearSelection;
  UpdateGizmo;
  if Assigned(onSelectionLost) then
    OnSelectionLost(self);
end;

procedure TVXGizmoEx.ClearSelection;
begin
  FSelectedObj := nil;
  FSelectedObjects.Clear;
end;

procedure TVXGizmoEx.LooseCursorSelection;
begin
  FShowMultiSelecting := False;
  if high(FSelectionRec) > 0 then
    SetLength(FSelectionRec, 0);
  flastcursorPos := Point(0, 0);
  fcursorPos := point(0, 0);
end;

procedure TVXGizmoEx.SetViewer(const Value: TVXSceneViewer);
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


procedure TVXGizmoEx.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FViewer then
      FViewer := nil;
    if AComponent = FRootGizmo then
      FRootGizmo := nil;
    if AComponent = FRootObjects then
      FRootObjects := nil;
    if AComponent = FGizmoTmpRoot then
      FGizmoTmpRoot := nil;
  end;

  if FHistory <> nil then
    FHistory.Notification(AComponent, Operation);
end;

function TVXGizmoEx.Undo: TVXGizmoExActionHistoryItem;
var
  I: Integer;
begin
  Result := FHistory.Undo;
  if Result = nil then
    Exit;
  FSelectedObjects.Clear;

  for I := 0 to Result.GizmoObjectCollection.Count - 1 do
    FSelectedObjects.AddHit(Result.GizmoObjectCollection.Items[I].EffectedObject, nil, 0, 0);

  UpdateGizmo;
end;

function TVXGizmoEx.Redo: TVXGizmoExActionHistoryItem;
var
  I: Integer;
begin
  Result := FHistory.Redo;
  if Result = nil then
    Exit;
  FSelectedObjects.Clear;

  for I := 0 to Result.GizmoObjectCollection.Count - 1 do
    if not Result.GizmoObjectCollection.Items[I].FReturnObject then
      FSelectedObjects.AddHit(Result.GizmoObjectCollection.Items[I].EffectedObject, nil, 0, 0);

  UpdateGizmo;
end;

////////////////////////////////////////////////////////////

procedure TVXGizmoExObjectItem.AssignFromObject(const AObject: TVXBaseSceneObject; AssignAndRemoveObj: Boolean = False);
begin
  if not AssignAndRemoveObj then
  begin
    EffectedObject := AObject;
    SetOldMatrix(AObject.Matrix^);
    if AObject is TVXFreeForm then
      FOldAutoScaling := TVXFreeForm(AObject).AutoScaling.AsVector;
  end
  else
  begin
    EffectedObject := AObject;
    FParentOldObject := EffectedObject.Parent;
    FIndexOldObject := EffectedObject.Index;
    FNameOldObject := EffectedObject.Name;
    FEffectedObject.MoveTo(GizmoTmpRoot);
    FReturnObject := True;
  end;
end;

constructor TVXGizmoExObjectItem.Create(AOwner: TCollection);
begin
  FReturnObject := False;
  inherited;
end;

destructor TVXGizmoExObjectItem.Destroy;
begin
  if FReturnObject then
    if assigned(fEffectedObject) then
      FreeAndNil(fEffectedObject);
  inherited;
end;

function TVXGizmoExObjectItem.GetGizmo: TVXGizmoEx;
begin
  if GetParent <> nil then
    Result := GetPArent.GetParent
  else
    Result := nil;
end;

function TVXGizmoExObjectItem.GetParent: TVXGizmoExObjectCollection;
begin
  Result := TVXGizmoExObjectCollection(GetOwner);
end;

procedure TVXGizmoExObjectItem.DoUndo;
begin
  if FEffectedObject = nil then
    Exit;

  if not FReturnObject then
  begin
    FEffectedObject.SetMatrix(FOldMatrix);
    if FEffectedObject is TVXFreeForm then
      TVXFreeForm(FEffectedObject).AutoScaling.AsVector := FOldAutoScaling;
  end
  else
  begin
    if fEffectedObject.Parent <> GizmoTmpRoot then
    begin
      fEffectedObject.MoveTo(FGizmoTmpRoot);
      Exit;
    end;

    FParentOldObject.Insert(FIndexOldObject, fEffectedObject);
  end;
end;

procedure TVXGizmoExObjectItem.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FEffectedObject then
      if FReturnObject then
        FreeAndNil(FEffectedObject)
      else
        FEffectedObject := nil;
    GizmoTmpRoot := nil;
  end;
end;

procedure TVXGizmoExObjectItem.SetEffectedObject(const Value: TVXBaseSceneObject);
begin
  FEffectedObject := Value;
end;

procedure TVXGizmoExObjectItem.SetOldMatrix(const Value: TMatrix);
begin
  FOldMatrix := Value;
end;

{ TVXGizmoExUndoCollection }

function TVXGizmoExObjectCollection.Add: TVXGizmoExObjectItem;
begin
  Result := TVXGizmoExObjectItem(inherited Add);
end;

function TVXGizmoExObjectCollection.GetItems(const Index: Integer): TVXGizmoExObjectItem;
begin
  Result := TVXGizmoExObjectItem(inherited GetItem(Index));
end;

function TVXGizmoExObjectCollection.GetParent: TVXGizmoEx;
begin
  Result := TVXGizmoEx(GetOwner);
end;

procedure TVXGizmoExObjectCollection.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  if Count <> 0 then
    for I := 0 to Count - 1 do
      GetItems(I).Notification(AComponent, Operation);
end;

procedure TVXGizmoExObjectCollection.RemoveByObject(const AObject: TVXCustomSceneObject);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if GetItems(I).FEffectedObject = AObject then
      GetItems(I).Free;
end;

procedure TVXGizmoExObjectCollection.SetItems(const Index: Integer; const Value: TVXGizmoExObjectItem);
begin
  GetItems(Index).Assign(Value);
end;

procedure TVXGizmoExObjectCollection.DoUndo;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    GetItems(I).DoUndo;
end;

/////////////////////////////////////////////////////////////

constructor TVXGizmoExActionHistoryItem.Create(AOwner: TCollection);
begin
  inherited;
  FGizmoObjectCollection := TVXGizmoExObjectCollection.Create(self, TVXGizmoExObjectItem);
end;

destructor TVXGizmoExActionHistoryItem.Destroy;
begin
  FGizmoObjectCollection.Free;
  inherited;
end;

procedure TVXGizmoExActionHistoryItem.SetObject(aValue: TObject);
begin
  if FObject <> AValue then
    FObject := AValue;
end;

procedure TVXGizmoExActionHistoryItem.SetGizmoObjectCollection(aValue: TVXGizmoExObjectCollection);
begin
  if FGizmoObjectCollection <> aValue then
    FGizmoObjectCollection := aValue;
end;

{ TVXGizmoExUndoCollection }

constructor TVXGizmoExActionHistoryCollection.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  MaxCount := 30;
  FItemIndex := -1;
  inherited;
end;

function TVXGizmoExActionHistoryCollection.Add: TVXGizmoExActionHistoryItem;
begin
  Result := nil;
  //If used undo then rewrite previous record
  if FItemIndex = Count - 1 then
  begin
    Result := TVXGizmoExActionHistoryItem(inherited Add);
    FItemIndex := FItemIndex + 1;
    ;
  end
  else
  if (FItemIndex >= 0) or (FItemIndex < Count - 1) then
  begin
    Result := Items[FItemIndex];
    FItemIndex := FItemIndex + 1;
  end;
  // if number of record greater then maxcount then delete elements
  if Count - 1 > MaxCount then
  begin
    Delete(0);
    FItemIndex := Count - 1;
  end;
end;

function TVXGizmoExActionHistoryCollection.GetItems(const Index: Integer): TVXGizmoExActionHistoryItem;
begin
  Result := TVXGizmoExActionHistoryItem(inherited GetItem(Index));
end;

procedure TVXGizmoExActionHistoryCollection.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  if Count <> 0 then
    for I := 0 to Count - 1 do
      GetItems(I).FGizmoObjectCollection.Notification(AComponent, Operation);
end;

procedure TVXGizmoExActionHistoryCollection.SetItems(const Index: Integer; const Value: TVXGizmoExActionHistoryItem);
begin
  GetItems(Index).Assign(Value);
end;

function TVXGizmoExActionHistoryCollection.Undo: TVXGizmoExActionHistoryItem;
begin
  Result := nil;
  if not (FItemIndex > 0) or not (FItemIndex <= Count - 1) then
    Exit;

  if FItemIndex <> 0 then
    FItemIndex := FItemIndex - 1;

  Result := Items[FItemIndex];
  Result.GizmoObjectCollection.DoUndo;
end;

function TVXGizmoExActionHistoryCollection.Redo: TVXGizmoExActionHistoryItem;
begin
  Result := nil;
  if not (FItemIndex >= 0) or not (FItemIndex < Count - 1) then
    Exit;
  if FItemIndex <> Count - 1 then
    FItemIndex := FItemIndex + 1;
  Result := Items[FItemIndex];
  Result.GizmoObjectCollection.DoUndo;
end;

procedure TVXGizmoExActionHistoryCollection.AddObjects(objs: TVXPickList);
var
  I: Integer;
begin
  with Add do
  begin
    for I := 0 to objs.Count - 1 do
      GizmoObjectCollection.Add.AssignFromObject(TVXBaseSceneObject(objs.Hit[I]));
  end;

end;

procedure TVXGizmoExActionHistoryCollection.AddObject(obj: TObject);
begin
  if obj = nil then
    Exit;
  Add.FObject := obj;
end;

procedure TVXGizmoExActionHistoryCollection.RemoveObjects(objs: TVXPickList);
var
  I: Integer;
begin
  if not Assigned(self.GizmoTmpRoot) then
    Exit;

  with Add do
    for I := 0 to objs.Count - 1 do
      if objs.Hit[I] <> nil then
        with GizmoObjectCollection.Add do
        begin
          GizmoTmpRoot := self.GizmoTmpRoot;
          AssignFromObject(TVXBaseSceneObject(objs.Hit[I]), True);
        end;

  objs.Clear;
end;


end.
