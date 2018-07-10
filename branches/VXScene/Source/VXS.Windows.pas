//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  OpenGL windows management classes and structures
}

unit VXS.Windows;

interface

{$I VXScene.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Math,
  System.UITypes,
  FMX.Graphics,
  FMX.Types,

  VXS.OpenGL,
  VXS.Scene,
  VXS.PersistentClasses,
  VXS.Strings,
  VXS.Coordinates,
  VXS.HUDObjects,
  VXS.Material,
  VXS.Context,
  VXS.BitmapFont,
  VXS.WindowsFont,
  VXS.VectorTypes,
  VXS.VectorGeometry,
  VXS.Gui,
  VXS.CrossPlatform,
  VXS.Color,
  VXS.RenderContextInfo,
  VXS.BaseClasses,
  VXS.Objects,
  VXS.State,
  VXS.Utils;

type

  TVXBaseComponent = class(TVXBaseGuiObject)
  private
    FGUIRedraw: Boolean;
    FGuiLayout: TVXGuiLayout;
    FGuiLayoutName: TVXGuiComponentName;
    FGuiComponent: TVXGuiComponent;
    FReBuildGui: Boolean;
    FRedrawAtOnce: Boolean;
    MoveX, MoveY: Single;
    FRenderStatus: TGUIDrawResult;
    FAlphaChannel: Single;
    FRotation: Single;
    FNoZWrite: Boolean;
    BlockRendering: Boolean;
    RenderingCount: Integer;
    BlockedCount: Integer;
    GuiDestroying: Boolean;
    FDoChangesOnProgress: Boolean;
    FAutosize: Boolean;
    procedure SetGUIRedraw(value: Boolean);
    procedure SetDoChangesOnProgress(const Value: Boolean);
    procedure SetAutosize(const Value: Boolean);
  protected
    procedure RenderHeader(var rci: TVXRenderContextInfo; renderSelf,
      renderChildren: Boolean);
    procedure RenderFooter(var rci: TVXRenderContextInfo; renderSelf,
      renderChildren: Boolean);
    procedure SetGuiLayout(NewGui: TVXGuiLayout); virtual;
    procedure SetGuiLayoutName(NewName: TVXGuiComponentName);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure SetRotation(const val: Single);
    procedure SetAlphaChannel(const val: Single);
    function StoreAlphaChannel: Boolean;
    procedure SetNoZWrite(const val: Boolean);
  public
    procedure BlockRender;
    procedure UnBlockRender;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure NotifyChange(Sender: TObject); override;
    procedure DoChanges; virtual;
    procedure MoveGUI(XRel, YRel: Single);
    procedure PlaceGUI(XPos, YPos: Single);
    procedure DoProgress(const progressTime: TVXProgressTimes); override;
    procedure DoRender(var rci: TVXRenderContextInfo; renderSelf, renderChildren:
      Boolean); override;
    procedure InternalRender(var rci: TVXRenderContextInfo; renderSelf,
      renderChildren: Boolean); virtual;
    property GUIRedraw: Boolean read FGUIRedraw write SetGUIRedraw;
    property ReBuildGui: Boolean read FReBuildGui write FReBuildGui;
  published
    property Autosize: Boolean read FAutosize write SetAutosize;
    property RedrawAtOnce: Boolean read FRedrawAtOnce write FRedrawAtOnce;
    property GuiLayout: TVXGuiLayout read FGuiLayout write SetGuiLayout;
    property GuiLayoutName: TVXGuiComponentName read FGuiLayoutName write
      SetGuiLayoutName;
    { This the ON-SCREEN rotation of the GuiComponent.
       Rotatation=0 is handled faster. }
    property Rotation: Single read FRotation write SetRotation;
    { If different from 1, this value will replace that of Diffuse.Alpha }
    property AlphaChannel: Single read FAlphaChannel write SetAlphaChannel stored
      StoreAlphaChannel;
    { If True, GuiComponent will not write to Z-Buffer.
       GuiComponent will STILL be maskable by ZBuffer test. }
    property NoZWrite: Boolean read FNoZWrite write SetNoZWrite;
    property DoChangesOnProgress: Boolean read FDoChangesOnProgress write
      SetDoChangesOnProgress;
    property Visible;
    property Width;
    property Height;
    property Left;
    property Top;
    property Position;
  end;

  TVXFocusControl = class;
  TVXBaseControl = class;

  TVXMouseAction = (ma_mouseup, ma_mousedown, ma_mousemove);

  TVXAcceptMouseQuery = procedure(Sender: TVXBaseControl; Shift: TShiftState;
    Action: TVXMouseAction; Button: TVXMouseButton; X, Y: Integer; var Accept:
    boolean) of object;
  TVXBaseControl = class(TVXBaseComponent)
  private
    FOnMouseDown: TVXMouseEvent;
    FOnMouseMove: TVXMouseMoveEvent;
    FOnMouseUp: TVXMouseEvent;
    FKeepMouseEvents: Boolean;
    FActiveControl: TVXBaseControl;
    FFocusedControl: TVXFocusControl;
    FOnAcceptMouseQuery: TVXAcceptMouseQuery;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FEnteredControl: TVXBaseControl;
  protected
    procedure InternalMouseDown(Shift: TShiftState; Button: TVXMouseButton; X,
      Y: Integer); virtual;
    procedure InternalMouseUp(Shift: TShiftState; Button: TVXMouseButton; X, Y:
      Integer); virtual;
    procedure InternalMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetActiveControl(NewControl: TVXBaseControl);
    procedure SetFocusedControl(NewControl: TVXFocusControl);
    function FindFirstGui: TVXBaseControl;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoMouseEnter;
    procedure DoMouseLeave;
  public
    function MouseDown(Sender: TObject; Button: TVXMouseButton; Shift:
      TShiftState; X, Y: Integer): Boolean; virtual;
    function MouseUp(Sender: TObject; Button: TVXMouseButton; Shift:
      TShiftState; X, Y: Integer): Boolean; virtual;
    function MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer):
      Boolean; virtual;
    procedure KeyPress(Sender: TObject; var Key: Char); virtual;
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      virtual;
    procedure KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
      virtual;
    property ActiveControl: TVXBaseControl read FActiveControl write
      SetActiveControl;
    property KeepMouseEvents: Boolean read FKeepMouseEvents write
      FKeepMouseEvents default false;
  published
    property FocusedControl: TVXFocusControl read FFocusedControl write
      SetFocusedControl;
    property OnMouseDown: TVXMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TVXMouseMoveEvent read FOnMouseMove write
      FOnMouseMove;
    property OnMouseUp: TVXMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnAcceptMouseQuery: TVXAcceptMouseQuery read FOnAcceptMouseQuery
      write FOnAcceptMouseQuery;
  end;

  TVXBaseFontControl = class(TVXBaseControl)
  private
    FBitmapFont: TVXCustomBitmapFont;
    FDefaultColor: TColorVector;
  protected
    function GetDefaultColor: TColor; //in VCL TDelphiColor
    procedure SetDefaultColor(value: TColor);  //in VCL TDelphiColor
    procedure SetBitmapFont(NewFont: TVXCustomBitmapFont);
    function GetBitmapFont: TVXCustomBitmapFont;
    procedure WriteTextAt(var rci: TVXRenderContextInfo; const X, Y: Single;
      const Data: UnicodeString; const Color: TColorVector); overload;
    procedure WriteTextAt(var rci: TVXRenderContextInfo; const X1, Y1, X2, Y2:
      Single; const Data: UnicodeString; const Color: TColorVector); overload;
    function GetFontHeight: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  published
    property BitmapFont: TVXCustomBitmapFont read GetBitmapFont write
      SetBitmapFont;
    property DefaultColor: TColor read GetDefaultColor write
      SetDefaultColor;
  end;

  TVXBaseTextControl = class(TVXBaseFontControl)
  private
    FCaption: UnicodeString;
  protected
    procedure SetCaption(const NewCaption: UnicodeString);
  public
  published
    property Caption: UnicodeString read FCaption write SetCaption;
  end;

  TVXFocusControl = class(TVXBaseTextControl)
  private
    FRootControl: TVXBaseControl;
    FFocused: Boolean;
    FOnKeyDown: TKeyEvent;
    FOnKeyUp: TKeyEvent;
    FOnKeyPress: TVXKeyPressEvent;
    FShiftState: TShiftState;
    FFocusedColor: TColorVector;
  protected
    procedure InternalKeyPress(var Key: Char); virtual;
    procedure InternalKeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure InternalKeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure SetFocused(Value: Boolean); virtual;
    function GetRootControl: TVXBaseControl;
    function GetFocusedColor: TColor; //in VCL TDelphiColor;
    procedure SetFocusedColor(const Val: TColor); //in VCL TDelphiColor;
  public
    destructor Destroy; override;
    procedure NotifyHide; override;
    procedure MoveTo(newParent: TVXBaseSceneObject); override;
    procedure ReGetRootControl;
    procedure SetFocus;
    procedure PrevControl;
    procedure NextControl;
    procedure KeyPress(Sender: TObject; var Key: Char); override;
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      override;
    procedure KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
      override;
  published
    property RootControl: TVXBaseControl read GetRootControl;
    property Focused: Boolean read FFocused write SetFocused;
    property FocusedColor: TColor read GetFocusedColor write
      SetFocusedColor;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnKeyPress: TVXKeyPressEvent read FOnKeyPress write FOnKeyPress;
  end;

  TVXCustomControl = class;
  TVXCustomRenderEvent = procedure(Sender: TVXCustomControl; Bitmap: TBitmap)
    of object;
  TVXCustomControl = class(TVXFocusControl)
  private
    FCustomData: Pointer;
    FCustomObject: TObject;
    FOnRender: TVXCustomRenderEvent;
    FMaterial: TVXMaterial;
    FBitmap: TBitmap;
    FInternalBitmap: TBitmap;
    FBitmapChanged: Boolean;
    FXTexCoord: Single;
    FYTexCoord: Single;
    FInvalidRenderCount: Integer;
    FMaxInvalidRenderCount: Integer;
    FCentered: Boolean;
    procedure SetCentered(const Value: Boolean);
  protected
    procedure OnBitmapChanged(Sender: TObject);
    procedure SetBitmap(ABitmap: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InternalRender(var rci: TVXRenderContextInfo; renderSelf,
      renderChildren: Boolean); override;
    procedure SetMaterial(AMaterial: TVXMaterial);
    property CustomData: Pointer read FCustomData write FCustomData;
    property CustomObject: TObject read FCustomObject write FCustomObject;
  published
    property OnRender: TVXCustomRenderEvent read FOnRender write FOnRender;
    property Centered: Boolean read FCentered write SetCentered;
    property Material: TVXMaterial read FMaterial write SetMaterial;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property MaxInvalidRenderCount: Integer read FMaxInvalidRenderCount write
      FMaxInvalidRenderCount;
  end;

  TVXPopupMenu = class;
  TVXPopupMenuClick = procedure(Sender: TVXPopupMenu; index: Integer; const
    MenuItemText: string) of object;

  TVXPopupMenu = class(TVXFocusControl)
  private
    FOnClick: TVXPopupMenuClick;
    FMenuItems: TStrings;
    FSelIndex: Integer;
    FMarginSize: Single;
    NewHeight: Single;
  protected
    procedure SetFocused(Value: Boolean); override;
    procedure SetMenuItems(Value: TStrings);
    procedure SetMarginSize(const val: Single);
    procedure SetSelIndex(const val: Integer);
    procedure InternalMouseDown(Shift: TShiftState; Button: TVXMouseButton; X,
      Y: Integer); override;
    procedure InternalMouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure OnStringListChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PopUp(Px, Py: Integer);
    procedure InternalRender(var rci: TVXRenderContextInfo; renderSelf,
      renderChildren: Boolean); override;
    procedure DoRender(var rci: TVXRenderContextInfo; renderSelf, renderChildren:
      Boolean); override;
    function MouseDown(Sender: TObject; Button: TVXMouseButton; Shift:
      TShiftState; X, Y: Integer): Boolean; override;
  published
    property MenuItems: TStrings read FMenuItems write SetMenuItems;
    property OnClick: TVXPopupMenuClick read FOnClick write FOnClick;
    property MarginSize: Single read FMarginSize write SetMarginSize;
    property SelIndex: Integer read FSelIndex write SetSelIndex;
  end;
  TVXForm = class;

  TVXFormCanRequest = procedure(Sender: TVXForm; var Can: Boolean) of object;
  TVXFormCloseOptions = (co_Hide, co_Ignore, co_Destroy);
  TVXFormCanClose = procedure(Sender: TVXForm; var CanClose: TVXFormCloseOptions)
    of object;
  TVXFormNotify = procedure(Sender: TVXForm) of object;
  TVXFormMove = procedure(Sender: TVXForm; var Left, Top: Single) of object;

  TVXForm = class(TVXBaseTextControl)
  private
    FOnCanMove: TVXFormCanRequest;
    FOnCanResize: TVXFormCanRequest;
    FOnCanClose: TVXFormCanClose;
    FOnShow: TVXFormNotify;
    FOnHide: TVXFormNotify;
    FOnMoving: TVXFormMove;
    Moving: Boolean;
    OldX: Integer;
    OldY: Integer;
    FTitleColor: TColorVector;
    FTitleOffset: Single;
  protected
    procedure InternalMouseDown(Shift: TShiftState; Button: TVXMouseButton; X,
      Y: Integer); override;
    procedure InternalMouseUp(Shift: TShiftState; Button: TVXMouseButton; X, Y:
      Integer); override;
    procedure InternalMouseMove(Shift: TShiftState; X, Y: Integer); override;
    function GetTitleColor: TColor; //in VCL TDelphiColor;
    procedure SetTitleColor(value: TColor); //in VCL TDelphiColor;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Close;

    procedure NotifyShow; override;
    procedure NotifyHide; override;
    function MouseUp(Sender: TObject; Button: TVXMouseButton; Shift:
      TShiftState; X, Y: Integer): Boolean; override;
    function MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer):
      Boolean; override;
    procedure InternalRender(var rci: TVXRenderContextInfo; renderSelf,
      renderChildren: Boolean); override;
  published
    property TitleColor: TColor read GetTitleColor write SetTitleColor;
    property OnCanMove: TVXFormCanRequest read FOnCanMove write FOnCanMove;
    property OnCanResize: TVXFormCanRequest read FOnCanResize write
      FOnCanResize;
    property OnCanClose: TVXFormCanClose read FOnCanClose write FOnCanClose;
    property OnShow: TVXFormNotify read FOnShow write FOnShow;
    property OnHide: TVXFormNotify read FOnHide write FOnHide;
    property OnMoving: TVXFormMove read FOnMoving write FOnMoving;
    property TitleOffset: Single read FTitleOffset write FTitleOffset;
  end;

  TVXPanel = class(TVXBaseControl)
  end;

  TVXCheckBox = class(TVXBaseControl)
  private
    FChecked: Boolean;
    FOnChange: TNotifyEvent;
    FGuiLayoutNameChecked: TVXGuiComponentName;
    FGuiCheckedComponent: TVXGuiComponent;
    FGroup: Integer;
  protected
    procedure SetChecked(NewChecked: Boolean);
    procedure InternalMouseDown(Shift: TShiftState; Button: TVXMouseButton; X,
      Y: Integer); override;
    procedure InternalMouseUp(Shift: TShiftState; Button: TVXMouseButton; X, Y:
      Integer); override;
    procedure SetGuiLayoutNameChecked(newName: TVXGuiComponentName);
    procedure SetGuiLayout(NewGui: TVXGuiLayout); override;
    procedure SetGroup(const val: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure InternalRender(var rci: TVXRenderContextInfo; renderSelf,
      renderChildren: Boolean); override;
    procedure NotifyChange(Sender: TObject); override;
  published
    property Group: Integer read FGroup write SetGroup;
    property Checked: Boolean read FChecked write SetChecked;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property GuiLayoutNameChecked: TVXGuiComponentName read FGuiLayoutNameChecked
      write SetGuiLayoutNameChecked;
  end;

  TVXButton = class(TVXFocusControl)
  private
    FPressed: Boolean;
    FOnButtonClick: TNotifyEvent;
    FGuiLayoutNamePressed: TVXGuiComponentName;
    FGuiPressedComponent: TVXGuiComponent;
    FBitBtn: TVXMaterial;
    FGroup: Integer;
    FLogicWidth: Single;
    FLogicHeight: Single;
    FXOffSet: Single;
    FYOffSet: Single;
    FAllowUp: Boolean;
  protected
    procedure SetPressed(NewPressed: Boolean);
    procedure InternalMouseDown(Shift: TShiftState; Button: TVXMouseButton; X,
      Y: Integer); override;
    procedure InternalMouseUp(Shift: TShiftState; Button: TVXMouseButton; X, Y:
      Integer); override;
    procedure InternalKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure InternalKeyUp(var Key: Word; Shift: TShiftState); override;
    procedure SetFocused(Value: Boolean); override;
    procedure SetGuiLayoutNamePressed(newName: TVXGuiComponentName);
    procedure SetGuiLayout(NewGui: TVXGuiLayout); override;
    procedure SetBitBtn(AValue: TVXMaterial);
    procedure DestroyHandle; override;
    procedure SetGroup(const val: Integer);
    procedure SetLogicWidth(const val: single);
    procedure SetLogicHeight(const val: single);
    procedure SetXOffset(const val: single);
    procedure SetYOffset(const val: single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InternalRender(var rci: TVXRenderContextInfo; renderSelf,
      renderChildren: Boolean); override;
  published
    property Group: Integer read FGroup write SetGroup;
    property BitBtn: TVXMaterial read FBitBtn write SetBitBtn;
    property Pressed: Boolean read FPressed write SetPressed;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write
      FOnButtonClick;
    property GuiLayoutNamePressed: TVXGuiComponentName read FGuiLayoutNamePressed
      write SetGuiLayoutNamePressed;
    property LogicWidth: Single read FLogicWidth write SetLogicWidth;
    property LogicHeight: Single read FLogicHeight write SetLogicHeight;
    property XOffset: Single read FXOffset write SetXOffset;
    property YOffset: Single read FYOffset write SetYOffset;
    property AllowUp: Boolean read FAllowUp write FAllowUp;
  end;

  TVXEdit = class(TVXFocusControl)
  private
    FOnChange: TNotifyEvent;
    FSelStart: Integer;
    FReadOnly: Boolean;
    FEditChar: string;
  protected
    procedure InternalMouseDown(Shift: TShiftState; Button: TVXMouseButton; X,
      Y: Integer); override;
    procedure InternalKeyPress(var Key: Char); override;
    procedure InternalKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure InternalKeyUp(var Key: Word; Shift: TShiftState); override;
    procedure SetFocused(Value: Boolean); override;
    procedure SetSelStart(const Value: Integer);
    procedure SetEditChar(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure InternalRender(var rci: TVXRenderContextInfo; renderSelf,
      renderChildren: Boolean); override;
  published
    property EditChar: string read FEditChar write SetEditChar;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property SelStart: Integer read FSelStart write SetSelStart;
  end;

  TVXLabel = class(TVXBaseTextControl)
  private
    FAlignment: TAlignment;
    FTextLayout: TVXTextLayout;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetTextLayout(const Value: TVXTextLayout);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure InternalRender(var rci: TVXRenderContextInfo; renderSelf,
      renderChildren: Boolean); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property TextLayout: TVXTextLayout read FTextLayout write SetTextLayout;
  end;

  TVXAdvancedLabel = class(TVXFocusControl)
  private
  protected
  public
    procedure InternalRender(var rci: TVXRenderContextInfo; renderSelf,
      renderChildren: Boolean); override;
  published
  end;

  TVXScrollbar = class(TVXFocusControl)
  private
    FMin: Single;
    FMax: Single;
    FStep: Single;
    FPos: Single;
    FPageSize: Single;
    FOnChange: TNotifyEvent;
    FGuiLayoutKnobName: TVXGuiComponentName;
    FGuiKnobComponent: TVXGuiComponent;
    FKnobRenderStatus: TGUIDrawResult;
    FScrollOffs: Single;
    FScrolling: Boolean;
    FHorizontal: Boolean;
    FLocked: Boolean;
  protected
    procedure SetMin(const val: Single);
    procedure SetMax(const val: Single);
    procedure SetPos(const val: Single);
    procedure SetPageSize(const val: Single);
    procedure SetHorizontal(const val: Boolean);
    procedure SetGuiLayoutKnobName(newName: TVXGuiComponentName);
    procedure SetGuiLayout(NewGui: TVXGuiLayout); override;

    function GetScrollPosY(ScrollPos: Single): Single;
    function GetYScrollPos(Y: Single): Single;

    function GetScrollPosX(ScrollPos: Single): Single;
    function GetXScrollPos(X: Single): Single;

    procedure InternalMouseDown(Shift: TShiftState; Button: TVXMouseButton; X,
      Y: Integer); override;
    procedure InternalMouseUp(Shift: TShiftState; Button: TVXMouseButton; X, Y:
      Integer); override;
    procedure InternalMouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure StepUp;
    procedure StepDown;
    procedure PageUp;
    procedure PageDown;
    function MouseUp(Sender: TObject; Button: TVXMouseButton; Shift:
      TShiftState; X, Y: Integer): Boolean; override;
    function MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer):
      Boolean; override;
    procedure InternalRender(var rci: TVXRenderContextInfo; renderSelf,
      renderChildren: Boolean); override;
  published
    property Horizontal: Boolean read FHorizontal write SetHorizontal;
    property Pos: Single read FPos write SetPos;
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
    property Step: Single read FStep write FStep;
    property PageSize: Single read FPageSize write SetPageSize;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property GuiLayoutKnobName: TVXGuiComponentName read FGuiLayoutKnobName write
      SetGuiLayoutKnobName;
    property Locked: Boolean read FLocked write FLocked default False;
  end;

  StringGrid = class(TVXFocusControl)
  private
    FSelCol, FSelRow: Integer;
    FRowSelect: Boolean;
    FColSelect: Boolean;
    FColumns: TStrings;
    FRows: TList;
    FHeaderColor: TColorVector;
    FMarginSize: Integer;
    FColumnSize: Integer;
    FRowHeight: Integer;
    FScrollbar: TVXScrollbar;
    FDrawHeader: Boolean;
  protected
    function GetCell(X, Y: Integer; out oCol, oRow: Integer): Boolean;
    procedure InternalMouseDown(Shift: TShiftState; Button: TVXMouseButton; X,
      Y: Integer); override;
    procedure SetColumns(const val: TStrings);
    procedure SetColSelect(const val: Boolean);
    function GetRow(index: Integer): TStringList;
    procedure SetRow(index: Integer; const val: TStringList);
    function GetRowCount: Integer;
    procedure SetRowCount(const val: Integer);
    procedure SetSelCol(const val: Integer);
    procedure SetSelRow(const val: Integer);
    procedure SetRowSelect(const val: Boolean);
    procedure SetDrawHeader(const val: Boolean);
    function GetHeaderColor: TColor;
    procedure SetHeaderColor(const val: TColor);
    procedure SetMarginSize(const val: Integer);
    procedure SetColumnSize(const val: Integer);
    procedure SetRowHeight(const val: Integer);
    procedure SetScrollbar(const val: TVXScrollbar);
    procedure SetGuiLayout(NewGui: TVXGuiLayout); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function Add(Data: array of string): Integer; overload;
    function Add(const Data: string): Integer; overload;
    procedure SetText(Data: string);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure NotifyChange(Sender: TObject); override;
    procedure InternalRender(var rci: TVXRenderContextInfo; renderSelf,
      renderChildren: Boolean); override;
    procedure OnStringListChange(Sender: TObject);
    property Row[index: Integer]: TStringList read GetRow write SetRow;
  published
    property HeaderColor: TColor read GetHeaderColor write SetHeaderColor; //in VCL TDelphiColor;
    property Columns: TStrings read FColumns write SetColumns;
    property MarginSize: Integer read FMarginSize write SetMarginSize;
    property ColumnSize: Integer read FColumnSize write SetColumnSize;
    property RowHeight: Integer read FRowHeight write SetRowHeight;
    property RowCount: Integer read GetRowCount write SetRowCount;
    property SelCol: Integer read FSelCol write SetSelCol;
    property SelRow: Integer read FSelRow write SetSelRow;
    property RowSelect: Boolean read FRowSelect write SetRowSelect;
    property ColSelect: Boolean read FColSelect write SetColSelect;
    property DrawHeader: Boolean read FDrawHeader write SetDrawHeader;
    property Scrollbar: TVXScrollbar read FScrollbar write SetScrollbar;
  end;

function UnpressGroup(CurrentObject: TVXBaseSceneObject; AGroupID: Integer):
  Boolean;

//-------------------------------------------------------------------------
implementation
//-------------------------------------------------------------------------

function UnpressGroup(CurrentObject: TVXBaseSceneObject; AGroupID: Integer):
  Boolean;

var
  XC: Integer;

begin
  Result := False;
  if CurrentObject is TVXButton then
    with CurrentObject as TVXButton do
    begin
      if Group = AGroupID then
        if Pressed then
        begin
          Pressed := False;
          Result := True;
          Exit;
        end;
    end;

  if CurrentObject is TVXCheckBox then
    with CurrentObject as TVXCheckBox do
    begin
      if Group = AGroupID then
        if Checked then
        begin
          Checked := False;
          Result := True;
          Exit;
        end;
    end;

  for XC := 0 to CurrentObject.Count - 1 do
  begin
    if UnpressGroup(CurrentObject.Children[XC], AGroupID) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TVXBaseComponent.SetGUIRedraw(value: Boolean);

begin
  FGUIRedraw := Value;
  if Value then
  begin
    if csDestroying in ComponentState then
      Exit;
    if (FRedrawAtOnce) or (csDesigning in ComponentState) then
    begin
      FGUIRedraw := False;
      StructureChanged;
    end;
  end;
end;

procedure TVXBaseComponent.BlockRender;

begin
  while BlockedCount <> 0 do
    Sleep(1);
  BlockRendering := True;
  while RenderingCount <> BlockedCount do
    Sleep(1);
end;

procedure TVXBaseComponent.UnBlockRender;

begin
  BlockRendering := False;
end;

procedure TVXBaseComponent.RenderHeader(var rci: TVXRenderContextInfo; renderSelf,
  renderChildren: Boolean);

var
  f: Single;
begin
  FGuiLayout.Material.Apply(rci);
  if AlphaChannel <> 1 then
    rci.VXStates.SetMaterialAlphaChannel(GL_FRONT, AlphaChannel);
  // Prepare matrices
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glLoadMatrixf(@TVXSceneBuffer(rci.buffer).BaseProjectionMatrix);
  if rci.renderDPI = 96 then
    f := 1
  else
    f := rci.renderDPI / 96;
  glScalef(f * 2 / rci.viewPortSize.cx, f * 2 / rci.viewPortSize.cy, 1);
  glTranslatef(f * Position.X - rci.viewPortSize.cx * 0.5,
    rci.viewPortSize.cy * 0.5 - f * Position.Y, 0);
  if Rotation <> 0 then
    glRotatef(Rotation, 0, 0, 1);
  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;
  rci.VXStates.Disable(stDepthTest);
  rci.VXStates.DepthWriteMask := False;
end;

procedure TVXBaseComponent.RenderFooter(var rci: TVXRenderContextInfo; renderSelf,
  renderChildren: Boolean);

begin
  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;
  FGuiLayout.Material.UnApply(rci);
end;

procedure TVXBaseComponent.SetGuiLayout(NewGui: TVXGuiLayout);

begin
  if FGuiLayout <> NewGui then
  begin
    if Assigned(FGuiLayout) then
    begin
      FGuiLayout.RemoveGuiComponent(Self);
    end;
    FGuiComponent := nil;
    FGuiLayout := NewGui;
    if Assigned(FGuiLayout) then
      if FGuiLayoutName <> '' then
        FGuiComponent := FGuiLayout.GuiComponents.FindItem(FGuiLayoutName);

    // in effect this code have been moved...
    if Assigned(FGuiLayout) then
      FGuiLayout.AddGuiComponent(Self);

    NotifyChange(Self);
  end;
end;

procedure TVXBaseComponent.SetGuiLayoutName(NewName: TVXGuiComponentName);

begin
  if FGuiLayoutName <> NewName then
  begin
    FGuiComponent := nil;
    FGuiLayoutName := NewName;
    if FGuiLayoutName <> '' then
      if Assigned(FGuiLayout) then
      begin
        FGuiComponent := FGuiLayout.GuiComponents.FindItem(FGuiLayoutName);
      end;
    NotifyChange(Self);
  end;
end;

procedure TVXBaseComponent.Notification(AComponent: TComponent; Operation:
  TOperation);

begin
  if Operation = opRemove then
  begin
    if AComponent = FGuiLayout then
    begin
      BlockRender;
      GuiLayout := nil;
      UnBlockRender;
    end;
  end;

  inherited;
end;

// SetRotation
//

procedure TVXBaseComponent.SetRotation(const val: Single);
begin
  if FRotation <> val then
  begin
    FRotation := val;
    NotifyChange(Self);
  end;
end;

// SetAlphaChannel
//

procedure TVXBaseComponent.SetAlphaChannel(const val: Single);
begin
  if val <> FAlphaChannel then
  begin
    if val < 0 then
      FAlphaChannel := 0
    else if val > 1 then
      FAlphaChannel := 1
    else
      FAlphaChannel := val;
    NotifyChange(Self);
  end;
end;

procedure TVXBaseComponent.SetAutosize(const Value: Boolean);
var
  MarginLeft, MarginCenter, MarginRight: Single;
  MarginTop, MarginMiddle, MarginBottom: Single;
  MaxWidth: Single;
  MaxHeight: Single;
  i: integer;
begin
  if FAutosize <> Value then
  begin
    FAutosize := Value;

    if FAutosize and Assigned(FGuiComponent) then
    begin
      MarginLeft := 0;
      MarginCenter := 0;
      MarginRight := 0;
      MarginTop := 0;
      MarginMiddle := 0;
      MarginBottom := 0;

      for i := 0 to FGuiComponent.Elements.Count - 1 do
        with FGuiComponent.Elements[i] do
        begin
          case Align of
            GLAlTopLeft, GLAlLeft, GLAlBottomLeft:
              begin
                MarginLeft := Max(MarginLeft, abs(BottomRight.X - TopLeft.X) *
                  Scale.X);
              end;
            GLAlTop, GLAlCenter, GLAlBottom:
              begin
                MarginCenter := Max(MarginCenter, abs(BottomRight.X - TopLeft.X)
                  * Scale.X);
              end;
            GLAlTopRight, GLAlRight, GLAlBottomRight:
              begin
                MarginRight := Max(MarginRight, abs(BottomRight.X - TopLeft.X) *
                  Scale.X);
              end;
          end;
        end;

      for i := 0 to FGuiComponent.Elements.Count - 1 do
        with FGuiComponent.Elements[i] do
        begin
          case Align of
            GLAlTopLeft, GLAlTop, GLAlTopRight:
              begin
                MarginTop := Max(MarginTop, abs(BottomRight.Y - TopLeft.Y) *
                  Scale.Y);
              end;
            GLAlLeft, GLAlCenter, GLAlRight:
              begin
                MarginMiddle := Max(MarginMiddle, abs(BottomRight.Y - TopLeft.Y)
                  * Scale.Y);
              end;
            GLAlBottomLeft, GLAlBottom, GLAlBottomRight:
              begin
                MarginBottom := Max(MarginBottom, abs(BottomRight.Y - TopLeft.Y)
                  * Scale.Y);
              end;
          end;
        end;

      MaxWidth := MarginLeft + MarginCenter + MarginRight;
      MaxHeight := MarginTop + MarginMiddle + MarginBottom;

      if MaxWidth > 0 then
        Width := MaxWidth;

      if MaxHeight > 0 then
        Height := MaxHeight;
    end;
  end;
end;

// StoreAlphaChannel
//

function TVXBaseComponent.StoreAlphaChannel: Boolean;
begin
  Result := (FAlphaChannel <> 1);
end;

// SetNoZWrite
//

procedure TVXBaseComponent.SetNoZWrite(const val: Boolean);
begin
  FNoZWrite := val;
  NotifyChange(Self);
end;

constructor TVXBaseComponent.Create(AOwner: TComponent);

begin
  inherited;
  FGuiLayout := nil;
  FGuiComponent := nil;
  BlockRendering := False;
  BlockedCount := 0;
  RenderingCount := 0;
  Width := 50;
  Height := 50;
  FReBuildGui := True;
  GuiDestroying := False;
  FAlphaChannel := 1;
end;

destructor TVXBaseComponent.Destroy;

begin
  GuiDestroying := True;
  while RenderingCount > 0 do
    Sleep(1);

  GuiLayout := nil;
  inherited;
end;

procedure TVXBaseComponent.NotifyChange(Sender: TObject);

begin
  if Sender = FGuiLayout then
  begin
    if (FGuiLayoutName <> '') and (GuiLayout <> nil) then
    begin
      BlockRender;
      FGuiComponent := GuiLayout.GuiComponents.FindItem(FGuiLayoutName);
      ReBuildGui := True;
      GUIRedraw := True;
      UnBlockRender;
    end
    else
    begin
      BlockRender;
      FGuiComponent := nil;
      ReBuildGui := True;
      GUIRedraw := True;
      UnBlockRender;
    end;
  end;
  if Sender = Self then
  begin
    ReBuildGui := True;
    GUIRedraw := True;
  end;
  inherited;
end;

procedure TVXBaseComponent.MoveGUI(XRel, YRel: Single);

var
  XC: Integer;

begin
  if RedrawAtOnce then
  begin
    BeginUpdate;
    try
      MoveX := MoveX + XRel;
      MoveY := MoveY + YRel;
      for XC := 0 to Count - 1 do
        if Children[XC] is TVXBaseComponent then
        begin
          (Children[XC] as TVXBaseComponent).MoveGUI(XRel, YRel);
        end;
      GUIRedraw := True;
      DoChanges;
    finally
      Endupdate;
    end;
  end
  else
  begin
    MoveX := MoveX + XRel;
    MoveY := MoveY + YRel;
    for XC := 0 to Count - 1 do
      if Children[XC] is TVXBaseComponent then
      begin
        (Children[XC] as TVXBaseComponent).MoveGUI(XRel, YRel);
      end;
    GUIRedraw := True;
  end;
end;

procedure TVXBaseComponent.PlaceGUI(XPos, YPos: Single);
begin
  MoveGUI(XPos - Position.X, YPos - Position.Y);
end;

procedure TVXBaseComponent.DoChanges;

var
  XC: Integer;

begin
  if GUIRedraw then
  begin
    GUIRedraw := False;
    BeginUpdate;
    try
      if MoveX <> 0 then
        Position.X := Position.X + MoveX;
      if MoveY <> 0 then
        Position.Y := Position.Y + MoveY;
      MoveX := 0;
      MoveY := 0;

      for XC := 0 to Count - 1 do
        if Children[XC] is TVXBaseComponent then
        begin
          (Children[XC] as TVXBaseComponent).DoChanges;
        end;
    finally
      EndUpdate;
    end;
  end
  else
  begin
    for XC := 0 to Count - 1 do
      if Children[XC] is TVXBaseComponent then
      begin
        (Children[XC] as TVXBaseComponent).DoChanges;
      end;
  end;
end;

procedure TVXBaseComponent.InternalRender(var rci: TVXRenderContextInfo;
  renderSelf, renderChildren: Boolean);

begin
  if Assigned(FGuiComponent) then
  begin
    try
      FGuiComponent.RenderToArea(0, 0, Width, Height, FRenderStatus,
        FReBuildGui);
    except
      on E: Exception do
        MessageBoxOK(E.Message,
          'Exception in GuiComponents InternalRender function');
    end;
  end;
end;

procedure TVXBaseComponent.DoRender(var rci: TVXRenderContextInfo; renderSelf,
  renderChildren: Boolean);

var
  B: Boolean;
begin
  Inc(RenderingCount);
  B := BlockRendering;
  if B then
  begin
    Inc(BlockedCount);
    while BlockRendering do
      sleep(1);
    Dec(BlockedCount);
  end;

  if not GuiDestroying then
    if RenderSelf then
      if FGuiLayout <> nil then
      begin
        RenderHeader(rci, renderSelf, renderChildren);

        InternalRender(rci, RenderSelf, RenderChildren);

        RenderFooter(rci, renderSelf, renderChildren);
        FReBuildGui := False;
      end;

  if renderChildren then
    if Count > 0 then
      Self.RenderChildren(0, Count - 1, rci);
  Dec(RenderingCount);
end;

procedure TVXBaseControl.InternalMouseDown(Shift: TShiftState; Button:
  TVXMouseButton; X, Y: Integer);

begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TVXBaseControl.InternalMouseUp(Shift: TShiftState; Button:
  TVXMouseButton; X, Y: Integer);

begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TVXBaseControl.InternalMouseMove(Shift: TShiftState; X, Y: Integer);

begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TVXBaseControl.SetActiveControl(NewControl: TVXBaseControl);

begin
  FActiveControl := NewControl;
end;

procedure TVXBaseControl.SetFocusedControl(NewControl: TVXFocusControl);

begin
  if NewControl <> FFocusedControl then
  begin
    if Assigned(FFocusedControl) then
      FFocusedControl.Focused := False;
    FFocusedControl := NewControl;
    if Assigned(FFocusedControl) then
      FFocusedControl.Focused := True;
  end;
end;

function TVXBaseControl.FindFirstGui: TVXBaseControl;

var
  tmpFirst: TVXBaseControl;
  TmpRoot: TVXBaseSceneObject;

begin
  tmpFirst := Self;

  TmpRoot := Self;
  while (TmpRoot is TVXBaseComponent) do
  begin
    if Assigned(TmpRoot.parent) then
    begin
      if TmpRoot.parent is TVXBaseComponent then
      begin
        TmpRoot := TmpRoot.parent as TVXBaseComponent;
        if TmpRoot is TVXBaseControl then
          tmpFirst := TmpRoot as TVXBaseControl;
      end
      else
        Break;
    end
    else
      Break;
  end;
  Result := tmpFirst;
end;

procedure TVXBaseControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if FEnteredControl <> nil then
    begin
      FEnteredControl.DoMouseLeave;
      FEnteredControl := nil;
    end;
  end;

  inherited;
end;

function TVXBaseControl.MouseDown(Sender: TObject; Button: TVXMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
var
  Xc: Integer;
  AcceptMouseEvent: Boolean;

begin
  Result := False;

  AcceptMouseEvent := RecursiveVisible and ((Position.X <= X) and (Position.X +
    Width > X) and (Position.Y <= Y) and (Position.Y + Height > Y));
  if Assigned(OnAcceptMouseQuery) then
    OnAcceptMouseQuery(Self, shift, ma_mousedown, Button, X, Y,
      AcceptMouseEvent);

  if AcceptMouseEvent then
  begin
    Result := True;
    if not FKeepMouseEvents then
    begin
      if Assigned(FActiveControl) then
        if FActiveControl.MouseDown(Sender, Button, Shift, X, Y) then
          Exit;

      for XC := count - 1 downto 0 do
        if FActiveControl <> Children[XC] then
        begin
          if Children[XC] is TVXBaseControl then
          begin
            if (Children[XC] as TVXBaseControl).MouseDown(Sender, button, shift,
              x, y) then
              Exit;
          end;
        end;
    end;
    InternalMouseDown(Shift, Button, X, Y);
  end;
end;

function TVXBaseControl.MouseUp(Sender: TObject; Button: TVXMouseButton; Shift:
  TShiftState; X, Y: Integer): Boolean;
var
  Xc: Integer;
  AcceptMouseEvent: Boolean;

begin
  Result := False;

  AcceptMouseEvent := RecursiveVisible and ((Position.X <= X) and (Position.X +
    Width > X) and (Position.Y <= Y) and (Position.Y + Height > Y));
  if Assigned(OnAcceptMouseQuery) then
    OnAcceptMouseQuery(Self, shift, ma_mouseup, Button, X, Y, AcceptMouseEvent);

  if AcceptMouseEvent then
  begin
    Result := True;
    if not FKeepMouseEvents then
    begin
      if Assigned(FActiveControl) then
        if FActiveControl.MouseUp(Sender, button, shift, x, y) then
          Exit;

      for XC := count - 1 downto 0 do
        if FActiveControl <> Children[XC] then
        begin
          if Children[XC] is TVXBaseControl then
          begin
            if (Children[XC] as TVXBaseControl).MouseUp(Sender, button, shift,
              x, y) then
              Exit;
          end;
        end;
    end;
    InternalMouseUp(Shift, Button, X, Y);
  end;
end;

function TVXBaseControl.MouseMove(Sender: TObject; Shift: TShiftState; X, Y:
  Integer): Boolean;
var
  Xc: Integer;
  AcceptMouseEvent: Boolean;

begin
  Result := False;

  AcceptMouseEvent := RecursiveVisible and ((Position.X <= X) and (Position.X +
    Width > X) and (Position.Y <= Y) and (Position.Y + Height > Y));
  if Assigned(OnAcceptMouseQuery) then
    OnAcceptMouseQuery(Self, shift, ma_mousemove, mbMiddle, X, Y,
      AcceptMouseEvent);

  if AcceptMouseEvent then
  begin
    Result := True;
    if not FKeepMouseEvents then
    begin
      if Assigned(FActiveControl) then
        if FActiveControl.MouseMove(Sender, shift, x, y) then
          Exit;

      for XC := count - 1 downto 0 do
        if FActiveControl <> Children[XC] then
        begin
          if Children[XC] is TVXBaseControl then
          begin
            if (Children[XC] as TVXBaseControl).MouseMove(Sender, shift, x, y)
              then
            begin
              if FEnteredControl <> (Children[XC] as TVXBaseControl) then
              begin
                if FEnteredControl <> nil then
                begin
                  FEnteredControl.DoMouseLeave;
                end;

                FEnteredControl := (Children[XC] as TVXBaseControl);

                if FEnteredControl <> nil then
                begin
                  FEnteredControl.DoMouseEnter;
                end;
              end;

              Exit;
            end;
          end;
        end;
    end;

    if FEnteredControl <> nil then
    begin
      FEnteredControl.DoMouseLeave;
      FEnteredControl := nil;
    end;

    InternalMouseMove(Shift, X, Y);
  end;
end;

procedure TVXBaseControl.KeyDown(Sender: TObject; var Key: Word; Shift:
  TShiftState);
begin
  if Assigned(FFocusedControl) then
  begin
    FFocusedControl.KeyDown(Sender, Key, Shift);
  end;
end;

procedure TVXBaseControl.KeyUp(Sender: TObject; var Key: Word; Shift:
  TShiftState);
begin
  if Assigned(FFocusedControl) then
  begin
    FFocusedControl.KeyUp(Sender, Key, Shift);
  end;
end;

procedure TVXBaseControl.KeyPress(Sender: TObject; var Key: Char);

begin
  if Assigned(FFocusedControl) then
  begin
    FFocusedControl.KeyPress(Sender, Key);
  end;
end;

procedure TVXFocusControl.InternalKeyPress(var Key: Char);
begin
  if assigned(FOnKeyPress) then
 { TODO : E2033 Types of actual and formal var parameters must be identical }
    (*FOnKeyPress(Self, Key);*)
end;

procedure TVXFocusControl.InternalKeyDown(var Key: Word; Shift: TShiftState);
begin
  if assigned(FOnKeyDown) then
 { TODO : E2033 Types of actual and formal var parameters must be identical }
  (*  FOnKeyDown(Self, Key, shift); *)
end;

procedure TVXFocusControl.InternalKeyUp(var Key: Word; Shift: TShiftState);
begin
  if assigned(FOnKeyUp) then
 { TODO : E2033 Types of actual and formal var parameters must be identical }
 (*   FOnKeyUp(Self, Key, shift);*)
end;

procedure TVXBaseControl.DoMouseEnter;
begin
  if Assigned(OnMouseEnter) then
    OnMouseEnter(Self);
end;

procedure TVXBaseControl.DoMouseLeave;
begin
  //leave all child controls
  if FEnteredControl <> nil then
  begin
    FEnteredControl.DoMouseLeave;
    FEnteredControl := nil;
  end;

  if Assigned(OnMouseLeave) then
    OnMouseLeave(Self);
end;

procedure TVXFocusControl.SetFocused(Value: Boolean);
begin
  if Value <> FFocused then
  begin
    FFocused := Value;
    GUIRedraw := True;
  end;
end;

function TVXFocusControl.GetRootControl: TVXBaseControl;

begin
  if not Assigned(FRootControl) then
  begin
    FRootControl := FindFirstGui;
  end;
  Result := FRootControl;
end;

procedure TVXFocusControl.NotifyHide;

begin
  inherited;
  if (RootControl.FFocusedControl = Self) and (self.focused) then
  begin
    RootControl.FocusedControl.PrevControl;
  end;
end;

procedure TVXFocusControl.ReGetRootControl;

begin
  FRootControl := FindFirstGui;
end;

function TVXFocusControl.GetFocusedColor: TColor; //in VCL TDelphiColor;

begin
  Result := ConvertColorVector(FFocusedColor);
end;

procedure TVXFocusControl.SetFocusedColor(const Val: TColor); //in VCL TDelphiColor;

begin
  FFocusedColor := ConvertWinColor(val);
  GUIRedraw := True;
end;

procedure TVXFocusControl.SetFocus;

begin
  RootControl.FocusedControl := Self;
end;

procedure TVXFocusControl.NextControl;

var
  Host: TVXBaseComponent;
  Index: Integer;
  IndexedChild: TVXBaseComponent;
  RestartedLoop: Boolean;

begin
  RestartedLoop := False;
  if Parent is TVXBaseComponent then
  begin
    Host := Parent as TVXBaseComponent;
    Index := Host.IndexOfChild(Self);
    while not Host.RecursiveVisible do
    begin
      if Host.Parent is TVXBaseComponent then
      begin
        IndexedChild := Host;
        Host := Host.Parent as TVXBaseComponent;
        Index := Host.IndexOfChild(IndexedChild);
      end
      else
      begin
        RootControl.FocusedControl := nil;
        Exit;
      end;
    end;

    while true do
    begin
      if Index > 0 then
      begin
        Dec(Index);
        if Host.Children[Index] is TVXFocusControl then
        begin
          with (Host.Children[Index] as TVXFocusControl) do
            if RecursiveVisible then
            begin
              SetFocus;
              Exit;
            end;
        end
        else
        begin
          if Host.Children[Index] is TVXBaseComponent then
          begin
            IndexedChild := Host.Children[Index] as TVXBaseComponent;
            if IndexedChild.RecursiveVisible then
            begin
              Host := IndexedChild;
              Index := Host.Count;
            end;
          end;
        end;
      end
      else
      begin
        if Host.Parent is TVXBaseComponent then
        begin
          Index := Host.Parent.IndexOfChild(Host);
          Host := Host.Parent as TVXBaseComponent;
        end
        else
        begin
          if RestartedLoop then
          begin
            SetFocus;
            Exit;
          end;
          Index := Host.Count;
          RestartedLoop := True;
        end;
      end;
    end;
  end;
end;

procedure TVXFocusControl.PrevControl;

var
  Host: TVXBaseComponent;
  Index: Integer;
  IndexedChild: TVXBaseComponent;
  RestartedLoop: Boolean;

begin
  RestartedLoop := False;
  if Parent is TVXBaseComponent then
  begin
    Host := Parent as TVXBaseComponent;
    Index := Host.IndexOfChild(Self);
    while not Host.RecursiveVisible do
    begin
      if Host.Parent is TVXBaseComponent then
      begin
        IndexedChild := Host;
        Host := Host.Parent as TVXBaseComponent;
        Index := Host.IndexOfChild(IndexedChild);
      end
      else
      begin
        RootControl.FocusedControl := nil;
        Exit;
      end;
    end;

    while true do
    begin
      Inc(Index);

      if Index < Host.Count then
      begin
        if Host.Children[Index] is TVXFocusControl then
        begin
          with (Host.Children[Index] as TVXFocusControl) do
            if RecursiveVisible then
            begin
              SetFocus;
              Exit;
            end;
        end;
        if Host.Children[Index] is TVXBaseComponent then
        begin
          IndexedChild := Host.Children[Index] as TVXBaseComponent;
          if IndexedChild.RecursiveVisible then
          begin
            Host := IndexedChild;
            Index := -1;
          end;
        end;
      end
      else
      begin
        if Host.Parent is TVXBaseComponent then
        begin
          IndexedChild := Host;
          Host := Host.Parent as TVXBaseComponent;
          Index := Host.IndexOfChild(IndexedChild);
        end
        else
        begin
          if RestartedLoop then
          begin
            RootControl.FocusedControl := nil;
            Exit;
          end;
          Index := -1;
          RestartedLoop := True;
        end;
      end;
    end;
  end;
end;

procedure TVXFocusControl.KeyPress(Sender: TObject; var Key: Char);

begin
  InternalKeyPress(Key);
  if Key = #9 then
  begin
    if ssShift in FShiftState then
    begin
      PrevControl;
    end
    else
    begin
      NextControl;
    end;
  end;
end;

procedure TVXFocusControl.KeyDown(Sender: TObject; var Key: Word; Shift:
  TShiftState);
begin
  FShiftState := Shift;
  InternalKeyDown(Key, Shift);
  if Key = glKey_TAB then
  begin
    if ssShift in FShiftState then
    begin
      PrevControl;
    end
    else
    begin
      NextControl;
    end;
  end;
end;

procedure TVXFocusControl.KeyUp(Sender: TObject; var Key: Word; Shift:
  TShiftState);
begin
  FShiftState := Shift;
  InternalKeyUp(Key, Shift);
  if Key = glKey_TAB then
  begin
    if ssShift in FShiftState then
    begin
      PrevControl;
    end
    else
    begin
      NextControl;
    end;
  end;

end;

{ base font control }

constructor TVXBaseFontControl.Create(AOwner: TComponent);

begin
  inherited;
  FBitmapFont := nil;
  FDefaultColor := clrBlack;
end;

destructor TVXBaseFontControl.Destroy;
begin
  inherited;
  BitmapFont := nil;
end;

procedure TVXBaseFontControl.SetBitmapFont(NewFont: TVXCustomBitmapFont);

begin
  if NewFont <> FBitmapFont then
  begin
    if Assigned(FBitmapFont) then
    begin
      FBitmapFont.RemoveFreeNotification(Self);
      FBitmapFont.UnRegisterUser(Self);
    end;
    FBitmapFont := NewFont;
    if Assigned(FBitmapFont) then
    begin
      FBitmapFont.RegisterUser(Self);
      FBitmapFont.FreeNotification(Self);
    end;
    GUIRedraw := True;
  end;
end;

function TVXBaseFontControl.GetBitmapFont: TVXCustomBitmapFont;

begin
  Result := nil;
  if Assigned(FBitmapFont) then
    Result := FBitmapFont
  else if Assigned(GuiLayout) then
    if Assigned(GuiLayout.BitmapFont) then
    begin
      if not (csDesigning in ComponentState) then
      begin
        if not GuiDestroying then
        begin
          BitmapFont := GuiLayout.BitmapFont;
          Result := FBitmapFont;
        end;
      end
      else
        Result := GuiLayout.BitmapFont;
    end;
end;

function TVXBaseFontControl.GetDefaultColor: TColor;

begin
  Result := ConvertColorVector(FDefaultColor);
end;

procedure TVXBaseFontControl.SetDefaultColor(value: TColor);

begin
  FDefaultColor := ConvertWinColor(value);
  GUIRedraw := True;
  NotifyChange(Self);
end;

procedure TVXBaseFontControl.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if (Operation = opRemove) and (AComponent = FBitmapFont) then
  begin
    BlockRender;
    BitmapFont := nil;
    UnBlockRender;
  end;
  inherited;
end;

{ GLWindow }

procedure TVXBaseTextControl.SetCaption(const NewCaption: UnicodeString);

begin
  FCaption := NewCaption;
  GuiRedraw := True;
end;

procedure TVXBaseFontControl.WriteTextAt(var rci: TVXRenderContextInfo; const X,
  Y: Single; const Data: UnicodeString; const Color: TColorVector);
var
  Position: TVector;
begin
  if Assigned(BitmapFont) then
  begin
    Position.X := Round(X);
    Position.Y := Round(Y);
    Position.Z := 0;
    Position.W := 0;
    BitmapFont.RenderString(rci, Data, taLeftJustify, tlTop, Color, @Position);
  end;
end;

procedure TVXBaseFontControl.WriteTextAt(var rci: TVXRenderContextInfo; const X1,
  Y1, X2, Y2: Single; const Data: UnicodeString; const Color: TColorVector);
var
  Position: TVector;
begin
  if Assigned(BitmapFont) then
  begin
    Position.X := Round(((X2 + X1 -
      BitmapFont.CalcStringWidth(Data)) * 0.5));
    Position.Y := Round(-((Y2 + Y1 - GetFontHeight) * 0.5)) + 2;
    Position.Z := 0;
    Position.W := 0;
    BitmapFont.RenderString(rci, Data, taLeftJustify, tlTop, Color, @Position);
  end;
end;

function TVXBaseFontControl.GetFontHeight: Integer;

begin
  if Assigned(BitmapFont) then
    if BitmapFont is TVXWindowsBitmapFont then
      Result := Round(Abs((BitmapFont as TVXWindowsBitmapFont).Font.Size)) //in VCL Height;
    else
      Result := BitmapFont.CharHeight
  else
    Result := -1;
end;

constructor TVXCustomControl.Create(AOwner: TComponent);

begin
  inherited;
  FMaterial := TVXMaterial.create(Self);
  FBitmap := TBitmap.create;
  FBitmap.OnChange := OnBitmapChanged;
  FInternalBitmap := nil;
  FInvalidRenderCount := 0;

  FXTexCoord := 1;
  FYTexCoord := 1;
end;

destructor TVXCustomControl.Destroy;
begin
  if Assigned(FInternalBitmap) then
    FInternalBitmap.Free;
  Bitmap.Free;
  FMaterial.Free;
  inherited;
end;

procedure TVXCustomControl.SetCentered(const Value: Boolean);
begin
  FCentered := Value;
end;

procedure TVXCustomControl.OnBitmapChanged(Sender: TObject);
begin
  FBitmapChanged := True;
end;

procedure TVXCustomControl.SetBitmap(ABitmap: TBitmap);
begin
  FBitmap.Assign(ABitmap);
end;

procedure TVXCustomControl.InternalRender(var rci: TVXRenderContextInfo;
  renderSelf, renderChildren: Boolean);

var
  X1, X2, Y1, Y2: Single;

begin
  if Assigned(OnRender) then
    OnRender(self, FBitmap);

  if FBitmapChanged then
    if FInvalidRenderCount >= FMaxInvalidRenderCount then
    begin
      FInvalidRenderCount := 0;
      if not Assigned(FInternalBitmap) then
        FInternalBitmap := TBitmap.Create;

      { TODO : E2129 Cannot assign to a read-only property }
      (*
      FInternalBitmap.PixelFormat := FBitmap.PixelFormat;
      *)
      FInternalBitmap.Width := RoundUpToPowerOf2(FBitmap.Width);
      FInternalBitmap.Height := RoundUpToPowerOf2(FBitmap.Height);
      { TODO : E2003 Undeclared identifier: 'CopyRect' }
      (*
      FInternalBitmap.Canvas.CopyRect(FBitmap.Canvas.ClipRect, FBitmap.Canvas,
        FBitmap.Canvas.ClipRect);
      *)
      FBitmapChanged := False;
      with Material.GetActualPrimaryTexture do
      begin
        Disabled := False;
        Image.Assign(FInternalBitmap);
      end;
      FXTexCoord := FBitmap.Width / FInternalBitmap.Width;
      FYTexCoord := FBitmap.Height / FInternalBitmap.Height;
    end
    else
      Inc(FInvalidRenderCount);

  if Assigned(FGuiComponent) then
  begin
    try
      if Centered then
        FGuiComponent.RenderToArea(-Width / 2, -Height / 2, Width, Height,
          FRenderStatus, FReBuildGui)
      else
        FGuiComponent.RenderToArea(0, 0, Width, Height, FRenderStatus,
          FReBuildGui);
    except
      on E: Exception do
        MessageBoxOK(E.Message,
          'Exception in TVXCustomControl InternalRender function');
    end;
    X1 := FRenderStatus[GLAlCenter].X1;
    X2 := FRenderStatus[GLAlCenter].X2;
    Y1 := -FRenderStatus[GLAlCenter].Y2;
    Y2 := -FRenderStatus[GLAlCenter].Y1;
  end
  else
  begin
    if Centered then
    begin
      X2 := Width / 2;
      Y1 := -Height / 2;
      X1 := -X2;
      Y2 := -Y1;
    end
    else
    begin
      X2 := Width;
      Y2 := -Height;
      X1 := 0;
      Y1 := 0;
    end;
  end;

  GuiLayout.Material.UnApply(rci);
  Material.Apply(rci);
  glBegin(GL_QUADS);

  glTexCoord2f(FXTexCoord, -FYTexCoord);
  glVertex2f(X2, Y2);

  glTexCoord2f(FXTexCoord, 0);
  glVertex2f(X2, Y1);

  glTexCoord2f(0, 0);
  glVertex2f(X1, Y1);

  glTexCoord2f(0, -FYTexCoord);
  glVertex2f(X1, Y2);

  glEnd();

  Material.UnApply(rci);
  GuiLayout.Material.Apply(rci);
end;

procedure TVXCustomControl.SetMaterial(AMaterial: TVXMaterial);

begin
  FMaterial.Assign(AMaterial);
end;

procedure TVXPopupMenu.SetFocused(Value: Boolean);

begin
  inherited;
  if not (csDesigning in ComponentState) then
    if not FFocused then
      Visible := False;
end;

procedure TVXPopupMenu.SetMenuItems(Value: TStrings);

begin
  FMenuItems.Assign(Value);
  NotifyChange(Self);
end;

procedure TVXPopupMenu.SetMarginSize(const val: Single);

begin
  if FMarginSize <> val then
  begin
    FMarginSize := val;
    NotifyChange(Self);
  end;
end;

procedure TVXPopupMenu.SetSelIndex(const val: Integer);

begin
  if FSelIndex <> val then
  begin
    FSelIndex := val;
    NotifyChange(Self);
  end;
end;

procedure TVXPopupMenu.InternalMouseDown(Shift: TShiftState; Button:
  TVXMouseButton; X, Y: Integer);
var
  ClickIndex: Integer;
  Tx: Single;
  Ty: Single;

begin
  Tx := X - Position.X;
  Ty := Y - Position.Y;
  if Button = mbLeft then
    if IsInRect(fRenderStatus[glAlCenter], Tx, Ty) then
      if Assigned(BitmapFont) then
      begin
        ClickIndex := Round(Int((Ty - fRenderStatus[glAlCenter].y1) /
          BitmapFont.CharHeight));
        if (ClickIndex >= 0) and (ClickIndex < FMenuItems.Count) then
        begin
          if Assigned(OnClick) then
            OnClick(Self, ClickIndex, FMenuItems[ClickIndex]);
          Visible := False;
        end;
      end;
end;

procedure TVXPopupMenu.InternalMouseMove(Shift: TShiftState; X, Y: Integer);
var
  Tx: Single;
  Ty: Single;
begin
  Tx := X - Position.X;
  Ty := Y - Position.Y;
  if IsInRect(fRenderStatus[glAlCenter], Tx, Ty) then
    if Assigned(BitmapFont) then
    begin
      SelIndex := Round(Int((Ty - fRenderStatus[glAlCenter].y1) /
        BitmapFont.CharHeight));
    end;
end;

procedure TVXPopupMenu.OnStringListChange(Sender: TObject);

var
  CenterHeight: Single;
  TextHeight: Single;
begin
  if not FReBuildGui then
  begin
    if Assigned(BitmapFont) then
      with FRenderStatus[GLalCenter] do
      begin
        CenterHeight := Y2 - Y1;
        CenterHeight := Round(CenterHeight + 0.499);
        TextHeight := BitmapFont.CharHeight * FMenuItems.Count;
        if CenterHeight <> TextHeight then // allways round up!
        begin
          Height := Height + TextHeight - CenterHeight;
        end;
      end;
  end;
end;

constructor TVXPopupMenu.Create(AOwner: TComponent);
begin
  inherited;
  FOnClick := nil;
  FMenuItems := TStringList.Create;
  (FMenuItems as TStringList).OnChange := OnStringListChange;
  FSelIndex := 0;
  NewHeight := -1;
end;

destructor TVXPopupMenu.Destroy;
begin
  inherited;
  FMenuItems.Free;
end;

procedure TVXPopupMenu.PopUp(Px, Py: Integer);
begin
  Position.X := PX;
  Position.Y := PY;
  Visible := True;
  SetFocus;
  RootControl.ActiveControl := Self;
end;

procedure TVXPopupMenu.InternalRender(var rci: TVXRenderContextInfo; renderSelf,
  renderChildren: Boolean);

var
  CenterHeight: Single;
  TextHeight: Single;
  YPos: Single;
  XPos: Single;
  XC: Integer;
  changedHeight: single;
begin
  if Assigned(FGuiComponent) then
  begin
    try
      if NewHeight <> -1 then
        FGuiComponent.RenderToArea(0, 0, Width, NewHeight, FRenderStatus,
          FReBuildGui)
      else
        FGuiComponent.RenderToArea(0, 0, Width, Height, FRenderStatus,
          FReBuildGui);
    except
      on E: Exception do
        MessageBoxOK(E.Message,
          'Exception in GuiComponents InternalRender function');
    end;
  end;
  if Assigned(BitmapFont) and (FMenuItems.Count > 0) then
    with FRenderStatus[GLalCenter] do
    begin
      CenterHeight := Y2 - Y1;
      CenterHeight := Round(CenterHeight + 0.499);
      TextHeight := BitmapFont.CharHeight * FMenuItems.Count;
      if CenterHeight <> TextHeight then // allways round up!
      begin
        changedHeight := Height + TextHeight - CenterHeight;
        if changedHeight <> newHeight then
        begin
          newHeight := changedHeight;
          InternalRender(rci, RenderSelf, RenderChildren);
        end;
      end
      else
      begin
        YPos := -Y1;
        XPos := X1 + MarginSize;
        for XC := 0 to FMenuItems.count - 1 do
        begin
          if FSelIndex = XC then
            WriteTextAt(rci, XPos, YPos, FMenuItems[XC], FFocusedColor)
          else
            WriteTextAt(rci, XPos, YPos, FMenuItems[XC], FDefaultColor);
          YPos := YPos - BitmapFont.CharHeight;
        end;
      end;
    end;
end;

procedure TVXPopupMenu.DoRender(var rci: TVXRenderContextInfo; renderSelf,
  renderChildren: Boolean);

begin
  inherited;
  // to avoid gui render-block deadlock!
  if NewHeight <> -1 then
  begin
    Height := NewHeight;
    NewHeight := -1;
  end;
end;

function TVXPopupMenu.MouseDown(Sender: TObject; Button: TVXMouseButton; Shift:
  TShiftState; X, Y: Integer): Boolean;
begin
  Result := inherited MouseDown(Sender, Button, Shift, X, Y);

  if (not Result) and (RootControl.ActiveControl = Self) then
  begin
    RootControl.ActiveControl := nil;
    NextControl;
  end;
end;

procedure TVXForm.InternalMouseDown(Shift: TShiftState; Button: TVXMouseButton;
  X, Y: Integer);

var
  CanMove: Boolean;
  YHere: Single;

begin
  YHere := Y - Position.Y;
  if YHere < FRenderStatus[GLALTop].Y2 then
  begin
    if Button = mbLeft then
    begin
      {      If contains(Width-22,Width-6,XHere) and contains(8,24,YHere) then
            Begin
              Close;
            End else{}
      begin
        CanMove := True;
        if Assigned(FOnCanMove) then
          FOnCanMove(Self, CanMove);
        if CanMove then
        begin
          OldX := X;
          OldY := Y;
          Moving := True;
          if Parent is TVXFocusControl then
            (Parent as TVXFocusControl).ActiveControl := Self;
        end;
      end;
    end;
  end
  else
    inherited;
end;

procedure TVXForm.InternalMouseUp(Shift: TShiftState; Button: TVXMouseButton; X,
  Y: Integer);

begin
  if (Button = mbLeft) and Moving then
  begin
    Moving := False;
    if Parent is TVXFocusControl then
      (Parent as TVXFocusControl).ActiveControl := nil;
    Exit;
  end;

  if Y - Position.Y < 27 then
  begin
  end
  else
    inherited;
end;

procedure TVXForm.InternalMouseMove(Shift: TShiftState; X, Y: Integer);

var
  XRel, YRel: Single;

begin
  if Moving then
  begin
    if (X <> OldX) or (Y <> OldY) then
    begin
      XRel := X - OldX;
      YRel := Y - OldY;

      XRel := XRel + Position.X;
      YRel := YRel + Position.Y;
      if Assigned(OnMoving) then
        OnMoving(Self, XRel, YRel);
      XRel := XRel - Position.X;
      YRel := YRel - Position.Y;

      MoveGUI(XRel, YRel);
      OldX := X;
      OldY := Y;

    end;
  end
  else if Y - Position.Y < 27 then
  begin

  end
  else
    inherited;
end;

function TVXForm.GetTitleColor: TColor;

begin
  Result := ConvertColorVector(FTitleColor);
end;

procedure TVXForm.SetTitleColor(value: TColor);

begin
  FTitleColor := ConvertWinColor(value);
  GUIRedraw := True;
end;

constructor TVXForm.Create(AOwner: TComponent);

begin
  inherited;
  FTitleOffset := 2;
end;

procedure TVXForm.Close;

var
  HowClose: TVXFormCloseOptions;

begin
  HowClose := co_hide;
  if Assigned(FOnCanClose) then
    FOnCanClose(Self, HowClose);
  case HowClose of
    co_hide: Visible := False;
    co_ignore: ;
    co_Destroy: Free;
  end;
end;

procedure TVXForm.NotifyShow;

begin
  inherited;
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

procedure TVXForm.NotifyHide;

begin
  inherited;
  if Assigned(FOnHide) then
    FOnHide(Self);
end;

function TVXForm.MouseUp(Sender: TObject; Button: TVXMouseButton; Shift:
  TShiftState; X, Y: Integer): Boolean;

begin
  if (Button = mbLeft) and (Moving) then
  begin
    Result := True;
    InternalMouseUp(Shift, Button, X, Y);
  end
  else
    Result := inherited MouseUp(Sender, Button, Shift, X, Y);
end;

function TVXForm.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer):
  Boolean;

begin
  if (Moving) then
  begin
    Result := True;
    InternalMouseMove(Shift, X, Y);
  end
  else
    Result := inherited MouseMove(Sender, Shift, X, Y);
end;

procedure TVXForm.InternalRender(var rci: TVXRenderContextInfo; renderSelf,
  renderChildren: Boolean);
var
  ATitleColor: TColorVector;
begin
  if Assigned(FGuiComponent) then
  begin
    FGuiComponent.RenderToArea(0, 0, Width, Height, FRenderStatus, FReBuildGui);

    ATitleColor := FTitleColor;
    ATitleColor.W := AlphaChannel;

    WriteTextAt(rci, ((FRenderStatus[GLAlTop].X2 + FRenderStatus[GLAlTop].X1 -
      BitmapFont.CalcStringWidth(Caption)) * 0.5),
      -((FRenderStatus[GLAlTop].Y2 + FRenderStatus[GLAlTop].Y1 - GetFontHeight) *
      0.5) + TitleOffset, Caption, ATitleColor);
  end;
end;

procedure TVXCheckBox.SetChecked(NewChecked: Boolean);

begin
  if NewChecked <> FChecked then
  begin
    BlockRender;
    try
      if NewChecked then
        if Group >= 0 then
          UnpressGroup(FindFirstGui, Group);

      FChecked := NewChecked;
    finally
      UnBlockRender;
    end;

    NotifyChange(Self);
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TVXCheckBox.InternalMouseDown(Shift: TShiftState; Button:
  TVXMouseButton; X, Y: Integer);
begin
  Checked := not Checked;
  inherited;
end;

procedure TVXCheckBox.InternalMouseUp(Shift: TShiftState; Button:
  TVXMouseButton; X, Y: Integer);

begin
  inherited;
end;

procedure TVXCheckBox.SetGuiLayoutNameChecked(newName: TVXGuiComponentName);

begin
  if FGuiLayoutNameChecked <> NewName then
  begin
    FGuiCheckedComponent := nil;
    FGuiLayoutNameChecked := NewName;
    if Assigned(FGuiLayout) then
    begin
      FGuiCheckedComponent :=
        FGuiLayout.GuiComponents.FindItem(FGuiLayoutNameChecked);
      FReBuildGui := True;
      GUIRedraw := True;
    end;
  end;
end;

procedure TVXCheckBox.SetGuiLayout(NewGui: TVXGuiLayout);

begin
  FGuiCheckedComponent := nil;
  inherited;
  if Assigned(FGuiLayout) then
  begin
    FGuiCheckedComponent :=
      FGuiLayout.GuiComponents.FindItem(FGuiLayoutNameChecked);
    FReBuildGui := True;
    GUIRedraw := True;
  end;
end;

procedure TVXCheckBox.SetGroup(const val: Integer);

begin
  FGroup := val;
  if Checked then
  begin
    BlockRender;
    FChecked := False;
    UnpressGroup(FindFirstGui, val);
    FChecked := true;
    UnBlockRender;
  end;
end;

constructor TVXCheckBox.Create(AOwner: TComponent);

begin
  inherited;
  FChecked := False;
  FGroup := -1;
end;

procedure TVXCheckBox.InternalRender(var rci: TVXRenderContextInfo; renderSelf,
  renderChildren: Boolean);
begin
  if Checked then
  begin
    if Assigned(FGuiCheckedComponent) then
    begin
      FGuiCheckedComponent.RenderToArea(0, 0, Width, Height, FRenderStatus,
        FReBuildGui);
    end;
  end
  else
  begin
    if Assigned(FGuiComponent) then
    begin
      FGuiComponent.RenderToArea(0, 0, Width, Height, FRenderStatus,
        FReBuildGui);
    end;
  end;
end;

procedure TVXCheckBox.NotifyChange(Sender: TObject);

begin
  if Sender = FGuiLayout then
  begin
    if (FGuiLayoutNameChecked <> '') and (GuiLayout <> nil) then
    begin
      BlockRender;
      FGuiCheckedComponent :=
        GuiLayout.GuiComponents.FindItem(FGuiLayoutNameChecked);
      ReBuildGui := True;
      GUIRedraw := True;
      UnBlockRender;
    end
    else
    begin
      BlockRender;
      FGuiCheckedComponent := nil;
      ReBuildGui := True;
      GUIRedraw := True;
      UnBlockRender;
    end;
  end;
  inherited;
end;

procedure TVXButton.SetPressed(NewPressed: Boolean);

begin
  if FPressed <> NewPressed then
  begin
    BlockRender;
    try
      if NewPressed then
        if Group >= 0 then
          UnpressGroup(RootControl, Group);

      FPressed := NewPressed;
    finally
      UnBlockRender;
    end;

    if FPressed then
      if Assigned(FOnButtonClick) then
        FOnButtonClick(Self);

    NotifyChange(Self);
  end;
end;

procedure TVXButton.InternalMouseDown(Shift: TShiftState; Button:
  TVXMouseButton; X, Y: Integer);
begin
  SetFocus;
  inherited;
  if Button = mbLeft then
    if AllowUp then
      Pressed := not Pressed
    else
      Pressed := True;
end;

procedure TVXButton.InternalMouseUp(Shift: TShiftState; Button: TVXMouseButton;
  X, Y: Integer);

begin
  if (Button = mbLeft) and (Group < 0) then
    Pressed := False;
  inherited;
end;

procedure TVXButton.InternalKeyDown(var Key: Word; Shift: TShiftState);

begin
  inherited;
  if Key = glKey_SPACE then
  begin
    Pressed := True;
  end;
  if Key = glKey_RETURN then
  begin
    Pressed := True;
  end;
end;

procedure TVXButton.InternalKeyUp(var Key: Word; Shift: TShiftState);

begin
  if ((Key = glKey_SPACE) or (Key = glKey_RETURN)) and (Group < 0) then
  begin
    Pressed := False;
  end;
  inherited;
end;

procedure TVXButton.SetFocused(Value: Boolean);
begin
  inherited;
  if (not FFocused) and (Group < 0) then
    Pressed := False;
end;

procedure TVXButton.SetGuiLayoutNamePressed(newName: TVXGuiComponentName);

begin
  if FGuiLayoutNamePressed <> NewName then
  begin
    FGuiPressedComponent := nil;
    FGuiLayoutNamePressed := NewName;
    if Assigned(FGuiLayout) then
    begin
      FGuiPressedComponent :=
        FGuiLayout.GuiComponents.FindItem(FGuiLayoutNamePressed);
      FReBuildGui := True;
      GUIRedraw := True;
    end;
  end;
end;

procedure TVXButton.SetGuiLayout(NewGui: TVXGuiLayout);

begin
  FGuiPressedComponent := nil;
  inherited;
  if Assigned(FGuiLayout) then
  begin
    FGuiPressedComponent :=
      FGuiLayout.GuiComponents.FindItem(FGuiLayoutNamePressed);
    FReBuildGui := True;
    GUIRedraw := True;
  end;
end;

procedure TVXButton.SetBitBtn(AValue: TVXMaterial);

begin
  FBitBtn.Assign(AValue);
  NotifyChange(Self);
end;

procedure TVXButton.DestroyHandle;
begin
  inherited;
  FBitBtn.DestroyHandles;
end;

procedure TVXButton.SetGroup(const val: Integer);

begin
  FGroup := val;
  if Pressed then
  begin
    BlockRender;
    FPressed := False;
    UnpressGroup(RootControl, Group);
    FPressed := True;
    UnBlockRender;
  end;
end;

procedure TVXButton.SetLogicWidth(const val: single);

begin
  FLogicWidth := val;
  NotifyChange(Self);
end;

procedure TVXButton.SetLogicHeight(const val: single);

begin
  FLogicHeight := val;
  NotifyChange(Self);
end;

procedure TVXButton.SetXOffset(const val: single);

begin
  FXOffSet := val;
  NotifyChange(Self);
end;

procedure TVXButton.SetYOffset(const val: single);

begin
  FYOffSet := val;
  NotifyChange(Self);
end;

constructor TVXButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBitBtn := TVXMaterial.Create(Self);
  FGroup := -1;
  FPressed := False;
end;

destructor TVXButton.Destroy;
begin
  inherited Destroy;
  FBitBtn.Free;
end;

procedure TVXButton.InternalRender(var rci: TVXRenderContextInfo; renderSelf,
  renderChildren: Boolean);

var
  B: Boolean;
  TexWidth: Integer;
  TexHeight: Integer;
  Material: TVXMaterial;
  LibMaterial: TVXLibMaterial;
  TextColor: TColorVector;

begin
  if Pressed then
  begin
    if Assigned(FGuiPressedComponent) then
    begin
      FGuiPressedComponent.RenderToArea(0, 0, Width, Height, FRenderStatus,
        FReBuildGui);
    end;
  end
  else
  begin
    if Assigned(FGuiComponent) then
    begin
      FGuiComponent.RenderToArea(0, 0, Width, Height, FRenderStatus,
        FReBuildGui);
    end;
  end;

  B := not BitBtn.Texture.Disabled;
  Material := nil;
  if not B then
  begin
    if (BitBtn.MaterialLibrary <> nil) and (BitBtn.MaterialLibrary is
      TVXMaterialLibrary) then
    begin

      LibMaterial :=
        TVXMaterialLibrary(BitBtn.MaterialLibrary).Materials.GetLibMaterialByName(BitBtn.LibMaterialName);
      if LibMaterial <> nil then
      begin
        Material := LibMaterial.Material;
        B := True;
      end;
    end;
  end
  else
  begin
    Material := BitBtn;
  end;

  if B then
    with FRenderStatus[GLAlCenter] do
    begin
      GuiLayout.Material.UnApply(rci);
      BitBtn.Apply(rci);

      TexWidth := Material.Texture.TexWidth;
      if TexWidth = 0 then
        TexWidth := Material.Texture.Image.Width;

      TexHeight := Material.Texture.TexHeight;
      if TexHeight = 0 then
        TexHeight := Material.Texture.Image.Height;

      glBegin(GL_QUADS);

      glTexCoord2f(0, 0);
      glVertex2f(X1 - XOffSet, -Y1 + YOffSet);

      glTexCoord2f(0, -(LogicHeight - 1) / TexHeight);
      glVertex2f(X1 - XOffSet, -Y1 + YOffset - LogicHeight + 1);

      glTexCoord2f((LogicWidth - 1) / TexWidth, -(LogicHeight - 1) /
        TexHeight);
      glVertex2f(X1 - XOffSet + LogicWidth - 1, -Y1 + YOffset - LogicHeight +
        1);

      glTexCoord2f((LogicWidth - 1) / TexWidth, 0);
      glVertex2f(X1 - XOffSet + LogicWidth - 1, -Y1 + YOffSet);

      glEnd();
      BitBtn.UnApply(rci);
      GuiLayout.Material.Apply(rci);
    end;

  if Assigned(BitmapFont) then
  begin

    if FFocused then
    begin
      TextColor := FFocusedColor;
    end
    else
    begin
      TextColor := FDefaultColor;
    end;
    TextColor.W := AlphaChannel;

    WriteTextAt(rci, FRenderStatus[GLALCenter].X1,
      FRenderStatus[GLALCenter].Y1,
      FRenderStatus[GLALCenter].X2,
      FRenderStatus[GLALCenter].Y2,
      Caption,
      TextColor);
  end;
end;

procedure TVXEdit.InternalMouseDown(Shift: TShiftState; Button: TVXMouseButton;
  X, Y: Integer);
begin
  if not FReadOnly then
    SetFocus;
  inherited;
end;

procedure TVXEdit.InternalKeyPress(var Key: Char);
begin
  if FReadOnly then
    exit;
  inherited;
  case Key of
    #8:
      begin
        if FSelStart > 1 then
        begin
          system.Delete(FCaption, FSelStart - 1, 1);
          Dec(FSelStart);
          GUIRedraw := True;
        end;
      end;
  else
    begin
      if Key >= #32 then
      begin
        system.Insert(Key, FCaption, SelStart);
        inc(FSelStart);
        GUIRedraw := True;
      end;
    end;
  end;
end;

procedure TVXEdit.InternalKeyDown(var Key: Word; Shift: TShiftState);
begin
  if FReadOnly then
    exit;
  inherited;
  case Key of
    glKey_DELETE:
      begin
        if FSelStart <= Length(Caption) then
        begin
          System.Delete(FCaption, FSelStart, 1);
          GUIRedraw := True;
        end;
      end;
    glKey_LEFT:
      begin
        if FSelStart > 1 then
        begin
          Dec(FSelStart);
          GUIRedraw := True;
        end;
      end;
    glKey_RIGHT:
      begin
        if FSelStart < Length(Caption) + 1 then
        begin
          Inc(FSelStart);
          GUIRedraw := True;
        end;
      end;
    glKey_HOME:
      begin
        if FSelStart > 1 then
        begin
          FSelStart := 1;
          GUIRedraw := True;
        end;
      end;
    glKey_END:
      begin
        if FSelStart < Length(Caption) + 1 then
        begin
          FSelStart := Length(Caption) + 1;
          GUIRedraw := True;
        end;
      end;
  end;

end;

procedure TVXEdit.InternalKeyUp(var Key: Word; Shift: TShiftState);

begin
  inherited;
end;

procedure TVXEdit.SetFocused(Value: Boolean);

begin
  inherited;
  if Value then
    SelStart := Length(Caption) + 1;
end;

procedure TVXEdit.SetSelStart(const Value: Integer);

begin
  FSelStart := Value;
  GUIRedraw := True;
end;

procedure TVXEdit.SetEditChar(const Value: string);

begin
  FEditChar := Value;
  GUIRedraw := True;
end;

constructor TVXEdit.Create(AOwner: TComponent);

begin
  inherited;
  FEditChar := '*';
end;

procedure TVXEdit.InternalRender(var rci: TVXRenderContextInfo; renderSelf,
  renderChildren: Boolean);
var
  Tekst: UnicodeString;
  pBig: Integer;
begin
  // Renders the background
  if Assigned(FGuiComponent) then
  begin
    FGuiComponent.RenderToArea(0, 0, Width, Height, FRenderStatus, FReBuildGui);
  end;
  // Renders the text
  if Assigned(FBitmapFont) then
  begin
    Tekst := Caption;

    if FFocused then
    begin
      // First put in the edit character where it should be.
      system.insert(FEditChar, Tekst, SelStart);
      // Next figure out if the string is too long.
      if FBitmapFont.CalcStringWidth(Tekst) > Width - 2 then
      begin
        // if it is then we need to check to see where SelStart is
        if SelStart >= Length(Tekst) - 1 then
        begin
          // SelStart is within close proximity of the end of the string
          // Calculate the % of text that we can use and return it against the length of the string.
          pBig := Trunc(Int(((Width - 2) /
            FBitmapFont.CalcStringWidth(Tekst)) * Length(Tekst)));
          dec(pBig);
          Tekst := Copy(Tekst, Length(Tekst) - pBig + 1, pBig);
        end
        else
        begin
          // SelStart is within close proximity of the end of the string
          // Calculate the % of text that we can use and return it against the length of the string.
          pBig := Trunc(Int(((Width - 2) /
            FBitmapFont.CalcStringWidth(Tekst)) * Length(Tekst)));
          dec(pBig);
          if SelStart + pBig < Length(Tekst) then
            Tekst := Copy(Tekst, SelStart, pBig)
          else
            Tekst := Copy(Tekst, Length(Tekst) - pBig + 1, pBig);
        end;
      end;
    end
    else
      { if FFocused then } if FBitmapFont.CalcStringWidth(Tekst) >
      Width - 2 then
      begin
        // The while loop should never execute more then once, but just in case its here.
        while FBitmapFont.CalcStringWidth(Tekst) > Width - 2 do
        begin
          // Calculate the % of text that we can use and return it against the length of the string.
          pBig := Trunc(Int(((Width - 2) /
            FBitmapFont.CalcStringWidth(Tekst)) * Length(Tekst)));
          Tekst := Copy(Tekst, 1, pBig);
        end;
      end;

    if FFocused then
    begin
      WriteTextAt(rci, FRenderStatus[GLAlLeft].X1, FRenderStatus[GLAlCenter].Y1,
        FRenderStatus[GLALCenter].X2, FRenderStatus[GLALCenter].Y2, Tekst,
        FFocusedColor);
    end
    else
    begin
      WriteTextAt(rci, FRenderStatus[GLAlLeft].X1, FRenderStatus[GLAlCenter].Y1,
        FRenderStatus[GLALCenter].X2, FRenderStatus[GLALCenter].Y2, Tekst,
        FDefaultColor);
    end;
  end;
end;

constructor TVXLabel.Create(AOwner: TComponent);
begin
  inherited;
  FTextLayout := tlCenter;
end;

procedure TVXLabel.InternalRender(var rci: TVXRenderContextInfo; renderSelf,
  renderChildren: Boolean);

var
  TekstPos: TVector;
  Tekst: UnicodeString;
  TextColor: TColorVector;
begin
  if Assigned(BitmapFont) then
  begin
    case Alignment of
      taLeftJustify:
        begin
          TekstPos.X := 0;
        end;
      taCenter:
        begin
          TekstPos.X := Width / 2;
        end;
      taRightJustify:
        begin
          TekstPos.X := Width;
        end;
    end;

    case TextLayout of
      tlTop:
        begin
          TekstPos.Y := 0;
        end;
      tlCenter:
        begin
          TekstPos.Y := Round(-Height / 2);
        end;
      tlBottom:
        begin
          TekstPos.Y := -Height;
        end;
    end;

    TekstPos.Z := 0;
    TekstPos.W := 0;

    Tekst := Caption;

    TextColor := FDefaultColor;
    TextColor.W := AlphaChannel;

    BitmapFont.RenderString(rci, Tekst, FAlignment, FTextLayout, TextColor,
      @TekstPos);
  end;
end;

procedure TVXLabel.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    NotifyChange(Self);
  end;
end;

procedure TVXLabel.SetTextLayout(const Value: TVXTextLayout);
begin
  if FTextLayout <> Value then
  begin
    FTextLayout := Value;
    NotifyChange(Self);
  end;
end;

procedure TVXAdvancedLabel.InternalRender(var rci: TVXRenderContextInfo;
  renderSelf, renderChildren: Boolean);

begin
  if Assigned(BitmapFont) then
  begin
    if Focused then
    begin
      WriteTextAt(rci, 8, -((Height - GetFontHeight) / 2) + 1, Caption,
        FFocusedColor);
    end
    else
    begin
      WriteTextAt(rci, 8, -((Height - GetFontHeight) / 2) + 1, Caption,
        FDefaultColor);
    end;
  end;
end;

procedure TVXScrollbar.SetMin(const val: Single);
begin
  if FMin <> val then
  begin
    FMin := val;
    if FPos < FMin then
      Pos := FMin;
    NotifyChange(Self);
  end;
end;

procedure TVXScrollbar.SetMax(const val: Single);
begin
  if FMax <> val then
  begin
    FMax := val;
    if FMax < FMin then
      FMax := FMin;
    if FPos > (FMax - FPageSize + 1) then
      Pos := (FMax - FPageSize + 1);
    NotifyChange(Self);
  end;
end;

procedure TVXScrollbar.SetPos(const val: Single);
begin
  if FPos <> val then
  begin
    FPos := val;
    if FPos < FMin then
      FPos := FMin;
    if FPos > (FMax - FPageSize + 1) then
      FPos := (FMax - FPageSize + 1);

    NotifyChange(Self);
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TVXScrollbar.SetPageSize(const val: Single);

begin
  if FPageSize <> val then
  begin
    FPageSize := val;
    if FPos > (FMax - FPageSize + 1) then
      Pos := (FMax - FPageSize + 1);
    NotifyChange(Self);
  end;
end;

procedure TVXScrollbar.SetHorizontal(const val: Boolean);

begin
  if FHorizontal <> val then
  begin
    FHorizontal := val;
    NotifyChange(Self);
  end;
end;

procedure TVXScrollbar.SetGuiLayoutKnobName(newName: TVXGuiComponentName);

begin
  if newName <> FGuiLayoutKnobName then
  begin
    FGuiKnobComponent := nil;
    FGuiLayoutKnobName := NewName;
    if Assigned(FGuiLayout) then
    begin
      FGuiKnobComponent :=
        FGuiLayout.GuiComponents.FindItem(FGuiLayoutKnobName);
      FReBuildGui := True;
      GUIRedraw := True;
    end;
  end;
end;

procedure TVXScrollbar.SetGuiLayout(NewGui: TVXGuiLayout);

begin
  FGuiKnobComponent := nil;
  inherited;
  if Assigned(FGuiLayout) then
  begin
    FGuiKnobComponent := FGuiLayout.GuiComponents.FindItem(FGuiLayoutKnobName);
    FReBuildGui := True;
    GUIRedraw := True;
  end;
end;

function TVXScrollbar.GetScrollPosY(ScrollPos: Single): Single;
begin
  with FRenderStatus[GLAlCenter] do
  begin
    Result := (ScrollPos - FMin) / (FMax - FMin) * (Y2 - Y1) + Y1;
  end;
end;

function TVXScrollbar.GetYScrollPos(Y: Single): Single;
begin
  with FRenderStatus[GLAlCenter] do
  begin
    Result := (Y - Y1) / (Y2 - Y1) * (FMax - FMin) + FMin;
  end;
end;

function TVXScrollbar.GetScrollPosX(ScrollPos: Single): Single;
begin
  with FRenderStatus[GLAlCenter] do
  begin
    Result := (ScrollPos - FMin) / (FMax - FMin) * (X2 - X1) + X1;
  end;
end;

function TVXScrollbar.GetXScrollPos(X: Single): Single;
begin
  with FRenderStatus[GLAlCenter] do
  begin
    Result := (X - X1) / (X2 - X1) * (FMax - FMin) + FMin;
  end;
end;

procedure TVXScrollbar.InternalMouseDown(Shift: TShiftState; Button:
  TVXMouseButton; X, Y: Integer);

var
  Tx, Ty: Single;

begin
  if (Button = mbLeft)
    and not FLocked then
  begin
    Tx := x - Position.X;
    Ty := y - Position.Y;
    // is in mid area ?
    if IsInRect(FRenderStatus[GLAlCenter], Tx, Ty) then
    begin
      if FHorizontal then
      begin
        Tx := GetxScrollPos(Tx);
        if Tx < FPos then
          PageUp
        else if Tx > FPos + FPageSize - 1 then
          PageDown
        else
        begin
          fScrolling := True;
          FScrollOffs := Tx - FPos;
          RootControl.ActiveControl := Self;
        end;
      end
      else
      begin
        Ty := GetYScrollPos(Ty);
        if Ty < FPos then
          PageUp
        else if Ty > FPos + FPageSize - 1 then
          PageDown
        else
        begin
          fScrolling := True;
          FScrollOffs := Ty - FPos;
          RootControl.ActiveControl := Self;
        end;
      end;
    end
    else
    begin
      // if not, is at end buttons ?
      if horizontal then
      begin
        if IsInRect(FRenderStatus[GLAlLeft], Tx, Ty) then
          StepUp;
        if IsInRect(FRenderStatus[GLAlRight], Tx, Ty) then
          StepDown;
      end
      else
      begin
        if IsInRect(FRenderStatus[GLAlTop], Tx, Ty) then
          StepUp;
        if IsInRect(FRenderStatus[GLAlBottom], Tx, Ty) then
          StepDown;
      end;
    end;
  end;
  inherited;
end;

procedure TVXScrollbar.InternalMouseUp(Shift: TShiftState; Button:
  TVXMouseButton; X, Y: Integer);
begin
  if fScrolling then
  begin
    fScrolling := False;
    RootControl.ActiveControl := nil;
  end;

  inherited;
end;

procedure TVXScrollbar.InternalMouseMove(Shift: TShiftState; X, Y: Integer);

var
  Tx: Single;
  Ty: Single;
begin
  if fScrolling then
    if FHorizontal then
    begin
      Tx := GetXScrollPos(x - Position.X) - FScrollOffs;
      Pos := Round(Tx);
    end
    else
    begin
      Ty := GetYScrollPos(y - Position.Y) - FScrollOffs;
      Pos := Round(Ty);
    end;

  inherited;
end;

constructor TVXScrollbar.Create(AOwner: TComponent);

begin
  inherited;
  FGuiKnobComponent := nil;
  FMin := 1;
  FMax := 10;
  FPos := 1;
  FStep := 1;
  FPageSize := 3;
  FOnChange := nil;
  FGuiLayoutKnobName := '';
  FScrollOffs := 0;
  FScrolling := False;
  FHorizontal := False;
end;

procedure TVXScrollbar.StepUp;

begin
  Pos := Pos - FStep;
end;

procedure TVXScrollbar.StepDown;
begin
  Pos := Pos + FStep;
end;

procedure TVXScrollbar.PageUp;
begin
  Pos := Pos - FPageSize;
end;

procedure TVXScrollbar.PageDown;
begin
  Pos := Pos + FPageSize;
end;

function TVXScrollbar.MouseUp(Sender: TObject; Button: TVXMouseButton; Shift:
  TShiftState; X, Y: Integer): Boolean;

begin
  if (Button = mbLeft) and (FScrolling) then
  begin
    Result := True;
    InternalMouseUp(Shift, Button, X, Y);
  end
  else
    Result := inherited MouseUp(Sender, Button, Shift, X, Y);
end;

function TVXScrollbar.MouseMove(Sender: TObject; Shift: TShiftState; X, Y:
  Integer): Boolean;

begin
  if (FScrolling) then
  begin
    Result := True;
    InternalMouseMove(Shift, X, Y);
  end
  else
    Result := inherited MouseMove(Sender, Shift, X, Y);
end;

procedure TVXScrollbar.InternalRender(var rci: TVXRenderContextInfo; renderSelf,
  renderChildren: Boolean);

var
  Start, Size: Integer;
begin
  if Assigned(FGuiComponent) then
  begin
    try
      FGuiComponent.RenderToArea(0, 0, Width, Height, FRenderStatus,
        FReBuildGui);
    except
      on E: Exception do
        MessageBoxOK(E.Message,
          'Exception in GuiComponents InternalRender function');
    end;
  end;
  if Assigned(FGuiKnobComponent) then
  begin
    try
      with FRenderStatus[GLAlCenter] do
      begin
        if FHorizontal then
        begin
          Start := Round(GetScrollPosX(FPos));
          if FPageSize + FPos > FMax + 1 then
            Size := Round(GetScrollPosX(FMax) - X1)
          else
            Size := Round(GetScrollPosX(FPageSize) - X1);

          FGuiKnobComponent.RenderToArea(Start, Y1, Start + Size, Y2,
            FKnobRenderStatus, True);
          //           Tag := start;
          //           tagfloat := size;
        end
        else
        begin
          Start := Round(GetScrollPosY(FPos));
          if FPageSize + FPos > FMax + 1 then
            Size := Round(GetScrollPosY(FMax) - Y1)
          else
            Size := Round(GetScrollPosY(FPageSize) - Y1);
          FGuiKnobComponent.RenderToArea(X1, Start, X2, Start + Size,
            FKnobRenderStatus, True);
          //           Tag := start;
          //           tagfloat := size;
        end;
      end;
    except
      on E: Exception do
        MessageBoxOK(E.Message,
          'Exception in GuiComponents InternalRender function');
    end;
  end;
end;

function StringGrid.GetCell(X, Y: Integer; out oCol, oRow: Integer): Boolean;

var
  ClientRect: TRectangle;
  XPos: Integer;
  YPos: Integer;
  XC, YC: Integer;

begin
  Result := False;
  if Assigned(BitmapFont) then
  begin
    if Assigned(FGuiComponent) then
    begin
      ClientRect.Left := Round(FRenderStatus[GLAlCenter].X1);
      ClientRect.Top := Round(FRenderStatus[GLAlCenter].Y1);
      ClientRect.Width := Round(FRenderStatus[GLAlCenter].X2);
      ClientRect.Height := Round(FRenderStatus[GLAlCenter].Y2);
    end
    else
    begin
      ClientRect.Left := 0;
      ClientRect.Top := 0;
      ClientRect.Width := Round(Width);
      ClientRect.Height := Round(Height);
    end;

    YPos := ClientRect.Top;
    if FDrawHeader then
      YPos := YPos + RowHeight;
    XPos := ClientRect.Left;

    if y < YPos then
      Exit;
    if x < XPos then
      Exit;

    XPos := XPos + MarginSize;

    for XC := 0 to Columns.Count - 1 do
    begin
      XPos := XPos + Integer(Columns.Objects[XC]);

      if x > XPos then
        continue;

      for YC := 0 to RowCount - 1 do
      begin
        YPos := YPos + RowHeight;
        if y < YPos then
        begin
          Result := True;
          if Assigned(Scrollbar) then
            oRow := YC + Round(Scrollbar.Pos) - 1
          else
            oRow := YC;

          oCol := XC;
          Exit;
        end;
      end;
    end;
  end;
end;

procedure StringGrid.InternalMouseDown(Shift: TShiftState; Button:
  TVXMouseButton; X, Y: Integer);

var
  tRow, tCol: Integer;
begin
  SetFocus;
  if GetCell(Round(X - Position.X), Round(Y - Position.Y), tCol, tRow) then
  begin
    SelCol := tCol;
    SelRow := tRow;
  end;
  inherited;
end;

procedure StringGrid.SetColumns(const val: TStrings);
var
  XC: Integer;
begin
  FColumns.Assign(val);
  for XC := 0 to Columns.Count - 1 do
    Columns.Objects[XC] := TObject(ColumnSize);
end;

procedure StringGrid.SetColSelect(const val: Boolean);
begin
  FColSelect := Val;
  NotifyChange(Self);
end;

function StringGrid.GetRow(index: Integer): TStringList;

begin
  if (index >= 0) and (index < FRows.Count) then
    Result := TStringList(FRows[index])
  else
    Result := nil;
end;

procedure StringGrid.SetRow(index: Integer; const val: TStringList);

begin
  if (index >= 0) then
  begin
    if (index >= RowCount) then
      RowCount := index + 1;

    TStringList(FRows[index]).Assign(val);
  end;
end;

function StringGrid.GetRowCount: Integer;

begin
  Result := FRows.count;
end;

procedure StringGrid.SetRowCount(const val: Integer);

var
  XC: Integer;

begin
  XC := FRows.count;
  if val <> XC then
  begin
    if val > XC then
    begin
      FRows.count := val;
      for XC := XC to val - 1 do
      begin
        FRows[XC] := TStringList.Create;
        TStringList(FRows[XC]).OnChange := OnStringListChange;
      end;
    end
    else
    begin
      for XC := XC - 1 downto val do
      begin
        TStringList(FRows[XC]).Free;
      end;
      FRows.count := val;
    end;
    if Assigned(Scrollbar) then
      Scrollbar.FMax := FRows.Count;
    NotifyChange(Self);
  end;
end;

procedure StringGrid.SetSelCol(const val: Integer);
begin
  if FSelCol <> Val then
  begin
    FSelCol := Val;
    NotifyChange(Self);
  end;
end;

procedure StringGrid.SetSelRow(const val: Integer);
begin
  if FSelRow <> Val then
  begin
    FSelRow := Val;
    NotifyChange(Self);
  end;
end;

procedure StringGrid.SetRowSelect(const val: Boolean);
begin
  FRowSelect := Val;
  NotifyChange(Self);
end;

procedure StringGrid.SetDrawHeader(const val: Boolean);

begin
  FDrawHeader := Val;
  NotifyChange(Self);
end;

function StringGrid.GetHeaderColor: TColor;

begin
  Result := ConvertColorVector(FHeaderColor);
end;

procedure StringGrid.SetHeaderColor(const val: TColor);

begin
  FHeaderColor := ConvertWinColor(val);
  GUIRedraw := True;
end;

procedure StringGrid.SetMarginSize(const val: Integer);

begin
  if FMarginSize <> val then
  begin
    FMarginSize := val;
    GUIRedraw := True;
  end;
end;

procedure StringGrid.SetColumnSize(const val: Integer);

var
  XC: Integer;

begin
  if FColumnSize <> val then
  begin
    FColumnSize := val;
    for XC := 0 to Columns.Count - 1 do
      Columns.Objects[XC] := TObject(ColumnSize);
    GUIRedraw := True;
  end;
end;

procedure StringGrid.SetRowHeight(const val: Integer);

begin
  if FRowHeight <> val then
  begin
    FRowHeight := val;
    GUIRedraw := True;
  end;
end;

procedure StringGrid.SetScrollbar(const val: TVXScrollbar);

begin
  if FScrollbar <> Val then
  begin
    if Assigned(FScrollbar) then
      FScrollbar.RemoveFreeNotification(Self);
    FScrollbar := Val;
    if Assigned(FScrollbar) then
      FScrollbar.FreeNotification(Self);
  end;
end;

procedure StringGrid.SetGuiLayout(NewGui: TVXGuiLayout);

begin
  inherited;
  if Assigned(Scrollbar) then
    if Scrollbar.GuiLayout <> nil then
      Scrollbar.GuiLayout := NewGui;
end;

constructor StringGrid.Create(AOwner: TComponent);

begin
  inherited;
  FRows := TList.Create;
  FColumns := TStringList.Create;
  TStringList(FColumns).OnChange := OnStringListChange;
  FSelCol := 0;
  FSelRow := 0;
  FRowSelect := True;
  FScrollbar := nil;
  FDrawHeader := True;
end;

destructor StringGrid.Destroy;

begin
  Scrollbar := nil;
  inherited;
  Clear;
  FRows.Free;
  FColumns.Free;
end;

procedure StringGrid.Clear;

begin
  RowCount := 0;
end;

procedure StringGrid.Notification(AComponent: TComponent; Operation:
  TOperation);

begin
  if (AComponent = FScrollbar) and (Operation = opRemove) then
  begin
    FScrollbar := nil;
  end;
  inherited;
end;

procedure StringGrid.NotifyChange(Sender: TObject);

begin
  if Sender = Scrollbar then
  begin
    ReBuildGui := True;
    GUIRedraw := True;
  end;
  inherited;
end;

procedure StringGrid.InternalRender(var rci: TVXRenderContextInfo; renderSelf,
  renderChildren: Boolean);

  function CellSelected(X, Y: Integer): Boolean;
  begin
    if (RowSelect and ColSelect) then
      Result := (Y = SelRow) or (x = SelCol)
    else if RowSelect then
      Result := Y = SelRow
    else if ColSelect then
      Result := X = SelCol
    else
      Result := (Y = SelRow) and (x = SelCol);
  end;

  function CellText(X, Y: Integer): string;

  begin
    with Row[y] do
      if (X >= 0) and (X < Count) then
        Result := strings[x]
      else
        Result := '';
  end;

var
  ClientRect: TRectangle;
  XPos: Integer;
  YPos: Integer;
  XC, YC: Integer;
  From, Till: Integer;

begin
  if Assigned(FGuiComponent) then
  begin
    try
      FGuiComponent.RenderToArea(0, 0, Width, Height, FRenderStatus,
        FReBuildGui);
      ClientRect.Left := Round(FRenderStatus[GLAlCenter].X1);
      ClientRect.Top := Round(FRenderStatus[GLAlCenter].Y1);
      ClientRect.Width := Round(FRenderStatus[GLAlCenter].X2);
      ClientRect.Height := Round(FRenderStatus[GLAlCenter].Y2);
    except
      on E: Exception do
        MessageBoxOK(E.Message,
          'Exception in GuiComponents InternalRender function');
    end;
  end
  else
  begin
    ClientRect.Left := 0;
    ClientRect.Top := 0;
    ClientRect.Width := Round(Width);
    ClientRect.Height := Round(Height);
  end;

  if Assigned(BitmapFont) then
  begin
    XPos := ClientRect.Left + MarginSize;

    if Assigned(Scrollbar) then
    begin
      Scrollbar.Position.X := Position.X + FRenderStatus[GLAlCenter].X2 -
        Scrollbar.Width;
      Scrollbar.Position.Y := Position.Y + FRenderStatus[GLAlCenter].Y1;
      Scrollbar.Height := FRenderStatus[GLAlCenter].Y2 -
        FRenderStatus[GLAlCenter].Y1;
      XC := (ClientRect.Height - ClientRect.Top);
      if FDrawHeader then
        YC := (XC div RowHeight) - 1
      else
        YC := (XC div RowHeight);

      Scrollbar.PageSize := YC;
      From := Round(Scrollbar.pos - 1);
      Till := Round(Scrollbar.pageSize + From - 1);
      if Till > RowCount - 1 then
        Till := RowCount - 1;
    end
    else
    begin
      From := 0;
      Till := RowCount - 1;
    end;

    for XC := 0 to Columns.Count - 1 do
    begin
      YPos := -ClientRect.Top;
      if FDrawHeader then
      begin
        WriteTextAt(rci, XPos, YPos, Columns[XC], FHeaderColor);
        YPos := YPos - RowHeight;
      end;
      for YC := From to Till do
      begin
        if CellSelected(XC, YC) then
          WriteTextAt(rci, XPos, YPos, CellText(XC, YC), FFocusedColor)
        else
          WriteTextAt(rci, XPos, YPos, CellText(XC, YC), FDefaultColor);
        YPos := YPos - RowHeight;
      end;
      XPos := XPos + Integer(Columns.Objects[XC]);
    end;
  end;
end;

procedure StringGrid.OnStringListChange(Sender: TObject);

begin
  NotifyChange(Self);
end;

function StringGrid.Add(Data: array of string): Integer;
var
  XC: Integer;
begin
  Result := RowCount;
  RowCount := RowCount + 1;
  for XC := 0 to Length(Data) - 1 do
    Row[Result].Add(Data[XC]);
end;

function StringGrid.Add(const Data: string): Integer;
begin
  Result := Add([Data]);
  if Assigned(Scrollbar) then
  begin
    if Result > Round(Scrollbar.pageSize + Scrollbar.pos - 2) then
      Scrollbar.pos := Result - Scrollbar.pageSize + 2;
  end;
end;

procedure StringGrid.SetText(Data: string);

var
  Posi: Integer;
begin
  Clear;
  while Data <> '' do
  begin
    Posi := Pos(#13#10, Data);
    if Posi > 0 then
    begin
      Add(Copy(Data, 1, Posi - 1));
      Delete(Data, 1, Posi + 1);
    end
    else
    begin
      Add(Data);
      Data := '';
    end;
  end;
end;

destructor TVXFocusControl.Destroy;
begin
  if Focused then
    RootControl.FocusedControl := nil;
  inherited;
end;

procedure TVXBaseComponent.DoProgress(const progressTime: TVXProgressTimes);
begin
  inherited;
  if FDoChangesOnProgress then
    DoChanges;

end;

procedure TVXBaseComponent.SetDoChangesOnProgress(const Value: Boolean);
begin
  FDoChangesOnProgress := Value;
end;

procedure TVXFocusControl.MoveTo(newParent: TVXBaseSceneObject);
begin
  inherited;
  ReGetRootControl;
end;

initialization
  RegisterClasses([TVXBaseControl, TVXPopupMenu, TVXForm, TVXPanel, TVXButton,
    TVXCheckBox, TVXEdit, TVXLabel, TVXAdvancedLabel, TVXScrollbar, StringGrid,
    TVXCustomControl]);
end.

