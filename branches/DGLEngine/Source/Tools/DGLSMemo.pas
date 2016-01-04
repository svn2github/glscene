//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ : DGLSMemo<p>

  <b>History : </b><font size=-1><ul>
  <li>28/12/15 - JD - Imported from GLScene
  </ul></font>
}

unit DGLSMemo;

interface

{$I DGLEngine.inc}

uses
  WinApi.Windows, WinApi.Messages,
  System.SysUtils, System.Classes, System.UITypes,
  VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.ClipBrd,
  VCL.StdCtrls, VCL.ExtCtrls;

type
  TBorderType = (btRaised, btLowered, btFlatRaised, btFlatLowered);
  TCommand    = Integer;

  TCellSize = record
    W, H: Integer;
  end;

  TCellPos = record
    X, Y: Integer;
  end;

  TFullPos = record
    LineNo, Pos: Integer;
  end;

  TLineProp = class
    FObject: TObject;
    FStyleNo: Integer;
    FInComment: Boolean;
    FInBrackets: Integer;
    FValidAttrs: Boolean;
    FCharAttrs: string;
  end;

  // ****************************************************************************************
  // TCharStyle
  //
  TCharStyle = class(TPersistent)
  private
    { Private Declarations }
    FTextColor, FBkColor: TColor;
    FStyle:               TFontStyles;
  published
    { Published Declarations }
    property TextColor: TColor read FTextColor write FTextColor;
    property BkColor:   TColor read FBkColor write FBkColor;
    property Style:     TFontStyles read FStyle write FStyle;
  end;

  // ****************************************************************************************
  // TStyleList
  //
  TStyleList = class(TList)
  private
    { Private Declarations }
    procedure CheckRange(Index: Integer);
    function GetTextColor(Index: Integer): TColor;
    procedure SetTextColor(Index: Integer; Value: TColor);
    function GetBkColor(Index: Integer): TColor;
    procedure SetBkColor(Index: Integer; Value: TColor);
    function GetStyle(Index: Integer): TFontStyles;
    procedure SetStyle(Index: Integer; Value: TFontStyles);
  protected
    { Protected Declarations }
    property TextColor[Index: Integer]: TColor read GetTextColor write SetTextColor;
    property BkColor[Index: Integer]:   TColor read GetBkColor write SetBkColor;
    property Style[Index: Integer]:     TFontStyles read GetStyle write SetStyle;
  public
    { Public Declarations }
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    function Add(ATextColor, ABkCOlor: TColor; AStyle: TFontStyles): Integer;
    procedure Change(Index: Integer; ATextColor, ABkCOlor: TColor; AStyle: TFontStyles);
  end;

  // ****************************************************************************************
  // TDGLAbstractMemoObject
  //
  TDGLAbstractMemoObject = class(TObject)
  public
    { Public Declarations }
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual; abstract;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual; abstract;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; virtual; abstract;
  end;

  // ****************************************************************************************
  // TDGLSMemoAbstractScrollableObject
  //
  TDGLSMemoScrollBar = class;

  TDGLSMemoAbstractScrollableObject = class(TCustomControl)
  protected
    { Protected Declarations }
    procedure DoScroll(Sender: TDGLSMemoScrollBar; ByValue: Integer); virtual; abstract;
    procedure DoScrollPage(Sender: TDGLSMemoScrollBar; ByValue: Integer); virtual; abstract;
  end;

  // ****************************************************************************************
  // TDGLSMemoScrollBar
  //
  TDGLSCustomMemo = class;

  TsbState = (sbsWait, sbsBack, sbsForward, sbsPageBack, sbsPageForward, sbsDragging);

  TDGLSMemoScrollBar = class(TDGLAbstractMemoObject)
  private
    { Private Declarations }
    FKind:                           TScrollBarKind;
    FParent:                         TDGLSMemoAbstractScrollableObject;
    FLeft, FTop, FWidth, FHeight:    Integer;
    FTotal, FMaxPosition, FPosition: Integer;
    FButtonLength:                   Integer;
    FState:                          TsbState;
    FXOffset, FYOffset:              Integer;
    procedure SetParams(Index: Integer; Value: Integer);
    procedure SetState(Value: TsbState);
    function GetRect: TRect;
    function GetThumbRect: TRect;
    function GetBackRect: TRect;
    function GetMiddleRect: TRect;
    function GetForwardRect: TRect;
    function GetPgBackRect: TRect;
    function GetPgForwardRect: TRect;
  public
    { Public Declarations }
    constructor Create(AParent: TDGLSMemoAbstractScrollableObject; AKind: TScrollBarKind);
    procedure PaintTo(ACanvas: TCanvas);

    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;

    function MoveThumbTo(X, Y: Integer): Integer;

    property Parent: TDGLSMemoAbstractScrollableObject read FParent;
    property Kind: TScrollBarKind read FKind write FKind;
    property State: TsbState read FState write SetState;
    property Left: Integer index 0 read FLeft write SetParams;
    property Top: Integer index 1 read FTop write SetParams;
    property Width: Integer index 2 read FWidth write SetParams;
    property Height: Integer index 3 read FHeight write SetParams;
    property Total: Integer index 4 read FTotal write SetParams;
    property MaxPosition: Integer index 5 read FMaxPosition write SetParams;
    property Position: Integer index 6 read FPosition write SetParams;
    property FullRect: TRect read GetRect;
    property ThumbRect: TRect read GetThumbRect;
    property BackRect: TRect read GetBackRect;
    property MiddleRect: TRect read GetMiddleRect;
    property ForwardRect: TRect read GetForwardRect;
    property PageForwardRect: TRect read GetPgForwardRect;
    property PageBackRect: TRect read GetPgBackRect;
  end;

  // ****************************************************************************************
  // TDGLSMemoStrings
  //
  TDGLSMemoStrings = class(TStringList)
  private
    { Private Declarations }
    FMemo:      TDGLSCustomMemo;
    FLockCount: Integer;
    FDeleting:  Boolean;
    procedure CheckRange(Index: Integer);
    function GetLineProp(Index: Integer): TLineProp;

    procedure SetLineStyle(Index: Integer; Value: Integer);
    function GetLineStyle(Index: Integer): Integer;
    function GetInComment(Index: Integer): Boolean;
    procedure SetInComment(Index: Integer; Value: Boolean);
    function GetInBrackets(Index: Integer): Integer;
    procedure SetInBrackets(Index: Integer; Value: Integer);
    function GetValidAttrs(Index: Integer): Boolean;
    procedure SetValidAttrs(Index: Integer; Value: Boolean);
    function GetCharAttrs(Index: Integer): string;
    procedure SetCharAttrs(Index: Integer; Value: string);
  protected
    { Protected Declarations }
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CreateProp(Index: Integer): TLineProp;
    property LineProp[Index: Integer]: TLineProp read GetLineProp;
    property Style[Index: Integer]: Integer read GetLineStyle write SetLineStyle;
    property InComment[Index: Integer]: Boolean read GetInComment write SetInComment;
    property InBrackets[Index: Integer]: Integer read GetInBrackets write SetInBrackets;
    property ValidAttrs[Index: Integer]: Boolean read GetValidAttrs write SetValidAttrs;
    property CharAttrs[Index: Integer]: string read GetCharAttrs write SetCharAttrs;
  public
    { Public Declarations }
    destructor Destroy; override;
    procedure Clear; override;
    function DoAdd(const S: string): Integer;
    function Add(const S: string): Integer; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    procedure Assign(Source: TPersistent); override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure DoInsert(Index: Integer; const S: string);
    procedure InsertObject(Index: Integer; const S: string; AObject: TObject); override;
    procedure Delete(Index: Integer); override;
    procedure LoadFromFile(const FileName: string); override;
  end;

  // ****************************************************************************************
  // TDGLSMemoGutter
  //
  TDGLSMemoGutter = class(TObject)
  private
    { Private Declarations }
    FMemo:                        TDGLSCustomMemo;
    FLeft, FTop, FWidth, FHeight: Integer;
    FColor:                       TColor;
    procedure SetParams(Index: Integer; Value: Integer);
    function GetRect: TRect;
  protected
    { Protected Declarations }
    procedure PaintTo(ACanvas: TCanvas);
    procedure Invalidate;
  public
    { Public Declarations }
    property Left:     Integer index 0 read FLeft write SetParams;
    property Top:      Integer index 1 read FTop write SetParams;
    property Width:    Integer index 2 read FWidth write SetParams;
    property Height:   Integer index 3 read FHeight write SetParams;
    property FullRect: TRect read GetRect;
  end;

  // ****************************************************************************************
  // TDGLSMemoUndo
  //
  TDGLSMemoUndo = class
  private
    { Private Declarations }
    FMemo:                  TDGLSCustomMemo;
    FUndoCurX0, FUndoCurY0: Integer;
    FUndoCurX, FUndoCurY:   Integer;
    FUndoText:              string;
  public
    { Public Declarations }
    constructor Create(ACurX0, ACurY0, ACurX, ACurY: Integer; AText: string);
    function Append(NewUndo: TDGLSMemoUndo): Boolean; virtual;
    procedure Undo;
    procedure Redo;
    procedure PerformUndo; virtual; abstract;
    procedure PerformRedo; virtual; abstract;
    property UndoCurX0: Integer read FUndoCurX0 write FUndoCurX0;
    property UndoCurY0: Integer read FUndoCurY0 write FUndoCurY0;
    property UndoCurX: Integer read FUndoCurX write FUndoCurX;
    property UndoCurY: Integer read FUndoCurY write FUndoCurY;
  end;

  // ****************************************************************************************
  TDGLSMemoInsCharUndo = class(TDGLSMemoUndo)
  public
    { Public Declarations }
    function Append(NewUndo: TDGLSMemoUndo): Boolean; override;
    procedure PerformUndo; override;
    procedure PerformRedo; override;
  end;

  // ****************************************************************************************
  TDGLSMemoDelCharUndo = class(TDGLSMemoUndo)
  private
    { Private Declarations }
    FIsBackspace: Boolean;
  public
    { Public Declarations }
    function Append(NewUndo: TDGLSMemoUndo): Boolean; override;
    procedure PerformUndo; override;
    procedure PerformRedo; override;
    property IsBackspace: Boolean read FIsBackspace write FIsBackspace;
  end;

  // ****************************************************************************************
  TDGLSMEmoDelLineUndo = class(TDGLSMemoUndo)
  private
    { Private Declarations }
    FIndex: Integer;
  public
    { Public Declarations }
    constructor Create(AIndex, ACurX0, ACurY0, ACurX, ACurY: Integer; AText: string);
    procedure PerformUndo; override;
    procedure PerformRedo; override;
  end;

  // ****************************************************************************************
  TDGLSMemoSelUndo = class(TDGLSMemoUndo)
  private
    { Private Declarations }
    FUndoSelStartX, FUndoSelStartY, FUndoSelEndX, FUndoSelEndY: Integer;
  public
    { Public Declarations }
    property UndoSelStartX: Integer read FUndoSelStartX write FUndoSelStartX;
    property UndoSelStartY: Integer read FUndoSelStartY write FUndoSelStartY;
    property UndoSelEndX:   Integer read FUndoSelEndX write FUndoSelEndX;
    property UndoSelEndY:   Integer read FUndoSelEndY write FUndoSelEndY;
  end;

  // ****************************************************************************************
  TDGLSMemoDeleteBufUndo = class(TDGLSMemoSelUndo)
  public
    { Public Declarations }
    procedure PerformUndo; override;
    procedure PerformRedo; override;
  end;

  // ****************************************************************************************
  TDGLSMemoPasteUndo = class(TDGLSMemoUndo)
  public
    { Public Declarations }
    procedure PerformUndo; override;
    procedure PerformRedo; override;
  end;

  // ****************************************************************************************
  TDGLSMemoUndoList = class(TList)
  private
    { Private Declarations }
    FPos:          Integer;
    FMemo:         TDGLSCustomMemo;
    FIsPerforming: Boolean;
    FLimit:        Integer;
  protected
    { Protected Declarations }
    function Get(Index: Integer): TDGLSMemoUndo;
    procedure SetLimit(Value: Integer);
  public
    { Public Declarations }
    constructor Create;
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    procedure Undo;
    procedure Redo;
    property Items[Index: Integer]: TDGLSMemoUndo read Get; default;
    property IsPerforming: Boolean read FIsPerforming write FIsPerforming;
    property Memo: TDGLSCustomMemo read FMemo write FMemo;
    property Pos: Integer read FPos write FPos;
    property Limit: Integer read FLimit write SetLimit;
  end;

  // --------------------------------------------------------------

  TGutterClickEvent  = procedure(Sender: TObject; LineNo: Integer) of object;
  TGutterDrawEvent   = procedure(Sender: TObject; ACanvas: TCanvas; LineNo: Integer; rct: TRect) of object;
  TGetLineAttrsEvent = procedure(Sender: TObject; LineNo: Integer; var Attrs: string) of object;
  TUndoChangeEvent   = procedure(Sender: TObject; CanUndo, CanRedo: Boolean) of object;
  TScrollMode        = (smAuto, smStrict);

  // ****************************************************************************************
  // TDGLSCustomMemo
  //
  TDGLSCustomMemo = class(TDGLSMemoAbstractScrollableObject)
  private
    { Private Declarations }
    FAutoIndent:                                                      Boolean;
    FMargin:                                                          Integer;
    FHiddenCaret, FCaretVisible:                                      Boolean;
    FCellSize:                                                        TCellSize;
    FCurX, FCurY:                                                     Integer;
    FLeftCol, FTopLine:                                               Integer;
    FTabSize:                                                         Integer;
    FFont:                                                            TFont;
    FBkColor:                                                         TColor;
    FSelColor:                                                        TColor;
    FSelBkColor:                                                      TColor;
    FReadOnly:                                                        Boolean;
    FDelErase:                                                        Boolean;
    FLines:                                                           TStrings;
    FSelStartX, FSelStartY, FSelEndX, FSelEndY, FPrevSelX, FPrevSelY: Integer;
    FScrollBars:                                                      System.UITypes.TScrollStyle;
    FScrollBarWidth:                                                  Integer;
    FGutter:                                                          TDGLSMemoGutter;
    FGutterWidth:                                                     Integer;
    sbVert, sbHorz:                                                   TDGLSMemoScrollBar;
    FStyles:                                                          TStyleList;
    FLineBitmap:                                                      TBitmap;
    FSelCharPos:                                                      TFullPos;
    FSelCharStyle:                                                    Integer;
    FLeftButtonDown:                                                  Boolean;
    FScrollMode:                                                      TScrollMode;
    FUndoList:                                                        TDGLSMemoUndoList;
    FFirstUndoList:                                                   TDGLSMemoUndoList;
    FUndoLimit:                                                       Integer;
    FLastMouseUpX, FLastMouseUpY:                                     Integer;
    FAfterDoubleClick:                                                Boolean;
    { events }
    FOnMoveCursor:      TNotifyEvent;
    FOnChange:          TNotifyEvent;
    FOnAttrChange:      TNotifyEvent;
    FOnStatusChange:    TNotifyEvent;
    FOnSelectionChange: TNotifyEvent;
    FOnGutterDraw:      TGutterDrawEvent;
    FOnGutterClick:     TGutterClickEvent;
    FOnGetLineAttrs:    TGetLineAttrsEvent;
    FOnUndoChange:      TUndoChangeEvent;
    FHideCursor:        Boolean;

    procedure SetHiddenCaret(Value: Boolean);

    procedure SetScrollBars(Value: System.UITypes.TScrollStyle);

    procedure SetGutterWidth(Value: Integer);
    procedure SetGutterColor(Value: TColor);
    function GetGutterColor: TColor;

    procedure SetCurX(Value: Integer);
    procedure SetCurY(Value: Integer);
    procedure SetFont(Value: TFont);
    procedure SetColor(Index: Integer; Value: TColor);
    function GetSelStart: TPoint;
    function GetSelEnd: TPoint;

    procedure SetLines(ALines: TStrings);
    procedure SetLineStyle(Index: Integer; Value: Integer);
    function GetLineStyle(Index: Integer): Integer;
    function GetInComment(Index: Integer): Boolean;
    procedure SetInComment(Index: Integer; Value: Boolean);
    function GetInBrackets(Index: Integer): Integer;
    procedure SetInBrackets(Index: Integer; Value: Integer);
    function GetValidAttrs(Index: Integer): Boolean;
    procedure SetValidAttrs(Index: Integer; Value: Boolean);
    function GetCharAttrs(Index: Integer): string;
    procedure SetCharAttrs(Index: Integer; Value: string);

    procedure ExpandSelection;
    function GetSelText: string;
    procedure SetSelText(const AValue: string);

    function GetSelLength: Integer;

    procedure MovePage(dP: Integer; Shift: TShiftState);
    procedure ShowCaret(State: Boolean);
    procedure MakeVisible;
    function GetVisible(Index: Integer): Integer;
    function MaxLength: Integer;

    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMEraseBkgnd(var Msg: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMSetFocus); message WM_KILLFOCUS;
    procedure WMMouseWheel(var Message: TMessage); message WM_MOUSEWHEEL;

    procedure MoveCursor(dX, dY: Integer; Shift: TShiftState);

    procedure ResizeEditor;
    procedure ResizeScrollBars;
    procedure ResizeGutter;
    procedure DoCommand(cmd: TCommand; const AShift: TShiftState);

    procedure DrawLine(LineNo: Integer);
    function IsLineVisible(LineNo: Integer): Boolean;
    procedure FreshLineBitmap;

    procedure SetUndoLimit(Value: Integer);

  protected
    { Protected Declarations }
    procedure WndProc(var Message: TMessage); override;

    function EditorRect: TRect;

    function LineRangeRect(FromLine, ToLine: Integer): TRect;
    function ColRangeRect(FromCol, ToCol: Integer): TRect;
    procedure InvalidateLineRange(FromLine, ToLine: Integer);

    function AddString(S: string): Integer;
    procedure InsertString(Index: Integer; S: string);

    procedure GoHome(Shift: TShiftState);
    procedure GoEnd(Shift: TShiftState);
    procedure InsertChar(C: Char);
    procedure DeleteChar(OldX, OldY: Integer);
    procedure DeleteLine(Index, OldX, OldY, NewX, NewY: Integer; FixUndo: Boolean);
    procedure BackSpace;
    procedure BackSpaceWord;

    function IndentCurrLine: string;
    procedure NewLine;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;

    procedure DrawMargin;
    procedure DrawGutter;
    procedure DrawScrollBars;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;

    procedure DoScroll(Sender: TDGLSMemoScrollBar; ByValue: Integer); override;
    procedure DoScrollPage(Sender: TDGLSMemoScrollBar; ByValue: Integer); override;

    property VisiblePosCount: Integer index 0 read GetVisible;
    property VisibleLineCount: Integer index 1 read GetVisible;
    property LastVisiblePos: Integer index 2 read GetVisible;
    property LastVisibleLine: Integer index 3 read GetVisible;

    procedure DeleteSelection(bRepaint: Boolean);

    procedure Changed(FromLine, ToLine: Integer); virtual;
    procedure AttrChanged(LineNo: Integer); virtual;
    procedure SelectionChanged; virtual;
    procedure StatusChanged; virtual;

    procedure ClearUndoList;
    procedure UndoChange;

    property AutoIndent: Boolean read FAutoIndent write FAutoIndent;
    property GutterWidth: Integer read FGutterWidth write SetGutterWidth;
    property GutterColor: TColor read GetGutterColor write SetGutterColor;
    property ScrollBars: System.UITypes.TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property Font: TFont read FFont write SetFont;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Lines: TStrings read FLines write SetLines;
    property BkColor: TColor index 0 read FBkColor write SetColor;
    property SelColor: TColor index 1 read FSelColor write SetColor;
    property SelBkColor: TColor index 2 read FSelBkColor write SetColor;
    property HiddenCaret: Boolean read FHiddenCaret write SetHiddenCaret;
    property TabSize: Integer read FTabSize write FTabSize;
    property ScrollMode: TScrollMode read FScrollMode write FScrollMode default smAuto;
    property UndoLimit: Integer read FUndoLimit write SetUndoLimit;
    property HideCursor: Boolean read FHideCursor write FHideCursor;

    property InComment[Index: Integer]: Boolean read GetInComment write SetInComment;
    property InBrackets[Index: Integer]: Integer read GetInBrackets write SetInBrackets;
    property ValidAttrs[Index: Integer]: Boolean read GetValidAttrs write SetValidAttrs;
    property CharAttrs[Index: Integer]: string read GetCharAttrs write SetCharAttrs;

    { events }
    property OnGutterClick: TGutterClickEvent read FOnGutterClick write FOnGutterClick;
    property OnGutterDraw: TGutterDrawEvent read FOnGutterDraw write FOnGutterDraw;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMoveCursor: TNotifyEvent read FOnMoveCursor write FOnMoveCursor;
    property OnAttrChange: TNotifyEvent read FOnAttrChange write FOnAttrChange;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnStatusChange: TNotifyEvent read FOnStatusChange write FOnStatusChange;
    property OnGetLineAttrs: TGetLineAttrsEvent read FOnGetLineAttrs write FOnGetLineAttrs;
    property OnUndoChange: TUndoChangeEvent read FOnUndoChange write FOnUndoChange;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyToClipBoard;
    procedure PasteFromClipBoard;
    procedure CutToClipBoard;
    procedure SelectLines(StartLine, EndLine: Integer);
    procedure SelectAll;
    property SelStart: TPoint read GetSelStart;
    property SelEnd: TPoint read GetSelEnd;
    property Selection: string read GetSelText write SetSelText;
    property SelLength: Integer read GetSelLength;
    procedure ClearSelection;
    procedure Clear;
    procedure SetCursor(ACurX, ACurY: Integer);
    function SelectLine(LineNo, StyleNo: Integer): Integer;
    procedure SelectChar(LineNo, Pos, StyleNo: Integer);
    function CellFromPos(X, Y: Integer): TCellPos;
    function CharFromPos(X, Y: Integer): TFullPos;
    function CellRect(ACol, ARow: Integer): TRect;
    function LineRect(ARow: Integer): TRect;
    function ColRect(ACol: Integer): TRect;
    function CharStyleNo(LineNo, Pos: Integer): Integer;
    procedure InsertTemplate(AText: string);
    procedure UnSelectChar;
    procedure Undo;
    procedure Redo;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    function FindText(Text: string; Options: TFindOptions; Select: Boolean): Boolean;
    property CurX: Integer read FCurX write SetCurX;
    property CurY: Integer read FCurY write SetCurY;
    property DelErase: Boolean read FDelErase write FDelErase;
    property LineStyle[Index: Integer]: Integer read GetLineStyle write SetLineStyle;
    property Styles: TStyleList read FStyles;
    property UndoList: TDGLSMemoUndoList read FUndoList write FUndoList;
  end;

  // ****************************************************************************************
  // TDGLSMemo
  //
  TDGLSMemo = class(TDGLSCustomMemo)
  published
    { Published Declarations }
    { : TControl }
    property PopupMenu;
    { : TCustomControl }
    property Align;
    property Enabled;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property ReadOnly;
    { : TDGLSCustomMemo }
    property AutoIndent;
    property GutterColor;
    property GutterWidth;
    property ScrollBars;
    property Font;
    property BkColor;
    property Selection;
    property SelColor;
    property SelBkColor;
    property Lines;
    property HiddenCaret;
    property TabSize;
    property ScrollMode;
    property UndoLimit;
    property DelErase;
    { : Inherited events }
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    { : Events }
    property OnGutterDraw;
    property OnGutterClick;
    property OnChange;
    property OnMoveCursor;
    property OnAttrChange;
    property OnSelectionChange;
    property OnStatusChange;
    property OnGetLineAttrs;
    property OnUndoChange;
  end;

  // ****************************************************************************************
  // TDGLSMemoStringList
  //
  TDGLSMemoStringList = class(TStringList)
  private
    { Private Declarations }
    procedure ReadStrings(Reader: TReader);
    procedure WriteStrings(Writer: TWriter);
  protected
    { Protected Declarations }
    procedure DefineProperties(Filer: TFiler); override;
  end;

  TDelimiters = TSysCharSet;
  TTokenType  = (ttWord, ttBracket, ttSpecial, ttDelimiter, ttSpace, ttEOL, ttInteger, ttFloat, ttComment, ttOther, ttWrongNumber);

  // ****************************************************************************************
  // --------------------------------------------------------------
  // SYNTAX MEMO - declaration
  // --------------------------------------------------------------
  TDGLSynHiMemo = class(TDGLSCustomMemo)
  private
    { Private declarations }
    FIsPainting: Boolean;
    FInComment:  Boolean;

    FWordList:                                          TDGLSMemoStringList;
    FSpecialList:                                       TDGLSMemoStringList;
    FBracketList:                                       TDGLSMemoStringList;
    FDelimiters:                                        TDelimiters;
    FInBrackets:                                        Integer;
    FLineComment:                                       string;
    FMultiCommentLeft:                                  string;
    FMultiCommentRight:                                 string;
    FDelimiterStyle:                                    TCharStyle;
    FCommentStyle:                                      TCharStyle;
    FNumberStyle:                                       TCharStyle;
    FDelimiterStyleNo, FCommentStyleNo, FNumberStyleNo: Integer;
    FCaseSensitive:                                     Boolean;
    function GetToken(S: string; var From: Integer; var TokenType: TTokenType; var StyleNo: Integer): string;
    procedure SetWordList(Value: TDGLSMemoStringList);
    procedure SetSpecialList(Value: TDGLSMemoStringList);
    procedure SetBracketList(Value: TDGLSMemoStringList);
    procedure FindLineAttrs(Sender: TObject; LineNo: Integer; var Attrs: string);
    procedure SetStyle(Index: Integer; Value: TCharStyle);
    procedure SetCaseSensitive(Value: Boolean);
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddWord(StyleNo: Integer; ArrS: array of string);
    procedure AddSpecial(StyleNo: Integer; ArrS: array of string);
    procedure AddBrackets(StyleNo: Integer; ArrS: array of string);

    property Delimiters: TDelimiters read FDelimiters write FDelimiters;
  published
    { Published declarations }

    { : TControl }
    property PopupMenu;
    { : TCustomControl }
    property Align;
    property Enabled;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property ReadOnly;
    { : TDGLSCustomMemo }
    property AutoIndent;
    property GutterColor;
    property GutterWidth;
    property ScrollBars;
    property Font;
    property BkColor;
    property SelColor;
    property SelBkColor;
    property Lines;
    property HiddenCaret;
    property TabSize;
    property ScrollMode;
    property UndoLimit;
    property DelErase;
    { : Inherited events }
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    { : Events }
    property OnGutterClick;
    property OnGutterDraw;
    property OnChange;
    property OnMoveCursor;
    property OnSelectionChange;
    property OnStatusChange;
    property OnUndoChange;
    { : TGLSSyntaxMemo }
    property LineComment:       string read FLineComment write FLineComment;
    property MultiCommentLeft:  string read FMultiCommentLeft write FMultiCommentLeft;
    property MultiCommentRight: string read FMultiCommentRight write FMultiCommentRight;

    property WordList:    TDGLSMemoStringList read FWordList write SetWordList;
    property SpecialList: TDGLSMemoStringList read FSpecialList write SetSpecialList;
    property BracketList: TDGLSMemoStringList read FBracketList write SetBracketList;

    property DelimiterStyle: TCharStyle index 0 read FDelimiterStyle write SetStyle;
    property CommentStyle:   TCharStyle index 1 read FCommentStyle write SetStyle;
    property NumberStyle:    TCharStyle index 2 read FNumberStyle write SetStyle;
    property CaseSensitive:  Boolean read FCaseSensitive write SetCaseSensitive;
  end;

// ****************************************************************************************

procedure Border(Canvas: TCanvas; rct: TRect; BorderType: TBorderType);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
  cmDelete        = VK_DELETE;
  cmBackSpace     = VK_BACK;
  cmWordBackSpace = 127; // Ctrl-BackSpace
  cmNewLine       = VK_RETURN;
  cmHome          = VK_HOME;
  cmEnd           = VK_END;
  cmPageUp        = VK_PRIOR;
  cmPageDown      = VK_NEXT;
  cmInsert        = VK_INSERT;
  cmDelLine       = 25; // Ctrl-Y
  cmCopy          = 3; // Ctrl-C
  cmCut           = 24; // Ctrl-X
  cmPaste         = 22; // Ctrl-V

resourcestring
  SObjectsNotSupported = 'Linked object not supported';

var
  bmScrollBarFill:  TBitmap;
  bmScrollBarUp:    TBitmap;
  bmScrollBarDown:  TBitmap;
  bmScrollBarLeft:  TBitmap;
  bmScrollBarRight: TBitmap;

  fIntelliWheelSupport: Boolean; // True if IntelliMouse + wheel enabled
  fIntelliMessage:      UINT; // message sent from mouse on wheel roll
  fIntelliScrollLines:  Integer; // number of lines to scroll per wheel roll

  // ------------------
  { Helper functions }
  {$IFDEF GLS_REGION}{$REGION 'Helper functions'}{$ENDIF}

function PointInRect(P: TPoint; rct: TRect): Boolean;
{$IFDEF GLS_INLINE}inline; {$ENDIF}
begin
  with rct do
    Result := (Left <= P.X) and (Top <= P.Y) and (Right >= P.X) and (Bottom >= P.Y);
end;

procedure Swap(var I1, I2: Integer);
{$IFDEF GLS_INLINE}inline; {$ENDIF}
var
  temp: Integer;
begin
  temp := I1;
  I1   := I2;
  I2   := temp;
end;

procedure OrderPos(var StartX, StartY, EndX, EndY: Integer);
{$IFDEF GLS_INLINE}inline; {$ENDIF}
begin
  if (EndY < StartY) or ((EndY = StartY) and (EndX < StartX)) then
  begin
    Swap(StartX, EndX);
    Swap(StartY, EndY);
  end;
end;

function TotalRect(rct1, rct2: TRect): TRect;
{$IFDEF GLS_INLINE}inline; {$ENDIF}
begin
  Result := rct1;
  with Result do
  begin
    if rct2.Left < Left then
      Left := rct2.Left;
    if rct2.Top < Top then
      Top := rct2.Top;
    if rct2.Right > Right then
      Right := rct2.Right;
    if rct2.Bottom > Bottom then
      Bottom := rct2.Bottom;
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
// ------------------
{ TDGLSCustomMemo functions }
{$IFDEF GLS_REGION}{$REGION 'TDGLSCustomMemo functions'}{$ENDIF}

procedure TDGLSCustomMemo.WndProc(var Message: TMessage);
  function GetShiftState: Integer;
  begin
    Result := 0;
    if GetAsyncKeyState(vk_Shift) < 0 then
      Result := Result or mk_Shift;
    if GetAsyncKeyState(vk_Control) < 0 then
      Result := Result or mk_Control;
    if GetAsyncKeyState(vk_LButton) < 0 then
      Result := Result or mk_LButton;
    if GetAsyncKeyState(vk_RButton) < 0 then
      Result := Result or mk_RButton;
    if GetAsyncKeyState(vk_MButton) < 0 then
      Result := Result or mk_MButton;
  end;

// ---------------------------------------------------
begin
  if (Message.Msg = fIntelliMessage) and (fIntelliMessage <> WM_MOUSEWHEEL) then
  begin
    PostMessage(Handle, WM_MOUSEWHEEL, MakeLong(GetShiftState, Message.wParam), Message.lParam);
  end
  else
    inherited;
end;

procedure IntelliMouseInit;
var
  hWndMouse:         hWnd;
  mQueryScrollLines: UINT;
  // --------------------------------------------
  function NativeMouseWheelSupport: Boolean;
  var
    ver: TOSVersionInfo;
  begin
    Result                  := False;
    ver.dwOSVersionInfoSize := sizeof(ver);
    // For Windows 98, assume dwMajorVersion = 5 (It's 4 for W95)
    // For NT, we need 4.0 or better.
    if GetVersionEx(ver) then
      case ver.dwPlatformID of
        ver_Platform_Win32_Windows:
          Result := ver.dwMajorVersion >= 5;
        ver_Platform_Win32_NT:
          Result := ver.dwMajorVersion >= 4;
      end;
    { Quick and dirty temporary hack for Windows 98 beta 3 }
    if (Result = False) and (ver.szCSDVersion = ' Beta 3') then
      Result := True;
  end;

// --------------------------------------------
begin
  if NativeMouseWheelSupport then
  begin
    fIntelliWheelSupport := Boolean(GetSystemMetrics(sm_MouseWheelPresent));
    SystemParametersInfo(spi_GetWheelScrollLines, 0, @fIntelliScrollLines, 0);
    fIntelliMessage := WM_MOUSEWHEEL;
  end
  else
  begin
    { Look for hidden mouse window }
    hWndMouse := FindWindow('MouseZ', 'Magellan MSWHEEL');
    if hWndMouse <> 0 then
    begin
      { We're in business - get the scroll line info }
      fIntelliWheelSupport := True;
      mQueryScrollLines    := RegisterWindowMessage('MSH_SCROLL_LINES_MSG');
      fIntelliScrollLines  := SendMessage(hWndMouse, mQueryScrollLines, 0, 0);
      { Finally, get the custom mouse message as well }
      fIntelliMessage := RegisterWindowMessage('MSWHEEL_ROLLMSG');
    end;
  end;
  if (fIntelliScrollLines < 0) or (fIntelliScrollLines > 100) then
    fIntelliScrollLines := 3;
end;

procedure TDGLSCustomMemo.WMMouseWheel(var Message: TMessage);
{$J+}
{$IFOPT R+} {$DEFINE StoreRangeCheck} {$ENDIF} {$R-}
const
  Delta: SmallInt = 0;
begin
  Delta := Delta + SmallInt(HiWord(Message.wParam));
  while Abs(Delta) >= 120 do
  begin
    if Delta < 0 then
    begin
      DoScroll(sbVert, fIntelliScrollLines);
      Delta := Delta + 120;
    end
    else
    begin
      DoScroll(sbVert, -fIntelliScrollLines);
      Delta := Delta - 120;
    end;
  end;
end;
{$J-}
{$IFDEF StoreRangeCheck} {$R+} {$UNDEF StoreRangeCheck} {$ENDIF}

procedure TDGLSCustomMemo.SetCursor(ACurX, ACurY: Integer);
begin
  ClearSelection;
  CurX := 0;
  CurY := ACurY;
  CurX := ACurX;
end;

function TDGLSCustomMemo.SelectLine(LineNo, StyleNo: Integer): Integer;
var
  rct: TRect;
begin
  Result            := LineStyle[LineNo];
  LineStyle[LineNo] := StyleNo;
  rct               := LineRect(LineNo);
  InvalidateRect(Handle, @rct, True);
end;

procedure TDGLSCustomMemo.SelectLines(StartLine, EndLine: Integer);
var
  rct: TRect;
begin
  FSelStartX := 0;
  FSelStartY := StartLine;
  FSelEndX   := Length(Lines[EndLine]);
  FSelEndY   := EndLine;
  rct        := LineRangeRect(FSelStartY, FSelEndY);
  SelectionChanged;
  InvalidateRect(Handle, @rct, True);
end;

procedure TDGLSCustomMemo.SelectChar(LineNo, Pos, StyleNo: Integer);
var
  rct: TRect;
begin
  UnSelectChar;
  FSelCharPos.LineNo := LineNo;
  FSelCharPos.Pos    := Pos;
  FSelCharStyle      := StyleNo;
  rct                := LineRect(LineNo);
  InvalidateRect(Handle, @rct, True);
end;

procedure TDGLSCustomMemo.UnSelectChar;
var
  rct: TRect;
begin
  with FSelCharPos do
  begin
    if LineNo < 0 then
      Exit;
    rct    := LineRect(LineNo);
    LineNo := -1;
    Pos    := -1;
  end;
  FSelCharStyle := -1;
  InvalidateRect(Handle, @rct, True);
end;

procedure TDGLSCustomMemo.Clear;
begin
  CurY     := 0;
  CurX     := 0;
  FLeftCol := 0;
  FTopLine := 0;
  Lines.Clear;
  TDGLSMemoStrings(Lines).DoAdd('');
  ClearUndoList;
  Invalidate;
end;

procedure TDGLSCustomMemo.SelectAll;
begin
  FSelStartY := 0;
  FSelStartX := 0;
  FSelEndY   := Lines.Count - 1;
  FSelEndX   := Length(Lines[Lines.Count - 1]);
  Invalidate;
end;

procedure SetClipboardCodePage(const CodePage: longint);
var
  Data:    THandle;
  DataPtr: Pointer;
begin
  // Define new code page for clipboard
  Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, 4);
  try
    DataPtr := GlobalLock(Data);
    try
      Move(CodePage, DataPtr^, 4);
      SetClipboardData(CF_LOCALE, Data);
    finally
      GlobalUnlock(Data);
    end;
  except
    GlobalFree(Data);
  end;
end;

procedure CopyStringToClipboard(const Value: string);
const
  RusLocale = (SUBLANG_DEFAULT shl $A) or LANG_RUSSIAN;
begin
  Clipboard.Open;
  SetClipboardCodePage(RusLocale);
  try
    Clipboard.AsText := Value;
  finally
    SetClipboardCodePage(RusLocale);
    Clipboard.Close;
  end;
end;

procedure TDGLSCustomMemo.CopyToClipBoard;
begin
  CopyStringToClipboard(GetSelText);
end;

procedure TDGLSCustomMemo.PasteFromClipBoard;
var
  H, len: Integer;
  Buff:   string;
begin
  H   := Clipboard.GetAsHandle(CF_TEXT);
  len := GlobalSize(H);
  if len = 0 then
    Exit;

  SetLength(Buff, len);
  SetLength(Buff, Clipboard.GetTextBuf(PChar(Buff), len));
  AdjustLineBreaks(Buff);

  SetSelText(Buff);
end;

procedure TDGLSCustomMemo.DeleteSelection(bRepaint: Boolean);
var
  xSelStartX, xSelStartY, xSelEndX, xSelEndY: Integer;
  i, len:                                     Integer;
  OldX, OldY:                                 Integer;
  S1, S2, S, AddSpaces:                       string;
  Undo:                                       TDGLSMemoDeleteBufUndo;
begin
  if (FSelStartY = FSelEndY) and (FSelStartX = FSelEndX) then
    Exit;

  OldX       := CurX;
  OldY       := CurY;
  xSelStartX := FSelStartX;
  xSelStartY := FSelStartY;
  xSelEndX   := FSelEndX;
  xSelEndY   := FSelEndY;
  OrderPos(xSelStartX, xSelStartY, xSelEndX, xSelEndY);

  if xSelStartY = xSelEndY then
  begin
    S1        := Copy(Lines[xSelStartY], xSelStartX + 1, xSelEndX - xSelStartX);
    S2        := '';
    AddSpaces := '';
  end
  else
  begin
    len       := Length(Lines[xSelStartY]);
    S1        := Copy(Lines[xSelStartY], xSelStartX + 1, len);
    AddSpaces := StringOfChar(' ', xSelStartX - len);
    S2        := Copy(Lines[xSelEndY], 1, xSelEndX);
  end;
  Lines[xSelStartY] := Copy(Lines[xSelStartY], 1, xSelStartX) + AddSpaces + Copy(Lines[xSelEndY], xSelEndX + 1, Length(Lines[xSelEndY]));
  S                 := S1;
  for i             := xSelStartY + 1 to xSelEndY do
  begin
    S := S + #13#10;
    if i <> xSelEndY then
      S := S + Lines[xSelStartY + 1];
    DeleteLine(xSelStartY + 1, -1, -1, -1, -1, False);
  end;
  S := S + S2;

  CurY := xSelStartY;
  CurX := xSelStartX;
  ClearSelection;

  Changed(xSelStartY, -1);
  SelectionChanged;
  if bRepaint then
    Invalidate;

  Undo               := TDGLSMemoDeleteBufUndo.Create(OldX, OldY, CurX, CurY, S);
  Undo.UndoSelStartX := xSelStartX;
  Undo.UndoSelStartY := xSelStartY;
  Undo.UndoSelEndX   := xSelEndX;
  Undo.UndoSelEndY   := xSelEndY;
  if Assigned(FUndoList) then
    FUndoList.Add(Undo);
end;

procedure TDGLSCustomMemo.CutToClipBoard;
begin
  Clipboard.SetTextBuf(PChar(GetSelText));
  DeleteSelection(True);
end;

function TDGLSCustomMemo.GetSelText: string;
var
  i:                                          Integer;
  xSelStartX, xSelStartY, xSelEndX, xSelEndY: Integer;
begin
  Result := '';
  if (FSelStartY = FSelEndY) and (FSelStartX = FSelEndX) then
    Exit;

  xSelStartX := FSelStartX;
  xSelStartY := FSelStartY;
  xSelEndX   := FSelEndX;
  xSelEndY   := FSelEndY;
  OrderPos(xSelStartX, xSelStartY, xSelEndX, xSelEndY);

  if xSelStartY = xSelEndY then
    Result := Copy(Lines[xSelStartY], xSelStartX + 1, xSelEndX - xSelStartX)
  else
  begin
    Result   := Copy(Lines[xSelStartY], xSelStartX + 1, Length(Lines[xSelStartY]));
    for i    := xSelStartY + 1 to xSelEndY - 1 do
      Result := Result + #13#10 + Lines[i];
    Result   := Result + #13#10 + Copy(Lines[xSelEndY], 1, xSelEndX);
  end;
end;

function TDGLSCustomMemo.GetSelStart: TPoint;
var
  xSelStartX, xSelStartY, xSelEndX, xSelEndY: Integer;
begin
  xSelStartX := FSelStartX;
  xSelStartY := FSelStartY;
  xSelEndX   := FSelEndX;
  xSelEndY   := FSelEndY;
  OrderPos(xSelStartX, xSelStartY, xSelEndX, xSelEndY);
  Result := Point(xSelStartX, xSelStartY);
end;

function TDGLSCustomMemo.GetSelEnd: TPoint;
var
  xSelStartX, xSelStartY, xSelEndX, xSelEndY: Integer;
begin
  xSelStartX := FSelStartX;
  xSelStartY := FSelStartY;
  xSelEndX   := FSelEndX;
  xSelEndY   := FSelEndY;
  OrderPos(xSelStartX, xSelStartY, xSelEndX, xSelEndY);
  Result := Point(xSelEndX, xSelEndY);
end;

procedure TDGLSCustomMemo.SetSelText(const AValue: string);
var
  i, k:                                       Integer;
  xSelStartX, xSelStartY, xSelEndX, xSelEndY: Integer;
  Buff, S:                                    string;
  OldX, OldY:                                 Integer;
begin
  Buff       := AValue;
  xSelStartX := FSelStartX;
  xSelStartY := FSelStartY;
  xSelEndX   := FSelEndX;
  xSelEndY   := FSelEndY;
  OrderPos(xSelStartX, xSelStartY, xSelEndX, xSelEndY);

  DeleteSelection(False);

  OldX := CurX;
  OldY := CurY;
  i    := Pos(#13#10, Buff);
  S    := Lines[xSelStartY];
  if i = 0 then
  begin
    Lines[xSelStartY] := Copy(S, 1, xSelStartX) + Buff + Copy(S, xSelStartX + 1, Length(S));
    CurX              := xSelStartX;
    if Buff <> '' then
      CurX := CurX + Length(Buff);
  end
  else
  begin
    k        := xSelStartY;
    Lines[k] := Copy(S, 1, xSelStartX) + Copy(Buff, 1, i - 1);
    TDGLSMemoStrings(Lines).DoInsert(k + 1, Copy(S, xSelStartX + 1, Length(S)));
    while True do
    begin
      Buff := Copy(Buff, i + 2, Length(Buff));
      i    := Pos(#13#10, Buff);
      k    := k + 1;
      if i = 0 then
        break;
      TDGLSMemoStrings(Lines).DoInsert(k, Copy(Buff, 1, i - 1));
    end;
    Lines[k] := Buff + Lines[k];
    CurY     := k;
    CurX     := Length(Buff);
  end;

  ClearSelection;
  Changed(xSelStartY, -1);
  if Assigned(FUndoList) then
    FUndoList.Add(TDGLSMemoPasteUndo.Create(OldX, OldY, CurX, CurY, AValue));
  Invalidate;
end;

function TDGLSCustomMemo.GetSelLength: Integer;
begin
  Result := Length(GetSelText);
end;

procedure TDGLSCustomMemo.Changed(FromLine, ToLine: Integer);
var
  i: Integer;
begin
  if ToLine < FromLine then
    ToLine        := Lines.Count - 1;
  for i           := FromLine to ToLine do
    ValidAttrs[i] := False;
  InvalidateLineRange(FromLine, ToLine);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDGLSCustomMemo.AttrChanged(LineNo: Integer);
begin
  ValidAttrs[LineNo] := False;
  InvalidateLineRange(LineNo, LineNo);
  if Assigned(FOnAttrChange) then
    FOnAttrChange(Self);
end;

procedure TDGLSCustomMemo.SelectionChanged;
begin
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
end;

procedure TDGLSCustomMemo.StatusChanged;
begin
  if Assigned(FOnStatusChange) then
    FOnStatusChange(Self);
end;

procedure TDGLSCustomMemo.ClearSelection;
var
  rct:     TRect;
  Changed: Boolean;
begin
  Changed    := not((FSelStartX = FSelEndX) and (FSelStartY = FSelEndY));
  rct        := LineRangeRect(FSelStartY, FSelEndY);
  FSelStartX := CurX;
  FSelStartY := CurY;
  FSelEndX   := CurX;
  FSelEndY   := CurY;
  FPrevSelX  := CurX;
  FPrevSelY  := CurY;
  if Changed then
  begin
    SelectionChanged;
    InvalidateRect(Handle, @rct, True);
  end;
  if Assigned(FOnMoveCursor) then
    FOnMoveCursor(Self);
end;

procedure TDGLSCustomMemo.ExpandSelection;
var
  rct: TRect;
begin
  rct       := LineRangeRect(FPrevSelY, CurY);
  FSelEndX  := CurX;
  FSelEndY  := CurY;
  FPrevSelX := CurX;
  FPrevSelY := CurY;
  SelectionChanged;
  InvalidateRect(Handle, @rct, True);
  if Assigned(FOnMoveCursor) then
    FOnMoveCursor(Self);
end;

function TDGLSCustomMemo.MaxLength: Integer;
var
  i, len: Integer;
begin
  Result := 0;
  for i  := 0 to Lines.Count - 1 do
  begin
    len := Length(Lines[i]);
    if len > Result then
      Result := len;
  end;
end;

procedure TDGLSCustomMemo.DoScroll(Sender: TDGLSMemoScrollBar; ByValue: Integer);
var
  eRect, scrRect, sbRect: TRect;
  Old:                    Integer;
begin
  eRect := EditorRect;
  case Sender.Kind of
    sbVertical:
      begin
        Old      := FTopLine;
        FTopLine := FTopLine + ByValue;
        if FTopLine > Sender.MaxPosition then
          FTopLine := Sender.MaxPosition;
        if FTopLine < 0 then
          FTopLine := 0;
        if Old <> FTopLine then
        begin
          ShowCaret(False);
          if CurY < FTopLine then
            CurY := FTopLine;
          if CurY > LastVisibleLine then
            CurY := LastVisibleLine;

          ScrollDC(Canvas.Handle, 0, (Old - FTopLine) * FCellSize.H, eRect, eRect, 0, @scrRect);
          InvalidateRect(Handle, @scrRect, True);
          sbRect := Sender.FullRect;
          InvalidateRect(Handle, @sbRect, True);
          FGutter.Invalidate;
          ShowCaret(True);
        end;
      end;
    sbHorizontal:
      begin
        Old      := FLeftCol;
        FLeftCol := FLeftCol + ByValue;
        if FLeftCol > Sender.MaxPosition then
          FLeftCol := Sender.MaxPosition;
        if FLeftCol < 0 then
          FLeftCol := 0;
        if Old <> FLeftCol then
        begin
          ShowCaret(False);
          if CurX < FLeftCol then
            CurX := FLeftCol;
          if CurX > LastVisiblePos then
            CurX := LastVisiblePos;
          ScrollDC(Canvas.Handle, (Old - FLeftCol) * FCellSize.W, 0, eRect, eRect, 0, @scrRect);
          InvalidateRect(Handle, @scrRect, True);
          sbRect := Sender.FullRect;
          InvalidateRect(Handle, @sbRect, True);
          ShowCaret(True);
        end;
      end;
  end;
end;

procedure TDGLSCustomMemo.DoScrollPage(Sender: TDGLSMemoScrollBar; ByValue: Integer);
begin
  case Sender.Kind of
    sbVertical:
      DoScroll(Sender, ByValue * VisibleLineCount);
    sbHorizontal:
      DoScroll(Sender, ByValue * VisiblePosCount);
  end;
end;

procedure TDGLSCustomMemo.SetLines(ALines: TStrings);
begin
  if ALines <> nil then
  begin
    FLines.Assign(ALines);
    Changed(0, -1);
    SelectionChanged;
    Invalidate;
  end;
end;

procedure TDGLSCustomMemo.SetLineStyle(Index: Integer; Value: Integer);
begin
  TDGLSMemoStrings(FLines).Style[Index] := Value;
  if IsLineVisible(Index) then
    AttrChanged(Index);
end;

function TDGLSCustomMemo.GetLineStyle(Index: Integer): Integer;
begin
  Result := TDGLSMemoStrings(FLines).Style[Index];
end;

function TDGLSCustomMemo.GetInComment(Index: Integer): Boolean;
begin
  Result := TDGLSMemoStrings(FLines).InComment[Index];
end;

procedure TDGLSCustomMemo.SetInComment(Index: Integer; Value: Boolean);
begin
  TDGLSMemoStrings(FLines).InComment[Index] := Value;
end;

function TDGLSCustomMemo.GetInBrackets(Index: Integer): Integer;
begin
  Result := TDGLSMemoStrings(FLines).InBrackets[Index];
end;

procedure TDGLSCustomMemo.SetInBrackets(Index: Integer; Value: Integer);
begin
  TDGLSMemoStrings(FLines).InBrackets[Index] := Value;
end;

function TDGLSCustomMemo.GetValidAttrs(Index: Integer): Boolean;
begin
  Result := TDGLSMemoStrings(FLines).ValidAttrs[Index];
end;

procedure TDGLSCustomMemo.SetValidAttrs(Index: Integer; Value: Boolean);
begin
  TDGLSMemoStrings(FLines).ValidAttrs[Index] := Value;
end;

function TDGLSCustomMemo.GetCharAttrs(Index: Integer): string;
begin
  Result := TDGLSMemoStrings(FLines).CharAttrs[Index];
end;

procedure TDGLSCustomMemo.SetCharAttrs(Index: Integer; Value: string);
begin
  TDGLSMemoStrings(FLines).CharAttrs[Index] := Value;
  if IsLineVisible(Index) then
    AttrChanged(Index);
end;

procedure TDGLSCustomMemo.SetCurX(Value: Integer);
var
  len:        Integer;
  WasVisible: Boolean;
begin
  if Value < 0 then
    if CurY = 0 then
      Value := 0
    else
    begin
      CurY  := CurY - 1;
      Value := Length(Lines[CurY]);
    end;

  if (CurY >= 0) and (CurY < Lines.Count) then
  begin
    len := Length(Lines[CurY]);
    if Value > len then
    begin
      Lines[CurY] := Lines[CurY] + StringOfChar(' ', Value - len);
      // Value := len;
      ValidAttrs[CurY] := False;
      InvalidateLineRange(CurY, CurY);
    end;
  end;

  FCurX := Value;

  WasVisible := FCaretVisible;
  if WasVisible then
    ShowCaret(False);
  MakeVisible;
  ResizeScrollBars;
  StatusChanged;
  if WasVisible then
    ShowCaret(True);
end;

procedure TDGLSCustomMemo.SetCurY(Value: Integer);
var
  Old:        Integer;
  WasVisible: Boolean;
begin
  WasVisible := FCaretVisible;
  if WasVisible then
    ShowCaret(False);
  Old := CurY;

  if Value < 0 then
    Value := 0;
  if Value >= Lines.Count then
    Value := Lines.Count - 1;

  FCurY := Value;
  if (CurY <> Old) and (Old >= 0) and (Old < Lines.Count) then
    Lines[Old] := TrimRight(Lines[Old]);
  CurX         := CurX;

  MakeVisible;
  ResizeScrollBars;
  StatusChanged;
  if WasVisible then
    ShowCaret(True);
end;

procedure TDGLSCustomMemo.MoveCursor(dX, dY: Integer; Shift: TShiftState);
var
  Selecting: Boolean;
  S:         string;
  // ------------------------------------------------------------
  function IsDelimiter(C: Char): Boolean;
  begin
    Result := Pos(C, ' .,;:/?!@#$%^&*(){}[]<>-+=|\') > 0;
  end;
// ------------------------------------------------------------
  function IsStopChar(C, cThis: Char): Boolean;
  begin
    Result := IsDelimiter(C) <> IsDelimiter(cThis);
  end;
// ------------------------------------------------------------
  procedure MoveWordLeft;
  begin
    CurX := CurX - 1;
    S    := TrimRight(Lines[CurY]);
    while CurX > 0 do
    begin
      if IsStopChar(S[CurX], S[CurX + 1]) then
        break;
      CurX := CurX - 1;
    end;
    if (CurX < 0) then
      if CurY > 0 then
      begin
        CurY := CurY - 1;
        CurX := Length(Lines[CurY]);
      end;
  end;
// ------------------------------------------------------------
  procedure MoveWordRight;
  var
    len: Integer;
  begin
    S    := TrimRight(Lines[CurY]);
    len  := Length(S);
    CurX := CurX + 1;
    while CurX < len do
    begin
      if IsStopChar(S[CurX + 1], S[CurX]) then
        break;
      CurX := CurX + 1;
    end;
    if CurX > len then
      if CurY < Lines.Count - 1 then
      begin
        CurY := CurY + 1;
        CurX := 0;
      end;
  end;

// ------------------------------------------------------------
begin
  Selecting := (ssShift in Shift) and (CurX = FPrevSelX) and (CurY = FPrevSelY);
  if ssCtrl in Shift then
  begin
    if dX > 0 then
      MoveWordRight;
    if dX < 0 then
      MoveWordLeft;
  end
  else
  begin
    CurY := CurY + dY;
    CurX := CurX + dX;
  end;
  if Selecting then
    ExpandSelection
  else
    ClearSelection;
end;

procedure TDGLSCustomMemo.MovePage(dP: Integer; Shift: TShiftState);
var
  eRect:        TRect;
  LinesPerPage: Integer;
  Selecting:    Boolean;
begin
  if FCellSize.H = 0 then
    Exit;
  Selecting    := (ssShift in Shift) and (CurX = FPrevSelX) and (CurY = FPrevSelY);
  eRect        := EditorRect;
  LinesPerPage := (eRect.Bottom - eRect.Top) div FCellSize.H - 1;
  CurY         := CurY + dP * LinesPerPage;
  if ssCtrl in Shift then
    if dP > 0 then
    begin
      CurY := Lines.Count - 1;
      CurX := Length(Lines[Lines.Count - 1]);
    end
    else
    begin
      CurY := 0;
      CurX := 0;
    end;
  if Selecting then
    ExpandSelection
  else
    ClearSelection;
end;

procedure TDGLSCustomMemo.GoHome(Shift: TShiftState);
var
  Selecting: Boolean;
begin
  Selecting := (ssShift in Shift) and (CurX = FPrevSelX) and (CurY = FPrevSelY);
  CurX      := 0;
  FLeftCol  := 0;
  if Selecting then
    ExpandSelection
  else
    ClearSelection;
end;

procedure TDGLSCustomMemo.GoEnd(Shift: TShiftState);
var
  Selecting: Boolean;
  S, S1:     string;
begin
  Selecting := (ssShift in Shift) and (CurX = FPrevSelX) and (CurY = FPrevSelY);

  S := Lines[CurY];
  if not Selecting then
    S         := TrimRight(S);
  S1          := TrimRight(Copy(S, CurX + 1, Length(S)));
  S           := Copy(S, 1, CurX);
  Lines[CurY] := S + S1;

  CurX := Length(Lines[CurY]);
  if Selecting then
    ExpandSelection
  else
    ClearSelection;
end;

procedure TDGLSCustomMemo.InsertChar(C: Char);
var
  S, S1:        string;
  NewPlace:     Integer;
  rct:          TRect;
  CurX0, CurY0: Integer;
begin
  CurX0    := CurX;
  CurY0    := CurY;
  S        := Lines[CurY];
  NewPlace := CurX + 1;
  if C = #9 then
  begin
    while (NewPlace mod TabSize) <> 0 do
      Inc(NewPlace);
    S1 := StringOfChar(' ', NewPlace - CurX);
  end
  else
    S1 := C;
  Insert(S1, S, CurX + 1);
  Lines[CurY] := S;
  CurX        := NewPlace;
  ClearSelection;
  rct := LineRect(CurY);
  Changed(CurY, CurY);

  if Assigned(FUndoList) then
    FUndoList.Add(TDGLSMemoInsCharUndo.Create(CurX0, CurY0, CurX, CurY, S1));

  InvalidateRect(Handle, @rct, True);
end;

procedure TDGLSCustomMemo.InsertTemplate(AText: string);
var
  i, NewCurX, NewCurY: Integer;
  Indent:              string;
  FoundCursor:         Boolean;
begin
  Indent := IndentCurrLine;

  DeleteSelection(False);
  ClearSelection;

  NewCurX     := CurX;
  NewCurY     := CurY;
  FoundCursor := False;
  i           := 1;
  while i <= Length(AText) do
  begin
    if AText[i] = #13 then
    begin
      if (i = Length(AText)) or (AText[i + 1] <> #10) then
        Insert(#10 + Indent, AText, i + 1);
      if not FoundCursor then
      begin
        Inc(NewCurY);
        NewCurX := Length(Indent);
      end;
      Inc(i, 1 + Length(Indent));
    end
    else if AText[i] = #7 then
    begin
      FoundCursor := True;
      Delete(AText, i, 1);
      Dec(i);
    end
    else if Ord(AText[i]) < Ord(' ') then
    begin
      Delete(AText, i, 1);
      Dec(i);
    end
    else if not FoundCursor then
      Inc(NewCurX);
    Inc(i);
  end;

  SetSelText(AText);
  SetCursor(NewCurX, NewCurY);
  ClearSelection;
  try
    SetFocus;
  except
  end;

end;

procedure TDGLSCustomMemo.DeleteChar(OldX, OldY: Integer);
var
  S, S1:       string;
  rct:         TRect;
  C:           Char;
  Undo:        TDGLSMemoDelCharUndo;
  IsBackspace: Boolean;
begin
  if FReadOnly then
    Exit;
  if OldX < 0 then
  begin
    OldX        := CurX;
    OldY        := CurY;
    IsBackspace := False;
  end
  else
    IsBackspace := True;

  ClearSelection;

  S  := Lines[CurY];
  S1 := Copy(S, CurX + 1, Length(S));
  if not IsBackspace then
    S1        := TrimRight(S1);
  S           := Copy(S, 1, CurX);
  Lines[CurY] := S + S1;

  if CurX < Length(Lines[CurY]) then
  begin
    S := Lines[CurY];
    C := S[CurX + 1];
    Delete(S, CurX + 1, 1);
    Lines[CurY] := S;
    Changed(CurY, CurY);
    rct              := LineRect(CurY);
    Undo             := TDGLSMemoDelCharUndo.Create(OldX, OldY, CurX, CurY, C);
    Undo.IsBackspace := IsBackspace;
    if Assigned(FUndoList) then
      FUndoList.Add(Undo);
  end
  else if CurY < Lines.Count - 1 then
  begin
    S           := Lines[CurY] + Lines[CurY + 1];
    Lines[CurY] := S;
    DeleteLine(CurY + 1, OldX, OldY, CurX, CurY, False);
    Changed(CurY, -1);
    rct              := EditorRect;
    Undo             := TDGLSMemoDelCharUndo.Create(OldX, OldY, CurX, CurY, #13);
    Undo.IsBackspace := IsBackspace;
    if Assigned(FUndoList) then
      FUndoList.Add(Undo);
  end;
  ClearSelection;
  InvalidateRect(Handle, @rct, True);
end;

procedure TDGLSCustomMemo.DeleteLine(Index, OldX, OldY, NewX, NewY: Integer; FixUndo: Boolean);
var
  rct: TRect;
  S:   string;
begin
  if Index < 0 then
    Index := CurY;
  if OldX < 0 then
  begin
    OldX := CurX;
    OldY := CurY;
  end;

  S := Lines[Index];

  TDGLSMemoStrings(Lines).FDeleting := True;
  if Lines.Count = 1 then
    TDGLSMemoStrings(Lines)[0] := ''
  else
    Lines.Delete(Index);
  TDGLSMemoStrings(Lines).FDeleting := False;

  ClearSelection;
  if Index >= Lines.Count then
    Changed(Index - 1, -1)
  else
    Changed(Index, -1);
  rct := EditorRect;
  InvalidateRect(Handle, @rct, True);

  if NewX < 0 then
  begin
    if Length(Lines[0]) < CurX then
      CurX := Length(Lines[0]);
    if Index >= Lines.Count then
      CurY := Index - 1
    else
      CurY := Index;
    NewX   := CurX;
    NewY   := CurY;
  end
  else
  begin
    CurX := NewX;
    CurY := NewY;
  end;
  if Assigned(FUndoList) and FixUndo then
    FUndoList.Add(TDGLSMEmoDelLineUndo.Create(Index, OldX, OldY, NewX, NewY, S));
end;

procedure TDGLSCustomMemo.BackSpace;
var
  OldX, OldY: Integer;
begin
  OldX := CurX;
  OldY := CurY;
  MoveCursor(-1, 0, []);
  if (OldX = CurX) and (OldY = CurY) then
    Exit;
  DeleteChar(OldX, OldY);
end;

procedure TDGLSCustomMemo.BackSpaceWord;
begin
  ClearSelection;
  MoveCursor(-1, 0, [ssShift, ssCtrl]);
  DeleteSelection(True);
end;

function TDGLSCustomMemo.IndentCurrLine: string;
var
  len, Count: Integer;
  CurS:       string;
begin
  Result := '';
  if not AutoIndent then
    Exit;
  CurS  := Lines[CurY];
  len   := Length(CurS);
  Count := 0;
  while (Count < CurX) and (Count < len) do
  begin
    if CurS[Count + 1] <> ' ' then
      break;
    Inc(Count);
  end;
  Result := StringOfChar(' ', Count);
end;

procedure TDGLSCustomMemo.NewLine;
var
  S, sIndent: string;
  OldX, OldY: Integer;
begin
  OldX        := CurX;
  OldY        := CurY;
  S           := Lines[CurY];
  sIndent     := IndentCurrLine;
  Lines[CurY] := Copy(S, 1, CurX);

  S := TrimRight(Copy(S, CurX + 1, Length(S)));
  if AutoIndent then
    while (Length(S) > 0) and (S[1] = ' ') do
      Delete(S, 1, 1);

  TDGLSMemoStrings(Lines).DoInsert(CurY + 1, sIndent + S);
  GoHome([]);
  MoveCursor(0, 1, []);
  CurX := Length(sIndent);
  ClearSelection;
  if Assigned(FUndoList) then
    FUndoList.Add(TDGLSMemoInsCharUndo.Create(OldX, OldY, CurX, CurY, #13 + sIndent));
  Invalidate;
  Changed(CurY - 1, -1);
end;

function TDGLSCustomMemo.AddString(S: string): Integer;
begin
  if Lines.Count = 0 then
    TDGLSMemoStrings(Lines).DoAdd('');
  MovePage(1, [ssCtrl]); // end of text
  if not((Lines.Count = 1) and (Lines[0] = '')) then
  begin
    TDGLSMemoStrings(Lines).DoAdd('');
    CurX := 0;
    CurY := Lines.Count;
    ClearSelection;
    // S := #13#10 + S;
  end;
  SetSelText(S);
  Result := Lines.Count - 1;
end;

procedure TDGLSCustomMemo.InsertString(Index: Integer; S: string);
begin
  CurY := Index;
  CurX := 0;
  ClearSelection;
  if not((Lines.Count = 1) and (Lines[0] = '')) then
    S := S + #13#10;
  SetSelText(S);
end;

procedure TDGLSCustomMemo.DoCommand(cmd: TCommand; const AShift: TShiftState);
begin
  case cmd of
    cmDelete:
      if not FReadOnly then
      begin
        if ssShift in AShift then
          CutToClipBoard
        else if FDelErase and (not((FSelStartX = FSelEndX) and (FSelStartY = FSelEndY))) then
          DeleteSelection(True)
        else
          DeleteChar(-1, -1);
      end;
    cmBackSpace:
      BackSpace;
    cmWordBackSpace:
      BackSpaceWord;
    cmNewLine:
      NewLine;
    cmDelLine:
      DeleteLine(-1, -1, -1, -1, -1, True);
    cmCopy:
      CopyToClipBoard;
    cmCut:
      CutToClipBoard;
    cmPaste:
      PasteFromClipBoard;
    cmHome:
      GoHome(AShift);
    cmEnd:
      GoEnd(AShift);
    cmPageDown:
      MovePage(1, AShift);
    cmPageUp:
      MovePage(-1, AShift);
    cmInsert:
      begin
        if ssShift in AShift then
          PasteFromClipBoard;
        if ssCtrl in AShift then
          CopyToClipBoard;
      end;
  end;
end;

procedure TDGLSCustomMemo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  ShowCaret(False);
  inherited;
  case Key of
    VK_LEFT:
      MoveCursor(-1, 0, Shift);
    VK_RIGHT:
      MoveCursor(1, 0, Shift);
    VK_UP:
      MoveCursor(0, -1, Shift);
    VK_DOWN:
      MoveCursor(0, 1, Shift);
    VK_HOME, VK_END, VK_DELETE:
      DoCommand(Key, Shift);
    VK_PRIOR, VK_NEXT:
      DoCommand(Key, Shift);
    VK_INSERT:
      DoCommand(Key, Shift);
  end;
  ShowCaret(True);
end;

procedure TDGLSCustomMemo.KeyPress(var Key: Char);
begin
  if FReadOnly then
    Exit;
  ShowCaret(False);
  inherited;
  if (Ord(Key) in [9, 32 .. 255]) and (Ord(Key) <> 127) then
  begin
    if FDelErase and (not((FSelStartX = FSelEndX) and (FSelStartY = FSelEndY))) then
      DeleteSelection(True);
    InsertChar(Key);
  end
  else
    DoCommand(Ord(Key), []);
  ShowCaret(True);
end;

procedure TDGLSCustomMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  newPos:    TCellPos;
  charPos:   TFullPos;
  Selecting: Boolean;
begin
  inherited;
  if not Focused then
  begin
    SetFocus;
    // Exit;
  end;

  if FAfterDoubleClick then
  begin
    FAfterDoubleClick := False;
    Exit;
  end;

  if Button <> mbLeft then
    Exit;

  if sbVert.MouseDown(Button, Shift, X, Y) then
    Exit;
  if sbHorz.MouseDown(Button, Shift, X, Y) then
    Exit;

  if PointInRect(Point(X, Y), EditorRect) then
  begin
    ShowCaret(False);
    newPos := CellFromPos(X, Y);
    CurY   := newPos.Y + FTopLine;
    CurX   := newPos.X + FLeftCol;
    if Assigned(FOnMoveCursor) then
      FOnMoveCursor(Self);

    Selecting := ssShift in Shift;
    if Button = mbLeft then
    begin
      if Selecting then
        ExpandSelection
      else
        ClearSelection;
      FLeftButtonDown := True;
    end
    else
      ShowCaret(True);
  end;

  if Assigned(FOnGutterClick) then
    if PointInRect(Point(X, Y), FGutter.FullRect) then
    begin
      charPos := CharFromPos(X, Y);
      if charPos.LineNo < Lines.Count then
        FOnGutterClick(Self, charPos.LineNo);
    end;
end;

procedure TDGLSCustomMemo.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  newPos: TCellPos;
begin
  inherited;
  if sbVert.MouseMove(Shift, X, Y) then
    Exit;
  if sbHorz.MouseMove(Shift, X, Y) then
    Exit;
  if PointInRect(Point(X, Y), EditorRect) then
  begin
    if (ssLeft in Shift) and FLeftButtonDown then
    begin
      newPos := CellFromPos(X, Y);
      CurY   := newPos.Y + FTopLine;
      CurX   := newPos.X + FLeftCol;
      ExpandSelection;
    end;
  end
end;

procedure TDGLSCustomMemo.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if sbVert.MouseUp(Button, Shift, X, Y) then
    Exit;
  if sbHorz.MouseUp(Button, Shift, X, Y) then
    Exit;
  if Button = mbLeft then
    ShowCaret(True);
  FLeftButtonDown := False;
  FLastMouseUpX   := X;
  FLastMouseUpY   := Y;
end;

procedure TDGLSCustomMemo.DblClick;
var
  clickPos:       TCellPos;
  clickX, clickY: Integer;
  // ------------------------------------------------------------
  // SELECT WORD
  // ------------------------------------------------------------
  procedure SelectWord;
  const
    stopChars: TSysCharSet = [' ', ';', '.', ',', ':', '?', '!', '''', '"', '<', '>', '/', '*', '+', '-', '=', '(', ')', '[', ']', '{', '}', '@', '#', '$', '%', '^', '&', '|', '\'];
  var
    S:   string;
    i:   Integer;
    rct: TRect;
  begin
    CurX := clickX;
    CurY := clickY;
    if (CurX = clickX) and (CurY = clickY) then
    begin
      S := Lines[clickY];
      if S[clickX + 1] = ' ' then
        Exit;

      i := clickX;
      while (i >= 0) and not CharInSet(S[i + 1], stopChars) do
        Dec(i);
      FSelStartY := clickY;
      FSelStartX := i + 1;

      i := clickX;
      while (i < Length(S)) and not CharInSet(S[i + 1], stopChars) do
        Inc(i);
      FSelEndY := clickY;
      FSelEndX := i;

      if FSelEndX <> FSelStartX then
      begin
        FAfterDoubleClick := True;
        rct               := LineRangeRect(CurY, CurY);
        SelectionChanged;
        InvalidateRect(Handle, @rct, True);
      end;
    end;
  end;

// ------------------------------------------------------------
begin

  if PointInRect(Point(FLastMouseUpX, FLastMouseUpY), EditorRect) then
  begin
    clickPos := CellFromPos(FLastMouseUpX, FLastMouseUpY);
    clickX   := clickPos.X + FLeftCol;
    clickY   := clickPos.Y + FTopLine;
    SelectWord;
  end;
  inherited;
end;

procedure TDGLSCustomMemo.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS or DLGC_WANTTAB;
end;

procedure TDGLSCustomMemo.WMEraseBkgnd(var Msg: TWmEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TDGLSCustomMemo.WMSize(var Msg: TWMSize);
begin
  if not(csLoading in ComponentState) then
    try
      ResizeEditor;
    except
    end;
end;

procedure TDGLSCustomMemo.WMSetCursor(var Msg: TWMSetCursor);
var
  P: TPoint;
begin
  Msg.Result := 1;
  GetCursorPos(P);
  P := ScreenToClient(P);
  if PointInRect(P, EditorRect) then
    WinApi.Windows.SetCursor(Screen.Cursors[crIBeam])
  else
    WinApi.Windows.SetCursor(Screen.Cursors[crArrow]);
end;

procedure TDGLSCustomMemo.WMSetFocus(var Msg: TWMSetFocus);
begin
  if FCellSize.H = 0 then
    SetFont(FFont);
  CreateCaret(Handle, HBITMAP(0), 2, FCellSize.H - 2);
  ShowCaret(True);
end;

procedure TDGLSCustomMemo.WMKillFocus(var Msg: TWMSetFocus);
begin
  DestroyCaret;
  FCaretVisible := False;
  inherited;
end;

procedure TDGLSCustomMemo.ShowCaret(State: Boolean);
var
  rct: TRect;
begin
  FCaretVisible := False;
  if not State then
    HideCaret(Handle)
  else if Focused and not HiddenCaret then
  begin
    rct := CellRect(CurX - FLeftCol, CurY - FTopLine);
    SetCaretPos(rct.Left, rct.Top + 1);
    WinApi.Windows.ShowCaret(Handle);
    FCaretVisible := True;
  end;
end;

function TDGLSCustomMemo.CellRect(ACol, ARow: Integer): TRect;
var
  rct: TRect;
begin
  rct := EditorRect;
  with FCellSize do
    Result := Rect(rct.Left + W * ACol, rct.Top + H * ARow, rct.Left + W * (ACol + 1), rct.Top + H * (ARow + 1));
end;

function TDGLSCustomMemo.LineRect(ARow: Integer): TRect;
var
  rct: TRect;
begin
  rct  := EditorRect;
  ARow := ARow - FTopLine;
  with FCellSize do
    Result := Rect(rct.Left, rct.Top + H * ARow, rct.Right, rct.Top + H * (ARow + 1));
end;

function TDGLSCustomMemo.ColRect(ACol: Integer): TRect;
var
  rct: TRect;
begin
  rct  := EditorRect;
  ACol := ACol - FLeftCol;
  with FCellSize do
    Result := Rect(rct.Left + W * ACol, rct.Top, rct.Left + W * (ACol + 1), rct.Bottom);
end;

function TDGLSCustomMemo.LineRangeRect(FromLine, ToLine: Integer): TRect;
var
  rct1, rct2: TRect;
begin
  rct1   := LineRect(FromLine);
  rct2   := LineRect(ToLine);
  Result := TotalRect(rct1, rct2);
end;

procedure TDGLSCustomMemo.InvalidateLineRange(FromLine, ToLine: Integer);
var
  rct: TRect;
begin
  if ToLine < FromLine then
    ToLine := Lines.Count - 1;
  rct      := LineRangeRect(FromLine, ToLine);
  if GutterWidth > 2 then
    rct.Left := FGutter.Left;
  InvalidateRect(Handle, @rct, True);
end;

function TDGLSCustomMemo.ColRangeRect(FromCol, ToCol: Integer): TRect;
var
  rct1, rct2: TRect;
begin
  rct1   := ColRect(FromCol);
  rct2   := ColRect(ToCol);
  Result := TotalRect(rct1, rct2);
end;

function TDGLSCustomMemo.CellFromPos(X, Y: Integer): TCellPos;
var
  rct: TRect;
begin
  rct := EditorRect;
  if (FCellSize.H = 0) and Assigned(FFont) then
    SetFont(FFont);
  if (FCellSize.W <> 0) and (FCellSize.H <> 0) then
  begin
    Result.X := (X - rct.Left) div FCellSize.W;
    Result.Y := (Y - rct.Top) div FCellSize.H;
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end;

function TDGLSCustomMemo.CharFromPos(X, Y: Integer): TFullPos;
var
  rct: TRect;
begin
  rct := EditorRect;
  if (FCellSize.H = 0) and Assigned(FFont) then
    SetFont(FFont);
  if (FCellSize.W <> 0) and (FCellSize.H <> 0) then
  begin
    Result.Pos    := (X - rct.Left) div FCellSize.W + FLeftCol;
    Result.LineNo := (Y - rct.Top) div FCellSize.H + FTopLine;
  end
  else
  begin
    Result.Pos    := 1;
    Result.LineNo := 1;
  end;
end;

procedure TDGLSCustomMemo.SetColor(Index: Integer; Value: TColor);
var
  eRect:   TRect;
  Changed: Boolean;
begin
  Changed := False;
  case Index of
    0:
      if FBkColor <> Value then
      begin
        FBkColor           := Value;
        FStyles.BkColor[0] := Value;
        Changed            := True;
      end;
    1:
      if FSelColor <> Value then
      begin
        FSelColor := Value;
        Changed   := True;
      end;
    2:
      if FSelBkColor <> Value then
      begin
        FSelBkColor := Value;
        Changed     := True;
      end;
  end;
  if Changed then
  begin
    eRect := EditorRect;
    InvalidateRect(Handle, @eRect, True);
  end;
end;

procedure TDGLSCustomMemo.SetFont(Value: TFont);
var
  wW, wi:      Integer;
  OldFontName: string;
  eRect:       TRect;
begin
  OldFontName      := Canvas.Font.Name;
  Canvas.Font.Name := Value.Name;
  wW               := Canvas.TextWidth('W');
  wi               := Canvas.TextWidth('i');
  Canvas.Font.Name := OldFontName;

  if wW <> wi then
    raise EAbort.Create('Monospace font required');

  FFont.Assign(Value);
  Canvas.Font.Assign(Value);
  FCellSize.W := Canvas.TextWidth('W');
  FCellSize.H := Canvas.TextHeight('W') + 1;

  if FCaretVisible then
  begin
    ShowCaret(False);
    DestroyCaret;
    CreateCaret(Handle, HBITMAP(0), 2, FCellSize.H - 2);
    ShowCaret(True);
  end;

  FStyles.TextColor[0] := FFont.Color;
  FStyles.Style[0]     := FFont.Style;

  eRect := EditorRect;
  InvalidateRect(Handle, @eRect, True);
end;

procedure TDGLSCustomMemo.SetGutterWidth(Value: Integer);
begin
  FGutterWidth   := Value;
  FGutter.FWidth := Value;
  if not(csLoading in ComponentState) then
    ResizeEditor;
end;

procedure TDGLSCustomMemo.SetGutterColor(Value: TColor);
begin
  if FGutter.FColor <> Value then
  begin
    FGutter.FColor := Value;
    FGutter.Invalidate;
  end;
end;

function TDGLSCustomMemo.GetGutterColor: TColor;
begin
  Result := FGutter.FColor;
end;

function TDGLSCustomMemo.CharStyleNo(LineNo, Pos: Integer): Integer;
var
  ChStyle: string;
begin
  Result := 0;
  if (LineNo < 0) or (LineNo >= Lines.Count) then
    Exit;

  ChStyle := CharAttrs[LineNo];
  if (Pos <= 0) or (Pos > Length(ChStyle)) then
    Exit;

  Result := Integer(ChStyle[Pos]);
end;

procedure TDGLSCustomMemo.DrawLine(LineNo: Integer);
var
  eRect, rct0, rct1, rct, lineRct:            TRect;
  LineSelStart, LineSelEnd, LineStyleNo, Pos: Integer;
  S, S1, S2, S3, ChStyle:                     string;
  // --------- FIND LINE SELECTION -------------
  procedure FindLineSelection;
  var
    len:                                        Integer;
    xSelStartX, xSelStartY, xSelEndX, xSelEndY: Integer;
  begin
    xSelStartX := FSelStartX;
    xSelStartY := FSelStartY;
    xSelEndX   := FSelEndX;
    xSelEndY   := FSelEndY;
    OrderPos(xSelStartX, xSelStartY, xSelEndX, xSelEndY);
    len          := Length(Lines[LineNo]);
    LineSelStart := 0;
    LineSelEnd   := 0;
    if xSelStartY = LineNo then
    begin
      LineSelStart := xSelStartX - FLeftCol;
      LineSelEnd   := len - FLeftCol;
    end
    else if (xSelStartY < LineNo) and (LineNo < xSelEndY) then
    begin
      LineSelStart := 0;
      LineSelEnd   := len - FLeftCol;
    end;

    if xSelEndY = LineNo then
      LineSelEnd := xSelEndX - FLeftCol;

    if LineSelEnd < LineSelStart then
      Swap(LineSelEnd, LineSelStart);

    if LineSelStart < 0 then
      LineSelStart := 0;
    S              := Copy(Lines[LineNo], FLeftCol + 1, len);
    S1             := Copy(S, 1, LineSelStart);
    S2             := Copy(S, LineSelStart + 1, LineSelEnd - LineSelStart);
    S3             := Copy(S, LineSelEnd + 1, len);
  end;
// ------------- DRAW PART ---------------------
  procedure DrawPart(Part: string; PartStyle, StartPos: Integer; var rct: TRect; IsSelection: Boolean);
  var
    len, W:      Integer;
    rctInternal: TRect;
  begin
    len := Length(Part);
    if len > 0 then
      with FLineBitmap.Canvas do
      begin
        W          := FCellSize.W * len;
        Font.Style := FStyles.Style[PartStyle];
        if IsSelection then
        begin
          Font.Color  := SelColor;
          Brush.Color := SelBkColor;
        end
        else
        begin
          if LineStyleNo = 0 then
          begin
            Font.Color  := FStyles.TextColor[PartStyle];
            Brush.Color := FStyles.BkColor[PartStyle];
          end
          else
          begin
            if (LineNo = FSelCharPos.LineNo) and (StartPos = FSelCharPos.Pos + 1) and (Length(Part) = 1) then
            begin
              Font.Color  := FStyles.TextColor[PartStyle];
              Brush.Color := FStyles.BkColor[PartStyle];
            end
            else
            begin
              Font.Color  := FStyles.TextColor[LineStyleNo];
              Brush.Color := FStyles.BkColor[LineStyleNo];
              Font.Style  := FStyles.Style[LineStyleNo];
            end;
          end;
        end;
        rct.Right          := rct.Left + W;
        rctInternal        := rct;
        rctInternal.Left   := rctInternal.Left - eRect.Left;
        rctInternal.Right  := rctInternal.Right - eRect.Left;
        rctInternal.Top    := rctInternal.Top - rct.Top;
        rctInternal.Bottom := rctInternal.Bottom - rct.Top;
        FillRect(rctInternal);
        DrawText(Handle, PChar(Part), len, rctInternal, DT_LEFT or DT_SINGLELINE or DT_NOPREFIX);
        rct0.Left := rct.Left + W;
        rct       := rct0;
      end;
  end;
// ------------- DRAW SEGMENTS ---------------------
  procedure DrawSegments(S: string; WorkPos: Integer; var rct: TRect; IsSelection: Boolean);
  var
    i, len, ThisStyle: Integer;
  begin
    while True do
    begin
      len := Length(S);
      if len = 0 then
        Exit;
      ThisStyle := Ord(ChStyle[WorkPos]);
      i         := 1;
      while (i <= len) and (ThisStyle = Ord(ChStyle[WorkPos + i - 1])) do
        Inc(i);
      DrawPart(Copy(S, 1, i - 1), ThisStyle, WorkPos, rct, IsSelection);
      Inc(WorkPos, i - 1);
      S := Copy(S, i, len);
    end;
  end;

// ---------------------------------------------
begin
  eRect   := EditorRect;
  rct     := CellRect(0, LineNo - FTopLine);
  rct0    := Rect(eRect.Left, rct.Top, eRect.Right, rct.Bottom);
  lineRct := rct0;

  if LineNo < Lines.Count then
  begin

    rct         := rct0;
    S           := Lines[LineNo];
    LineStyleNo := LineStyle[LineNo];
    ChStyle     := CharAttrs[LineNo];
    FindLineSelection;

    if not Assigned(FOnGetLineAttrs) then
      ChStyle := StringOfChar(#0, Length(Lines[LineNo]));

    if Length(S) > 0 then
      if (FSelCharStyle >= 0) and (LineNo = FSelCharPos.LineNo) then
        ChStyle[FSelCharPos.Pos + 1] := Char(FSelCharStyle);

    Pos := FLeftCol + 1; // 1
    DrawSegments(S1, Pos, rct, False);
    Inc(Pos, Length(S1));
    DrawSegments(S2, Pos, rct, True);
    Inc(Pos, Length(S2));
    DrawSegments(S3, Pos, rct, False);

    // else begin
    // DrawPart(S1,StyleNo,rct,False);
    // DrawPart(S2,StyleNo,rct,True);
    // DrawPart(S3,StyleNo,rct,False);
    // end;

    rct1        := rct;
    rct1.Left   := rct1.Left - eRect.Left;
    rct1.Right  := rct1.Right - eRect.Left;
    rct1.Top    := rct1.Top - rct.Top;
    rct1.Bottom := rct1.Bottom - rct.Top;
    with FLineBitmap.Canvas do
    begin
      Brush.Color := FStyles.BkColor[LineStyleNo];
      FillRect(rct1);
    end;

    with lineRct do
      BitBlt(Canvas.Handle, Left, Top, Right - Left, Bottom - Top, FLineBitmap.Canvas.Handle, 0, 0, SRCCOPY);
  end
  else
    with Canvas do
    begin
      Brush.Color := BkColor;
      FillRect(rct0);
    end;
end;

procedure TDGLSCustomMemo.SetHiddenCaret(Value: Boolean);
begin
  if Value <> FHiddenCaret then
  begin
    FHiddenCaret := Value;
    if Focused then
      if FHiddenCaret = FCaretVisible then
        ShowCaret(not FHiddenCaret);
  end;
end;

procedure Border(Canvas: TCanvas; rct: TRect; BorderType: TBorderType);
const
  Colors: array [TBorderType] of array [1 .. 4] of TColor = (($D0D0D0, clWhite, clGray, clBlack), (clGray, clBlack, $D0D0D0, clWhite), (clWhite, clWhite, clWhite, clGray), (clGray, clWhite, clWhite, clGray));
begin
  with Canvas do
  begin
    Pen.Color := Colors[BorderType][1];
    MoveTo(rct.Left, rct.Bottom - 1);
    LineTo(rct.Left, rct.Top);
    LineTo(rct.Right, rct.Top);
    if BorderType in [btRaised, btLowered] then
    begin
      Pen.Color := Colors[BorderType][2];
      MoveTo(rct.Left + 1, rct.Bottom);
      LineTo(rct.Left + 1, rct.Top + 1);
      LineTo(rct.Right, rct.Top + 1);
      Pen.Color := Colors[BorderType][3];
      MoveTo(rct.Left + 1, rct.Bottom - 2);
      LineTo(rct.Right - 2, rct.Bottom - 2);
      LineTo(rct.Right - 2, rct.Top + 1);
    end;
    Pen.Color := Colors[BorderType][4];
    MoveTo(rct.Left, rct.Bottom - 1);
    LineTo(rct.Right - 1, rct.Bottom - 1);
    LineTo(rct.Right - 1, rct.Top);
  end;
end;

function TDGLSCustomMemo.EditorRect: TRect;
var
  l, t, r, b: Integer;
begin
  l := 2;
  r := Width - 2;
  t := 2;
  b := Height - 2;
  if GutterWidth > 2 then
    l := l + GutterWidth;
  if FScrollBars in [ssBoth, ssVertical] then
    r := r - FScrollBarWidth;
  if FScrollBars in [ssBoth, ssHorizontal] then
    b    := b - FScrollBarWidth;
  Result := Rect(l + FMargin, t, r, b);
end;

procedure TDGLSCustomMemo.DrawMargin;
var
  eRect: TRect;
  i:     Integer;
begin
  eRect := EditorRect;
  with Canvas do
  begin
    Pen.Color := clWhite;
    for i     := 1 to FMargin do
    begin
      MoveTo(eRect.Left - i, eRect.Top);
      LineTo(eRect.Left - i, eRect.Bottom + 1);
    end;
  end;
end;

procedure TDGLSCustomMemo.DrawGutter;
begin
  if GutterWidth < 2 then
    Exit;
  ResizeGutter;
  FGutter.PaintTo(Canvas);
end;

procedure TDGLSCustomMemo.DrawScrollBars;
begin
  ResizeScrollBars;
  if FScrollBars in [ssBoth, ssVertical] then
    sbVert.PaintTo(Canvas);
  if FScrollBars in [ssBoth, ssHorizontal] then
    sbHorz.PaintTo(Canvas);
  if FScrollBars = ssBoth then
    with Canvas do
    begin
      Brush.Color := clSilver;
      FillRect(Rect(sbVert.Left, sbHorz.Top + 1, sbVert.Left + sbVert.Width, sbHorz.Top + sbHorz.Height));
    end;
end;

procedure TDGLSCustomMemo.FreshLineBitmap;
var
  eRect: TRect;
begin
  eRect := EditorRect;
  with FLineBitmap do
  begin
    Width  := eRect.Right - eRect.Left;
    Height := FCellSize.H;
    FLineBitmap.Canvas.Font.Assign(Self.Canvas.Font);
  end;
end;

procedure TDGLSCustomMemo.Paint;
var
  pTop, pBottom: TFullPos;
  rct, eRect:    TRect;
  i:             Integer;
  clipRgn:       HRGN;
  Attrs:         string;
begin
  if TDGLSMemoStrings(Lines).FLockCount > 0 then
    Exit;
  with Canvas do
  begin
    if FCellSize.H = 0 then
      SetFont(FFont);
    FreshLineBitmap;

    Border(Canvas, Rect(0, 0, Width, Height), btLowered);
    DrawMargin;
    DrawGutter;
    DrawScrollBars;

    eRect   := EditorRect;
    clipRgn := CreateRectRgn(eRect.Left, eRect.Top, eRect.Right, eRect.Bottom);
    ExtSelectClipRgn(Canvas.Handle, clipRgn, RGN_AND);
    DeleteObject(clipRgn);

    rct     := Canvas.ClipRect;
    pTop    := CharFromPos(rct.Left, rct.Top);
    pBottom := CharFromPos(rct.Left, rct.Bottom);

    if Assigned(FOnGetLineAttrs) then
      for i := 0 to Lines.Count - 1 do
        if not ValidAttrs[i] then
        begin
          FOnGetLineAttrs(Self, i, Attrs);
          CharAttrs[i]  := Attrs;
          ValidAttrs[i] := True;
        end;

    for i := pTop.LineNo to pBottom.LineNo do
      DrawLine(i);
  end;
end;

function TDGLSCustomMemo.GetVisible(Index: Integer): Integer;
var
  Coord: TFullPos;
  Cell:  TCellPos;
  eRect: TRect;
begin
  eRect := EditorRect;
  Coord := CharFromPos(eRect.Right - 1, eRect.Bottom - 1);
  Cell  := CellFromPos(eRect.Right - 1, eRect.Bottom - 1);
  case Index of
    0:
      Result := Cell.X;
    1:
      Result := Cell.Y;
    2:
      Result := Coord.Pos - 1;
    3:
      Result := Coord.LineNo - 1;
  else
    Result := 0;
  end;
end;

function TDGLSCustomMemo.IsLineVisible(LineNo: Integer): Boolean;
begin
  if FCellSize.H = 0 then
    SetFont(FFont);
  Result := (FTopLine <= LineNo) and (LineNo <= LastVisibleLine + 1);
end;

procedure TDGLSCustomMemo.MakeVisible;
var
  Modified: Boolean;
begin
  Modified := False;
  if CurX < FLeftCol then
  begin
    FLeftCol := CurX - 2;
    if FLeftCol < 0 then
      FLeftCol := 0;
    Modified   := True;
  end;
  if CurX > LastVisiblePos then
  begin
    if (FScrollBars in [ssBoth, ssHorizontal]) or (ScrollMode = smAuto) then
    begin
      FLeftCol := FLeftCol + CurX - LastVisiblePos + 2;
    end
    else
      CurX   := LastVisiblePos;
    Modified := True;
  end;
  if CurY < FTopLine then
  begin
    FTopLine := CurY;
    if FTopLine < 0 then
      FTopLine := 0;
    Modified   := True;
  end;
  if CurY > LastVisibleLine then
  begin
    if (FScrollBars in [ssBoth, ssVertical]) or (ScrollMode = smAuto) then
    begin
      FTopLine := FTopLine + CurY - LastVisibleLine;
    end
    else
      CurY   := LastVisibleLine;
    Modified := True;
  end;
  if Modified then
    Invalidate;
end;

procedure TDGLSCustomMemo.ResizeEditor;
begin
  ResizeScrollBars;
  ResizeGutter;
  MakeVisible;
  Invalidate;
end;

function TDGLSCustomMemo.FindText(Text: string; Options: TFindOptions; Select: Boolean): Boolean;
var
  i, P:      Integer;
  S1, s0, S: string;
  // -----------------------------------------------------------
  function LastPos(Substr, S: string): Integer;
  var
    i, j, lenSub: Integer;
  begin
    Result := 0;
    lenSub := Length(Substr);
    i      := Length(S) - lenSub + 1;
    while i > 0 do
    begin
      if S[i] = Substr[1] then
      begin
        Result := i;
        for j  := i + 1 to i + lenSub - 1 do
          if S[j] <> Substr[j - i + 1] then
          begin
            Result := 0;
            break;
          end;
      end;
      if Result <> 0 then
        break;
      Dec(i);
    end;
  end;

// -----------------------------------------------------------
begin
  Result := False;
  if not(frMatchCase in Options) then
    Text := AnsiLowerCase(Text);

  if SelLength > 0 then
    ClearSelection;
  S  := Lines[CurY];
  s0 := Copy(S, 1, CurX);
  S1 := Copy(S, CurX + 1, Length(S));
  i  := CurY;

  while True do
  begin

    if not(frMatchCase in Options) then
    begin
      s0 := AnsiLowerCase(s0);
      S1 := AnsiLowerCase(S1);
    end;

    if frDown in Options then
      P := Pos(Text, S1)
    else
      P := LastPos(Text, s0);

    if P > 0 then
    begin
      Result := True;
      CurY   := i;
      if frDown in Options then
        CurX := Length(s0) + P - 1
      else
        CurX := P - 1;
      if Select then
      begin
        if not(frDown in Options) then
          CurX := CurX + Length(Text);
        ClearSelection;
        if frDown in Options then
          CurX := CurX + Length(Text)
        else
          CurX := CurX - Length(Text);
        ExpandSelection;
      end;
      break;
    end;

    if frDown in Options then
      Inc(i)
    else
      Dec(i);
    if (i < 0) or (i > Lines.Count - 1) then
      break;
    if frDown in Options then
    begin
      s0 := '';
      S1 := Lines[i];
    end
    else
    begin
      s0 := Lines[i];
      S1 := '';
    end;

  end;

end;

procedure TDGLSCustomMemo.ResizeScrollBars;
var
  eRect, sbRect:                  TRect;
  MaxLen, OldMax, NewTop, Margin: Integer;
begin
  eRect := EditorRect;
  if FScrollBars in [ssBoth, ssVertical] then
  begin
    with sbVert do
    begin
      Width       := 16;
      Height      := eRect.Bottom - eRect.Top + 1;
      Left        := eRect.Right;
      Top         := eRect.Top;
      OldMax      := MaxPosition;
      MaxPosition := (Lines.Count - 1) - (LastVisibleLine - FTopLine);
      NewTop      := FTopLine;
      if (FTopLine > 0) and (LastVisibleLine > Lines.Count - 1) then
      begin
        Dec(NewTop, LastVisibleLine - (Lines.Count - 1));
        if NewTop < 0 then
          NewTop    := 0;
        MaxPosition := NewTop;
      end;
      if MaxPosition < 0 then
        MaxPosition := 0;
      Position      := NewTop;
      Total         := Lines.Count;
      if OldMax <> MaxPosition then
      begin
        if NewTop <> FTopLine then
        begin
          DoScroll(sbVert, NewTop - FTopLine);
          FGutter.Invalidate;
        end;
        sbRect := sbVert.FullRect;
        InvalidateRect(Handle, @sbRect, True);
      end;
    end;
  end;
  if FScrollBars in [ssBoth, ssHorizontal] then
  begin
    MaxLen := MaxLength;
    with sbHorz do
    begin
      Width := Self.Width - 4;
      if FScrollBars = ssBoth then
        Width := Width - sbVert.Width;
      Height  := 16;
      Left    := 2;
      Top     := eRect.Bottom;
      OldMax  := MaxPosition;

      Margin := LastVisiblePos - MaxLen;
      if Margin < 2 then
        Margin    := 2;
      MaxPosition := MaxLen - (LastVisiblePos - FLeftCol) + Margin;

      if MaxPosition < 0 then
        MaxPosition := 0;
      Position      := FLeftCol;
      Total         := MaxLen;
      if OldMax <> MaxPosition then
      begin
        if MaxPosition = 0 then
        begin
          FLeftCol := 0;
          InvalidateRect(Handle, @eRect, True);;
          FGutter.Invalidate;
        end;
        sbRect := sbHorz.FullRect;
        InvalidateRect(Handle, @sbRect, True);
      end;
    end;
  end;
end;

procedure TDGLSCustomMemo.ResizeGutter;
var
  eRect: TRect;
begin
  eRect := EditorRect;
  with FGutter do
  begin
    Height := eRect.Bottom - eRect.Top;
  end;
end;

procedure TDGLSCustomMemo.CreateParams(var Params: TCreateParams);
begin
  inherited;
end;

procedure TDGLSCustomMemo.Undo;
begin
  FUndoList.Undo;
end;

procedure TDGLSCustomMemo.Redo;
begin
  FUndoList.Redo;
end;

procedure TDGLSCustomMemo.SetUndoLimit(Value: Integer);
begin
  if (FUndoLimit <> Value) then
  begin
    if Value <= 0 then
      Value := 1;
    if Value > 100 then
      Value         := 100;
    FUndoLimit      := Value;
    FUndoList.Limit := Value;
  end;
end;

procedure TDGLSCustomMemo.UndoChange;
begin
  if Assigned(FOnUndoChange) then
    FOnUndoChange(Self, (FUndoList.Pos < FUndoList.Count), (FUndoList.Pos > 0));
end;

function TDGLSCustomMemo.CanUndo: Boolean;
begin
  Result := FUndoList.FPos < FUndoList.Count;
end;

function TDGLSCustomMemo.CanRedo: Boolean;
begin
  Result := FUndoList.FPos > 0;
end;

procedure TDGLSCustomMemo.ClearUndoList;
begin
  if Assigned(FUndoList) then
    FUndoList.Clear;
end;

procedure TDGLSCustomMemo.SetScrollBars(Value: System.UITypes.TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    if not(csLoading in ComponentState) then
      ResizeEditor;
  end;
end;

constructor TDGLSCustomMemo.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks, csReplicatable];
  Width        := 100;
  Height       := 40;
  TabStop      := True;
  Cursor       := crIBeam;

  FFont      := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 10;
  Canvas.Font.Assign(FFont);

  FHiddenCaret  := False;
  FCaretVisible := False;

  FCurX                          := 0;
  FCurY                          := 0;
  FLeftCol                       := 0;
  FTopLine                       := 0;
  FTabSize                       := 4;
  FMargin                        := 2;
  FAutoIndent                    := True;
  FLines                         := TDGLSMemoStrings.Create;
  TDGLSMemoStrings(FLines).FMemo := Self;

  FScrollBars     := ssBoth;
  FScrollBarWidth := 16;
  sbVert          := TDGLSMemoScrollBar.Create(Self, sbVertical);
  sbVert.Width    := FScrollBarWidth;
  sbHorz          := TDGLSMemoScrollBar.Create(Self, sbHorizontal);
  sbHorz.Height   := FScrollBarWidth;

  FGutter := TDGLSMemoGutter.Create;
  with FGutter do
  begin
    FLeft   := 2;
    FTop    := 2;
    FWidth  := 0;
    FHeight := 0;
    FColor  := clBtnFace;
    FMemo   := Self;
  end;

  FSelStartX := 0;
  FSelStartY := 0;
  FSelEndX   := 0;
  FSelEndY   := 0;

  FBkColor    := clWhite;
  FSelColor   := clWhite;
  FSelBkColor := clNavy;

  FStyles := TStyleList.Create;
  FStyles.Add(clBlack, clWhite, []);

  FSelCharPos.LineNo := -1;
  FSelCharPos.Pos    := -1;
  FSelCharStyle      := -1;

  FLineBitmap := TBitmap.Create;

  FLeftButtonDown := False;
  FScrollMode     := smAuto;

  FUndoList      := TDGLSMemoUndoList.Create;
  FFirstUndoList := FUndoList;
  FUndoList.Memo := Self;

  FUndoLimit := 100;

  TDGLSMemoStrings(FLines).DoAdd('');

  FAfterDoubleClick := False;

end;

destructor TDGLSCustomMemo.Destroy;
begin
  FFont.Free;
  FLines.Free;
  FGutter.Free;
  sbVert.Free;
  sbHorz.Free;
  FStyles.Free;
  FLineBitmap.Free;
  FFirstUndoList.Free;
  inherited;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
// ------------------
{ TDGLSMemoScrollBar functions }
{$IFDEF GLS_REGION}{$REGION 'TDGLSMemoScrollBar functions'}{$ENDIF}

procedure TDGLSMemoScrollBar.SetParams(Index: Integer; Value: Integer);
begin
  case Index of
    0:
      if Left <> Value then
        FLeft := Value;
    1:
      if Top <> Value then
        FTop := Value;
    2:
      if Width <> Value then
        FWidth := Value;
    3:
      if Height <> Value then
        FHeight := Value;
    4:
      if Total <> Value then
        FTotal := Value;
    5:
      if MaxPosition <> Value then
        FMaxPosition := Value;
    6:
      if Position <> Value then
        FPosition := Value;
  end;
end;

constructor TDGLSMemoScrollBar.Create(AParent: TDGLSMemoAbstractScrollableObject; AKind: TScrollBarKind);
begin
  FParent       := AParent;
  FButtonLength := 16;
  FKind         := AKind;
  FState        := sbsWait;
end;

function TDGLSMemoScrollBar.GetRect: TRect;
begin
  Result := Rect(Left, Top, Left + Width, Top + Height);
end;

function TDGLSMemoScrollBar.GetThumbRect: TRect;
var
  TotalLen, FreeLen, ThumbLen, ThumbOffset, ThumbCoord: Integer;
  k:                                                    double;
begin
  if MaxPosition <= 0 then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;
  if Kind = sbVertical then
    TotalLen := Height
  else
    TotalLen := Width;
  FreeLen    := TotalLen - 2 * FButtonLength;

  k := (Total - MaxPosition) / MaxPosition;
  if k > 0 then
  begin
    ThumbLen := round(FreeLen * k / (1 + k));
    if ThumbLen < 8 then
      ThumbLen := 8;
  end
  else
    ThumbLen := 8;

  if ThumbLen >= FreeLen then
    Result := Rect(0, 0, 0, 0)
  else
  begin
    ThumbOffset := round((FreeLen - ThumbLen) * Position / MaxPosition);
    ThumbCoord  := FButtonLength + ThumbOffset;
    if Kind = sbVertical then
      Result := Rect(Left + 1, Top + ThumbCoord, Left + Width, Top + ThumbCoord + ThumbLen)
    else
      Result := Rect(Left + ThumbCoord, Top + 1, Left + ThumbCoord + ThumbLen, Top + Height);
  end;
end;

function TDGLSMemoScrollBar.GetBackRect: TRect;
begin
  if Kind = sbVertical then
    Result := Rect(Left + 1, Top, Left + Width, Top + FButtonLength)
  else
    Result := Rect(Left, Top + 1, Left + FButtonLength, Top + Height);
end;

function TDGLSMemoScrollBar.GetMiddleRect: TRect;
var
  bRect, fRect: TRect;
begin
  bRect := BackRect;
  fRect := ForwardRect;
  if Kind = sbVertical then
    Result := Rect(Left + 1, bRect.Bottom, Left + Width, fRect.Top)
  else
    Result := Rect(bRect.Right, Top + 1, fRect.Left, Top + Height);
end;

function TDGLSMemoScrollBar.GetForwardRect: TRect;
begin
  if Kind = sbVertical then
    Result := Rect(Left + 1, Top + Height - FButtonLength, Left + Width, Top + Height)
  else
    Result := Rect(Left + Width - FButtonLength, Top + 1, Left + Width, Top + Height);
end;

function TDGLSMemoScrollBar.GetPgBackRect: TRect;
var
  thRect: TRect;
begin
  thRect := GetThumbRect;
  if thRect.Bottom = 0 then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;
  if Kind = sbVertical then
    Result := Rect(Left + 1, Top + FButtonLength, Left + Width, thRect.Top - 1)
  else
    Result := Rect(Left + FButtonLength, Top + 1, thRect.Left - 1, Top + Height);
end;

function TDGLSMemoScrollBar.GetPgForwardRect: TRect;
var
  thRect: TRect;
begin
  thRect := GetThumbRect;
  if thRect.Bottom = 0 then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;
  if Kind = sbVertical then
    Result := Rect(Left + 1, thRect.Bottom, Left + Width, Top + Height - FButtonLength)
  else
    Result := Rect(thRect.Right, Top + 1, Left + Width - FButtonLength, Top + Height);
end;

procedure TDGLSMemoScrollBar.PaintTo(ACanvas: TCanvas);
var
  sRect, mRect, gRect, thRect: TRect;
  iconX, iconY, Shift:         Integer;
begin
  with ACanvas do
  begin
    if Kind = sbVertical then
    begin
      Pen.Color := clSilver;
      MoveTo(Left, Top);
      LineTo(Left, Top + Height);

      sRect       := BackRect;
      Brush.Color := clSilver;
      FillRect(sRect);
      if State = sbsBack then
      begin
        Shift     := 1;
        Pen.Color := clGray;
        with sRect do
          Rectangle(Left, Top, Right, Bottom);
      end
      else
      begin
        Shift := 0;
        Border(ACanvas, sRect, btFlatRaised);
      end;
      iconX := sRect.Left + (Width - 1 - 7) div 2;
      iconY := sRect.Top + (FButtonLength - 8) div 2;
      Draw(iconX + Shift, iconY + Shift, bmScrollBarUp);

      gRect       := ForwardRect;
      Brush.Color := clSilver;
      FillRect(gRect);
      if State = sbsForward then
      begin
        Shift     := 1;
        Pen.Color := clGray;
        with gRect do
          Rectangle(Left, Top, Right, Bottom);
      end
      else
      begin
        Shift := 0;
        Border(ACanvas, gRect, btFlatRaised);
      end;
      iconX := gRect.Left + (Width - 1 - 7) div 2;
      iconY := gRect.Top + (FButtonLength - 8) div 2;
      Draw(iconX + Shift, iconY + Shift, bmScrollBarDown);

      mRect := Rect(sRect.Left, sRect.Bottom, gRect.Right, gRect.Top);
    end
    else
    begin
      Pen.Color := clSilver;
      MoveTo(Left, Top);
      LineTo(Left + Width, Top);

      sRect       := BackRect;
      Brush.Color := clSilver;
      FillRect(sRect);
      if State = sbsBack then
      begin
        Shift     := 1;
        Pen.Color := clGray;
        with sRect do
          Rectangle(Left, Top, Right, Bottom);
      end
      else
      begin
        Shift := 0;
        Border(ACanvas, sRect, btFlatRaised);
      end;
      iconX := sRect.Left + Shift + (FButtonLength - 8) div 2;
      iconY := sRect.Top + Shift + (Height - 1 - 7) div 2;
      Draw(iconX + Shift, iconY + Shift, bmScrollBarLeft);

      gRect       := ForwardRect;
      Brush.Color := clSilver;
      FillRect(gRect);
      if State = sbsForward then
      begin
        Shift     := 1;
        Pen.Color := clGray;
        with gRect do
          Rectangle(Left, Top, Right, Bottom);
      end
      else
      begin
        Shift := 0;
        Border(ACanvas, gRect, btFlatRaised);
      end;
      iconX := gRect.Left + (FButtonLength - 8) div 2;
      iconY := gRect.Top + (Height - 1 - 7) div 2;
      Draw(iconX + Shift, iconY + Shift, bmScrollBarRight);

      mRect := Rect(sRect.Right, sRect.Top, gRect.Left, gRect.Bottom);
    end;

    Brush.Bitmap := bmScrollBarFill;
    FillRect(mRect);
    Brush.Bitmap := nil;
    if State = sbsPageBack then
    begin
      Brush.Color := clGray;
      FillRect(PageBackRect);
    end;
    if State = sbsPageForward then
    begin
      Brush.Color := clGray;
      FillRect(PageForwardRect);
    end;

    thRect      := ThumbRect;
    Brush.Color := clSilver;
    FillRect(thRect);
    Border(ACanvas, thRect, btFlatRaised);
  end;
end;

procedure TDGLSMemoScrollBar.SetState(Value: TsbState);
begin
  if FState <> Value then
  begin
    FState := Value;
  end;
end;

function TDGLSMemoScrollBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  sRect, gRect, thRect, pbRect, pfRect: TRect;
begin
  Result := False;
  if (Width = 0) or (Height = 0) then
    Exit;
  sRect  := BackRect;
  gRect  := ForwardRect;
  pbRect := PageBackRect;
  pfRect := PageForwardRect;
  thRect := ThumbRect;
  if PointInRect(Point(X, Y), sRect) then
  begin
    State := sbsBack;
    InvalidateRect(Parent.Handle, @sRect, True);
    Result := True;
    Exit;
  end;
  if PointInRect(Point(X, Y), gRect) then
  begin
    State := sbsForward;
    InvalidateRect(Parent.Handle, @gRect, True);
    Result := True;
    Exit;
  end;
  if PointInRect(Point(X, Y), pbRect) then
  begin
    State := sbsPageBack;
    InvalidateRect(Parent.Handle, @pbRect, True);
    Result := True;
    Exit;
  end;
  if PointInRect(Point(X, Y), pfRect) then
  begin
    State := sbsPageForward;
    InvalidateRect(Parent.Handle, @pfRect, True);
    Result := True;
    Exit;
  end;
  if PointInRect(Point(X, Y), thRect) then
  begin
    State    := sbsDragging;
    FXOffset := X - thRect.Left;
    FYOffset := Y - thRect.Top;
    Result   := True;
    Exit;
  end;

end;

function TDGLSMemoScrollBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  sRect, gRect, thRect, pbRect, pfRect: TRect;
begin
  Result := False;
  if (Width = 0) or (Height = 0) then
    Exit;
  sRect  := BackRect;
  gRect  := ForwardRect;
  pbRect := PageBackRect;
  pfRect := PageForwardRect;
  thRect := ThumbRect;
  case State of
    sbsBack:
      begin
        State := sbsWait;
        InvalidateRect(Parent.Handle, @sRect, True);
        FParent.DoScroll(Self, -1);
        Result := True;
        Exit;
      end;
    sbsForward:
      begin
        State := sbsWait;
        InvalidateRect(Parent.Handle, @gRect, True);
        FParent.DoScroll(Self, 1);
        Result := True;
        Exit;
      end;
    sbsPageBack:
      begin
        State := sbsWait;
        InvalidateRect(Parent.Handle, @pbRect, True);
        FParent.DoScrollPage(Self, -1);
        Result := True;
        Exit;
      end;
    sbsPageForward:
      begin
        State := sbsWait;
        InvalidateRect(Parent.Handle, @pfRect, True);
        FParent.DoScrollPage(Self, 1);
        Result := True;
        Exit;
      end;
    sbsDragging:
      begin
        State  := sbsWait;
        Result := True;
        Exit;
      end;
  end;
end;

function TDGLSMemoScrollBar.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
var
  sRect, gRect, thRect, pbRect, pfRect: TRect;
begin
  Result := False;
  if (Width = 0) or (Height = 0) then
    Exit;
  sRect  := BackRect;
  gRect  := ForwardRect;
  pbRect := PageBackRect;
  pfRect := PageForwardRect;
  thRect := ThumbRect;
  case State of
    sbsBack:
      if not PointInRect(Point(X, Y), sRect) then
      begin
        State := sbsWait;
        InvalidateRect(Parent.Handle, @sRect, True);
        Result := True;
        Exit;
      end;
    sbsForward:
      if not PointInRect(Point(X, Y), gRect) then
      begin
        State := sbsWait;
        InvalidateRect(Parent.Handle, @gRect, True);
        Result := True;
        Exit;
      end;
    sbsPageBack:
      if not PointInRect(Point(X, Y), pbRect) then
      begin
        State := sbsWait;
        InvalidateRect(Parent.Handle, @pbRect, True);
        Result := True;
        Exit;
      end;
    sbsPageForward:
      if not PointInRect(Point(X, Y), pfRect) then
      begin
        State := sbsWait;
        InvalidateRect(Parent.Handle, @pfRect, True);
        Result := True;
        Exit;
      end;
    sbsDragging:
      begin
        MoveThumbTo(X, Y);
        Result := True;
        Exit;
      end;
  end;
end;

function TDGLSMemoScrollBar.MoveThumbTo(X, Y: Integer): Integer;
var
  thRect, mRect:                             TRect;
  FreeLen, ThumbLen, NewPosition, NewOffset: Integer;
begin
  thRect    := ThumbRect;
  mRect     := MiddleRect;
  NewOffset := 0;
  FreeLen   := 0;
  ThumbLen  := 0;
  case Kind of
    sbVertical:
      begin
        FreeLen   := mRect.Bottom - mRect.Top;
        ThumbLen  := thRect.Bottom - thRect.Top;
        NewOffset := Y - FYOffset - (Top + FButtonLength);
      end;
    sbHorizontal:
      begin
        FreeLen   := mRect.Right - mRect.Left;
        ThumbLen  := thRect.Right - thRect.Left;
        NewOffset := X - FXOffset - (Left + FButtonLength);
      end
  end;
  NewPosition := round(NewOffset * MaxPosition / (FreeLen - ThumbLen));
  Result      := NewPosition - Position;
  if NewPosition <> Position then
  begin
    Parent.DoScroll(Self, NewPosition - Position);
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
// ------------------
{ TDGLSMemoGutter }
{$IFDEF GLS_REGION}{$REGION 'TDGLSMemoGutter'}{$ENDIF}

procedure TDGLSMemoGutter.SetParams(Index: Integer; Value: Integer);
begin
  case Index of
    0:
      FLeft := Value;
    1:
      FTop := Value;
    2:
      FWidth := Value;
    3:
      FHeight := Value;
  end;
end;

procedure TDGLSMemoGutter.PaintTo(ACanvas: TCanvas);
var
  LineNo, t, H: Integer;
begin
  with ACanvas do
  begin
    Pen.Color := clGray;
    MoveTo(Left + Width - 1, Top);
    LineTo(Left + Width - 1, Top + Height);
    Pen.Color := clWhite;
    MoveTo(Left + Width - 2, Top);
    LineTo(Left + Width - 2, Top + Height);
    Brush.Color := Self.FColor;
    FillRect(Rect(Left, Top, Left + Width - 2, Top + Height));
    if Assigned(FMemo.OnGutterDraw) then
    begin
      t      := Top;
      H      := FMemo.FCellSize.H;
      LineNo := FMemo.FTopLine;
      while t < Top + Height do
      begin
        FMemo.OnGutterDraw(FMemo, ACanvas, LineNo, Rect(Left, t, Left + Width - 2, t + H));
        t := t + H;
        Inc(LineNo);
        if LineNo >= FMemo.Lines.Count then
          break;
      end;
    end;
  end;
end;

procedure TDGLSMemoGutter.Invalidate;
var
  gRect: TRect;
begin
  gRect := Rect(Left, Top, Left + Width, Top + Height);
  InvalidateRect(FMemo.Handle, @gRect, True);
end;

function TDGLSMemoGutter.GetRect: TRect;
begin
  Result := Rect(Left, Top, Left + Width, Top + Height);
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
// ------------------
{ TStyleList }
{$IFDEF GLS_REGION}{$REGION 'TStyleList'}{$ENDIF}

procedure TStyleList.CheckRange(Index: Integer);
begin
  if (Index < 0) or (Index >= Count) then
    raise EListError.Create('Incorrect list item index ' + IntToStr(Index));
end;

destructor TStyleList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TStyleList.Change(Index: Integer; ATextColor, ABkCOlor: TColor; AStyle: TFontStyles);
var
  P: TCharStyle;
begin
  CheckRange(Index);
  P           := TCharStyle(Items[Index]);
  P.TextColor := ATextColor;
  P.BkColor   := ABkCOlor;
  P.Style     := AStyle;
end;

function TStyleList.Add(ATextColor, ABkCOlor: TColor; AStyle: TFontStyles): Integer;
var
  P: TCharStyle;
begin
  P := TCharStyle.Create;
  with P do
  begin
    TextColor := ATextColor;
    BkColor   := ABkCOlor;
    Style     := AStyle;
  end;
  Result := inherited Add(P);
end;

procedure TStyleList.Clear;
begin
  while Count > 0 do
    Delete(0);
end;

procedure TStyleList.Delete(Index: Integer);
var
  P: TCharStyle;
begin
  CheckRange(Index);
  P := TCharStyle(Items[Index]);
  P.Free;
  inherited;
end;

function TStyleList.GetTextColor(Index: Integer): TColor;
begin
  CheckRange(Index);
  Result := TCharStyle(Items[Index]).TextColor;
end;

procedure TStyleList.SetTextColor(Index: Integer; Value: TColor);
begin
  CheckRange(Index);
  TCharStyle(Items[Index]).TextColor := Value;
end;

function TStyleList.GetBkColor(Index: Integer): TColor;
begin
  CheckRange(Index);
  Result := TCharStyle(Items[Index]).BkColor;
end;

procedure TStyleList.SetBkColor(Index: Integer; Value: TColor);
begin
  CheckRange(Index);
  TCharStyle(Items[Index]).BkColor := Value;
end;

function TStyleList.GetStyle(Index: Integer): TFontStyles;
begin
  CheckRange(Index);
  Result := TCharStyle(Items[Index]).Style;
end;

procedure TStyleList.SetStyle(Index: Integer; Value: TFontStyles);
begin
  CheckRange(Index);
  TCharStyle(Items[Index]).Style := Value;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
// ------------------
{ TDGLSMemoStrings }
{$IFDEF GLS_REGION}{$REGION 'TDGLSMemoStrings'}{$ENDIF}

destructor TDGLSMemoStrings.Destroy;
var
  P: TObject;
begin
  while Count > 0 do
  begin
    P := inherited GetObject(0);
    P.Free;
    inherited Delete(0);
  end;
  inherited;
end;

procedure TDGLSMemoStrings.Clear;
begin
  while Count > 0 do
  begin
    Delete(0);
    if (Count = 1) and (Strings[0] = '') then
      break;
  end;
end;

procedure TDGLSMemoStrings.Assign(Source: TPersistent);
var
  P: TObject;
begin
  if Source is TStrings then
  begin
    BeginUpdate;
    try
      while Count > 0 do
      begin
        P := inherited GetObject(0);
        P.Free;
        inherited Delete(0);
      end;
      // inherited Clear;
      AddStrings(TStrings(Source));
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

function TDGLSMemoStrings.DoAdd(const S: string): Integer;
begin
  Result := inherited AddObject(S, nil);
end;

function TDGLSMemoStrings.Add(const S: string): Integer;
begin
  if Assigned(FMemo.Parent) then
    Result := FMemo.AddString(S)
  else
    Result := DoAdd(S);
end;

function TDGLSMemoStrings.AddObject(const S: string; AObject: TObject): Integer;
begin
  if AObject <> nil then
    raise EInvalidOp.Create(SObjectsNotSupported);
  Result := DoAdd(S);
end;

procedure TDGLSMemoStrings.InsertObject(Index: Integer; const S: string; AObject: TObject);
begin
  if AObject <> nil then
    raise EInvalidOp.Create(SObjectsNotSupported);
  DoInsert(Index, S);
end;

procedure TDGLSMemoStrings.DoInsert(Index: Integer; const S: string);
begin
  InsertItem(Index, S, nil);
end;

procedure TDGLSMemoStrings.Insert(Index: Integer; const S: string);
begin
  if Assigned(FMemo) then
    FMemo.InsertString(Index, S)
  else
    DoInsert(Index, S);
end;

procedure TDGLSMemoStrings.Delete(Index: Integer);
var
  P: TObject;
begin
  if (Index < 0) or (Index > Count - 1) then
    Exit;
  if FDeleting or (not Assigned(FMemo)) then
  begin
    P := inherited GetObject(Index);
    P.Free;
    inherited;
  end
  else
  begin
    FMemo.DeleteLine(Index, -1, -1, -1, -1, True);
  end;
end;

procedure TDGLSMemoStrings.LoadFromFile(const FileName: string);
begin
  with FMemo do
  begin
    ClearSelection;
    ClearUndoList;
    CurX := 0;
    CurY := 0;
  end;
  Clear;
  inherited;
  FMemo.Invalidate;
end;

procedure TDGLSMemoStrings.SetUpdateState(Updating: Boolean);
begin
  if Updating then
    Inc(FLockCount)
  else if FLockCount > 0 then
    Dec(FLockCount);
end;

procedure TDGLSMemoStrings.CheckRange(Index: Integer);
begin
  if (Index < 0) or (Index >= Count) then
    raise EListError('Incorrect index of list item ' + IntToStr(Index));
end;

function TDGLSMemoStrings.GetObject(Index: Integer): TObject;
begin
  CheckRange(Index);
  Result := inherited GetObject(Index);
  if Assigned(Result) and (Result is TLineProp) then
    Result := TLineProp(Result).FObject;
end;

procedure TDGLSMemoStrings.PutObject(Index: Integer; AObject: TObject);
var
  P: TObject;
begin
  CheckRange(Index);
  P := Objects[Index];
  if Assigned(P) and (P is TLineProp) then
    TLineProp(P).FObject := AObject
  else
    inherited PutObject(Index, AObject);
end;

function TDGLSMemoStrings.GetLineProp(Index: Integer): TLineProp;
var
  P: TObject;
begin
  CheckRange(Index);
  Result := nil;
  P      := inherited GetObject(Index);
  if Assigned(P) and (P is TLineProp) then
    Result := TLineProp(P);
end;

function TDGLSMemoStrings.CreateProp(Index: Integer): TLineProp;
begin
  Result := TLineProp.Create;
  with Result do
  begin
    FStyleNo    := 0;
    FInComment  := False;
    FInBrackets := -1;
    FValidAttrs := False;
    FCharAttrs  := '';
    FObject     := Objects[Index];
  end;
  inherited PutObject(Index, Result);
end;

function TDGLSMemoStrings.GetLineStyle(Index: Integer): Integer;
var
  P: TLineProp;
begin
  P := LineProp[Index];
  if P = nil then
    Result := 0
  else
    Result := P.FStyleNo;
end;

procedure TDGLSMemoStrings.SetLineStyle(Index: Integer; Value: Integer);
var
  P: TLineProp;
begin
  P := LineProp[Index];
  if P = nil then
    P        := CreateProp(Index);
  P.FStyleNo := Value;
end;

function TDGLSMemoStrings.GetInComment(Index: Integer): Boolean;
var
  P: TLineProp;
begin
  P := LineProp[Index];
  if P = nil then
    Result := False
  else
    Result := P.FInComment;
end;

procedure TDGLSMemoStrings.SetInComment(Index: Integer; Value: Boolean);
var
  P: TLineProp;
begin
  P := LineProp[Index];
  if P = nil then
    P          := CreateProp(Index);
  P.FInComment := Value;
end;

function TDGLSMemoStrings.GetInBrackets(Index: Integer): Integer;
var
  P: TLineProp;
begin
  P := LineProp[Index];
  if P = nil then
    Result := -1
  else
    Result := P.FInBrackets;
end;

procedure TDGLSMemoStrings.SetInBrackets(Index: Integer; Value: Integer);
var
  P: TLineProp;
begin
  P := LineProp[Index];
  if P = nil then
    P           := CreateProp(Index);
  P.FInBrackets := Value;
end;

function TDGLSMemoStrings.GetValidAttrs(Index: Integer): Boolean;
var
  P: TLineProp;
begin
  P := LineProp[Index];
  if P = nil then
    Result := False
  else
    Result := P.FValidAttrs;
end;

procedure TDGLSMemoStrings.SetValidAttrs(Index: Integer; Value: Boolean);
var
  P: TLineProp;
begin
  P := LineProp[Index];
  if P = nil then
    P           := CreateProp(Index);
  P.FValidAttrs := Value;
end;

function TDGLSMemoStrings.GetCharAttrs(Index: Integer): string;
var
  P: TLineProp;
begin
  P := LineProp[Index];
  if P = nil then
    Result := ''
  else
    Result := P.FCharAttrs;
end;

procedure TDGLSMemoStrings.SetCharAttrs(Index: Integer; Value: string);
var
  P: TLineProp;
begin
  P := LineProp[Index];
  if P = nil then
    P          := CreateProp(Index);
  P.FCharAttrs := Value;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
// ------------------
{ TDGLSMemoUndo }
{$IFDEF GLS_REGION}{$REGION 'TDGLSMemoUndo'}{$ENDIF}

constructor TDGLSMemoUndo.Create(ACurX0, ACurY0, ACurX, ACurY: Integer; AText: string);
begin
  inherited Create;
  FUndoCurX0 := ACurX0;
  FUndoCurY0 := ACurY0;
  FUndoCurX  := ACurX;
  FUndoCurY  := ACurY;
  FUndoText  := AText;
end;

procedure TDGLSMemoUndo.Undo;
begin
  if Assigned(FMemo) then
    with FMemo do
    begin
      CurY := FUndoCurY;
      CurX := FUndoCurX;
      PerformUndo;
      CurY := FUndoCurY0;
      CurX := FUndoCurX0;
    end;
end;

procedure TDGLSMemoUndo.Redo;
begin
  if Assigned(FMemo) then
    with FMemo do
    begin
      CurY := FUndoCurY0;
      CurX := FUndoCurX0;
      PerformRedo;
      CurY := FUndoCurY;
      CurX := FUndoCurX;
    end;
end;

function TDGLSMemoUndo.Append(NewUndo: TDGLSMemoUndo): Boolean;
begin
  Result := False;
end;

procedure TDGLSMemoInsCharUndo.PerformUndo;
var
  i:        Integer;
  CurrLine: string;
begin
  for i := Length(FUndoText) downto 1 do
  begin
    CurrLine := FMemo.Lines[FMemo.CurY];
    if ((FUndoText[i] = #13) and (FMemo.CurX = 0)) or (FUndoText[i] = CurrLine[FMemo.CurX]) then
      FMemo.BackSpace;
  end;
end;

procedure TDGLSMemoInsCharUndo.PerformRedo;
var
  i: Integer;
begin
  with FMemo do
    for i := 1 to Length(FUndoText) do
      if FUndoText[i] = #13 then
        NewLine
      else
        InsertChar(FUndoText[i]);
end;

function TDGLSMemoInsCharUndo.Append(NewUndo: TDGLSMemoUndo): Boolean;
begin
  Result := False;
  if not((NewUndo is TDGLSMemoInsCharUndo) and (NewUndo.UndoCurX0 = FUndoCurX) and (NewUndo.UndoCurY0 = FUndoCurY)) then
    Exit;
  FUndoText := FUndoText + NewUndo.FUndoText;
  FUndoCurX := NewUndo.UndoCurX;
  FUndoCurY := NewUndo.UndoCurY;
  Result    := True;
end;

procedure TDGLSMemoDelCharUndo.PerformUndo;
var
  i: Integer;
begin
  with FMemo do
    for i := 1 to Length(FUndoText) do
    begin
      if not FIsBackspace then
      begin
        CurY := FUndoCurY0;
        CurX := FUndoCurX0;
      end;
      if FUndoText[i] = #13 then
        NewLine
      else
        InsertChar(FUndoText[i]);
    end;
end;

procedure TDGLSMemoDelCharUndo.PerformRedo;
var
  i: Integer;
begin
  with FMemo do
    for i := 1 to Length(FUndoText) do
      if FIsBackspace then
        BackSpace
      else
        DeleteChar(-1, -1);
end;

function TDGLSMemoDelCharUndo.Append(NewUndo: TDGLSMemoUndo): Boolean;
begin
  Result := False;
  if not((NewUndo is TDGLSMemoDelCharUndo) and (NewUndo.UndoCurX0 = FUndoCurX) and (NewUndo.UndoCurY0 = FUndoCurY)) then
    Exit;
  if TDGLSMemoDelCharUndo(NewUndo).FIsBackspace <> FIsBackspace then
    Exit;
  FUndoText := NewUndo.FUndoText + FUndoText;
  FUndoCurX := NewUndo.UndoCurX;
  FUndoCurY := NewUndo.UndoCurY;
  Result    := True;
end;

constructor TDGLSMEmoDelLineUndo.Create(AIndex, ACurX0, ACurY0, ACurX, ACurY: Integer; AText: string);
begin
  inherited Create(ACurX0, ACurY0, ACurX, ACurY, AText);
  FIndex := AIndex;
end;

procedure TDGLSMEmoDelLineUndo.PerformUndo;
var
  SaveCurX: Integer;
begin
  with FMemo do
  begin
    SaveCurX := CurX;
    CurX     := 0;
    ClearSelection;
    SetSelText(PChar(FUndoText + #13#10));
    CurX := SaveCurX;
  end;
end;

procedure TDGLSMEmoDelLineUndo.PerformRedo;
begin
  FMemo.DeleteLine(FIndex, FUndoCurX0, FUndoCurY0, FUndoCurX, FUndoCurY, True);
end;

procedure TDGLSMemoDeleteBufUndo.PerformUndo;
begin
  with FMemo do
  begin
    ClearSelection;
    SetSelText(PChar(FUndoText));
  end;
end;

procedure TDGLSMemoDeleteBufUndo.PerformRedo;
begin
  with FMemo do
  begin
    FSelStartX := FUndoSelStartX;
    FSelStartY := FUndoSelStartY;
    FSelEndX   := FUndoSelEndX;
    FSelEndY   := FUndoSelEndY;
    DeleteSelection(True);
  end;
end;

procedure TDGLSMemoPasteUndo.PerformUndo;
begin
  with FMemo do
  begin
    FSelStartX := FUndoCurX0;
    FSelStartY := FUndoCurY0;
    FSelEndX   := FUndoCurX;
    FSelEndY   := FUndoCurY;
    DeleteSelection(True);
  end;
end;

procedure TDGLSMemoPasteUndo.PerformRedo;
begin
  with FMemo do
  begin
    ClearSelection;
    SetSelText(PChar(FUndoText));
  end;
end;

constructor TDGLSMemoUndoList.Create;
begin
  inherited;
  FPos          := 0;
  FIsPerforming := False;
  FLimit        := 100;
end;

destructor TDGLSMemoUndoList.Destroy;
begin
  Clear;
  inherited;
end;

function TDGLSMemoUndoList.Get(Index: Integer): TDGLSMemoUndo;
begin
  Result := TDGLSMemoUndo(inherited Get(Index));
end;

function TDGLSMemoUndoList.Add(Item: Pointer): Integer;
begin
  Result := -1;
  if FIsPerforming then
  begin
    TDGLSMemoUndo(Item).Free;
    Exit;
  end;

  if (Count > 0) and Items[0].Append(TDGLSMemoUndo(Item)) then
  begin
    TDGLSMemoUndo(Item).Free;
    Exit;
  end;

  TDGLSMemoUndo(Item).FMemo := Self.FMemo;
  if FPos > 0 then
    while FPos > 0 do
    begin
      Delete(0);
      Dec(FPos);
    end;
  Insert(0, Item);
  if Count > FLimit then
    Delete(Count - 1);
  Memo.UndoChange;
  Result := 0;
end;

procedure TDGLSMemoUndoList.Clear;
begin
  while Count > 0 do
    Delete(0);
  FPos := 0;
  with Memo do
    if not(csDestroying in ComponentState) then
      UndoChange;
end;

procedure TDGLSMemoUndoList.Delete(Index: Integer);
begin
  TDGLSMemoUndo(Items[Index]).Free;
  inherited;
end;

procedure TDGLSMemoUndoList.Undo;
var
  OldAutoIndent: Boolean;
begin
  if FPos < Count then
  begin
    OldAutoIndent   := Memo.AutoIndent;
    Memo.AutoIndent := False;
    FIsPerforming   := True;
    Items[FPos].Undo;
    Inc(FPos);
    FIsPerforming   := False;
    Memo.AutoIndent := OldAutoIndent;
    Memo.UndoChange;
  end;
end;

procedure TDGLSMemoUndoList.Redo;
var
  OldAutoIndent: Boolean;
begin
  if FPos > 0 then
  begin
    OldAutoIndent   := Memo.AutoIndent;
    Memo.AutoIndent := False;
    FIsPerforming   := True;
    Dec(FPos);
    Items[FPos].Redo;
    FIsPerforming   := False;
    Memo.AutoIndent := OldAutoIndent;
    Memo.UndoChange;
  end;
end;

procedure TDGLSMemoUndoList.SetLimit(Value: Integer);
begin
  if FLimit <> Value then
  begin
    if Value <= 0 then
      Value := 10;
    if Value > 0 then
      Value := 100;
    FLimit  := Value;
    Clear;
  end;
end;

procedure TDGLSynHiMemo.Paint;
begin
  FIsPainting := True;
  try
    DelimiterStyle := FDelimiterStyle;
    CommentStyle   := FCommentStyle;
    NumberStyle    := FNumberStyle;
    inherited;
  finally
    FIsPainting := False;
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
// ------------------
{ TDGLSynHiMemo }
{$IFDEF GLS_REGION}{$REGION 'TDGLSynHiMemo'}{$ENDIF}

procedure TDGLSynHiMemo.SetStyle(Index: Integer; Value: TCharStyle);
var
  No:    Integer;
  eRect: TRect;
begin
  No := -1;
  case Index of
    0:
      No := FDelimiterStyleNo;
    1:
      No := FCommentStyleNo;
    2:
      No := FNumberStyleNo;
  end;
  with Value do
    Styles.Change(No, TextColor, BkColor, Style);
  if not FIsPainting then
  begin
    eRect := EditorRect;
    InvalidateRect(Handle, @eRect, True);
  end;
end;

procedure TDGLSynHiMemo.SetWordList(Value: TDGLSMemoStringList);
begin
  FWordList.Assign(Value);
end;

procedure TDGLSynHiMemo.SetSpecialList(Value: TDGLSMemoStringList);
begin
  FSpecialList.Assign(Value);
end;

procedure TDGLSynHiMemo.SetBracketList(Value: TDGLSMemoStringList);
begin
  FBracketList.Assign(Value);
end;

procedure TDGLSynHiMemo.SetCaseSensitive(Value: Boolean);
var
  LineNo: Integer;
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive       := Value;
    for LineNo           := 0 to Lines.Count - 1 do
      ValidAttrs[LineNo] := False;
    Invalidate;
  end;
end;

function TDGLSynHiMemo.GetToken(S: string; var From: Integer; var TokenType: TTokenType; var StyleNo: Integer): string;
var
  i, toStart, toEnd, len, LenSpec: Integer;
  Done:                            Boolean;
  Brackets:                        string;
  IntPart:                         Integer;
  WasPoint:                        Boolean;
  // -------------------------------------------------------------
  function StartsFrom(S: string; Pos: Integer; s0: string): Boolean;
  begin
    Result := (StrLComp(PChar(S) + Pos - 1, PChar(s0), Length(s0)) = 0);
  end;
// -------------------------------------------------------------
  function Equal(S1, S2: string): Boolean;
  begin
    if FCaseSensitive then
      Result := S1 = S2
    else
      Result := AnsiLowerCase(S1) = AnsiLowerCase(S2);
  end;

begin
  toStart   := From;
  toEnd     := From;
  TokenType := ttOther;
  StyleNo   := 0;
  len       := Length(S);
  // End of line
  if From > len then
  begin
    From      := -1;
    Result    := '';
    TokenType := ttEOL;
    StyleNo   := 0;
    Exit;
  end;
  // Begin of multiline comment
  if (MultiCommentLeft <> '') and (MultiCommentRight <> '') and StartsFrom(S, From, MultiCommentLeft) then
  begin
    Result     := MultiCommentLeft;
    FInComment := True;
    TokenType  := ttComment;
    StyleNo    := FCommentStyleNo;
    Inc(From, Length(MultiCommentLeft));
    Exit;
  end;
  // Inside multiline comment
  if FInComment then
  begin
    toEnd := toStart;
    while (toEnd <= len) and (not StartsFrom(S, toEnd, MultiCommentRight)) do
      Inc(toEnd);
    if toEnd > len then
    begin
      Result := Copy(S, From, toEnd - From);
      From   := toEnd;
    end
    else
    begin
      FInComment := False;
      toEnd      := toEnd + Length(MultiCommentRight);
      Result     := Copy(S, From, toEnd - From);
      From       := toEnd;
    end;
    TokenType := ttComment;
    StyleNo   := FCommentStyleNo;
    Exit;
  end;

  // Inside brikets
  if FInBrackets >= 0 then
  begin
    Brackets := FBracketList[FInBrackets];
    toEnd    := toStart + 1;
    while (toEnd <= len) and (S[toEnd] <> Brackets[2]) do
      Inc(toEnd);
    StyleNo := Integer(FBracketList.Objects[FInBrackets]);
    if toEnd <= len then
    begin
      FInBrackets := -1;
      From        := toEnd + 1;
    end
    else
      From    := toEnd;
    Result    := Copy(S, toStart, toEnd - toStart + 1);
    TokenType := ttBracket;
    Exit;
  end;
  // Spaces
  while (toStart <= len) and (S[toStart] = ' ') do
    Inc(toStart);
  if toStart > From then
  begin
    Result    := Copy(S, From, toStart - From);
    From      := toStart;
    TokenType := ttSpace;
    StyleNo   := 0;
    Exit;
  end;
  // Comment
  if (FLineComment <> '') and StartsFrom(S, From, FLineComment) then
  begin
    Result    := Copy(S, From, len);
    From      := len + 1;
    TokenType := ttComment;
    StyleNo   := FCommentStyleNo;
    Exit;
  end;

  // Special keyword
  Done  := False;
  for i := 0 to FSpecialList.Count - 1 do
  begin
    LenSpec := Length(FSpecialList[i]);
    if StrLComp(PChar(S) + toStart - 1, PChar(FSpecialList[i]), LenSpec) = 0 then
    begin
      toEnd     := toStart + LenSpec - 1;
      StyleNo   := Integer(FSpecialList.Objects[i]);
      TokenType := ttSpecial;
      From      := toEnd + 1;
      Done      := True;
      break;
    end;
  end;
  // Brickets
  if not Done then
  begin
    for i := 0 to FBracketList.Count - 1 do
    begin
      Brackets := FBracketList[i];
      if S[toStart] = Brackets[1] then
      begin
        FInBrackets := i;
        toEnd       := toStart + 1;
        while (toEnd <= len) and (S[toEnd] <> Brackets[2]) do
          Inc(toEnd);
        if toEnd <= len then
          FInBrackets := -1
        else
          Dec(toEnd);
        StyleNo   := Integer(FBracketList.Objects[i]);
        TokenType := ttBracket;
        Done      := True;
        break;
      end;
    end;
  end;
  // Delimeters
  if not Done and CharInSet(S[toStart], Delimiters) then
  begin
    toEnd     := toStart;
    StyleNo   := FDelimiterStyleNo;
    TokenType := ttDelimiter;
    Done      := True;
  end;
  // --- Integer or float type
  if not Done and CharInSet(S[toStart], ['0' .. '9', '.']) then
  begin
    IntPart   := 0;
    WasPoint  := False;
    toEnd     := toStart;
    Done      := True;
    TokenType := ttInteger;
    StyleNo   := FNumberStyleNo;
    while (toEnd <= len) and CharInSet(S[toEnd], ['0' .. '9', '.']) do
    begin
      if S[toEnd] = '.' then
      begin
        if not WasPoint then
        begin
          WasPoint  := True;
          TokenType := ttFloat;
        end
        else
        begin
          TokenType := ttWrongNumber;
          Color     := clRed;
        end;
      end
      else if not WasPoint then
        try
          IntPart := IntPart * 10 + Ord(S[toEnd]) - Ord('0');
        except
          IntPart := MaxInt;
        end;
      Inc(toEnd);
    end;
    Dec(toEnd);
  end;
  // Select word
  if not Done then
  begin
    toEnd := toStart;
    while (toEnd <= len) and not CharInSet(S[toEnd], Delimiters) do
      Inc(toEnd);
    Dec(toEnd);
  end;
  // Find in dictionary
  Result := Copy(S, toStart, toEnd - toStart + 1);
  for i  := 0 to FWordList.Count - 1 do
    if Equal(Result, FWordList[i]) then
    begin
      StyleNo   := Integer(FWordList.Objects[i]);
      TokenType := ttWord;
      break;
    end;
  From := toEnd + 1;
end;

procedure TDGLSynHiMemo.FindLineAttrs(Sender: TObject; LineNo: Integer; var Attrs: string);
var
  i, From, TokenLen:      Integer;
  S, Token:               string;
  TokenType:              TTokenType;
  StyleNo, OldInBrackets: Integer;
  OldInComment:           Boolean;
begin
  S := Lines[LineNo];
  SetLength(Attrs, Length(S));
  FInComment  := InComment[LineNo];
  FInBrackets := InBrackets[LineNo];
  From        := 1;
  while True do
  begin
    Token := GetToken(S, From, TokenType, StyleNo);
    if TokenType = ttEOL then
      break;
    TokenLen   := Length(Token);
    for i      := From - TokenLen to From - 1 do
      Attrs[i] := Char(StyleNo);
  end;
  if LineNo < Lines.Count - 1 then
  begin
    OldInComment  := InComment[LineNo + 1];
    OldInBrackets := InBrackets[LineNo + 1];
    if OldInComment <> FInComment then
    begin
      InComment[LineNo + 1]  := FInComment;
      ValidAttrs[LineNo + 1] := False;
    end;
    if OldInBrackets <> FInBrackets then
    begin
      InBrackets[LineNo + 1] := FInBrackets;
      ValidAttrs[LineNo + 1] := False;
    end;
  end;
end;

procedure TDGLSynHiMemo.AddWord(StyleNo: Integer; ArrS: array of string);
var
  i: Integer;
begin
  for i := Low(ArrS) to high(ArrS) do
    FWordList.AddObject(ArrS[i], TObject(StyleNo));
end;

procedure TDGLSynHiMemo.AddSpecial(StyleNo: Integer; ArrS: array of string);
var
  i: Integer;
begin
  for i := Low(ArrS) to high(ArrS) do
    FSpecialList.AddObject(ArrS[i], TObject(StyleNo));
end;

procedure TDGLSynHiMemo.AddBrackets(StyleNo: Integer; ArrS: array of string);
var
  i: Integer;
begin
  for i := Low(ArrS) to high(ArrS) do
    FBracketList.AddObject(ArrS[i], TObject(StyleNo));
end;

constructor TDGLSynHiMemo.Create(AOwner: TComponent);
begin
  inherited;
  FInBrackets  := -1;
  FIsPainting  := False;
  FInComment   := False;
  FWordList    := TDGLSMemoStringList.Create;
  FSpecialList := TDGLSMemoStringList.Create;
  FBracketList := TDGLSMemoStringList.Create;

  FDelimiterStyle := TCharStyle.Create;
  with FDelimiterStyle do
  begin
    TextColor := clBlue;
    BkColor   := clWhite;
    Style     := [];
  end;

  FCommentStyle := TCharStyle.Create;
  with FCommentStyle do
  begin
    TextColor := clYellow;
    BkColor   := clSkyBlue;
    Style     := [fsItalic];
  end;

  FNumberStyle := TCharStyle.Create;
  with FNumberStyle do
  begin
    TextColor := clNavy;
    BkColor   := clWhite;
    Style     := [fsBold];
  end;

  FDelimiterStyleNo := Styles.Add(clBlue, clWhite, []);
  FCommentStyleNo   := Styles.Add(clSilver, clWhite, [fsItalic]);
  FNumberStyleNo    := Styles.Add(clNavy, clWhite, [fsBold]);

  OnGetLineAttrs := FindLineAttrs;
  Delimiters     := [' ', ',', ';', ':', '.', '(', ')', '{', '}', '[', ']', '=', '+', '-', '*', '/', '^', '%', '<', '>', '"', '''', #13, #10];
end;

destructor TDGLSynHiMemo.Destroy;
begin
  FWordList.Free;
  FSpecialList.Free;
  FBracketList.Free;
  FDelimiterStyle.Free;
  FCommentStyle.Free;
  FNumberStyle.Free;
  inherited;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
// ------------------
{ TDGLSMemoStringList }
{$IFDEF GLS_REGION}{$REGION 'TDGLSMemoStringList'}{$ENDIF}

procedure TDGLSMemoStringList.ReadStrings(Reader: TReader);
var
  i: Integer;
begin
  try
    Reader.ReadListBegin;
    Clear;
    while not Reader.EndOfList do
    begin
      i          := Add(Reader.ReadString);
      Objects[i] := TObject(Reader.ReadInteger);
    end;
    Reader.ReadListEnd;
  finally
  end;
end;

// --------------------------------------------------------------
// STRING LIST - WRITE STRINGS
// --------------------------------------------------------------

procedure TDGLSMemoStringList.WriteStrings(Writer: TWriter);
var
  i: Integer;
begin
  with Writer do
  begin
    WriteListBegin;
    for i := 0 to Count - 1 do
    begin
      WriteString(Strings[i]);
      WriteInteger(Integer(Objects[i]));
    end;
    WriteListEnd;
  end;
end;

// --------------------------------------------------------------
// STRING LIST - DEFINE PROPERTIES
// --------------------------------------------------------------

procedure TDGLSMemoStringList.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Strings', ReadStrings, WriteStrings, Count > 0);
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
// ------------------
{ ScrollBar bitmaps }
{$IFDEF GLS_REGION}{$REGION 'ScrollBar bitmaps'}{$ENDIF}

procedure CreateScrollBarBitmaps;
var
  i, j: Integer;
begin
  bmScrollBarFill := TBitmap.Create;
  with bmScrollBarFill, Canvas do
  begin
    Width       := 8;
    Height      := 8;
    Transparent := False;
    for i       := 0 to 7 do
      for j     := 0 to 7 do
        if Odd(i + j) then
          Pixels[i, j] := clSilver;
  end;

  bmScrollBarUp := TBitmap.Create;
  with bmScrollBarUp, Canvas do
  begin
    Width       := 7;
    Height      := 8;
    Brush.Color := clSilver;
    FillRect(Rect(0, 0, Width, Height));
    Pixels[3, 2] := clBlack;
    MoveTo(2, 3);
    LineTo(5, 3);
    MoveTo(1, 4);
    LineTo(6, 4);
    MoveTo(0, 5);
    LineTo(7, 5);
  end;

  bmScrollBarDown := TBitmap.Create;
  with bmScrollBarDown, Canvas do
  begin
    Width       := 7;
    Height      := 8;
    Brush.Color := clSilver;
    FillRect(Rect(0, 0, Width, Height));
    MoveTo(0, 2);
    LineTo(7, 2);
    MoveTo(1, 3);
    LineTo(6, 3);
    MoveTo(2, 4);
    LineTo(5, 4);
    Pixels[3, 5] := clBlack;
  end;

  bmScrollBarLeft := TBitmap.Create;
  with bmScrollBarLeft, Canvas do
  begin
    Width       := 8;
    Height      := 7;
    Brush.Color := clSilver;
    FillRect(Rect(0, 0, Width, Height));
    Pixels[2, 3] := clBlack;
    MoveTo(3, 2);
    LineTo(3, 5);
    MoveTo(4, 1);
    LineTo(4, 6);
    MoveTo(5, 0);
    LineTo(5, 7);
  end;

  bmScrollBarRight := TBitmap.Create;
  with bmScrollBarRight, Canvas do
  begin
    Width       := 8;
    Height      := 7;
    Brush.Color := clSilver;
    FillRect(Rect(0, 0, Width, Height));
    MoveTo(2, 0);
    LineTo(2, 7);
    MoveTo(3, 1);
    LineTo(3, 6);
    MoveTo(4, 2);
    LineTo(4, 5);
    Pixels[5, 3] := clBlack;
  end;

end;
// ------------------ FREE SCROLL BAR BITMAPs -------------------

procedure FreeScrollBarBitmaps;
begin
  bmScrollBarFill.Free;
  bmScrollBarUp.Free;
  bmScrollBarDown.Free;
  bmScrollBarLeft.Free;
  bmScrollBarRight.Free;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

RegisterClasses([TDGLSynHiMemo]);
CreateScrollBarBitmaps;
IntelliMouseInit;

finalization

FreeScrollBarBitmaps;

end.
