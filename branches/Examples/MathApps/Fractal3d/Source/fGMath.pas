{*******************************************************}
{                                                       }
{                      Tachyon Unit                     }
{    Vector Raster Geographic Information Synthesis     }
{                     VOICE  ..  Tracer                 }
{                     GRIP ICE .. Tongs                 }
{                Digital Terrain Mapping                }
{               Image Locatable Holographics            }
{                          SOS MAP                      }
{  Surreal Object Synthesis Multimedia Analysis Product }
{                   Fractal3D  Life MOW                 }
{       Copyright (c) 1995,2006  Ivan Lee Herring       }
{                                                       }
{*******************************************************}
unit fGMath;
{MyPixelFormat; pf24bit;    DIYFilename}
{in the Drawing...
bRotateImage:=False;
Repeat Application.ProcessMessages until (bRotateImage=True)}
{
1: For i := 0 to 255 do begin Colors[0,i]:=0; Colors[1,i]:=0;Colors[2,i]:=255-i;
ColorArray[i]:=RGB(Colors[0,i],Colors[1,i],Colors[2,i]);
}
interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Buttons,
  ColorGrd, ExtDlgs, Spin;

type
  TMathForm = class(TForm)
    PageControl1: TPageControl;
    AttractorsTS: TTabSheet;
    PopulationTS: TTabSheet;
    CurvesTS: TTabSheet;
    MGARG: TRadioGroup;
    MGPRG: TRadioGroup;
    MGCVKRG: TRadioGroup;
    MGCPRG: TRadioGroup;
    MGCCSRG: TRadioGroup;
    MGCHRG: TRadioGroup;
    MGCSRG: TRadioGroup;
    MGCDCRG: TRadioGroup;
    IteratedFunctionsTS: TTabSheet;
    MGIFRG: TRadioGroup;
    MGLCRG: TRadioGroup;
    MGVRG: TRadioGroup;
    MGCirclesRG: TRadioGroup;
    MGAOK: TBitBtn;
    MGAHelp: TBitBtn;
    MGPHelp: TBitBtn;
    MGPOK: TBitBtn;
    MGCHelp: TBitBtn;
    MGCOK: TBitBtn;
    MGIFHelp: TBitBtn;
    MGIFOK: TBitBtn;
    AHREdit1: TEdit;
    AHREdit2: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MGCurvesRG: TRadioGroup;
    Bevel1: TBevel;
    TreeTS: TTabSheet;
    TreeSHEdit: TEdit;
    TreeSWEdit: TEdit;
    TreeLAEdit: TEdit;
    TreeRAEdit: TEdit;
    TreeLBAEdit: TEdit;
    TreeRBAEdit: TEdit;
    TreeRLEdit: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    MGTreeHelp: TBitBtn;
    MGTreeOK: TBitBtn;
    DIYTS: TTabSheet;
    DIYPallette: TRadioGroup;
    DIYXEdit: TEdit;
    DIYYEdit: TEdit;
    DIYZEdit: TEdit;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    DIYSetup: TSpeedButton;
    DIYAdd: TSpeedButton;
    DIYMemo: TMemo;
    DIYRun: TSpeedButton;
    DIYHyEdit: TEdit;
    DIYWxEdit: TEdit;
    AHREdit3: TEdit;
    DIYBitmapLoader: TSpeedButton;
    DIYBitmapEdit: TEdit;
    DIYLxEdit: TEdit;
    DIYClipit: TSpeedButton;
    DIYMagicLoader: TSpeedButton;
    Label17: TLabel;
    DIYHelp: TSpeedButton;
    Label18: TLabel;
    PopEdit1: TEdit;
    PopEdit2: TEdit;
    ApollianiaEdit: TEdit;
    DIYFont: TSpeedButton;
    DIYPaintBrush: TSpeedButton;
    DIYLight: TSpeedButton;
    DIYSave: TSpeedButton;
    DIYLoad: TSpeedButton;
    DIYLand: TSpeedButton;
    Label19: TLabel;
    Label20: TLabel;
    DIYGear: TSpeedButton;
    DIYRotate: TSpeedButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    FontDialog1: TFontDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    G_Math_Image: TImage;
    DIYSet: TPanel;
    Numb3DTS: TTabSheet;
    NFb: TEdit;
    NFe: TEdit;
    NFd: TEdit;
    NFg: TEdit;
    NFp: TEdit;
    NFf: TEdit;
    NFa: TEdit;
    NFc: TEdit;
    NFn: TEdit;
    NFm: TEdit;
    NFq: TEdit;
    NFr: TEdit;
    NFYScale: TEdit;
    NFXScale: TEdit;
    NFXOff: TEdit;
    NFYOff: TEdit;
    NFBeta: TEdit;
    NFAlpha: TEdit;
    NFGamma: TEdit;
    NFHorizon: TEdit;
    NFh: TEdit;
    ClearHeaded: TSpeedButton;
    NFOpen: TSpeedButton;
    NFHelp: TSpeedButton;
    NFSave: TSpeedButton;
    NFRun: TSpeedButton;
    NumbFileEdit: TEdit;
    TDColor1: TPanel;
    TDColor4: TPanel;
    TDColor3: TPanel;
    TDColor2: TPanel;
    TreeMouser: TSpeedButton;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    NFColor2: TPanel;
    NFColor1: TPanel;
    NFColor3: TPanel;
    NFColor4: TPanel;
    V2Colorbox: TPanel;
    V1Colorbox: TPanel;
    PolygonBtn: TSpeedButton;
    V3Colorbox: TPanel;
    V4Colorbox: TPanel;
    NumbIterations: TEdit;
    Label61: TLabel;
    NumbFunLevel: TEdit;
    Label62: TLabel;
    NumbUpDown: TUpDown;
    TreeLoader: TSpeedButton;
    TreeNameEdit: TEdit;
    TreeSaver: TSpeedButton;
    Label63: TLabel;
    ColorDialog1: TColorDialog;
    NumbMouser: TSpeedButton;
    Bevel3: TBevel;
    Sky2DTS: TTabSheet;
    Label44: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label60: TLabel;
    Label64: TLabel;
    SkyE: TEdit;
    SkyF: TEdit;
    SkyB: TEdit;
    SkyA: TEdit;
    Label65: TLabel;
    Label66: TLabel;
    Label68: TLabel;
    SkyP: TEdit;
    SkyD: TEdit;
    SkyC: TEdit;
    Label69: TLabel;
    Edit33: TEdit;
    SkyUpDown: TUpDown;
    Label70: TLabel;
    Label71: TLabel;
    Label72: TLabel;
    Label73: TLabel;
    SkyYOff: TEdit;
    SkyXOff: TEdit;
    SkyYScale: TEdit;
    SkyXScale: TEdit;
    Label77: TLabel;
    SkyHorizon: TEdit;
    SkyIterations: TEdit;
    Label78: TLabel;
    SkyColor3: TPanel;
    SkyColor4: TPanel;
    SkyMouser: TSpeedButton;
    SkyColor2: TPanel;
    SkyColor1: TPanel;
    SkyClear: TSpeedButton;
    SkyLoader: TSpeedButton;
    SkyNameEdit: TEdit;
    SkySaver: TSpeedButton;
    SkyShow: TSpeedButton;
    SkyHelp: TSpeedButton;
    Bevel2: TBevel;
    TreeYOEdit: TEdit;
    Label75: TLabel;
    TreeXOEdit: TEdit;
    Label76: TLabel;
    DIYLyEdit: TEdit;
    NFPy: TEdit;
    NFQx: TEdit;
    Label80: TLabel;
    Label81: TLabel;
    DIYFileEdit: TEdit;
    DIYAddLoad: TSpeedButton;
    DIYMemo2: TMemo;
    DIYEEdit: TEdit;
    Label82: TLabel;
    KEdit1: TEdit;
    KEdit2: TEdit;
    KEdit3: TEdit;
    KEdit4: TEdit;
    KEdit5: TEdit;
    k2Edit1: TEdit;
    k2Edit2: TEdit;
    k2Edit3: TEdit;
    k2Edit4: TEdit;
    k2Edit5: TEdit;
    RandomTS: TTabSheet;
    MGBMRG: TRadioGroup;
    TurtlesTS: TTabSheet;
    Label83: TLabel;
    MGBMrEdit: TEdit;
    Label84: TLabel;
    MGBMdEdit: TEdit;
    MGBMHelpBtn: TBitBtn;
    MGBMOKBtn: TBitBtn;
    Label85: TLabel;
    TPIEdit: TEdit;
    TDegreeEdit: TEdit;
    THelpBtn: TBitBtn;
    TurtleOK: TBitBtn;
    BumpTS: TTabSheet;
    TurtleRG: TRadioGroup;
    FractalRG: TRadioGroup;
    TAxiomEdit: TEdit;
    TProjEdit: TEdit;
    Label89: TLabel;
    Label90: TLabel;
    TurtleFileEdit: TEdit;
    TFileOpen: TSpeedButton;
    TFileSave: TSpeedButton;
    TPruleEdit: TEdit;
    TurtleUpDown: TUpDown;
    MGASARG: TRadioGroup;
    FHelp: TBitBtn;
    FOK: TSpeedButton;
    Label21: TLabel;
    FXEdit: TEdit;
    Label22: TLabel;
    FYEdit: TEdit;
    Label23: TLabel;
    FZxEdit: TEdit;
    Label24: TLabel;
    FZyEdit: TEdit;
    Label87: TLabel;
    FrEdit: TEdit;
    Label88: TLabel;
    FdEdit: TEdit;
    Label99: TLabel;
    FhEdit: TEdit;
    FiEdit: TEdit;
    Label100: TLabel;
    Label101: TLabel;
    TXMinEdit: TEdit;
    Label102: TLabel;
    TYMinEdit: TEdit;
    Label103: TLabel;
    TZxmaxEdit: TEdit;
    Label104: TLabel;
    TZymaxEdit: TEdit;
    TMouseBtn: TSpeedButton;
    TClearBtn: TSpeedButton;
    Label105: TLabel;
    Collage: TTabSheet;
    Label1: TLabel;
    Label86: TLabel;
    Label106: TLabel;
    Label107: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label108: TLabel;
    Label109: TLabel;
    Label110: TLabel;
    Label111: TLabel;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Label112: TLabel;
    Label113: TLabel;
    Label114: TLabel;
    Label115: TLabel;
    Edit9: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit14: TEdit;
    Label116: TLabel;
    CollageUpDown: TUpDown;
    Edit15: TEdit;
    Label117: TLabel;
    Label118: TLabel;
    Label119: TLabel;
    Label120: TLabel;
    Label121: TLabel;
    Edit16: TEdit;
    Edit17: TEdit;
    Edit18: TEdit;
    Edit19: TEdit;
    Label122: TLabel;
    Label123: TLabel;
    Label124: TLabel;
    Label125: TLabel;
    Edit20: TEdit;
    Edit21: TEdit;
    Edit22: TEdit;
    Edit23: TEdit;
    Label126: TLabel;
    Edit24: TEdit;
    Label127: TLabel;
    Edit25: TEdit;
    Label128: TLabel;
    Edit26: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    Panel3: TPanel;
    Panel4: TPanel;
    CollageClear: TSpeedButton;
    CollageOpen: TSpeedButton;
    Edit27: TEdit;
    CollageSave: TSpeedButton;
    CollageRun: TSpeedButton;
    CollageHelp: TSpeedButton;
    TKanEdit: TEdit;
    Label129: TLabel;
    TCircledEdit: TEdit;
    MGACRG: TRadioGroup;
    TStrLenEdit: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    DOfLevelEdit: TEdit;
    Label138: TLabel;
    SierpinskiEdit: TEdit;
    RTimeEdit: TEdit;
    Label142: TLabel;
    FRotate: TSpeedButton;
    FClear: TSpeedButton;
    FgEdit: TEdit;
    Label144: TLabel;
    Label145: TLabel;
    FkEdit: TEdit;
    DragonTS: TTabSheet;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label67: TLabel;
    Label36: TLabel;
    IterationEdit: TEdit;
    QEdit: TEdit;
    PEdit: TEdit;
    GammaEdit: TEdit;
    BetaEdit: TEdit;
    AlphaEdit: TEdit;
    OffEditY: TEdit;
    OffEditX: TEdit;
    FDEditY: TEdit;
    FDEditX: TEdit;
    Label74: TLabel;
    Label79: TLabel;
    MGFernOK: TBitBtn;
    MGDragon3DOK: TBitBtn;
    FernNameEdit: TEdit;
    FernSaver: TSpeedButton;
    FernLoader: TSpeedButton;
    DColor1: TPanel;
    DColor2: TPanel;
    DColor3: TPanel;
    DColor4: TPanel;
    BitBtn1: TBitBtn;
    Label33: TLabel;
    Edit10: TEdit;
    Edit28: TEdit;
    Edit29: TEdit;
    Edit30: TEdit;
    Label34: TLabel;
    RadioGroup1: TRadioGroup;
    UpDowna: TUpDown;
    UpDownd: TUpDown;
    UpDowng: TUpDown;
    UpDownh: TUpDown;
    UpDowne: TUpDown;
    UpDownb: TUpDown;
    UpDownc: TUpDown;
    UpDownf: TUpDown;
    UpDownm: TUpDown;
    UpDownr: TUpDown;
    UpDownq: TUpDown;
    UpDownn: TUpDown;
    UpDownp: TUpDown;
    NumbRemCB: TCheckBox;
    NumbAwake: TCheckBox;
    GLBtn: TSpeedButton;
    procedure MGARGClick(Sender: TObject);
    procedure MGLCRGClick(Sender: TObject);
    procedure MGCCSRGClick(Sender: TObject);
    procedure MGCVKRGClick(Sender: TObject);
    procedure MGCPRGClick(Sender: TObject);
    procedure MGCHRGClick(Sender: TObject);
    procedure MGCSRGClick(Sender: TObject);
    procedure MGCDCRGClick(Sender: TObject);
    procedure MGVRGClick(Sender: TObject);
    procedure MGCirclesRGClick(Sender: TObject);
    procedure MGIFRGClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MGAHelpClick(Sender: TObject);
    procedure MGPHelpClick(Sender: TObject);
    procedure MGCHelpClick(Sender: TObject);
    procedure MGIFHelpClick(Sender: TObject);
    procedure MGTreeHelpClick(Sender: TObject);
    procedure MGAOKClick(Sender: TObject);
    procedure MGPOKClick(Sender: TObject);
    procedure MGCOKClick(Sender: TObject);
    procedure MGIFOKClick(Sender: TObject);
    procedure MGTreeOKClick(Sender: TObject);
    procedure treesetup;
    procedure treerun;
    procedure DIYAddClick(Sender: TObject);
    procedure DIYSetupClick(Sender: TObject);
    procedure DIYFontClick(Sender: TObject);
    procedure DIYLoadClick(Sender: TObject);
    procedure DIYSaveClick(Sender: TObject);
    procedure DIYLandClick(Sender: TObject);

    procedure DIYGearClick(Sender: TObject);
    procedure DIYLightClick(Sender: TObject);
    procedure DIYRunClick(Sender: TObject);
    procedure DIYRotateClick(Sender: TObject);
    procedure DIYHelpClick(Sender: TObject);
    procedure DIYMagicLoaderClick(Sender: TObject);
    procedure DIYBitmapLoaderClick(Sender: TObject);
    procedure DIYPaintBrushClick(Sender: TObject);
    procedure MGFernOKClick(Sender: TObject);
    procedure MGFernrun;
    procedure NumbUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure SelectNextNumbFun(Way: Boolean);
    procedure NumbSkull(HeadBanger: Integer);
    procedure LivingDead(Brains: Integer);
    procedure DeadHead(Alive: Integer);
    procedure SelectNextSkyFun(Way: Boolean);
    procedure SkyPilot(Fog: Integer);
    procedure SkyPen(Smoke: Integer);
    procedure SkyDiver(Oil: Integer);
    procedure SetAllVColors;
    procedure V1ColorboxClick(Sender: TObject);
    procedure V2ColorboxClick(Sender: TObject);
    procedure V3ColorboxClick(Sender: TObject);
    procedure V4ColorboxClick(Sender: TObject);
    procedure MGDragon3DOKClick(Sender: TObject);
    procedure MGDragon3DO;
    procedure gen3ddrgSetup;
    procedure TreeMouserClick(Sender: TObject);
    procedure DoTreeLoader(FilesS: string);
    procedure TreeLoaderClick(Sender: TObject);
    procedure SkyLoaderClick(Sender: TObject);
    procedure NumbOpen(FilesS: string);
    procedure SkyLoaderDo(FilesS: string);
    procedure NFOpenClick(Sender: TObject);
    procedure DoFernLoader(FilesS: string);
    procedure FernLoaderClick(Sender: TObject);
    procedure TreeSaverClick(Sender: TObject);
    procedure FernSaverClick(Sender: TObject);
    procedure SkySaverClick(Sender: TObject);
    procedure NFSaveClick(Sender: TObject);
    procedure ClearHeadedClick(Sender: TObject);
    procedure NFRunClick(Sender: TObject);
    procedure NFHelpClick(Sender: TObject);
    procedure SkyHelpClick(Sender: TObject);
    procedure SkyUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure SkyClearClick(Sender: TObject);
    procedure SkyShowClick(Sender: TObject);
    procedure PolygonBtnClick(Sender: TObject);
    procedure DIYAddLoadClick(Sender: TObject);
    procedure BitmapBlotto(FileName: string);
    procedure THelpBtnClick(Sender: TObject);
    procedure TurtleOKClick(Sender: TObject);
    procedure TurtleRun;
    procedure TurtleUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure SelectNextTurtleFun(Way: Boolean);
    procedure TurtlePresent(TurtleLevelDo: Integer);
    procedure TurtleNext(TurtleLevelDo: Integer);
    procedure TurtleLoadDo(FilesS: string);
    procedure TFileOpenClick(Sender: TObject);
    procedure TFileSaveClick(Sender: TObject);
    procedure MGBMHelpBtnClick(Sender: TObject);
    procedure FHelpClick(Sender: TObject);
    procedure TClearBtnClick(Sender: TObject);
    procedure MGBMOKBtnClick(Sender: TObject);
    procedure FOKClick(Sender: TObject);
    procedure MGASARGClick(Sender: TObject);
    procedure MGACRGClick(Sender: TObject);
    procedure CollageClearClick(Sender: TObject);
    procedure CollageOpenClick(Sender: TObject);
    procedure CollageSaveClick(Sender: TObject);
    procedure CollageHelpClick(Sender: TObject);
    procedure CollageRunClick(Sender: TObject);
    procedure DIYClipitClick(Sender: TObject);
    procedure FClearClick(Sender: TObject);
    procedure FRotateClick(Sender: TObject);
    procedure UpDownaClick(Sender: TObject; Button: TUDBtnType);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure NumbRemCBClick(Sender: TObject);
    procedure NumbAwakeClick(Sender: TObject);
    procedure UpDownbClick(Sender: TObject; Button: TUDBtnType);
    procedure UpDowncClick(Sender: TObject; Button: TUDBtnType);
    procedure UpDowndClick(Sender: TObject; Button: TUDBtnType);
    procedure UpDowneClick(Sender: TObject; Button: TUDBtnType);
    procedure UpDownfClick(Sender: TObject; Button: TUDBtnType);
    procedure UpDowngClick(Sender: TObject; Button: TUDBtnType);
    procedure UpDownhClick(Sender: TObject; Button: TUDBtnType);
    procedure UpDownmClick(Sender: TObject; Button: TUDBtnType);
    procedure UpDownnClick(Sender: TObject; Button: TUDBtnType);
    procedure UpDownqClick(Sender: TObject; Button: TUDBtnType);
    procedure UpDownrClick(Sender: TObject; Button: TUDBtnType);
    procedure UpDownpClick(Sender: TObject; Button: TUDBtnType);
    procedure GLBtnClick(Sender: TObject);

  private
    { Private declarations }
    function GetLevel: Integer;
  public
    { Public declarations }
  end;

var
  MathForm: TMathForm;
  DIYFileFLN: string = '';
  DIYFileFL: string = '';
  FontStorage: TFont;
  TKan, Axiom: string;
  TPI, TCircled, TDegrees, TurtleDirN,
  TXminI, TmaxI, TYminI, TYmaxI: Integer;
  ProductionRules: array[1..52] of string;

implementation
uses fUGlobal, fMain,  fXYZ3D, fAbout,
  FAnno, fGStyle,
  fUMathA, fUMathC, fUMathF, fUMathIF,
  fUMathP, fUTurtlep{, fMathGL};


{$R *.DFM}
var g_x, g_y, g_xz, g_yz: Extended;





procedure generate(x1, y1, x2, y2, x3, y3, level, y_max: integer;
  color1, color2: TColor); forward;

procedure ranFillOval(x, y, b: integer;
  color: TColor; aspect: Extended);
var col, row, end_x, end_y, kx, radius: integer;
  a: Extended;
  a_square, b_square, b_test: longint;
begin
  a := b / aspect;
  a_square := Round(a * a);
  radius := b;
  b := b;
  b_square := b * b;
  x := x + (FYImageX div 2);
  y := (FYImageY div 2) - y;
  end_x := x + Round(a);
  end_y := y + Round(b);
  for col := x - Round(a) to end_x do begin
    b_test := b_square - (b_square * (col - x) * (col - x)) div
      a_square;
    for row := y - b to end_y do begin
      kx := Random(25205 div radius);
      if ((row - y) * (row - y) <= b_test)
        and (kx < (col - x + 20)) then begin
        MainForm.Image2.Canvas.Pixels[col, row] := color;
      end;
    end;
  end;
end;

procedure midpoint(x: Extended; y: Extended);
var
  r, w: Extended;
  seed: longint;
begin
  seed := Round(FYImageY * y + x); {350 * y + x}
  RandSeed := seed;
  r := 0.33333 + Random / 3.0;
  w := 0.015 + Random / 50.0;
  if Random < 0.5 then
    w := -w;
  g_xz := r * x - (w + 0.05) * y;
  g_yz := r * y + (w + 0.05) * x;
end;

procedure node(x1: integer; y1: integer; x2: integer; y2: integer;
  x3: integer; y3: integer; x4: integer; y4: integer; x5: integer;
  y5: integer; x6: integer; y6: integer; level, y_max: integer;
  color1, color2: Tcolor);
begin
  if level <> 0 then
  begin
    generate(x1, y1, x6, y6, x4, y4, level - 1, y_max, color1,
      color2);
    generate(x2, y2, x4, y4, x5, y5, level - 1, y_max, color1,
      color2);
    generate(x3, y3, x5, y5, x6, y6, level - 1, y_max, color1,
      color2);
    generate(x4, y4, x5, y5, x6, y6, level - 1, y_max, color1,
      color2);
  end;
end;

procedure plot_triangle(x1, y1, x2, y2, x3, y3, y_max: integer;
  color1, color2: Tcolor);
var Color: TColor;
       {color: integer; }
  zt, ytt: Extended;
begin
  if y1 > y2 then ytt := y1
  else ytt := y2;
  if ytt < y3 then ytt := y3;
  zt := 1 - ((ytt + (FYImageY div 2)) * (ytt + (FYImageY div 2))) /
    ((y_max + (FYImageY div 2)) * (y_max + (FYImageY div 2)));
  if Random <= zt then color := color1
  else color := color2; {color3} {randomly set another}
{y_max} if ytt + (FYImageY div 2) <
    ((y_max + (FYImageY div 2)) / 4) then color := color1;
  if ytt + (FYImageY div 2) >
    (0.98 * (y_max + (FYImageY div 2))) then color := color2;
  Triangle[1].x := x1 + (FYImageX div 2);
  Triangle[1].y := (FYImageY div 2) - y1;
  Triangle[2].x := x2 + (FYImageX div 2);
  Triangle[2].y := (FYImageY div 2) - y2;
  Triangle[3].x := x3 + (FYImageX div 2);
  Triangle[3].y := (FYImageY div 2) - y3;
  MainForm.Image2.Canvas.Pen.Color := color;
  MainForm.Image2.Canvas.Brush.Color := color;
  MainForm.Image2.Canvas.Polygon(Triangle);
end;

procedure generate(x1, y1, x2, y2, x3, y3, level, y_max: integer;
  color1, color2: Tcolor);
var x4, x5, x6, y4, y5, y6: integer;
begin
  g_x := x2 - x1;
  g_y := y2 - y1;
  midpoint(g_x, g_y);
  x4 := x1 + Round(g_xz);
  y4 := y1 + Round(g_yz);
  g_x := x1 - x3;
  g_y := y1 - y3;
  midpoint(g_x, g_y);
  x6 := x3 + Round(g_xz);
  y6 := y3 + Round(g_yz);
  g_x := x3 - x2;
  g_y := y3 - y2;
  midpoint(g_x, g_y);
  x5 := x2 + Round(g_xz);
  y5 := y2 + Round(g_yz);
  if level = 0 then
  begin
    plot_triangle(x1, y1, x6, y6, x4, y4, y_max, color1, color2);
    plot_triangle(x2, y2, x4, y4, x5, y5, y_max, color1, color2);
    plot_triangle(x3, y3, x5, y5, x6, y6, y_max, color1, color2);
    plot_triangle(x4, y4, x5, y5, x6, y6, y_max, color1, color2);
  end
  else
    node(x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6, level,
      y_max,
      color1, color2);
end;

procedure gen_quad(x1, y1, x2, y2, x3, y3, x4, y4, level, y_max:
  integer;
  color1, color2: Tcolor);
begin
  generate(x1, y1, x2, y2, x3, y3, level, y_max, color1, color2);
  generate(x1, y1, x4, y4, x3, y3, level, y_max, color1, color2);
end;
(************************************************************)
procedure TMathForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MathFormX := MathForm.left;
  MathFormY := MathForm.top;
  DoSaver;
end;

procedure TMathForm.FormCreate(Sender: TObject);
begin
  left := MathFormX;{  left := 498;}
  top := MathFormY;{ top := 113;}
  OpenPictureDialog1.InitialDir := FractalDir;
  OpenDialog1.InitialDir := FractalDir;
  SaveDialog1.InitialDir := FractalDir;
  ForceCurrentDirectory := True;
  DIYFilename := 'NONE.BMP';
  SetAllVColors;
  V4Colorbox.Color := V4Color;
  V3Colorbox.Color := V3Color;
  V2Colorbox.Color := V2Color;
  V1Colorbox.Color := V1Color;
  SkyUpDown.Position := 0;
  SkyLevel := 0;
  NumbUpDown.Position := 0;
  NumbLevel := 0;
  CollageUpDown.Position := 0;
  CollageLevel := 0;
  TurtleUpDown.Position := 1;
  TurtleLevel := 1;
  IsNumbREMFast:=True;
  isNumbAwake:=False;
end;



(**************************************************)

(**************************************************)

procedure TMathForm.MGAHelpClick(Sender: TObject);
begin
  Application.HelpContext(2100);
end;

procedure TMathForm.MGPHelpClick(Sender: TObject);
begin
  Application.HelpContext(2200);
end;

procedure TMathForm.MGBMHelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(2250); {2250}
end;

procedure TMathForm.MGCHelpClick(Sender: TObject);
begin
  Application.HelpContext(2300); {}
end;

procedure TMathForm.MGIFHelpClick(Sender: TObject);
begin
  Application.HelpContext(2400); {}
end;

procedure TMathForm.FHelpClick(Sender: TObject);
begin
  Application.HelpContext(2450); {2450}
end;

procedure TMathForm.DIYHelpClick(Sender: TObject);
begin
  Application.HelpContext(2500); {}
end;

procedure TMathForm.MGTreeHelpClick(Sender: TObject);
begin
  Application.HelpContext(2600); {}
end;

procedure TMathForm.SkyHelpClick(Sender: TObject);
begin
  Application.HelpContext(2700); {2700}
end;

procedure TMathForm.NFHelpClick(Sender: TObject);
begin
  Application.HelpContext(2800); {}
end;



procedure TMathForm.CollageHelpClick(Sender: TObject);
begin
  Application.HelpContext(3000); {3000}
end;

procedure TMathForm.THelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(3100); {Turtle}
end;
(**************************************************)
(**************************************************)
(**************************************************)


(**************************************************)

procedure TMathForm.MGARGClick(Sender: TObject);
begin
  MGLCRG.ItemIndex := -1;
  MGASARG.ItemIndex := -1;
  MGACRG.ItemIndex := -1;
{Set other to -1 MGLCRGClick MGARGClick}
end;

procedure TMathForm.MGLCRGClick(Sender: TObject);
begin
  MGARG.ItemIndex := -1;
  MGASARG.ItemIndex := -1;
  MGACRG.ItemIndex := -1;
{Set other to -1 MGLCRGClick MGARGClick}
end;

procedure TMathForm.MGASARGClick(Sender: TObject);
begin
  MGARG.ItemIndex := -1;
  MGACRG.ItemIndex := -1;
  MGLCRG.ItemIndex := -1;
end;

procedure TMathForm.MGACRGClick(Sender: TObject);
begin
  MGARG.ItemIndex := -1;
  MGASARG.ItemIndex := -1;
  MGLCRG.ItemIndex := -1;
end;

procedure TMathForm.MGAOKClick(Sender: TObject);
var V1e, V2e, V3e: Extended; V1i, V2i, Code: Integer;
begin
  MainForm.DoImageStart;
  if (MGARG.ItemIndex > -1) then
  begin {U_Math_A}
    case MGARG.ItemIndex of
      0: Lorenz;
      1: Strange; { Strange;} {Vortex}
      2: Duffing; {Procedure Duffing;}
      3: Rossler; {Procedure Rossler;}
      4: Henon_Attractor;
    end; end else
    if (MGASARG.ItemIndex > -1) then
    begin {U_Math_A}
      case MGASARG.ItemIndex of
        0: begin {procedure Kaneko1;}
            val(KEdit1.Text, V1e, Code);
            val(KEdit2.Text, V2e, Code);
            val(KEdit3.Text, V3e, Code);
            val(KEdit4.Text, V1i, Code);
            val(KEdit5.Text, V2i, Code);
            Kaneko1
              (V1e {KanA 01.3}, V2e {KanD 0.1}, V3e
                {KanStep 0.01 :Extended;},
              V1i {DisKan 30}, V2i {KanTeen 3000 :Integer});
          end;
        1: begin {procedure Kaneko2;}
            val(K2Edit1.Text, V1e, Code);
            val(K2Edit2.Text, V2e, Code);
            val(K2Edit3.Text, V3e, Code);
            val(K2Edit4.Text, V1i, Code);
            val(K2Edit5.Text, V2i, Code);
            Kaneko2(V1e {KanA 0.3}, V2e {KanD 1.75}, V3e
              {KanStep 0.02 :Extended;},
              V1i {DisKan 36}, V2i {KanTeen 3000 :Integer});
          end;
        2: {Henon Range}
          begin
            val(AHREdit1.Text, V1e, Code);
            val(AHREdit2.Text, V2e, Code);
            val(AHREdit3.Text, V2i, Code);
            Henon2(V1e, V2e, V2i);
          end;
      end; end else
      if (MGACRG.ItemIndex > -1) then
      begin {Unit U_Math_A; }
        SetChemical;
        case MGACRG.ItemIndex of
          0: StrangeChemicalsa;
          1: StrangeChemicalsb;
          2: StrangeChemicalsc;
          3: StrangeChemicalsd;
          4: StrangeChemicalse;
          5: StrangeChemicalsf;
          6: StrangeChemicalsg;
          7: begin
              StrangeChemicalsa;
              StrangeChemicalsb;
              StrangeChemicalsc;
              StrangeChemicalsd;
              StrangeChemicalse;
              StrangeChemicalsf;
              StrangeChemicalsg;
            end;
        end; end else
        if (MGLCRG.ItemIndex > -1) then
        begin {Unit U_Math_A;  Procedure LimitCycles;}
          case MGLCRG.ItemIndex of
            0: LimitCycles(0); {Rayleigh}
            1: LimitCycles(1); {Vanderpol}
            2: LimitCycles(2); {Brusselator}
            3: LimitCycles(3); {All Three}
{Unit U_Math_A;  Procedure LimitCycles;}
          end;
        end else DoMessages(28);
  Mainform.DoImageDone;
end;
(**************************************************)


(**************************************************)

procedure TMathForm.MGPOKClick(Sender: TObject);
var V1e, V2e: Extended; Code: Integer;
begin
  if (MGPRG.ItemIndex > -1) then begin {U_Math_P}
    MainForm.DoImageStart;
    case MGPRG.ItemIndex of
      0: bifurc(0.95, 0.005, 0);
      1: bifurc(0.95, 0.0005, 1);
      2: begin val(PopEdit1.Text, V1e, Code);
          val(PopEdit2.Text, V2e, Code);
          bifurc(V1e, V2e, 2); end;
      3: feigen;
      4: bifbaum(0.95, 0.005, 0);
      5: bifbaum(0.95, 0.0005, 1);
      6: begin val(PopEdit1.Text, V1e, Code);
          val(PopEdit2.Text, V2e, Code);
          bifbaum(V1e, V2e, 2); end;
      7: Malthus_1;
      8: Malthus_2;
  {I Map}{UNIT B_Quad; Procedure DO_B_Quad;}
{X Van XVSN}
    end; {Case} end;
  Mainform.DoImageDone;
end;
(**************************************************)

procedure TMathForm.MGBMOKBtnClick(Sender: TObject);
var V1e, V2e, V3e {,V3e,V4e}: Extended; {Vx,Vy,Vz,Vi,} Codey, Code:
Integer;
begin
  if (MGBMRG.ItemIndex > -1) then begin {U_Math_P}
    MainForm.DoImageStart;
    case MGBMRG.ItemIndex of
      0: begin val(MGBMrEdit.Text, V1e, Code);
          Codey := Round(V1e * 100);
          BBrownian(Codey); end;
      1: begin val(MGBMrEdit.Text, V1e, Code);
          Codey := Round(V1e * 100);
          BBrown2d(Codey); end;
      2: begin
          val(MGBMrEdit.Text, V1e, Code);
          val(MGBMdEdit.Text, V2e, Code);
          val(RTimeEdit.Text, V3e, Code);
          Codey := Round(V3e);
          Brown_Gauss(V1e, V2e, Codey);
        {1.1,2.2 Procedure Brown_Gauss(F1,D : Extended);}
        end;
      3: WhiteBrown;
      4: Cellar; {B_B_Cell Procedure Cellar;}
      5: CellTorus; {Procedure CellTorus;}
      6: CellZap; {Procedure CellZap;}
      7: HiFiLines;
    end; end;
  Mainform.DoImageDone;
end;
(**************************************************)
(**************************************************)
(**************************************************)


(**************************************************)

(**************************************************)

(**************************************************)


(**************************************************)

procedure TMathForm.FRotateClick(Sender: TObject);
begin
  inc(RotatorCuff);
  if (RotatorCuff > 2) then RotatorCuff := 0;
end;
procedure TMathForm.GLBtnClick(Sender: TObject);
begin
{MathGL.Show;}
end;
(**************************************************)

procedure TMathForm.FOKClick(Sender: TObject);
var V1e, V2e: Extended; Vx, Vy, Vzx, Vzy, Vi, {Vh,} Code: Integer;
begin
  if (FractalRG.ItemIndex > -1) then begin {U_Math_F}
    MainForm.DoImageStart;
    case FractalRG.ItemIndex of
      0: {Sin Shadow}
        {Procedure SinShadow(Plot_Angle, M_x:Integer;
        View_Angle, Illum_Angle: Extended);}
        SinShadow(90, 20, 1, 3.0, 0.3);
        { B_B_Shad Procedure SinShadow;}
        {Plot_Angle  := 90.0;
        View_Angle  := 3.0;
        Illum_Angle := 0.3;
        M_x := 20.0;}
      1: {Sin Shadow [X] [Y] [r] [d] (90.0, 3.0, 0.3,20.0)}
        begin
          val(FgEdit.Text, VX, Code); Codefx(FgEdit.Text, Code);
          val(FhEdit.Text, VY, Code); Codefx(FhEdit.Text, Code);
          val(FiEdit.Text, Vi, Code); Codefx(FiEdit.Text, Code);
          val(FrEdit.Text, V1e, Code); Codefx(FrEdit.Text, Code);
          val(FdEdit.Text, V2e, Code); Codefx(FdEdit.Text, Code);
          SinShadow(VX, VY, Vi, V1e, V2e);
        end;
      2: {cosine Shadow}
        {Procedure CoSinShadow(Plot_Angle, M_x:Integer;
        View_Angle, Illum_Angle: Extended);}
        CoSinShadow(90, 20, 1, 3.0, 0.3);
      3: {cosine Shadow [X] [Y] [Zx] [i]}
        begin
          val(FgEdit.Text, VX, Code); Codefx(FgEdit.Text, Code);
          val(FhEdit.Text, VY, Code); Codefx(FhEdit.Text, Code);
          val(FiEdit.Text, Vi, Code); Codefx(FiEdit.Text, Code);
          val(FrEdit.Text, V1e, Code); Codefx(FrEdit.Text, Code);
          val(FdEdit.Text, V2e, Code); Codefx(FdEdit.Text, Code);
        {Procedure CoSinShadow(Plot_Angle, M_x:Integer;
        View_Angle, Illum_Angle: Extended);}
          CoSinShadow(VX, VY, Vi, V1e, V2e);
        end;
      4: {Sin Lattice (grid) [X] [Y] [Z] [i] (16,8,30,30)}
        begin
          val(FgEdit.Text, VX, Code); Codefx(FgEdit.Text, Code);
          val(FhEdit.Text, VY, Code); Codefx(FhEdit.Text, Code);
          val(FiEdit.Text, VZx, Code); Codefx(FiEdit.Text, Code);
          val(FkEdit.Text, VZy, Code); Codefx(FkEdit.Text, Code);
        {Procedure B_3D_Map(N,L,VA,HA:Integer);}
          B_3D_Map(Vx, Vy, Vzx, Vzy);
        {16,8,30,30  B_B3D; Procedure B_3D_Map(N,L,VA,HA:Integer);}
        end;
      5: RotatingCube;
      6: FallingColumn;
      7: {Diffusion Limited Aggregration (DLA) [r] [d] [i]}
        begin
        {val( FrEdit.Text,V1e,Code);
        val( FdEdit.Text,V2e,Code);}
          val(FiEdit.Text, Vi, Code); Codefx(FiEdit.Text, Code);
        {Procedure DLA(Freq,F_Dim: Extended;Intensity:Integer);}
          DLA({V1e, V2e,} Vi);
        end;
      8: begin
          val(FiEdit.Text, Vi, Code); Codefx(FiEdit.Text, Code);
          DLAC({V1e, V2e,} Vi);
        end;
      9: begin
          val(FiEdit.Text, Vi, Code); Codefx(FiEdit.Text, Code);
          DLACentroid(Vi);
        end;

    end; {of case}
  end;
  Mainform.DoImageDone;
end;


procedure TMathForm.FClearClick(Sender: TObject);
var Code: Integer; SizeStr: string;
begin
{TXminI}
  FrEdit.Text := '3.0';
  FdEdit.Text := '0.3';
  FgEdit.Text := '90';
  FhEdit.Text := '20';
  FiEdit.Text := '2';
  FkEdit.Text := '20';
  FXEdit.Text := '0';
  val(FXEdit.Text, TXminI, Code); Codefx(FXEdit.Text, Code);
{TYminI}
  FYEdit.Text := '0';
  val(FYEdit.Text, TYminI, Code); Codefx(FYEdit.Text, Code);
{TmaxI}
  str(FYImageX, SizeStr);
  FZxEdit.Text := SizeStr;
  val(FZxEdit.Text, TmaxI, Code); Codefx(FZxEdit.Text, Code);
{TYmaxI:Integer;}
  str(FYImageY, SizeStr);
  FZyEdit.Text := SizeStr;
  val(FZyEdit.Text, TYmaxI, Code); Codefx(FZyEdit.Text, Code);
end;
(**************************************************)

procedure TMathForm.MGCCSRGClick(Sender: TObject);
begin
{Set other to -1}
  MGCDCRG.ItemIndex := -1;
  MGCSRG.ItemIndex := -1;
  MGCHRG.ItemIndex := -1;
  MGCPRG.ItemIndex := -1;
  MGCVKRG.ItemIndex := -1;
{MGCCSRGClick}
end;

procedure TMathForm.MGCVKRGClick(Sender: TObject);
begin
{Set other to -1}
  MGCDCRG.ItemIndex := -1;
  MGCSRG.ItemIndex := -1;
  MGCHRG.ItemIndex := -1;
  MGCPRG.ItemIndex := -1;
{MGCVKRGClick}
  MGCCSRG.ItemIndex := -1;
end;

procedure TMathForm.MGCPRGClick(Sender: TObject);
begin
{Set other to -1}
  MGCDCRG.ItemIndex := -1;
  MGCSRG.ItemIndex := -1;
  MGCHRG.ItemIndex := -1;
{MGCPRGClick}
  MGCVKRG.ItemIndex := -1;
  MGCCSRG.ItemIndex := -1;
end;

procedure TMathForm.MGCHRGClick(Sender: TObject);
begin
{Set other to -1}
  MGCDCRG.ItemIndex := -1;
  MGCSRG.ItemIndex := -1;
{MGCHRGClick}
  MGCPRG.ItemIndex := -1;
  MGCVKRG.ItemIndex := -1;
  MGCCSRG.ItemIndex := -1;
end;

procedure TMathForm.MGCSRGClick(Sender: TObject);
begin
{Set other to -1}
  MGCDCRG.ItemIndex := -1;
{MGCSRGClick}
  MGCHRG.ItemIndex := -1;
  MGCPRG.ItemIndex := -1;
  MGCVKRG.ItemIndex := -1;
  MGCCSRG.ItemIndex := -1;
end;

procedure TMathForm.MGCDCRGClick(Sender: TObject);
begin
{Set other to -1
MGCDCRGClick}
  MGCSRG.ItemIndex := -1;
  MGCHRG.ItemIndex := -1;
  MGCPRG.ItemIndex := -1;
  MGCVKRG.ItemIndex := -1;
  MGCCSRG.ItemIndex := -1;
end;

function TMathForm.GetLevel: Integer;
begin
  GetLevel := (MGCurvesRG.ItemIndex + 1);
end;

procedure TMathForm.MGCOKClick(Sender: TObject);
begin
  MainForm.DoImageStart;
  if (MGCurvesRG.ItemIndex > -1) then begin {U_Math_C}
{MGCurvesRG.ItemIndex}
    if (MGCCSRG.ItemIndex > -1) then begin {U_Math_C}
      case MGCCSRG.ItemIndex of
        0: Cesaro(GetLevel);
          { B_C_PCES; Procedure Cesaro(level : integer);}
        1: CesaroMD(GetLevel); {Procedure CesaroMD(level : integer); }
        2: Cesaro2(GetLevel); {Procedure Cesaro2(level : integer);}
        3: polya(GetLevel);
          {B_C_SNOW;procedure polya(level : integer);}
        4: PGosper(GetLevel); {Procedure PGosper(Level : Integer);}
        5: snow7(GetLevel); {procedure snow7(level : integer);}
        6: snowhall(GetLevel); {procedure snowhall(level : integer);}
        7: snow13(GetLevel); {procedure snow13( level : integer);}
      end; end;
    if (MGCVKRG.ItemIndex > -1) then begin {U_Math_C}
      case MGCVKRG.ItemIndex of
        0: Snoflake(GetLevel);
          {B_C_GOSP; Procedure Snoflake(level : integer);}
        1: Gosper(GetLevel); {Procedure Gosper(level : integer); }
        2: hkoch8(GetLevel); {Procedure hkoch8(level : integer);}
        3: qkoch3(GetLevel);
          {B_C_KOCH;Procedure qkoch3(level : integer);}
        4: qkoch8(GetLevel); {Procedure qkoch8(level : integer);}
        5: qkoch18(GetLevel); {Procedure qkoch18(level : integer);}
        6: qkoch32(GetLevel); {Procedure qkoch32(level : integer);}
        7: qkoch50(GetLevel); {Procedure qkoch50(level : integer);}
      end; end;
    if (MGCPRG.ItemIndex > -1) then begin {U_Math_C}
      case MGCPRG.ItemIndex of
        0: Peanoor(GetLevel);
          { B_C_PCES;procedure Peanoor(level : integer);}
        1: Peanomod(GetLevel); {procedure Peanomod(level : integer);}
      end; end;
    if (MGCHRG.ItemIndex > -1) then begin {U_Math_C}
      case MGCHRG.ItemIndex of
        0: hilbert(GetLevel);
          { B_C_HILB; procedure hilbert(level : integer);}
        1: hilbert2(GetLevel); {procedure hilbert2(level : integer);}
        2: hil3d(GetLevel); {procedure hil3d(level : integer);}
      end; end;
    if (MGCSRG.ItemIndex > -1) then begin {U_Math_C}
      case MGCSRG.ItemIndex of
        0: sierp(GetLevel);
          {B_C_Sier; procedure sierp(level : integer);} { of 8 }
        1: sierbox(GetLevel); {procedure sierbox(level : integer); }
          { of 3 }
        2: siergask(GetLevel);
          {procedure siergask(level : integer);] { fast enough at all 5 levels }
      end; end;
    if (MGCDCRG.ItemIndex > -1) then begin {U_Math_C}
      case MGCDCRG.ItemIndex of
        0: Dragon_Curve(GetLevel);
          {  B_C_Sier; procedure Dragon_Curve;}
        1: TwinDrag(GetLevel); {procedure TwinDrag;}
      end; end;
  end else DoMessages(30111);
  Mainform.DoImageDone;
end;
(**************************************************)

(**************************************************)
(**************************************************)
(**************************************************)

procedure TMathForm.MGVRGClick(Sender: TObject);
begin
{Set other to -1}
  MGIFRG.ItemIndex := -1;
  MGCirclesRG.ItemIndex := -1;
{MGVRGClick}
end;

procedure TMathForm.MGCirclesRGClick(Sender: TObject);
begin
{Set other to -1}
  MGIFRG.ItemIndex := -1;
{MGCirclesRGClick}
  MGVRG.ItemIndex := -1;
end;


procedure TMathForm.MGIFRGClick(Sender: TObject);
begin
{Set other to -1
MGIFRGClick}
  MGCirclesRG.ItemIndex := -1;
  MGVRG.ItemIndex := -1;
end;



procedure TMathForm.MGIFOKClick(Sender: TObject);
var ALevel, Code: Integer;
begin
  MainForm.DoImageStart;
  if (MGVRG.ItemIndex > -1) then begin {U_Math_I}
    case MGVRG.ItemIndex of
      0: TwoButtes;
      1: PuertoRico;
      2: forest;
      3: LightsOn;
      4: Earth;
    end; end;
  if (MGCirclesRG.ItemIndex > -1) then begin {U_Math_I}
    case MGCirclesRG.ItemIndex of
      0: begin
          treesetup;
          trees(0, -135, 20, 14,
            80, 2.0, 2.2,
            20, 28,
            clOlive, clSilver, clGreen, clLime); end;
{trees(XOffset,YOffset,width, level : Integer;
                 height,left_alpha,right_alpha,
                 left_angle,right_angle : Extended;
                 Color1,Color2,Color3,Color4:Tcolor );}
      1: begin
          val(ApollianiaEdit.text, ALevel, Code);
          apolly(ALevel);
        end;
      2: pharoh;
    end; end;
  if (MGIFRG.ItemIndex > -1) then begin {U_Math_I}
    case MGIFRG.ItemIndex of
      0: Image(1); {{Procedure Image(WhichOne : integer); }
      1: image3d; {Procedure image3d; }
      2: Image(2); {Unit B_RImage; }
      3: Image(3);
      4: Image(4);
      5: ifsdet(2); {procedure ifsdet;}
      6: ifsdet(1);
      7: sierchet3; {procedure sierchet3;}
      8: sierchet; {Procedure sierchet;  }
      9: sierchet2; {procedure sierchet2;}
      10: begin {Sierpinski [0.? ]... does not work...too variable}
          val(SierpinskiEdit.text, ALevel, Code);
          Sierpinski(ALevel);
        end;
    end; end;
  Mainform.DoImageDone;
end;
(**************************************************)
(**************************************************)

procedure TMathForm.MGTreeOKClick(Sender: TObject);
begin
{B_Imagin;
Set up Screen here so Tree can be used by all 3 callers
Call trees with an input... allow inputs in Tree page}
  MainForm.DoImageStart;
{treesetup;}
  treerun;
  Mainform.DoImageDone;
end;

procedure TMathForm.treesetup;
begin
  with MainForm.Image2.Canvas do begin
    Brush.Color := FBackGroundColor;
    Brush.Style := bsSolid;
    FillRect(Rect(0, 0, FYImageX, FYImageY));
    MainForm.Show;
    Application.ProcessMessages;
  end; end;

procedure TMathForm.treerun;
var XOff, YOff, Twidth, Tlevel, Code: Integer;
  Theight, Tleft_alpha, Tright_alpha,
    Tleft_angle, Tright_angle: Extended;
begin
  val(TreeXOEdit.Text, XOff, Code);
  val(TreeYOEdit.Text, YOff, Code);
  val(TreeSWEdit.Text, Twidth, Code);
  val(TreeRLEdit.Text, Tlevel, Code);
  val(TreeSHEdit.Text, Theight, Code);
  val(TreeLAEdit.Text, Tleft_alpha, Code);
  val(TreeRAEdit.Text, Tright_alpha, Code);
  val(TreeLBAEdit.Text, Tleft_angle, Code);
  val(TreeRBAEdit.Text, Tright_angle, Code);
  trees(XOff, YOff, Twidth, Tlevel, Theight, Tleft_alpha,
    Tright_alpha,
    Tleft_angle, Tright_angle,
    V1Color, V2Color, V3Color, V4Color);
{procedure trees(xoff,yoff,width, level : Integer;
                 height,left_alpha,right_alpha,
                 left_angle,right_angle         : Extended );
                 4 colors}
end;

procedure TMathForm.TreeMouserClick(Sender: TObject);
begin
  bMousing := (not bMousing);
end;

procedure TMathForm.DoTreeLoader(FilesS: string);
var MyFilesS: string; Code: integer;
  TreeFile: TextFile;
begin {Actually load the data into the form}
  if (FileExists(FilesS)) then begin
    AssignFile(TreeFile, FilesS);
    Reset(TreeFile);
    if IoResult <> 0 then
    begin
      DoMessages(30102);
    end;
    Readln(TreeFile, MyFilesS);
    TreeXOEdit.Text := MyFilesS;
    Readln(TreeFile, MyFilesS);
    TreeYOEdit.Text := MyFilesS;
    Readln(TreeFile, MyFilesS);
    TreeSHEdit.Text := MyFilesS;
    Readln(TreeFile, MyFilesS);
    TreeSWEdit.Text := MyFilesS;
    Readln(TreeFile, MyFilesS);
    TreeLAEdit.Text := MyFilesS;
    Readln(TreeFile, MyFilesS);
    TreeRAEdit.Text := MyFilesS;
    Readln(TreeFile, MyFilesS);
    TreeLBAEdit.Text := MyFilesS;
    Readln(TreeFile, MyFilesS);
    TreeRBAEdit.Text := MyFilesS;
    Readln(TreeFile, MyFilesS);
    TreeRLEdit.Text := MyFilesS;
    Readln(TreeFile, MyFilesS);
    val(MyFilesS, V1Color, Code); Codefx(MyFilesS, Code);
    Readln(TreeFile, MyFilesS);
    val(MyFilesS, V2Color, Code); Codefx(MyFilesS, Code);
    Readln(TreeFile, MyFilesS);
    val(MyFilesS, V3Color, Code); Codefx(MyFilesS, Code);
    Readln(TreeFile, MyFilesS);
    val(MyFilesS, V4Color, Code); Codefx(MyFilesS, Code);
    CloseFile(TreeFile);
    SetAllVColors; {Reset Colors}
  end;
end;

procedure TMathForm.TreeLoaderClick(Sender: TObject);
var MyFilesS: string;
{TreeFile:TextFile;}
begin { Display Open dialog box }
  OpenDialog1.InitialDir:=FormulasDir;
  OpenDialog1.Filename := TreeNameEdit.Text;
  OpenDialog1.Filter := 'Tree(*.FLT)|*.FLT';
  if OpenDialog1.Execute then begin
    MyFilesS := Uppercase(ExtractFileExt(OpenDialog1.FileName));
    FormulasDir:=ExtractFilePath(OpenDialog1.FileName);    
    if MyFilesS = '.FLT' then begin
      MyFilesS := ExtractFileName(OpenDialog1.FileName);
      TreeNameEdit.Text := MyFilesS;
      DoTreeLoader(OpenDialog1.FileName);
    end;
  end;
end;
{SaveDialog1}

procedure TMathForm.TreeSaverClick(Sender: TObject);
var MyFilesS: string;
  TreeFile: TextFile;
begin { Display Open dialog box }
  SaveDialog1.Filename := TreeNameEdit.Text;
  SaveDialog1.Filter := 'Tree(*.FLT)|*.FLT';
  SaveDialog1.InitialDir:=FormulasDir;  
  if SaveDialog1.Execute then begin
    MyFilesS := Uppercase(ExtractFileExt(SaveDialog1.FileName));
    FormulasDir:=ExtractFilePath(SaveDialog1.FileName);     
    if MyFilesS = '.FLT' then begin
      MyFilesS := ExtractFileName(SaveDialog1.FileName);
      TreeNameEdit.Text := MyFilesS;
      AssignFile(TreeFile, SaveDialog1.FileName);
      Rewrite(TreeFile);
      if IoResult <> 0 then
      begin
        DoMessages(30102);
      end;
      MyFilesS := TreeXOEdit.Text;
      Writeln(TreeFile, MyFilesS);
      MyFilesS := TreeYOEdit.Text;
      Writeln(TreeFile, MyFilesS);
      MyFilesS := TreeSHEdit.Text;
      Writeln(TreeFile, MyFilesS);
      MyFilesS := TreeSWEdit.Text;
      Writeln(TreeFile, MyFilesS);
      MyFilesS := TreeLAEdit.Text;
      Writeln(TreeFile, MyFilesS);
      MyFilesS := TreeRAEdit.Text;
      Writeln(TreeFile, MyFilesS);
      MyFilesS := TreeLBAEdit.Text;
      Writeln(TreeFile, MyFilesS);
      MyFilesS := TreeRBAEdit.Text;
      Writeln(TreeFile, MyFilesS);
      MyFilesS := TreeRLEdit.Text;
      Writeln(TreeFile, MyFilesS);
      MyFilesS := C2SW(V1Color);
      Writeln(TreeFile, MyFilesS);
      MyFilesS := C2SW(V2Color);
      Writeln(TreeFile, MyFilesS);
      MyFilesS := C2SW(V3Color);
      Writeln(TreeFile, MyFilesS);
      MyFilesS := C2SW(V4Color);
      Writeln(TreeFile, MyFilesS);
      CloseFile(TreeFile);
    end;
  end;
end;


(**************************************************)
(**************************************************)

procedure TMathForm.DoFernLoader(FilesS: string);
var MyFilesS: string; Code: Integer;
  FernFile: TextFile;
begin {Actually load the data into the form}
  if (FileExists(FilesS)) then begin
    AssignFile(FernFile, FilesS);
    Reset(FernFile);
    if IoResult <> 0 then
    begin
      DoMessages(30102);
    end; {Read Ferns and or Dragons}
    Readln(FernFile, MyFilesS);
    FDEditX.Text := MyFilesS;
    Readln(FernFile, MyFilesS);
    FDEditY.Text := MyFilesS;
    Readln(FernFile, MyFilesS);
    OffEditX.Text := MyFilesS;
    Readln(FernFile, MyFilesS);
    OffEditY.Text := MyFilesS;
    Readln(FernFile, MyFilesS);
    AlphaEdit.Text := MyFilesS;
    Readln(FernFile, MyFilesS);
    BetaEdit.Text := MyFilesS;
    Readln(FernFile, MyFilesS);
    GammaEdit.Text := MyFilesS;
    Readln(FernFile, MyFilesS);
    PEdit.Text := MyFilesS;
    Readln(FernFile, MyFilesS);
    QEdit.Text := MyFilesS;
    Readln(FernFile, MyFilesS);
    IterationEdit.Text := MyFilesS;
{V1..V4} Readln(FernFile, MyFilesS);
    val(MyFilesS, V1Color, Code); Codefx(MyFilesS, Code);
    Readln(FernFile, MyFilesS);
    val(MyFilesS, V2Color, Code); Codefx(MyFilesS, Code);
    Readln(FernFile, MyFilesS);
    val(MyFilesS, V3Color, Code); Codefx(MyFilesS, Code);
    Readln(FernFile, MyFilesS);
    val(MyFilesS, V4Color, Code); Codefx(MyFilesS, Code);
    CloseFile(FernFile);
    SetAllVColors; {Reset Colors}
  end;
end;

procedure TMathForm.FernLoaderClick(Sender: TObject);
var MyFilesS: string;
begin { Display Open dialog box }
  OpenDialog1.InitialDir:=FormulasDir;
  OpenDialog1.Filename := FernNameEdit.Text;
  OpenDialog1.Filter := 'Fern Dragon (*.FL?)|*.FLD;*.FLF;';
  if OpenDialog1.Execute then begin
    MyFilesS := Uppercase(ExtractFileExt(OpenDialog1.FileName));
    FormulasDir:=ExtractFilePath(OpenDialog1.FileName);    
    if ((MyFilesS = '.FLF') or (MyFilesS = '.FLD')) then begin
      MyFilesS := ExtractFileName(OpenDialog1.FileName);
      FernNameEdit.Text := MyFilesS;
      DoFernLoader(OpenDialog1.FileName);
    end;
  end;
end;

procedure TMathForm.FernSaverClick(Sender: TObject);
var MyFilesS: string;
  FernFile: TextFile;
begin { Display Open dialog box }
  SaveDialog1.Filename := FernNameEdit.Text;
  SaveDialog1.Filter := 'Fern Dragon (*.FL?)|*.FLD;*.FLF;';
  SaveDialog1.InitialDir:=FormulasDir;  
  if SaveDialog1.Execute then begin
    MyFilesS := Uppercase(ExtractFileExt(SaveDialog1.FileName));
    FormulasDir:=ExtractFilePath(SaveDialog1.FileName);
    if ((MyFilesS = '.FLF') or (MyFilesS = '.FLD')) then begin
      MyFilesS := ExtractFileName(SaveDialog1.FileName);
      FernNameEdit.Text := MyFilesS;
      AssignFile(FernFile, SaveDialog1.FileName);
      Rewrite(FernFile);
      if IoResult <> 0 then
      begin
              DoMessages(30102);
      end; {Read Ferns and or Dragons}
      MyFilesS := FDEditX.Text;
      Writeln(FernFile, MyFilesS);
      MyFilesS := FDEditY.Text;
      Writeln(FernFile, MyFilesS);
      MyFilesS := OffEditX.Text;
      Writeln(FernFile, MyFilesS);
      MyFilesS := OffEditY.Text;
      Writeln(FernFile, MyFilesS);
      MyFilesS := AlphaEdit.Text;
      Writeln(FernFile, MyFilesS);
      MyFilesS := BetaEdit.Text;
      Writeln(FernFile, MyFilesS);
      MyFilesS := GammaEdit.Text;
      Writeln(FernFile, MyFilesS);
      MyFilesS := PEdit.Text;
      Writeln(FernFile, MyFilesS);
      MyFilesS := QEdit.Text;
      Writeln(FernFile, MyFilesS);
      MyFilesS := IterationEdit.Text;
      Writeln(FernFile, MyFilesS);
      MyFilesS := C2SW(V1Color);
      Writeln(FernFile, MyFilesS);
      MyFilesS := C2SW(V2Color);
      Writeln(FernFile, MyFilesS);
      MyFilesS := C2SW(V3Color);
      Writeln(FernFile, MyFilesS);
      MyFilesS := C2SW(V4Color);
      Writeln(FernFile, MyFilesS);
      CloseFile(FernFile);
    end;
  end;
end;

procedure TMathForm.MGFernOKClick(Sender: TObject);
begin
 {Set up Screen here so Fern can be used by all 3 callers}
  MainForm.DoImageStart;
  MainForm.Image2.Canvas.TextOut(10, 10, 'Fern');
  MGFernrun;
  Mainform.DoImageDone;
end;

procedure TMathForm.MGFernrun;
var
  Code, xscale, yscale, xoffset, yoffset, Iterations: Integer;
  Alpha, Beta, Gamma: Extended;
{Color1.TColor;}
begin
  val(FDEditX.Text, xscale, Code);
  val(FDEditY.Text, yscale, Code);
  val(OffEditX.Text, xoffset, Code);
  val(OffEditY.Text, yoffset, Code);
  val(AlphaEdit.Text, Alpha, Code);
  val(BetaEdit.Text, Beta, Code);
  val(GammaEdit.Text, Gamma, Code);
  val(IterationEdit.Text, Iterations, Code);
  if Alpha > 0 then
    Fern3DImage(
      xscale, yscale, xoffset, yoffset, Iterations, {:Integer;}
      Alpha, Beta, Gamma, {:Extended;}
{TDColor1.Color,TDColor2.Color,TDColor3.Color,}
      V1Color)
  else
{Change it so it does 3D fern at Left or Right direction}
    FernImage(
      xscale, yscale, xoffset, yoffset, {:Integer;} Iterations,
{Alpha.Beta,Gamma,:Double;}
      V1Color {V2,V3,V4:TColor});
end;
(**************************************************)

procedure TMathForm.MGDragon3DOKClick(Sender: TObject);
begin
 {Set up Screen here so Dragon can be used by all 3 callers}
  MainForm.DoImageStart;
  gen3ddrgSetup;
  MGDragon3DO;
  Mainform.DoImageDone;
end;

procedure TMathForm.gen3ddrgSetup;
{var TempColor:TColor;}
begin
  MainForm.Image2.Canvas.TextOut(10, 10, 'Dragon');
end; {425.09 for #1,  1005.19 for #2} { 998.6}

procedure TMathForm.MGDragon3DO;
var
  alpha, beta, gamma, scale,
    x_offset, y_offset, QVal, Pe: Extended;
  Iterations, code: integer;
begin
  val(FDEditX.Text, scale, Code);
  val(OffEditX.Text, x_offset, Code);
  val(OffEditY.Text, y_offset, Code);
  val(AlphaEdit.Text, Alpha, Code);
  val(BetaEdit.Text, Beta, Code);
  val(GammaEdit.Text, Gamma, Code);
  val(PEdit.Text, Pe, Code);
  val(QEdit.Text, QVal, Code);
  val(IterationEdit.Text, Iterations, Code);
  if Alpha > 0 then
    gen3ddrg(alpha, beta, gamma,
      scale, x_offset, y_offset, QVal,
      Iterations, V1Color)
  else
    DragOutdo(Pe, QVal, Scale, x_offset, y_offset,
      Iterations, V1Color);
end;

(**************************************************)
(**************************************************)

procedure TMathForm.NumbOpen(FilesS: string);
var NumbFile: BrainFile;
begin
         {Actually load the data into the form}
  if (FileExists(FilesS)) then begin
    AssignFile(NumbFile, FilesS);
    Reset(NumbFile);
    if IoResult <> 0 then
    begin
            DoMessages(30102);
    end; {Read Numb files  NumbSkullBrain:NoBrainer;}
    Read(NumbFile, NumbSkullBrain);
    CloseFile(NumbFile);
    NumbUpDown.Position := 0;
    NumbLevel := 0;
    DeadHead(NumbLevel);
    NumbSkull(NumbLevel);
  end;
end;

procedure TMathForm.NFOpenClick(Sender: TObject);
var MyFilesS: string;
begin { Display Open dialog box }
  OpenDialog1.InitialDir:=FormulasDir;
  OpenDialog1.Filter := 'Numb (*.FLN)|*.FLN';
  OpenDialog1.Filename := NumbFileEdit.text;
  if OpenDialog1.Execute then begin
    MyFilesS := Uppercase(ExtractFileExt(OpenDialog1.FileName));
    if MyFilesS = '.FLN' then begin
      MyFilesS := ExtractFileName(OpenDialog1.FileName);
      NumbFileEdit.Text := MyFilesS;
      NumbOpen(OpenDialog1.FileName);
    end;
  end;
end;

procedure TMathForm.NFSaveClick(Sender: TObject);
var MyFilesS: string;
  NumbFile: BrainFile;
{NumbFile:File of NoBrainer;}
begin { Display Open dialog box }
  SaveDialog1.Filter := 'Numb (*.FLN)|*.FLN';
  SaveDialog1.Filename := NumbFileEdit.text;
  if SaveDialog1.Execute then begin
    MyFilesS := Uppercase(ExtractFileExt(SaveDialog1.FileName));
    FormulasDir:=ExtractFilePath(SaveDialog1.FileName);    
    if (MyFilesS = '.FLN') then begin
      MyFilesS := ExtractFileName(SaveDialog1.FileName);
      NumbFileEdit.Text := MyFilesS;
      LivingDead(NumbLevel); {Get current data}
      AssignFile(NumbFile, SaveDialog1.FileName);
      Rewrite(NumbFile);
      if IoResult <> 0 then begin
              DoMessages(30103);
            end;
           {Write Numb files NumbSkullBrain:NoBrainer;}
      Write(NumbFile, NumbSkullBrain);
      CloseFile(NumbFile);
      if IoResult <> 0 then
            begin
              DoMessages(30103);
            end;
    end;
  end;
end;

(**************************************************)

procedure TMathForm.NumbUpDownClick(Sender: TObject; Button:
  TUDBtnType);
begin
  if (Button = btNext) then begin
    if ((NumbLevel + 1) < 4) then
      SelectNextNumbFun(Button = btNext)
  end
  else begin if ((NumbLevel - 1) >= 0) then
      SelectNextNumbFun(Button = btNext); end;
{ When Button is btPrev, the Down or Left arrow was clicked,
  and the value of Position is about to decrease by Increment.}
end;

procedure TMathForm.SelectNextNumbFun(Way: Boolean);
begin
{Read Present before changing}
  LivingDead(NumbLevel);
  if Way then inc(NumbLevel) else dec(NumbLevel);
{Write Next to display}
  NumbSkull(NumbLevel);
end;

procedure TMathForm.LivingDead(Brains: Integer);
var iV, Code: Integer; eV: Extended;
begin
  val(NFXScale.Text, iV, Code); Codefx(NFXScale.Text, Code);
  NumbSkullBrain.xscale := iV;
  val(NFYScale.Text, iV, Code); Codefx(NFYScale.Text, Code);
  NumbSkullBrain.yscale := iV;
  val(NFXOff.Text, iV, Code); Codefx(NFXOff.Text, Code);
  NumbSkullBrain.xoffset := iV;
  val(NFYOff.Text, iV, Code); Codefx(NFYOff.Text, Code);
  NumbSkullBrain.yoffset := iV;
  val(NFHorizon.Text, iV, Code); Codefx(NFHorizon.Text, Code);
  NumbSkullBrain.Horizon := iV;
  val(NumbIterations.Text, iV, Code); Codefx(NumbIterations.Text,
    Code);
  NumbSkullBrain.Iterations := iV;

  val(NFAlpha.Text, eV, Code); Codefx(NFAlpha.Text, Code);
  NumbSkullBrain.Alpha := eV;
  val(NFBeta.Text, eV, Code); Codefx(NFBeta.Text, Code);
  NumbSkullBrain.Beta := eV;
  val(NFGamma.Text, eV, Code); Codefx(NFGamma.Text, Code);
  NumbSkullBrain.Gamma := eV;
  val(NFPy.Text, eV, Code); Codefx(NFPy.Text, Code);
  NumbSkullBrain.VPy := eV;
  val(NFQx.Text, eV, Code); Codefx(NFQx.Text, Code);
  NumbSkullBrain.HQx := eV;
{}
  val(NFa.Text, eV, Code); Codefx(NFa.Text, Code);
  NumbSkullBrain.a[Brains] := eV;
  val(NFb.Text, eV, Code); Codefx(NFb.Text, Code);
  NumbSkullBrain.b[Brains] := eV;
  val(NFc.Text, eV, Code); Codefx(NFc.Text, Code);
  NumbSkullBrain.c[Brains] := eV;
  val(NFd.Text, eV, Code); Codefx(NFd.Text, Code);
  NumbSkullBrain.d[Brains] := eV;
  val(NFe.Text, eV, Code); Codefx(NFe.Text, Code);
  NumbSkullBrain.e[Brains] := eV;
  val(NFf.Text, eV, Code); Codefx(NFf.Text, Code);
  NumbSkullBrain.f[Brains] := eV;
  val(NFp.Text, eV, Code); Codefx(NFp.Text, Code);
  NumbSkullBrain.p[Brains] := eV;
  val(NFg.Text, eV, Code); Codefx(NFg.Text, Code);
  NumbSkullBrain.g[Brains] := eV;
  val(NFh.Text, eV, Code); Codefx(NFh.Text, Code);
  NumbSkullBrain.h[Brains] := eV;
  val(NFm.Text, eV, Code); Codefx(NFm.Text, Code);
  NumbSkullBrain.m[Brains] := eV;
  val(NFn.Text, eV, Code); Codefx(NFn.Text, Code);
  NumbSkullBrain.n[Brains] := eV;
  val(NFq.Text, eV, Code); Codefx(NFq.Text, Code);
  NumbSkullBrain.q[Brains] := eV;
  val(NFr.Text, eV, Code); Codefx(NFr.Text, Code);
  NumbSkullBrain.r[Brains] := eV;
  NumbSkullBrain.V1 := V1Color;
  NumbSkullBrain.V2 := V2Color;
  NumbSkullBrain.V3 := V3Color;
  NumbSkullBrain.V4 := V4Color;
{xscale,yscale,xoffset,yoffset,Horizon,Iterations: integer;
Alpha, Beta, Gamma,VPy,HQx:Extended;
a,b,c,d,e,f,g,h,m,n,p,q,r:array[0..3] of Extended;
V1,V2,V3,V4:Tcolor;}
end;

procedure TMathForm.DeadHead(Alive: Integer);
var ChangeS: string;
begin {Place Numbers into Form test strings}
  str(NumbSkullBrain.xscale, ChangeS);
  NFXScale.Text := ChangeS;
  str(NumbSkullBrain.yscale, ChangeS);
  NFYScale.Text := ChangeS;
  str(NumbSkullBrain.xoffset, ChangeS);
  NFXOff.Text := ChangeS;
  str(NumbSkullBrain.yoffset, ChangeS);
  NFYOff.Text := ChangeS;
  str(NumbSkullBrain.Horizon, ChangeS);
  NFHorizon.Text := ChangeS;
  str(NumbSkullBrain.Iterations, ChangeS);
  NumbIterations.Text := ChangeS;

  str(NumbSkullBrain.Alpha: 24: 20, ChangeS);
  NFAlpha.Text := ChangeS;
  str(NumbSkullBrain.Beta: 24: 20, ChangeS);
  NFBeta.Text := ChangeS;
  str(NumbSkullBrain.Gamma: 24: 20, ChangeS);
  NFGamma.Text := ChangeS;
  str(NumbSkullBrain.VPy: 24: 20, ChangeS);
  NFPy.Text := ChangeS;
  str(NumbSkullBrain.HQx: 24: 20, ChangeS);
  NFQx.Text := ChangeS;

  V1Color := NumbSkullBrain.V1;
  V2Color := NumbSkullBrain.V2;
  V3Color := NumbSkullBrain.V3;
  V4Color := NumbSkullBrain.V4;
  SetAllVColors;
end;

procedure TMathForm.NumbSkull(HeadBanger: Integer);
var ChangeS: string;
begin {Place Numbers into Form test strings}
  str(NumbSkullBrain.a[HeadBanger]: 24: 20, ChangeS);
  NFa.Text := ChangeS;
  str(NumbSkullBrain.b[HeadBanger]: 24: 20, ChangeS);
  NFb.Text := ChangeS;
  str(NumbSkullBrain.c[HeadBanger]: 24: 20, ChangeS);
  NFc.Text := ChangeS;
  str(NumbSkullBrain.d[HeadBanger]: 24: 20, ChangeS);
  NFd.Text := ChangeS;
  str(NumbSkullBrain.e[HeadBanger]: 24: 20, ChangeS);
  NFe.Text := ChangeS;
  str(NumbSkullBrain.f[HeadBanger]: 24: 20, ChangeS);
  NFf.Text := ChangeS;
  str(NumbSkullBrain.g[HeadBanger]: 24: 20, ChangeS);
  NFg.Text := ChangeS;
  str(NumbSkullBrain.h[HeadBanger]: 24: 20, ChangeS);
  NFh.Text := ChangeS;
  str(NumbSkullBrain.m[HeadBanger]: 24: 20, ChangeS);
  NFm.Text := ChangeS;
  str(NumbSkullBrain.n[HeadBanger]: 24: 20, ChangeS);
  NFn.Text := ChangeS;
  str(NumbSkullBrain.p[HeadBanger]: 24: 20, ChangeS);
  NFp.Text := ChangeS;
  str(NumbSkullBrain.q[HeadBanger]: 24: 20, ChangeS);
  NFq.Text := ChangeS;
  str(NumbSkullBrain.r[HeadBanger]: 24: 20, ChangeS);
  NFr.Text := ChangeS;
end;


procedure TMathForm.ClearHeadedClick(Sender: TObject);
var i: integer;
begin
  NumbSkullBrain.xscale := 0;
  NumbSkullBrain.yscale := 0;
  NumbSkullBrain.xoffset := 0;
  NumbSkullBrain.yoffset := 0;
  NumbSkullBrain.Horizon := 0;
  NumbSkullBrain.Iterations := 0;

  NumbSkullBrain.Alpha := 0;
  NumbSkullBrain.Beta := 0;
  NumbSkullBrain.Gamma := 0;
  NumbSkullBrain.VPy := 0;
  NumbSkullBrain.HQx := 0;
  for i := 0 to 3 do begin
    NumbSkullBrain.a[i] := 0;
    NumbSkullBrain.b[i] := 0;
    NumbSkullBrain.c[i] := 0;
    NumbSkullBrain.d[i] := 0;
    NumbSkullBrain.e[i] := 0;
    NumbSkullBrain.f[i] := 0;
    NumbSkullBrain.g[i] := 0;
    NumbSkullBrain.h[i] := 0;
    NumbSkullBrain.m[i] := 0;
    NumbSkullBrain.n[i] := 0;
    NumbSkullBrain.p[i] := 0;
    NumbSkullBrain.q[i] := 0;
    NumbSkullBrain.r[i] := 0;
  end;
  NumbSkullBrain.V1 := V1Color;
  NumbSkullBrain.V2 := V2Color;
  NumbSkullBrain.V3 := V3Color;
  NumbSkullBrain.V4 := V4Color;
  NumbUpDown.Position := 0;
  NumbLevel := 0;
  DeadHead(NumbLevel);
  NumbSkull(NumbLevel);
end;

procedure TMathForm.NumbRemCBClick(Sender: TObject);
begin
IsNumbREMFast:=(not IsNumbREMFast);
NumbRemCB.Checked:= IsNumbREMFast;
end;
procedure TMathForm.NumbAwakeClick(Sender: TObject);
begin
isNumbAwake:=(not isNumbAwake);
NumbAwake.Checked:= isNumbAwake;
end;

procedure TMathForm.UpDownaClick(Sender: TObject; Button: TUDBtnType);
begin
  If ((Button = btNext) and (IsNumbREMFast))then
    NumbSkullBrain.a[NumbLevel]:= (0.1+NumbSkullBrain.a[NumbLevel])
   else If ((Button = btNext) and (not IsNumbREMFast))then
    NumbSkullBrain.a[NumbLevel]:= (0.01+NumbSkullBrain.a[NumbLevel])
   else If ((Button = btPrev) and (IsNumbREMFast))then
    NumbSkullBrain.a[NumbLevel]:= (NumbSkullBrain.a[NumbLevel]-0.1)
   else If ((Button = btPrev) and (not IsNumbREMFast))then
    NumbSkullBrain.a[NumbLevel]:= (NumbSkullBrain.a[NumbLevel]-0.01);
  NumbSkull(NumbLevel);{Change the display strings}
  If isNumbAwake then
  begin
    MainForm.DoImageStart;
    SetNumb3DImage;
    Numb3DImage {(NumbSkullBrain)};
    Mainform.DoImageDone;
  end;
end;


procedure TMathForm.UpDownbClick(Sender: TObject; Button: TUDBtnType);
begin
  If ((Button = btNext) and (IsNumbREMFast))then
    NumbSkullBrain.b[NumbLevel]:= (0.1+NumbSkullBrain.b[NumbLevel])
   else If ((Button = btNext) and (not IsNumbREMFast))then
    NumbSkullBrain.b[NumbLevel]:= (0.01+NumbSkullBrain.b[NumbLevel])
   else If ((Button = btPrev) and (IsNumbREMFast))then
    NumbSkullBrain.b[NumbLevel]:= (NumbSkullBrain.b[NumbLevel]-0.1)
   else If ((Button = btPrev) and (not IsNumbREMFast))then
    NumbSkullBrain.b[NumbLevel]:= (NumbSkullBrain.b[NumbLevel]-0.01);
  NumbSkull(NumbLevel);{Change the display strings}
  If isNumbAwake then
  begin
    MainForm.DoImageStart;
    SetNumb3DImage;
    Numb3DImage {(NumbSkullBrain)};
    Mainform.DoImageDone;
  end;
end;

procedure TMathForm.UpDowncClick(Sender: TObject; Button: TUDBtnType);
begin
  If ((Button = btNext) and (IsNumbREMFast))then
    NumbSkullBrain.c[NumbLevel]:= (0.1+NumbSkullBrain.c[NumbLevel])
   else If ((Button = btNext) and (not IsNumbREMFast))then
    NumbSkullBrain.c[NumbLevel]:= (0.01+NumbSkullBrain.c[NumbLevel])
   else If ((Button = btPrev) and (IsNumbREMFast))then
    NumbSkullBrain.c[NumbLevel]:= (NumbSkullBrain.c[NumbLevel]-0.1)
   else If ((Button = btPrev) and (not IsNumbREMFast))then
    NumbSkullBrain.c[NumbLevel]:= (NumbSkullBrain.c[NumbLevel]-0.01);
  NumbSkull(NumbLevel);{Change the display strings}
  If isNumbAwake then
  begin
    MainForm.DoImageStart;
    SetNumb3DImage;
    Numb3DImage {(NumbSkullBrain)};
    Mainform.DoImageDone;
  end;
end;

procedure TMathForm.UpDowndClick(Sender: TObject; Button: TUDBtnType);
begin
  If ((Button = btNext) and (IsNumbREMFast))then
    NumbSkullBrain.d[NumbLevel]:= (0.1+NumbSkullBrain.d[NumbLevel])
   else If ((Button = btNext) and (not IsNumbREMFast))then
    NumbSkullBrain.d[NumbLevel]:= (0.01+NumbSkullBrain.d[NumbLevel])
   else If ((Button = btPrev) and (IsNumbREMFast))then
    NumbSkullBrain.d[NumbLevel]:= (NumbSkullBrain.d[NumbLevel]-0.1)
   else If ((Button = btPrev) and (not IsNumbREMFast))then
    NumbSkullBrain.d[NumbLevel]:= (NumbSkullBrain.d[NumbLevel]-0.01);
  NumbSkull(NumbLevel);{Change the display strings}
  If isNumbAwake then
  begin
    MainForm.DoImageStart;
    SetNumb3DImage;
    Numb3DImage {(NumbSkullBrain)};
    Mainform.DoImageDone;
  end;
end;

procedure TMathForm.UpDowneClick(Sender: TObject; Button: TUDBtnType);
begin
  If ((Button = btNext) and (IsNumbREMFast))then
    NumbSkullBrain.e[NumbLevel]:= (0.1+NumbSkullBrain.e[NumbLevel])
   else If ((Button = btNext) and (not IsNumbREMFast))then
    NumbSkullBrain.e[NumbLevel]:= (0.01+NumbSkullBrain.e[NumbLevel])
   else If ((Button = btPrev) and (IsNumbREMFast))then
    NumbSkullBrain.e[NumbLevel]:= (NumbSkullBrain.e[NumbLevel]-0.1)
   else If ((Button = btPrev) and (not IsNumbREMFast))then
    NumbSkullBrain.e[NumbLevel]:= (NumbSkullBrain.e[NumbLevel]-0.01);
  NumbSkull(NumbLevel);{Change the display strings}
  If isNumbAwake then
  begin
    MainForm.DoImageStart;
    SetNumb3DImage;
    Numb3DImage {(NumbSkullBrain)};
    Mainform.DoImageDone;
  end;
end;

procedure TMathForm.UpDownfClick(Sender: TObject; Button: TUDBtnType);
begin
  If ((Button = btNext) and (IsNumbREMFast))then
    NumbSkullBrain.f[NumbLevel]:= (0.1+NumbSkullBrain.f[NumbLevel])
   else If ((Button = btNext) and (not IsNumbREMFast))then
    NumbSkullBrain.f[NumbLevel]:= (0.01+NumbSkullBrain.f[NumbLevel])
   else If ((Button = btPrev) and (IsNumbREMFast))then
    NumbSkullBrain.f[NumbLevel]:= (NumbSkullBrain.f[NumbLevel]-0.1)
   else If ((Button = btPrev) and (not IsNumbREMFast))then
    NumbSkullBrain.f[NumbLevel]:= (NumbSkullBrain.f[NumbLevel]-0.01);
  NumbSkull(NumbLevel);{Change the display strings}
  If isNumbAwake then
  begin
    MainForm.DoImageStart;
    SetNumb3DImage;
    Numb3DImage {(NumbSkullBrain)};
    Mainform.DoImageDone;
  end;
end;

procedure TMathForm.UpDowngClick(Sender: TObject; Button: TUDBtnType);
begin
  If ((Button = btNext) and (IsNumbREMFast))then
    NumbSkullBrain.g[NumbLevel]:= (0.1+NumbSkullBrain.g[NumbLevel])
   else If ((Button = btNext) and (not IsNumbREMFast))then
    NumbSkullBrain.g[NumbLevel]:= (0.01+NumbSkullBrain.g[NumbLevel])
   else If ((Button = btPrev) and (IsNumbREMFast))then
    NumbSkullBrain.g[NumbLevel]:= (NumbSkullBrain.g[NumbLevel]-0.1)
   else If ((Button = btPrev) and (not IsNumbREMFast))then
    NumbSkullBrain.g[NumbLevel]:= (NumbSkullBrain.g[NumbLevel]-0.01);
  NumbSkull(NumbLevel);{Change the display strings}
  If isNumbAwake then
  begin
    MainForm.DoImageStart;
    SetNumb3DImage;
    Numb3DImage {(NumbSkullBrain)};
    Mainform.DoImageDone;
  end;
end;

procedure TMathForm.UpDownhClick(Sender: TObject; Button: TUDBtnType);
begin
  If ((Button = btNext) and (IsNumbREMFast))then
    NumbSkullBrain.h[NumbLevel]:= (0.1+NumbSkullBrain.h[NumbLevel])
   else If ((Button = btNext) and (not IsNumbREMFast))then
    NumbSkullBrain.h[NumbLevel]:= (0.01+NumbSkullBrain.h[NumbLevel])
   else If ((Button = btPrev) and (IsNumbREMFast))then
    NumbSkullBrain.h[NumbLevel]:= (NumbSkullBrain.h[NumbLevel]-0.1)
   else If ((Button = btPrev) and (not IsNumbREMFast))then
    NumbSkullBrain.h[NumbLevel]:= (NumbSkullBrain.h[NumbLevel]-0.01);
  NumbSkull(NumbLevel);{Change the display strings}
  If isNumbAwake then
  begin
    MainForm.DoImageStart;
    SetNumb3DImage;
    Numb3DImage {(NumbSkullBrain)};
    Mainform.DoImageDone;
  end;
end;

procedure TMathForm.UpDownmClick(Sender: TObject; Button: TUDBtnType);
begin
  If ((Button = btNext) and (IsNumbREMFast))then
    NumbSkullBrain.m[NumbLevel]:= (0.1+NumbSkullBrain.m[NumbLevel])
   else If ((Button = btNext) and (not IsNumbREMFast))then
    NumbSkullBrain.m[NumbLevel]:= (0.01+NumbSkullBrain.m[NumbLevel])
   else If ((Button = btPrev) and (IsNumbREMFast))then
    NumbSkullBrain.m[NumbLevel]:= (NumbSkullBrain.m[NumbLevel]-0.1)
   else If ((Button = btPrev) and (not IsNumbREMFast))then
    NumbSkullBrain.m[NumbLevel]:= (NumbSkullBrain.m[NumbLevel]-0.01);
  NumbSkull(NumbLevel);{Change the display strings}
  If isNumbAwake then
  begin
    MainForm.DoImageStart;
    SetNumb3DImage;
    Numb3DImage {(NumbSkullBrain)};
    Mainform.DoImageDone;
  end;
end;

procedure TMathForm.UpDownnClick(Sender: TObject; Button: TUDBtnType);
begin
  If ((Button = btNext) and (IsNumbREMFast))then
    NumbSkullBrain.n[NumbLevel]:= (0.1+NumbSkullBrain.n[NumbLevel])
   else If ((Button = btNext) and (not IsNumbREMFast))then
    NumbSkullBrain.n[NumbLevel]:= (0.01+NumbSkullBrain.n[NumbLevel])
   else If ((Button = btPrev) and (IsNumbREMFast))then
    NumbSkullBrain.n[NumbLevel]:= (NumbSkullBrain.n[NumbLevel]-0.1)
   else If ((Button = btPrev) and (not IsNumbREMFast))then
    NumbSkullBrain.n[NumbLevel]:= (NumbSkullBrain.n[NumbLevel]-0.01);
  NumbSkull(NumbLevel);{Change the display strings}
  If isNumbAwake then
  begin
    MainForm.DoImageStart;
    SetNumb3DImage;
    Numb3DImage {(NumbSkullBrain)};
    Mainform.DoImageDone;
  end;
end;

procedure TMathForm.UpDownqClick(Sender: TObject; Button: TUDBtnType);
begin
  If ((Button = btNext) and (IsNumbREMFast))then
    NumbSkullBrain.q[NumbLevel]:= (0.1+NumbSkullBrain.q[NumbLevel])
   else If ((Button = btNext) and (not IsNumbREMFast))then
    NumbSkullBrain.q[NumbLevel]:= (0.01+NumbSkullBrain.q[NumbLevel])
   else If ((Button = btPrev) and (IsNumbREMFast))then
    NumbSkullBrain.q[NumbLevel]:= (NumbSkullBrain.q[NumbLevel]-0.1)
   else If ((Button = btPrev) and (not IsNumbREMFast))then
    NumbSkullBrain.q[NumbLevel]:= (NumbSkullBrain.q[NumbLevel]-0.01);
  NumbSkull(NumbLevel);{Change the display strings}
  If isNumbAwake then
  begin
    MainForm.DoImageStart;
    SetNumb3DImage;
    Numb3DImage {(NumbSkullBrain)};
    Mainform.DoImageDone;
  end;
end;

procedure TMathForm.UpDownrClick(Sender: TObject; Button: TUDBtnType);
begin
  If ((Button = btNext) and (IsNumbREMFast))then
    NumbSkullBrain.r[NumbLevel]:= (0.1+NumbSkullBrain.r[NumbLevel])
   else If ((Button = btNext) and (not IsNumbREMFast))then
    NumbSkullBrain.r[NumbLevel]:= (0.01+NumbSkullBrain.r[NumbLevel])
   else If ((Button = btPrev) and (IsNumbREMFast))then
    NumbSkullBrain.r[NumbLevel]:= (NumbSkullBrain.r[NumbLevel]-0.1)
   else If ((Button = btPrev) and (not IsNumbREMFast))then
    NumbSkullBrain.r[NumbLevel]:= (NumbSkullBrain.r[NumbLevel]-0.01);
  NumbSkull(NumbLevel);{Change the display strings}
  If isNumbAwake then
  begin
    MainForm.DoImageStart;
    SetNumb3DImage;
    Numb3DImage {(NumbSkullBrain)};
    Mainform.DoImageDone;
  end;
end;

procedure TMathForm.UpDownpClick(Sender: TObject; Button: TUDBtnType);
begin
  If ((Button = btNext) and (IsNumbREMFast))then
    NumbSkullBrain.p[NumbLevel]:= (0.1+NumbSkullBrain.p[NumbLevel])
   else If ((Button = btNext) and (not IsNumbREMFast))then
    NumbSkullBrain.p[NumbLevel]:= (0.01+NumbSkullBrain.p[NumbLevel])
   else If ((Button = btPrev) and (IsNumbREMFast))then
    NumbSkullBrain.p[NumbLevel]:= (NumbSkullBrain.p[NumbLevel]-0.1)
   else If ((Button = btPrev) and (not IsNumbREMFast))then
    NumbSkullBrain.p[NumbLevel]:= (NumbSkullBrain.p[NumbLevel]-0.01);
  NumbSkull(NumbLevel);{Change the display strings}
  If isNumbAwake then
  begin
    MainForm.DoImageStart;
    SetNumb3DImage;
    Numb3DImage {(NumbSkullBrain)};
    Mainform.DoImageDone;
  end;
end;
(**************************************************)

procedure TMathForm.NFRunClick(Sender: TObject);
begin
  LivingDead(NumbLevel); {Get current data}
  MainForm.DoImageStart;
  SetNumb3DImage;
  Numb3DImage {(NumbSkullBrain)};
  Mainform.DoImageDone;
end;
(**************************************************)
(**************************************************)


(**************************************************)
(**************************************************)

procedure TMathForm.SkyUpDownClick(Sender: TObject; Button:
  TUDBtnType);
begin
  if (Button = btNext) then begin
    if ((SkyLevel + 1) < 4) then
      SelectNextSkyFun(Button = btNext)
  end
  else begin if ((SkyLevel - 1) >= 0) then
      SelectNextSkyFun(Button = btNext); end;
{ When Button is btPrev, the Down or Left arrow was clicked,
  and the value of Position is about to decrease by Increment.}
end;

procedure TMathForm.SelectNextSkyFun(Way: Boolean);
begin
{Read Present before changing}
  SkyDiver(SkyLevel);
  if Way then inc(SkyLevel) else dec(SkyLevel);
{Write Next to display}
  SkyPilot(SkyLevel);
end;

procedure TMathForm.SkyPilot(Fog: Integer);
var ChangeS: string;
begin {Place Numbers into Form test strings}
  str(SkyKing.a[Fog]: 24: 20, ChangeS);
  SkyA.Text := ChangeS;
  str(SkyKing.b[Fog]: 24: 20, ChangeS);
  SkyB.Text := ChangeS;
  str(SkyKing.c[Fog]: 24: 20, ChangeS);
  SkyC.Text := ChangeS;
  str(SkyKing.d[Fog]: 24: 20, ChangeS);
  SkyD.Text := ChangeS;
  str(SkyKing.e[Fog]: 24: 20, ChangeS);
  SkyE.Text := ChangeS;
  str(SkyKing.f[Fog]: 24: 20, ChangeS);
  SkyF.Text := ChangeS;
  str(SkyKing.p[Fog]: 24: 20, ChangeS);
  SkyP.Text := ChangeS;
end;
{xscale,yscale,xoffset,yoffset,Horizon,Iterations: integer;
a,b,c,d,e,f,p:array[0..3] of Double;
V1,V2,V3,V4:Tcolor;}

procedure TMathForm.SkyPen(Smoke: Integer);
var ChangeS: string;
begin
  str(SkyKing.XScale, ChangeS);
  SkyXScale.Text := ChangeS;
  str(SkyKing.yscale, ChangeS);
  SkyYScale.Text := ChangeS;
  str(SkyKing.xoffset, ChangeS);
  SkyXOff.Text := ChangeS;
  str(SkyKing.yoffset, ChangeS);
  SkyYOff.Text := ChangeS;
  str(SkyKing.Horizon, ChangeS);
  SkyHorizon.Text := ChangeS;
  str(SkyKing.Iterations, ChangeS);
  SkyIterations.Text := ChangeS;

{xscale,yscale,xoffset,yoffset,Horizon,Iterations}
  V1Color := SkyKing.V1;
  V2Color := SkyKing.V2;
  V3Color := SkyKing.V3;
  V4Color := SkyKing.V4;
  SetAllVColors;
end;

procedure TMathForm.SkyDiver(Oil: Integer);
var iV, Code: Integer; eV: Extended;
begin
  val(SkyXScale.Text, iV, Code); Codefx(SkyXScale.Text, Code);
  SkyKing.xscale := iV;
  val(SkyYScale.Text, iV, Code); Codefx(SkyYScale.Text, Code);
  SkyKing.yscale := iV;
  val(SkyXOff.Text, iV, Code); Codefx(SkyXOff.Text, Code);
  SkyKing.xoffset := iV;
  val(SkyYOff.Text, iV, Code); Codefx(SkyYOff.Text, Code);
  SkyKing.yoffset := iV;
  val(SkyHorizon.Text, iV, Code); Codefx(SkyHorizon.Text, Code);
  SkyKing.Horizon := iV;
  val(SkyIterations.Text, iV, Code); Codefx(SkyIterations.Text,
    Code);
  SkyKing.Iterations := iV;

  val(Skya.Text, eV, Code); Codefx(Skya.Text, Code);
  SkyKing.a[Oil] := eV;
  val(Skyb.Text, eV, Code); Codefx(Skyb.Text, Code);
  SkyKing.b[Oil] := eV;
  val(Skyc.Text, eV, Code); Codefx(Skyc.Text, Code);
  SkyKing.c[Oil] := eV;
  val(Skyd.Text, eV, Code); Codefx(Skyd.Text, Code);
  SkyKing.d[Oil] := eV;
  val(Skye.Text, eV, Code); Codefx(Skye.Text, Code);
  SkyKing.e[Oil] := eV;
  val(Skyf.Text, eV, Code); Codefx(Skyf.Text, Code);
  SkyKing.f[Oil] := eV;
  val(Skyp.Text, eV, Code); Codefx(Skyp.Text, Code);
  SkyKing.p[Oil] := eV;
  SkyKing.V1 := V1Color;
  SkyKing.V2 := V2Color;
  SkyKing.V3 := V3Color;
  SkyKing.V4 := V4Color;
end;


procedure TMathForm.SkyClearClick(Sender: TObject);
var i: integer;
begin
  SkyKing.xscale := 0;
  SkyKing.yscale := 0;
  SkyKing.xoffset := 0;
  SkyKing.yoffset := 0;
  SkyKing.Horizon := 0;
  SkyKing.Iterations := 0;

  for i := 0 to 3 do begin
    SkyKing.a[i] := 0;
    SkyKing.b[i] := 0;
    SkyKing.c[i] := 0;
    SkyKing.d[i] := 0;
    SkyKing.e[i] := 0;
    SkyKing.f[i] := 0;
    SkyKing.p[i] := 0;
  end;
  SkyKing.V1 := V1Color;
  SkyKing.V2 := V2Color;
  SkyKing.V3 := V3Color;
  SkyKing.V4 := V4Color;
  SkyUpDown.Position := 0;
  SkyLevel := 0;
  SkyPilot(SkyLevel);
  SkyPen(SkyLevel);
end;

procedure TMathForm.SkyShowClick(Sender: TObject);
begin
  SkyDiver(SkyLevel); {Get current data}
  MainForm.DoImageStart;
  SetSkyImage;
  SkyImage;
  Mainform.DoImageDone;
(*
{Procedure SkyImage (fern)
(xscale,yscale,xoffset,yoffset,Iterations,Horizon:Integer;
                        FTempColor:TColor);
var a,b,c,d,e,f,p: array[0..3] of Extended;}
Sky2D = record
{Brain:Headcase; }
xscale,yscale,xoffset,yoffset,Horizon,Iterations: integer;
{Alpha, Beta, Gamma,P,Q:Extended; }
a,b,c,d,e,f,{g,h,m,n,q,r,}p:array[0..3] of Double;
V1,V2,V3,V4:Tcolor;
end;
*)
end;
(**************************************************)

procedure TMathForm.SkyLoaderDo(FilesS: string);
var fSkyFile: SkyFile;
begin {Actually load the data into the form}
  if (FileExists(FilesS)) then begin
    AssignFile(fSkyFile, FilesS);
    Reset(fSkyFile);
    if IoResult <> 0 then
    begin
      DoMessages(30102);
    end; {Read Sky files  SkyKing:Sky2D;}
    Read(fSkyFile, SkyKing);
    CloseFile(fSkyFile);
    SkyUpDown.Position := 0;
    SkyLevel := 0;
    SkyPilot(SkyLevel);
    SkyPen(SkyLevel);
  end;
end;

procedure TMathForm.SkyLoaderClick(Sender: TObject);
var MyFilesS: string;
begin { Display Open dialog box }
  OpenDialog1.InitialDir:=FormulasDir;
  OpenDialog1.Filter := 'Sky (*.FLS)|*.FLS';
  OpenDialog1.Filename := SkyNameEdit.Text;
  if OpenDialog1.Execute then begin
    MyFilesS := Uppercase(ExtractFileExt(OpenDialog1.FileName));
    FormulasDir:=ExtractFilePath(OpenDialog1.FileName);
    if MyFilesS = '.FLS' then begin
      MyFilesS := ExtractFileName(OpenDialog1.FileName);
      SkyNameEdit.Text := MyFilesS;
      SkyLoaderDo(OpenDialog1.FileName);
    end;
  end;
end;

procedure TMathForm.SkySaverClick(Sender: TObject);
var MyFilesS: string;
  fSkyFile: SkyFile;
begin { Display Open dialog box }
  SaveDialog1.InitialDir:=FormulasDir;
  SaveDialog1.Filter := 'Sky (*.FLS)|*.FLS';
  SaveDialog1.Filename := SkyNameEdit.Text;
  if SaveDialog1.Execute then begin
    MyFilesS := Uppercase(ExtractFileExt(SaveDialog1.FileName));
    FormulasDir:=ExtractFilePath(SaveDialog1.FileName);
    if MyFilesS = '.FLS' then begin
      MyFilesS := ExtractFileName(SaveDialog1.FileName);
      SkyNameEdit.Text := MyFilesS;
      AssignFile(fSkyFile, SaveDialog1.FileName);
      Rewrite(fSkyFile);
      if IoResult <> 0 then
      begin
        DoMessages(30102);
      end; {Write Sky files}
      SkyDiver(SkyLevel);
      Write(fSkyFile, SkyKing);
      CloseFile(fSkyFile);
    end;
  end;
end;

(**************************************************)
(**************************************************)

procedure TMathForm.DIYSetupClick(Sender: TObject);
{var maxcolx,maxrowy:Integer;}
begin
  if (not bVista) then begin
    bVista := True;
    DIYSet.Color := clLime;
    MainForm.DoImageStart;
{MainForm.Show;}
    with MainForm.Image2.Canvas do begin
{	maxcolx :=(FYImageX-1);
 maxrowy := (FYImageY-1);}
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,maxcolx,maxrowy));
Pen.Color := RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
Brush.Color:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));}
      DIYMemo.Clear;
      DIYMemo.Lines.Add('DIY List');
      Mainform.DoImageDone;
    end;
  end else begin
    bVista := False;
    DIYSet.Color := clRed;
  end;
end;

procedure TMathForm.DIYBitmapLoaderClick(Sender: TObject);
var{PixelString,} MyFilesExtension: string;
{  Pixeli: Integer;}
begin
  if OpenPictureDialog1.Execute then begin
    MyFilesExtension :=
      Uppercase(ExtractFileExt(OpenPictureDialog1.FileName));
    if MyFilesExtension = '.BMP' then begin
        { Add code to open OpenPictureDialog1.FileName }
      G_Math_Image.Picture.Bitmap.TransparentMode := tmAuto;
      G_Math_Image.Picture.LoadFromFile(OpenPictureDialog1.FileName);
      if (G_Math_Image.Picture.Bitmap.PixelFormat <> MyPixelFormat)
        then begin
{        if (PixelScanSize = 4) then Pixeli := 32 else
          Pixeli := 24;
        str(Pixeli, PixelString);}
        DoMessages(12);
      end else begin
        {PLACE into holding pattern}
        DIYBitmapEdit.Text :=
          ExtractFileName(OpenPictureDialog1.FileName);
        DIYFilename := OpenPictureDialog1.FileName;
      end;
    end else
      DoMessages(30093);
  end;
end;

procedure TMathForm.DIYLoadClick(Sender: TObject);
var MyFilesExtension: string;
begin { Display Open dialog box }
  OpenDialog1.Filename := DIYFileFL;
  OpenDialog1.Filter := 'DIY (*.DIY)|*.DIY';
  if OpenDialog1.Execute then begin
    MyFilesExtension :=
      Uppercase(ExtractFileExt(OpenDialog1.FileName));
    if MyFilesExtension = '.DIY' then begin
      DIYFileFL := OpenDialog1.Filename;
      DIYMemo.Lines.LoadFromFile(OpenDialog1.Filename);
    end;
  end;
end;

procedure TMathForm.DIYAddLoadClick(Sender: TObject);
var MyFilesExtension: string;
begin { Display Open dialog box }
  OpenDialog1.Filename := DIYFileFL;
  OpenDialog1.Filter := 'DIY (*.DIY)|*.DIY';
  if OpenDialog1.Execute then begin
    MyFilesExtension :=
      Uppercase(ExtractFileExt(OpenDialog1.FileName));
    if MyFilesExtension = '.DIY' then begin
        {Load into #2 then copy into #1}
      DIYMemo2.Lines.LoadFromFile(OpenDialog1.Filename);
      DIYMemo.Lines.AddStrings(DIYMemo2.Lines);
    end;
  end;
end;

procedure TMathForm.DIYSaveClick(Sender: TObject);
begin
  SaveDialog1.Filename := DIYFileFL;
  SaveDialog1.Filter := 'DIY (*.DIY)|*.DIY';
  if SaveDialog1.Execute then { Display Open dialog box }
  begin
    DIYMemo.Lines.SaveToFile(SaveDialog1.Filename);
  end;
end;

procedure TMathForm.DIYFontClick(Sender: TObject);
begin
  {If Font is chosen then GET and KEEP what it is}
  MainForm.DoImageStart; {never undone  }
  if FontDialog1.Execute then
  MainForm.Image2.Canvas.Font := FontDialog1.Font;
    {Always 1, changed or not}
  if MainForm.Image2.Canvas.Font.Color <> FBackGroundColor then
    MainForm.Image2.Canvas.Brush.Color := FBackGroundColor else
    MainForm.Image2.Canvas.Brush.Color := (16000000 -
      FBackGroundColor);
{Store the Font as a FILE and Load it when RUN}
{  FontStorage := FractalFont;}
{DIYTextStorage:String;
FontStorage:=FractalFont;}

  DIYAnnotate := True;
  AnnotationForm.Show;
end;
{If the combination of ... specifies a font
 that is not available on the system,
Windows substitutes a different font.
Name,
type TFontName = type string;
property Name: TFontName;
CharSet,
type TFontCharset = 0..255;
property Charset: TFontCharset nodefault;
Pitch,
type TFontPitch = (fpDefault, fpVariable, fpFixed);
property Pitch: TFontPitch;
 and Size
 Specifies the height of the font in points.
property Size: Integer;
Color,
property Color: TColor;
Height,
property Height: Integer;
Use Height to specify
the height of the font in pixels.
Style
type
  TFontStyle = (fsBold, fsItalic, fsUnderline, fsStrikeOut);
  TFontStyles = set of TFontStyle;
property Style: TFontStyles;}



procedure TMathForm.DIYLandClick(Sender: TObject);
begin
  if ((DIYPallette.ItemIndex > -1) and (bVista)) then begin
{Activate Mouse to place the OBJECT's 'Symbolized' on Main Display
 and then Get the Coordinate data to Place in the Edit boxes
and Prepare for Add ing to the Memo}
    bVistaLive := True;
    bMousing := True;
    ITriangling := 0;
{(dtEllipse,    dtCSEllipse,   dtPolygon,
  dtVTriangle,   dtTriangle,    dtTSTriangle,
  dtVPRectangle, dtVERectangle, dtVSRectangle,
  dtVDRectangle, dtVTRectangle, dtVFRectangle, dtVNRectangle,
  dtVMRectangle, dtVARectangle, dtVWRectangle, dtVCRectangle,
  dtVBRectangle, dtVRectangle,  dtRectangle,   dtRoundRect,
  dtLine,dtPBrush);}
{
TriangleZE        dtVTriangle
QuadrangleZE      dtVRectangle
Ellipse (Circle)  dtEllipse
Tri Angle         dtTriangle
Rectangle         dtRectangle
Pandora's Box.vlp     dtVPRectangle
Empty Blank.vle       dtVERectangle
Line              dtLine
C Shadow [L]      dtCSEllipse
T Shadow [L]      dtTSTriangle

Fractal.FLM      dtVMRectangle
Turtle.vla       dtVARectangle
Tree.FLT         dtVTRectangle
Fern.FLF         dtVFRectangle
Dragon.FLD       dtVDRectangle
Sky Cloud.FLS    dtVSRectangle
Numb.FLN         dtVNRectangle
Collage.FLO      dtVCRectangle
Torso.FLW        dtVWRectangle
Bitmap.bmp       dtVBRectangle
}
    if (ZoomingOn) then MainForm.UnZoom;
    with MainForm.Image2.Canvas do begin
      case DIYPallette.ItemIndex of
        0: begin ITriangling := 0;
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsDiagCross;
            MainForm.DrawingTool := dtVTriangle;
          end;
        {LineMaker Polygon? Triangle}
        1: begin ITriangling := 0;
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsDiagCross;
            MainForm.DrawingTool := dtVRectangle;
          end;
        {Rectangle X1Y1X2Y2 R L  Rectangle}
        2: begin
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsSolid;
            MainForm.DrawingTool := dtEllipse;
          end;
        {Ellipse X1Y1X2Y2 R L  Circle}
        3: begin ITriangling := 0;
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsSolid;
            MainForm.DrawingTool := dtTriangle;
          end;
        { Triangle}
        4: begin
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsSolid;
            MainForm.DrawingTool := dtRectangle;
          end;
        {Rectangle X1Y1X2Y2 R L  Flatland}
        5: begin
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsSolid;
            MainForm.DrawingTool := dtVPRectangle;
          end;
        {Rectangle X1Y1X2Y2 R L  Pandora}
        6: begin
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsSolid;
            MainForm.DrawingTool := dtVERectangle;
          end;
        {Rectangle X1Y1X2Y2 R L  Empty Blank}
        7: begin
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsSolid;
            MainForm.DrawingTool := dtLine;
          end;
        {Line X1Y1X2Y2 R L  Line}
        8: begin
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsBDiagonal;
            MainForm.DrawingTool := dtCSEllipse;
          end;
        {Ellipse X1Y1X2Y2 R L C Shadow}
        9: begin ITriangling := 0;
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsBDiagonal;
            MainForm.DrawingTool := dtTSTriangle;
          end;
        {LineMaker-Polygon? X1Y1X2Y2 R L T Shadow}
        10: begin
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsHorizontal;
            MainForm.DrawingTool := dtVMRectangle;
          end;
        {Rectangle X1Y1X2Y2 R L  Fractal}
        11: begin
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsHorizontal;
            MainForm.DrawingTool := dtVARectangle;
          end;
        {Rectangle X1Y1X2Y2 R L  Turtle}
        12: begin
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsHorizontal;
            MainForm.DrawingTool := dtVTRectangle;
          end;
        {Rectangle X1Y1X2Y2 R L  Tree}
        13: begin
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsHorizontal;
            MainForm.DrawingTool := dtVFRectangle;
          end;
        {Rectangle X1Y1X2Y2 R L  Fern}

        14: begin
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsFDiagonal;
            MainForm.DrawingTool := dtVDRectangle;
          end;
        {Rectangle X1Y1X2Y2 R L  Dragon}
        15: begin
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsFDiagonal;
            MainForm.DrawingTool := dtVSRectangle;
          end;
        {dtVSRectangle X1Y1X2Y2 R L Sky Cloud}
        16: begin
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsFDiagonal;
            MainForm.DrawingTool := dtVNRectangle;
          end;
        {Rectangle X1Y1X2Y2 R L  Numb Fun}
        17: begin
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsHorizontal;
            MainForm.DrawingTool := dtVCRectangle;
          end;
        {Rectangle X1Y1X2Y2 R L  Collage}
        18: begin
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsHorizontal;
            MainForm.DrawingTool := dtVWRectangle;
          end;
        {Rectangle X1Y1X2Y2 R L  Torso}
        19: begin
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsCross;
            MainForm.DrawingTool := dtVBRectangle;
          end;
        {Rectangle X1Y1X2Y2 R L  Bitmap}
      end; {of case}
    end;
{NEVER UNDONE   Mainform.DoImageDone;}
  end;
end;

procedure TMathForm.DIYRotateClick(Sender: TObject);
begin
 {Highlight the selected Object
 Place the mouse in the area to 'Pull"
 Record Location/direction as Z
  DIYZ:=0;}
end;

procedure TMathForm.DIYLightClick(Sender: TObject);
begin
 {Highlight the selected Object
 Place the mouse in the area to 'Light"
 Record Location/direction as L
   DIYL:=0;}
  bLightning := True;
end;


procedure TMathForm.DIYAddClick(Sender: TObject);
var DIYS: string;
begin
{ADD to  list... NO CHECKING DONE, better be right
Read data text according to required Object inputs}
{Hopefully all the data from edit boxes is correct}
{Place data-info onto the DIYMemo...
 for processing or saving}
  if ((DIYPallette.ItemIndex > -1) and (bVista)) then begin
    bMousing := False;
    bLightning := False;
{ (dtSCEllipse, dtEllipse, dtCSEllipse,
  dtVTriangle,  dtTriangle, dtTSTriangle,
  dtVDRectangle, dtVRectangle,  dtVTRectangle,  dtVFRectangle,
  dtVNRectangle, dtRectangle, dtVBRectangle, dtRoundRect,
  dtLine);}
    case DIYPallette.ItemIndex of
{DIYXY,DIYX2Y2 DIYX3Y3, DIYX4Y4: TPoint;
DIYZ,DIYL:Integer;
DIYFilename (Bitmap)}
{DIYTextStorage:String;      DIYWxEdit DIYHyEdit
FontStorage:=FractalFont;}
      0: begin
          DIYMemo.Lines.Append('dtVTriangle');
          DIYMemo.Lines.Append('X : ' + DIYXEdit.Text);
          DIYMemo.Lines.Append('Y : ' + DIYYEdit.Text);
          DIYMemo.Lines.Append('X2: ' + DIYWxEdit.Text);
          DIYMemo.Lines.Append('Y2: ' + DIYHyEdit.Text);
          str(DIYX3Y3.X, DIYS);
          MathForm.DIYMemo.Lines.Append('X3: ' + DIYS);
          str(DIYX3Y3.Y, DIYS);
          MathForm.DIYMemo.Lines.Append('Y3: ' + DIYS);
          DIYMemo.Lines.Append('Z : ' + DIYZEdit.Text);
          DIYMemo.Lines.Append('E : ' + DIYEEdit.Text);
          DIYMemo.Lines.Append('V1: ' + C2SW(V1Color));
          DIYMemo.Lines.Append('V2: ' + C2SW(V2Color));
        end; {DIYXY,DIYX2Y2 DIYX3Y3, DIYX4Y4
        dtVTriangle FTriangle X1Y1X2Y2 R L}
      1: begin
          DIYMemo.Lines.Append('dtVRectangle');
          DIYMemo.Lines.Append('X : ' + DIYXEdit.Text);
          DIYMemo.Lines.Append('Y : ' + DIYYEdit.Text);
          DIYMemo.Lines.Append('X2: ' + DIYWxEdit.Text);
          DIYMemo.Lines.Append('Y2: ' + DIYHyEdit.Text);
          str(DIYX3Y3.X, DIYS);
          MathForm.DIYMemo.Lines.Append('X3: ' + DIYS);
          str(DIYX3Y3.Y, DIYS);
          MathForm.DIYMemo.Lines.Append('Y3: ' + DIYS);
          str(DIYX4Y4.X, DIYS);
          MathForm.DIYMemo.Lines.Append('X4: ' + DIYS);
          str(DIYX4Y4.Y, DIYS);
          MathForm.DIYMemo.Lines.Append('Y4: ' + DIYS);
          DIYMemo.Lines.Append('Z : ' + DIYZEdit.Text);
          DIYMemo.Lines.Append('E : ' + DIYEEdit.Text);
          DIYMemo.Lines.Append('V1: ' + C2SW(V1Color));
          DIYMemo.Lines.Append('V2: ' + C2SW(V2Color));
        end; {DIYXY,DIYX2Y2
        dtVRectangle FRectangle X1Y1X2Y2 R L}
      2: begin
          DIYMemo.Lines.Append('dtEllipse');
          DIYMemo.Lines.Append('X : ' + DIYXEdit.Text);
          DIYMemo.Lines.Append('Y : ' + DIYYEdit.Text);
          DIYMemo.Lines.Append('X2: ' + DIYWxEdit.Text);
          DIYMemo.Lines.Append('Y2: ' + DIYHyEdit.Text);
          DIYMemo.Lines.Append('V1: ' + C2SW(V1Color));
          DIYMemo.Lines.Append('V2: ' + C2SW(V2Color));
        end; {DIYXY,DIYX2Y2
        dtEllipse Ellipse... Circle X1Y1X2Y2 R L}
      3: begin
          DIYMemo.Lines.Append('dtTriangle');
          DIYMemo.Lines.Append('X : ' + DIYXEdit.Text);
          DIYMemo.Lines.Append('Y : ' + DIYYEdit.Text);
          DIYMemo.Lines.Append('X2: ' + DIYWxEdit.Text);
          DIYMemo.Lines.Append('Y2: ' + DIYHyEdit.Text);
          str(DIYX3Y3.X, DIYS);
          MathForm.DIYMemo.Lines.Append('X3: ' + DIYS);
          str(DIYX3Y3.Y, DIYS);
          MathForm.DIYMemo.Lines.Append('Y3: ' + DIYS);
          DIYMemo.Lines.Append('V1: ' + C2SW(V1Color));
          DIYMemo.Lines.Append('V2: ' + C2SW(V2Color));
        end; {DIYXY,DIYX2Y2 DIYX3Y3, DIYX4Y4
        dtTriangle Triangle Lines X1Y1X2Y2 R L}
      4: begin
          DIYMemo.Lines.Append('dtRectangle');
          DIYMemo.Lines.Append('X : ' + DIYXEdit.Text);
          DIYMemo.Lines.Append('Y : ' + DIYYEdit.Text);
          DIYMemo.Lines.Append('X2: ' + DIYWxEdit.Text);
          DIYMemo.Lines.Append('Y2: ' + DIYHyEdit.Text);
          DIYMemo.Lines.Append('V1: ' + C2SW(V1Color));
          DIYMemo.Lines.Append('V2: ' + C2SW(V2Color));
        end; {DIYXY,DIYX2Y2
        dtRectangle Flatland X1Y1X2Y2 R L}
      5: begin
          DIYMemo.Lines.Append('dtVPRectangle');
          DIYMemo.Lines.Append('X : ' + DIYXEdit.Text);
          DIYMemo.Lines.Append('Y : ' + DIYYEdit.Text);
          DIYMemo.Lines.Append('X2: ' + DIYWxEdit.Text);
          DIYMemo.Lines.Append('Y2: ' + DIYHyEdit.Text);
          DIYMemo.Lines.Append('V1: ' + C2SW(V1Color));
          DIYMemo.Lines.Append('V2: ' + C2SW(V2Color));
          DIYMemo.Lines.Append('V3: ' + C2SW(V3Color));
          DIYMemo.Lines.Append('V4: ' + C2SW(V4Color));
          DIYMemo.Lines.Append('V4: ' + DIYZEdit.Text);
        end; {DIYXY,DIYX2Y2
        dtRectangle Pandora X1Y1X2Y2 R L}
      6: begin
          DIYMemo.Lines.Append('dtVERectangle');
          DIYMemo.Lines.Append('X : ' + DIYXEdit.Text);
          DIYMemo.Lines.Append('Y : ' + DIYYEdit.Text);
          DIYMemo.Lines.Append('X2: ' + DIYWxEdit.Text);
          DIYMemo.Lines.Append('Y2: ' + DIYHyEdit.Text);
          DIYMemo.Lines.Append('V1: ' + C2SW(V1Color));
          DIYMemo.Lines.Append('V2: ' + C2SW(V2Color));
          DIYMemo.Lines.Append('CC: ' + C2SW(Color256S));

        end; {DIYXY,DIYX2Y2
        dtRectangle Ramped  X1Y1X2Y2 R L}
      7: begin
          DIYMemo.Lines.Append('dtLine');
          DIYMemo.Lines.Append('X : ' + DIYXEdit.Text);
          DIYMemo.Lines.Append('Y : ' + DIYYEdit.Text);
          DIYMemo.Lines.Append('X2: ' + DIYWxEdit.Text);
          DIYMemo.Lines.Append('Y2: ' + DIYHyEdit.Text);
          DIYMemo.Lines.Append('V1: ' + C2SW(V1Color));
          DIYMemo.Lines.Append('V2: ' + C2SW(V2Color));
        end; {DIYXY,DIYX2Y2
        dtLine Line X1Y1X2Y2 R L}
      8: begin
          DIYMemo.Lines.Append('dtCSEllipse');
          DIYMemo.Lines.Append('X : ' + DIYXEdit.Text);
          DIYMemo.Lines.Append('Y : ' + DIYYEdit.Text);
          DIYMemo.Lines.Append('X2: ' + DIYWxEdit.Text);
          DIYMemo.Lines.Append('Y2: ' + DIYHyEdit.Text);
          DIYMemo.Lines.Append('Lx: ' + DIYLxEdit.Text);
          DIYMemo.Lines.Append('Ly: ' + DIYLyEdit.Text);
          DIYMemo.Lines.Append('V1: ' + C2SW(V1Color));
          DIYMemo.Lines.Append('V2: ' + C2SW(V2Color));
        end; {DIYXY,DIYX2Y2
        DIYZ,DIYL:Integer;
        dtCSEllipse C Shadow X1Y1X2Y2 R L}
      9: begin
          DIYMemo.Lines.Append('dtTSTriangle');
          DIYMemo.Lines.Append('X : ' + DIYXEdit.Text);
          DIYMemo.Lines.Append('Y : ' + DIYYEdit.Text);
          DIYMemo.Lines.Append('X2: ' + DIYWxEdit.Text);
          DIYMemo.Lines.Append('Y2: ' + DIYHyEdit.Text);
          str(DIYX3Y3.X, DIYS);
          MathForm.DIYMemo.Lines.Append('X3: ' + DIYS);
          str(DIYX3Y3.Y, DIYS);
          MathForm.DIYMemo.Lines.Append('Y3: ' + DIYS);
          DIYMemo.Lines.Append('Lx: ' + DIYLxEdit.Text);
          DIYMemo.Lines.Append('Ly: ' + DIYLyEdit.Text);
          DIYMemo.Lines.Append('V1: ' + C2SW(V1Color));
          DIYMemo.Lines.Append('V2: ' + C2SW(V2Color));
        end; {DIYXY,DIYX2Y2 DIYX3Y3, DIYX4Y4
        DIYZ,DIYL:Integer;
        dtTSTriangle T Shadow X1Y1X2Y2 R L}
      10: begin
          DIYMemo.Lines.Append('dtVMRectangle');
          DIYMemo.Lines.Append('X : ' + DIYXEdit.Text);
          DIYMemo.Lines.Append('Y : ' + DIYYEdit.Text);
          DIYMemo.Lines.Append('X2: ' + DIYWxEdit.Text);
          DIYMemo.Lines.Append('Y2: ' + DIYHyEdit.Text);
          DIYMemo.Lines.Append('V1: ' + C2SW(V1Color));
          DIYMemo.Lines.Append('V2: ' + C2SW(V2Color));
{ALL THE Fractal Variables !!! DIYFileFLN}
          DIYMemo.Lines.Append(DIYFileFLN);
        end; {DIYXY,DIYX2Y2
        dtVNRectangle Fern X1Y1X2Y2 R L}
      11: begin
          DIYMemo.Lines.Append('dtVARectangle');
          DIYMemo.Lines.Append('X : ' + DIYXEdit.Text);
          DIYMemo.Lines.Append('Y : ' + DIYYEdit.Text);
          DIYMemo.Lines.Append('X2: ' + DIYWxEdit.Text);
          DIYMemo.Lines.Append('Y2: ' + DIYHyEdit.Text);
          DIYMemo.Lines.Append('V1: ' + C2SW(V1Color));
          DIYMemo.Lines.Append('V2: ' + C2SW(V2Color));
{ALL THE Turtle Variables !!! DIYFileFLN}
          DIYMemo.Lines.Append(DIYFileFLN);
        end; {DIYXY,DIYX2Y2
        dtVNRectangle Fern X1Y1X2Y2 R L}
      12: begin
          DIYMemo.Lines.Append('dtVTRectangle');
          DIYMemo.Lines.Append('X : ' + DIYXEdit.Text);
          DIYMemo.Lines.Append('Y : ' + DIYYEdit.Text);
          DIYMemo.Lines.Append('X2: ' + DIYWxEdit.Text);
          DIYMemo.Lines.Append('Y2: ' + DIYHyEdit.Text);
          DIYMemo.Lines.Append('V1: ' + C2SW(V1Color));
          DIYMemo.Lines.Append('V2: ' + C2SW(V2Color));
{ALL THE TREE Variables !!!}
          DIYMemo.Lines.Append(DIYFileFLN);
        end; {DIYXY,DIYX2Y2
        dtVTRectangle Tree X1Y1X2Y2 R L}
      13: begin
          DIYMemo.Lines.Append('dtVFRectangle');
          DIYMemo.Lines.Append('X : ' + DIYXEdit.Text);
          DIYMemo.Lines.Append('Y : ' + DIYYEdit.Text);
          DIYMemo.Lines.Append('X2: ' + DIYWxEdit.Text);
          DIYMemo.Lines.Append('Y2: ' + DIYHyEdit.Text);
          DIYMemo.Lines.Append('V1: ' + C2SW(V1Color));
          DIYMemo.Lines.Append('V2: ' + C2SW(V2Color));
{ALL THE FERN Variables !!! DIYFileFLN}
          DIYMemo.Lines.Append(DIYFileFLN);
        end; {DIYXY,DIYX2Y2
        dtVFRectangle Fern X1Y1X2Y2 R L}
      14: begin
          DIYMemo.Lines.Append('dtVDRectangle');
          DIYMemo.Lines.Append('X : ' + DIYXEdit.Text);
          DIYMemo.Lines.Append('Y : ' + DIYYEdit.Text);
          DIYMemo.Lines.Append('X2: ' + DIYWxEdit.Text);
          DIYMemo.Lines.Append('Y2: ' + DIYHyEdit.Text);
          DIYMemo.Lines.Append('V1: ' + C2SW(V1Color));
          DIYMemo.Lines.Append('V2: ' + C2SW(V2Color));
{ALL THE Dragon Variables !!!}
          DIYMemo.Lines.Append(DIYFileFLN);
        end; {DIYXY,DIYX2Y2
        dtVDRectangle Dragon X1Y1X2Y2 R L}
      15: begin
          DIYMemo.Lines.Append('dtVSRectangle');
          DIYMemo.Lines.Append('X : ' + DIYXEdit.Text);
          DIYMemo.Lines.Append('Y : ' + DIYYEdit.Text);
          DIYMemo.Lines.Append('X2: ' + DIYWxEdit.Text);
          DIYMemo.Lines.Append('Y2: ' + DIYHyEdit.Text);
          DIYMemo.Lines.Append('V1: ' + C2SW(V1Color));
          DIYMemo.Lines.Append('V2: ' + C2SW(V2Color));
{ALL THE Sky Cloud Variables !!!}
          DIYMemo.Lines.Append(DIYFileFLN);
        end; {DIYXY,DIYX2Y2
        dtSCEllipse Sky Cloud X1Y1X2Y2 R L}
      16: begin
          DIYMemo.Lines.Append('dtVNRectangle');
          DIYMemo.Lines.Append('X : ' + DIYXEdit.Text);
          DIYMemo.Lines.Append('Y : ' + DIYYEdit.Text);
          DIYMemo.Lines.Append('X2: ' + DIYWxEdit.Text);
          DIYMemo.Lines.Append('Y2: ' + DIYHyEdit.Text);
          DIYMemo.Lines.Append('V1: ' + C2SW(V1Color));
          DIYMemo.Lines.Append('V2: ' + C2SW(V2Color));
{ALL THE Numb Variables !!! DIYFileFLN}
          DIYMemo.Lines.Append(DIYFileFLN);
        end; {DIYXY,DIYX2Y2
        dtVNRectangle Fern X1Y1X2Y2 R L}
      17: begin
          DIYMemo.Lines.Append('dtVCRectangle');
          DIYMemo.Lines.Append('X : ' + DIYXEdit.Text);
          DIYMemo.Lines.Append('Y : ' + DIYYEdit.Text);
          DIYMemo.Lines.Append('X2: ' + DIYWxEdit.Text);
          DIYMemo.Lines.Append('Y2: ' + DIYHyEdit.Text);
          DIYMemo.Lines.Append('V1: ' + C2SW(V1Color));
          DIYMemo.Lines.Append('V2: ' + C2SW(V2Color));
{ALL THE Collage Variables !!! DIYFileFLN}
          DIYMemo.Lines.Append(DIYFileFLN);
        end; {DIYXY,DIYX2Y2
        dtVNRectangle Fern X1Y1X2Y2 R L}
      18: begin
          DIYMemo.Lines.Append('dtVWRectangle');
          DIYMemo.Lines.Append('X : ' + DIYXEdit.Text);
          DIYMemo.Lines.Append('Y : ' + DIYYEdit.Text);
          DIYMemo.Lines.Append('X2: ' + DIYWxEdit.Text);
          DIYMemo.Lines.Append('Y2: ' + DIYHyEdit.Text);
          DIYMemo.Lines.Append('V1: ' + C2SW(V1Color));
          DIYMemo.Lines.Append('V2: ' + C2SW(V2Color));
{ALL THE Torso Variables !!! DIYFileFLN}
          DIYMemo.Lines.Append(DIYFileFLN);
        end; {DIYXY,DIYX2Y2
        dtVNRectangle Fern X1Y1X2Y2 R L}
      19: begin
          DIYMemo.Lines.Append('dtVBRectangle');
          DIYMemo.Lines.Append('X : ' + DIYXEdit.Text);
          DIYMemo.Lines.Append('Y : ' + DIYYEdit.Text);
          DIYMemo.Lines.Append('X2: ' + DIYWxEdit.Text);
          DIYMemo.Lines.Append('Y2: ' + DIYHyEdit.Text);
          DIYMemo.Lines.Append('V1: ' + C2SW(V1Color));
          DIYMemo.Lines.Append('V2: ' + C2SW(V2Color));
          DIYMemo.Lines.Append(DIYFilename);
        end; {DIYXY,DIYX2Y2
        DIYFilename (Bitmap)     DIYFilename
        dtVBRectangle Bitmap X1Y1X2Y2 R L}
    end; {of case}
  end; end;



procedure TMathForm.DIYClipitClick(Sender: TObject);
begin
  DIYMemo.SelectAll;
  DIYMemo.CopyToClipboard;
end;

procedure TMathForm.DIYGearClick(Sender: TObject);

var
  TransferS, GearS: string;
  ColdX, ReSets, Code, Counter: Integer;
  dCounter, Ramper: Double;
  F_File: file of TFont;

begin

  if ((DIYFileFL = '') or (not bVista)) then
    DoMessages(30203) else
     {After Loading a FILE
        Semi-Run the objects to place
        'Symbol' ized objects on Main Display}
  begin
    if (ZoomingOn) then MainForm.UnZoom;
    ReSets := Color256S;
{ (dtSCEllipse, dtEllipse, dtCSEllipse,
  dtVTriangle,  dtTriangle, dtTSTriangle,
  dtVDRectangle, dtVRectangle,  dtVTRectangle,  dtVFRectangle,
  dtVNRectangle, dtRectangle, dtVBRectangle, dtRoundRect,
  dtLine);}

    with MainForm.Image2.Canvas do begin
{ Brush.Color:=FBackGroundColor;
 Brush.Style:=bsSolid;
 FillRect(Rect(0,0,FYImageX-1,FYImageY-1));
 Pen.Color :=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
 Font.Color:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
 MainForm.Show;}
      Counter := 1; {Skip first DIY List title}
      while (DIYMemo.Lines.Count > Counter) do begin
        GearS := DIYMemo.Lines[Counter];
       {Read from Memo and decipher-remake into data
        and then call DrawShape(Origin, Point(X, Y), pmCopy);}
        if ((GearS = 'DIY List')) then begin
          inc(Counter); {just in case any were added}
        end else
          if ((GearS = 'dtEllipse')) then begin
            inc(Counter);
            GearS := DIYMemo.Lines[Counter];
            TransferS := Copy(GearS, 4, Length(GearS));
            val(TransferS, DIYXY.X, Code); Codefx(TransferS, Code);
            inc(Counter);
            GearS := DIYMemo.Lines[Counter];
            TransferS := Copy(GearS, 4, Length(GearS));
            val(TransferS, DIYXY.Y, Code); Codefx(TransferS, Code);
            inc(Counter);
            GearS := DIYMemo.Lines[Counter];
            TransferS := Copy(GearS, 4, Length(GearS));
            val(TransferS, DIYX2Y2.X, Code); Codefx(TransferS, Code);
            inc(Counter);
            GearS := DIYMemo.Lines[Counter];
            TransferS := Copy(GearS, 4, Length(GearS));
            val(TransferS, DIYX2Y2.Y, Code); Codefx(TransferS, Code);
            inc(Counter);
            GearS := DIYMemo.Lines[Counter];
            TransferS := Copy(GearS, 4, Length(GearS));
            val(TransferS, V1Color, Code); Codefx(TransferS, Code);
            inc(Counter);
            GearS := DIYMemo.Lines[Counter];
            TransferS := Copy(GearS, 4, Length(GearS));
            val(TransferS, V2Color, Code); Codefx(TransferS, Code);
            inc(Counter);
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsSolid;
            MainForm.DrawingTool := dtEllipse;
            MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3, DIYX4Y4,
              pmCopy);
          end else
            if ((GearS = 'dtCSEllipse')) then begin
              inc(Counter);
              GearS := DIYMemo.Lines[Counter];
              TransferS := Copy(GearS, 4, Length(GearS));
              val(TransferS, DIYXY.X, Code); Codefx(TransferS, Code);
              inc(Counter);
              GearS := DIYMemo.Lines[Counter];
              TransferS := Copy(GearS, 4, Length(GearS));
              val(TransferS, DIYXY.Y, Code); Codefx(TransferS, Code);
              inc(Counter);
              GearS := DIYMemo.Lines[Counter];
              TransferS := Copy(GearS, 4, Length(GearS));
              val(TransferS, DIYX2Y2.X, Code); Codefx(TransferS,
                Code);
              inc(Counter);
              GearS := DIYMemo.Lines[Counter];
              TransferS := Copy(GearS, 4, Length(GearS));
              val(TransferS, DIYX2Y2.Y, Code); Codefx(TransferS,
                Code);
              inc(Counter); {DIYZ,DIYL:Integer;}
              GearS := DIYMemo.Lines[Counter];
              TransferS := Copy(GearS, 4, Length(GearS));
              val(TransferS, DIYL.X, Code); Codefx(TransferS, Code);
              inc(Counter);
              GearS := DIYMemo.Lines[Counter];
              TransferS := Copy(GearS, 4, Length(GearS));
              val(TransferS, DIYL.Y, Code); Codefx(TransferS, Code);
              GearS := DIYMemo.Lines[Counter];
              TransferS := Copy(GearS, 4, Length(GearS));
              val(TransferS, V1Color, Code); Codefx(TransferS, Code);
              inc(Counter);
              GearS := DIYMemo.Lines[Counter];
              TransferS := Copy(GearS, 4, Length(GearS));
              val(TransferS, V2Color, Code); Codefx(TransferS, Code);
              inc(Counter);
              Pen.Color := V1Color;
              Brush.Color := V1Color;
              Brush.Style := bsBDiagonal {Solid};
              MainForm.DrawingTool := dtCSEllipse;
              MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3, DIYX4Y4,
                pmCopy);
{RanfillOval... Convert the coordinates into parameters}
        {Given that it is a circle...
        b:= x/2
        aspect:=( (x/y) / (640/480)}
        {ranFillOval(x,y,b: integer;
                color: TColor;aspect: Extended);}
            end else {DIYZ,DIYL:Integer;}
              if ((GearS = 'dtVSRectangle')) then begin
                inc(Counter);
                GearS := DIYMemo.Lines[Counter];
                TransferS := Copy(GearS, 4, Length(GearS));
                val(TransferS, DIYXY.X, Code); Codefx(TransferS,
                  Code);
                inc(Counter);
                GearS := DIYMemo.Lines[Counter];
                TransferS := Copy(GearS, 4, Length(GearS));
                val(TransferS, DIYXY.Y, Code); Codefx(TransferS,
                  Code);
                inc(Counter);
                GearS := DIYMemo.Lines[Counter];
                TransferS := Copy(GearS, 4, Length(GearS));
                val(TransferS, DIYX2Y2.X, Code); Codefx(TransferS,
                  Code);
                inc(Counter);
                GearS := DIYMemo.Lines[Counter];
                TransferS := Copy(GearS, 4, Length(GearS));
                val(TransferS, DIYX2Y2.Y, Code); Codefx(TransferS,
                  Code);
                inc(Counter);
                GearS := DIYMemo.Lines[Counter];
                TransferS := Copy(GearS, 4, Length(GearS));
                val(TransferS, V1Color, Code); Codefx(TransferS,
                  Code);
                inc(Counter);
                GearS := DIYMemo.Lines[Counter];
                TransferS := Copy(GearS, 4, Length(GearS));
                val(TransferS, V2Color, Code); Codefx(TransferS,
                  Code);
                inc(Counter);
{read file name and PROCESS}
                GearS := DIYMemo.Lines[Counter];
                inc(Counter);
{        If (not(GearS = ''))then begin
        SkyLoaderDo(GearS);
        SkyDiver(SkyLevel);
        SkyImage;
                end else } begin
                  Pen.Color := V1Color;
                  Brush.Color := V1Color;
                  Brush.Style := bsFDiagonal {Solid};
                  MainForm.DrawingTool := dtVSRectangle;
                  MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3,
                    DIYX4Y4, pmCopy);
                end;
              end else
                if ((GearS = 'dtVTriangle')) then begin
                  inc(Counter);
                  GearS := DIYMemo.Lines[Counter];
                  TransferS := Copy(GearS, 4, Length(GearS));
                  val(TransferS, DIYXY.X, Code); Codefx(TransferS,
                    Code);
                  inc(Counter);
                  GearS := DIYMemo.Lines[Counter];
                  TransferS := Copy(GearS, 4, Length(GearS));
                  val(TransferS, DIYXY.Y, Code); Codefx(TransferS,
                    Code);
                  inc(Counter);
                  GearS := DIYMemo.Lines[Counter];
                  TransferS := Copy(GearS, 4, Length(GearS));
                  val(TransferS, DIYX2Y2.X, Code); Codefx(TransferS,
                    Code);
                  inc(Counter);
                  GearS := DIYMemo.Lines[Counter];
                  TransferS := Copy(GearS, 4, Length(GearS));
                  val(TransferS, DIYX2Y2.Y, Code); Codefx(TransferS,
                    Code);
                  inc(Counter);
                  GearS := DIYMemo.Lines[Counter];
                  TransferS := Copy(GearS, 4, Length(GearS));
                  val(TransferS, DIYX3Y3.X, Code); Codefx(TransferS,
                    Code);
                  inc(Counter);
                  GearS := DIYMemo.Lines[Counter];
                  TransferS := Copy(GearS, 4, Length(GearS));
                  val(TransferS, DIYX3Y3.Y, Code); Codefx(TransferS,
                    Code);
                  inc(Counter);
                  GearS := DIYMemo.Lines[Counter];
                  TransferS := Copy(GearS, 4, Length(GearS));
                  val(TransferS, DIYZ, Code); Codefx(TransferS,
                    Code);
                  inc(Counter);
                  GearS := DIYMemo.Lines[Counter];
                  TransferS := Copy(GearS, 4, Length(GearS));
                  val(TransferS, DIYE, Code); Codefx(TransferS,
                    Code);
                  inc(Counter);
                  GearS := DIYMemo.Lines[Counter];
                  TransferS := Copy(GearS, 4, Length(GearS));
                  val(TransferS, V1Color, Code); Codefx(TransferS,
                    Code);
                  inc(Counter);
                  GearS := DIYMemo.Lines[Counter];
                  TransferS := Copy(GearS, 4, Length(GearS));
                  val(TransferS, V2Color, Code); Codefx(TransferS,
                    Code);
                  inc(Counter);
                  Pen.Color := V1Color;
                  Brush.Color := V1Color;
                  Brush.Style := bsDiagCross;
                  MainForm.DrawingTool := dtVTriangle;
                  ITriangling := 10;
                  MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3,
                    DIYX4Y4, pmCopy);
{procedure generate(x1,y1,x2,y2,x3,y3,level,DIYE: integer;
        color1,color2: TColor);}
                end else {bsDiagCross DIYXY,DIYX2Y2 DIYX3Y3, DIYX4Y4: TPoint;}
                  if (GearS = 'dtTSTriangle') then begin
                    inc(Counter);
                    GearS := DIYMemo.Lines[Counter];
                    TransferS := Copy(GearS, 4, Length(GearS));
                    val(TransferS, DIYXY.X, Code); Codefx(TransferS,
                      Code);
                    inc(Counter);
                    GearS := DIYMemo.Lines[Counter];
                    TransferS := Copy(GearS, 4, Length(GearS));
                    val(TransferS, DIYXY.Y, Code); Codefx(TransferS,
                      Code);
                    inc(Counter);
                    GearS := DIYMemo.Lines[Counter];
                    TransferS := Copy(GearS, 4, Length(GearS));
                    val(TransferS, DIYX2Y2.X, Code);
                      Codefx(TransferS, Code);
                    inc(Counter);
                    GearS := DIYMemo.Lines[Counter];
                    TransferS := Copy(GearS, 4, Length(GearS));
                    val(TransferS, DIYX2Y2.Y, Code);
                      Codefx(TransferS, Code);
                    inc(Counter);
                    GearS := DIYMemo.Lines[Counter];
                    TransferS := Copy(GearS, 4, Length(GearS));
                    val(TransferS, DIYX3Y3.X, Code);
                      Codefx(TransferS, Code);
                    inc(Counter);
                    GearS := DIYMemo.Lines[Counter];
                    TransferS := Copy(GearS, 4, Length(GearS));
                    val(TransferS, DIYX3Y3.Y, Code);
                      Codefx(TransferS, Code);
                    inc(Counter);
                    GearS := DIYMemo.Lines[Counter];
                    TransferS := Copy(GearS, 4, Length(GearS));
                    val(TransferS, DIYL.X, Code); Codefx(TransferS,
                      Code);
                    inc(Counter);
                    GearS := DIYMemo.Lines[Counter];
                    TransferS := Copy(GearS, 4, Length(GearS));
                    val(TransferS, DIYL.Y, Code); Codefx(TransferS,
                      Code);
                    inc(Counter);
                    GearS := DIYMemo.Lines[Counter];
                    TransferS := Copy(GearS, 4, Length(GearS));
                    val(TransferS, V1Color, Code); Codefx(TransferS,
                      Code);
                    inc(Counter);
                    GearS := DIYMemo.Lines[Counter];
                    TransferS := Copy(GearS, 4, Length(GearS));
                    val(TransferS, V2Color, Code); Codefx(TransferS,
                      Code);
                    inc(Counter);
                    Pen.Color := V1Color;
                    Brush.Color := V1Color;
                    Brush.Style := bsBDiagonal;
                    MainForm.DrawingTool := dtTSTriangle;
                    MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3,
                      DIYX4Y4, pmCopy);
{change to do for a triangle
        ranFillOval(x,y,b: integer;
                color: TColor;aspect: Extended);
}
                  end else {bsBDiagonal DIYXY,DIYX2Y2 DIYX3Y3, DIYX4Y4: TPoint;}
                    if (GearS = 'dtTriangle') then begin
                      inc(Counter);
                      GearS := DIYMemo.Lines[Counter];
                      TransferS := Copy(GearS, 4, Length(GearS));
                      val(TransferS, DIYXY.X, Code);
                        Codefx(TransferS, Code);
                      inc(Counter);
                      GearS := DIYMemo.Lines[Counter];
                      TransferS := Copy(GearS, 4, Length(GearS));
                      val(TransferS, DIYXY.Y, Code);
                        Codefx(TransferS, Code);
                      inc(Counter);
                      GearS := DIYMemo.Lines[Counter];
                      TransferS := Copy(GearS, 4, Length(GearS));
                      val(TransferS, DIYX2Y2.X, Code);
                        Codefx(TransferS, Code);
                      inc(Counter);
                      GearS := DIYMemo.Lines[Counter];
                      TransferS := Copy(GearS, 4, Length(GearS));
                      val(TransferS, DIYX2Y2.Y, Code);
                        Codefx(TransferS, Code);
                      inc(Counter);
                      GearS := DIYMemo.Lines[Counter];
                      TransferS := Copy(GearS, 4, Length(GearS));
                      val(TransferS, DIYX3Y3.X, Code);
                        Codefx(TransferS, Code);
                      inc(Counter);
                      GearS := DIYMemo.Lines[Counter];
                      TransferS := Copy(GearS, 4, Length(GearS));
                      val(TransferS, DIYX3Y3.Y, Code);
                        Codefx(TransferS, Code);
                      inc(Counter);
                      GearS := DIYMemo.Lines[Counter];
                      TransferS := Copy(GearS, 4, Length(GearS));
                      val(TransferS, V1Color, Code);
                        Codefx(TransferS, Code);
                      inc(Counter);
                      GearS := DIYMemo.Lines[Counter];
                      TransferS := Copy(GearS, 4, Length(GearS));
                      val(TransferS, V2Color, Code);
                        Codefx(TransferS, Code);
                      inc(Counter);
                      Pen.Color := V1Color;
                      Brush.Color := V1Color;
                      Brush.Style := bsSolid;
                      MainForm.DrawingTool := dtTriangle;
                      MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3,
                        DIYX4Y4, pmCopy);
                    end else {no fill DIYXY,DIYX2Y2 DIYX3Y3, DIYX4Y4: TPoint;}
                      if ((GearS = 'dtVRectangle')) then begin {}
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, DIYXY.X, Code);
                          Codefx(TransferS, Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, DIYXY.Y, Code);
                          Codefx(TransferS, Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, DIYX2Y2.X, Code);
                          Codefx(TransferS, Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, DIYX2Y2.Y, Code);
                          Codefx(TransferS, Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, DIYX3Y3.X, Code);
                          Codefx(TransferS, Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, DIYX3Y3.Y, Code);
                          Codefx(TransferS, Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, DIYX4Y4.X, Code);
                          Codefx(TransferS, Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, DIYX4Y4.Y, Code);
                          Codefx(TransferS, Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, DIYZ, Code); Codefx(TransferS,
                          Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, DIYE, Code); Codefx(TransferS,
                          Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, V1Color, Code);
                          Codefx(TransferS, Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, V2Color, Code);
                          Codefx(TransferS, Code);
                        inc(Counter);
                        Pen.Color := V1Color;
                        Brush.Color := V1Color;
                        Brush.Style := bsDiagCross {Solid};
                        MainForm.DrawingTool := dtVRectangle;
                        ITriangling := 10;
                        MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3,
                          DIYX4Y4, pmCopy);
        {gen_quad (x1,y1,x2,y2,x3, y3, x4,y4,level,DIYE: integer;
 color1,color2: Tcolor);}
                      end else {bsDiagCross}
                        if ((GearS = 'dtVTRectangle')) then begin {}
                          inc(Counter);
                          GearS := DIYMemo.Lines[Counter];
                          TransferS := Copy(GearS, 4, Length(GearS));
                          val(TransferS, DIYXY.X, Code);
                            Codefx(TransferS, Code);
                          inc(Counter);
                          GearS := DIYMemo.Lines[Counter];
                          TransferS := Copy(GearS, 4, Length(GearS));
                          val(TransferS, DIYXY.Y, Code);
                            Codefx(TransferS, Code);
                          inc(Counter);
                          GearS := DIYMemo.Lines[Counter];
                          TransferS := Copy(GearS, 4, Length(GearS));
                          val(TransferS, DIYX2Y2.X, Code);
                            Codefx(TransferS, Code);
                          inc(Counter);
                          GearS := DIYMemo.Lines[Counter];
                          TransferS := Copy(GearS, 4, Length(GearS));
                          val(TransferS, DIYX2Y2.Y, Code);
                            Codefx(TransferS, Code);
                          inc(Counter);
                          GearS := DIYMemo.Lines[Counter];
                          TransferS := Copy(GearS, 4, Length(GearS));
                          val(TransferS, V1Color, Code);
                            Codefx(TransferS, Code);
                          inc(Counter);
                          GearS := DIYMemo.Lines[Counter];
                          TransferS := Copy(GearS, 4, Length(GearS));
                          val(TransferS, V2Color, Code);
                            Codefx(TransferS, Code);
                          inc(Counter);
{read file name and PROCESS}
                          GearS := DIYMemo.Lines[Counter];
                          inc(Counter);
{        If (not(GearS = ''))then begin
        DoTreeLoader(GearS);
        treerun;
        end;}
                          Pen.Color := V1Color;
                          Brush.Color := V1Color;
                          Brush.Style := bsHorizontal {Solid};
                          MainForm.DrawingTool := dtVTRectangle;
                          MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3,
                            DIYX4Y4, pmCopy);
                        end else {bsHorizontal}
                          if ((GearS = 'dtVFRectangle')) then begin {}
                            inc(Counter);
                            GearS := DIYMemo.Lines[Counter];
                            TransferS := Copy(GearS, 4,
                              Length(GearS));
                            val(TransferS, DIYXY.X, Code);
                              Codefx(TransferS, Code);
                            inc(Counter);
                            GearS := DIYMemo.Lines[Counter];
                            TransferS := Copy(GearS, 4,
                              Length(GearS));
                            val(TransferS, DIYXY.Y, Code);
                              Codefx(TransferS, Code);
                            inc(Counter);
                            GearS := DIYMemo.Lines[Counter];
                            TransferS := Copy(GearS, 4,
                              Length(GearS));
                            val(TransferS, DIYX2Y2.X, Code);
                              Codefx(TransferS, Code);
                            inc(Counter);
                            GearS := DIYMemo.Lines[Counter];
                            TransferS := Copy(GearS, 4,
                              Length(GearS));
                            val(TransferS, DIYX2Y2.Y, Code);
                              Codefx(TransferS, Code);
                            inc(Counter);
                            GearS := DIYMemo.Lines[Counter];
                            TransferS := Copy(GearS, 4,
                              Length(GearS));
                            val(TransferS, V1Color, Code);
                              Codefx(TransferS, Code);
                            inc(Counter);
                            GearS := DIYMemo.Lines[Counter];
                            TransferS := Copy(GearS, 4,
                              Length(GearS));
                            val(TransferS, V2Color, Code);
                              Codefx(TransferS, Code);
                            inc(Counter);
{read file name and PROCESS}
                            GearS := DIYMemo.Lines[Counter];
                            inc(Counter);
{        If (not(GearS = ''))then begin
        DoFernLoader(GearS);
        MGFernrun;
        end;}
                            Pen.Color := V1Color;
                            Brush.Color := V1Color;
                            Brush.Style := bsHorizontal {Solid};
                            MainForm.DrawingTool := dtVFRectangle;
                            MainForm.DrawShape(DIYXY, DIYX2Y2,
                              DIYX3Y3, DIYX4Y4, pmCopy);
                          end else {bsHorizontal}
                            if ((GearS = 'dtVDRectangle')) then
                              begin {}
                              inc(Counter);
                              GearS := DIYMemo.Lines[Counter];
                              TransferS := Copy(GearS, 4,
                                Length(GearS));
                              val(TransferS, DIYXY.X, Code);
                                Codefx(TransferS, Code);
                              inc(Counter);
                              GearS := DIYMemo.Lines[Counter];
                              TransferS := Copy(GearS, 4,
                                Length(GearS));
                              val(TransferS, DIYXY.Y, Code);
                                Codefx(TransferS, Code);
                              inc(Counter);
                              GearS := DIYMemo.Lines[Counter];
                              TransferS := Copy(GearS, 4,
                                Length(GearS));
                              val(TransferS, DIYX2Y2.X, Code);
                                Codefx(TransferS, Code);
                              inc(Counter);
                              GearS := DIYMemo.Lines[Counter];
                              TransferS := Copy(GearS, 4,
                                Length(GearS));
                              val(TransferS, DIYX2Y2.Y, Code);
                                Codefx(TransferS, Code);
                              inc(Counter);
                              GearS := DIYMemo.Lines[Counter];
                              TransferS := Copy(GearS, 4,
                                Length(GearS));
                              val(TransferS, V1Color, Code);
                                Codefx(TransferS, Code);
                              inc(Counter);
                              GearS := DIYMemo.Lines[Counter];
                              TransferS := Copy(GearS, 4,
                                Length(GearS));
                              val(TransferS, V2Color, Code);
                                Codefx(TransferS, Code);
                              inc(Counter);
{read file name and PROCESS}
                              GearS := DIYMemo.Lines[Counter];
                              inc(Counter);
{        If (not(GearS = ''))then begin
        DoFernLoader(GearS);
        MGDragon3DO;
        end;}
                              Pen.Color := V1Color;
                              Brush.Color := V1Color;
                              Brush.Style := bsFDiagonal {Solid};
                              MainForm.DrawingTool := dtVDRectangle;
                              MainForm.DrawShape(DIYXY, DIYX2Y2,
                                DIYX3Y3, DIYX4Y4, pmCopy);
                            end else {bsFDiagonal}
                              if ((GearS = 'dtVWRectangle')) then
                                begin {}
                                inc(Counter);
                                GearS := DIYMemo.Lines[Counter];
                                TransferS := Copy(GearS, 4,
                                  Length(GearS));
                                val(TransferS, DIYXY.X, Code);
                                  Codefx(TransferS, Code);
                                inc(Counter);
                                GearS := DIYMemo.Lines[Counter];
                                TransferS := Copy(GearS, 4,
                                  Length(GearS));
                                val(TransferS, DIYXY.Y, Code);
                                  Codefx(TransferS, Code);
                                inc(Counter);
                                GearS := DIYMemo.Lines[Counter];
                                TransferS := Copy(GearS, 4,
                                  Length(GearS));
                                val(TransferS, DIYX2Y2.X, Code);
                                  Codefx(TransferS, Code);
                                inc(Counter);
                                GearS := DIYMemo.Lines[Counter];
                                TransferS := Copy(GearS, 4,
                                  Length(GearS));
                                val(TransferS, DIYX2Y2.Y, Code);
                                  Codefx(TransferS, Code);
                                inc(Counter);
                                GearS := DIYMemo.Lines[Counter];
                                TransferS := Copy(GearS, 4,
                                  Length(GearS));
                                val(TransferS, V1Color, Code);
                                  Codefx(TransferS, Code);
                                inc(Counter);
                                GearS := DIYMemo.Lines[Counter];
                                TransferS := Copy(GearS, 4,
                                  Length(GearS));
                                val(TransferS, V2Color, Code);
                                  Codefx(TransferS, Code);
                                inc(Counter);
{read file name and PROCESS}
                                GearS := DIYMemo.Lines[Counter];
                                inc(Counter);
{        If (not(GearS=''))then begin
Torso
        NumbOpen(GearS);LivingDead(NumbLevel);Numb3DImage;
        end;}
                                Pen.Color := V1Color;
                                Brush.Color := V1Color;
                                Brush.Style := bsFDiagonal;
                                MainForm.DrawingTool :=
                                  dtVWRectangle;
                                MainForm.DrawShape(DIYXY, DIYX2Y2,
                                  DIYX3Y3, DIYX4Y4, pmCopy);
                              end else {bsFDiagonal}
                                if ((GearS = 'dtVCRectangle')) then
                                  begin {}
                                  inc(Counter);
                                  GearS := DIYMemo.Lines[Counter];
                                  TransferS := Copy(GearS, 4,
                                    Length(GearS));
                                  val(TransferS, DIYXY.X, Code);
                                    Codefx(TransferS, Code);
                                  inc(Counter);
                                  GearS := DIYMemo.Lines[Counter];
                                  TransferS := Copy(GearS, 4,
                                    Length(GearS));
                                  val(TransferS, DIYXY.Y, Code);
                                    Codefx(TransferS, Code);
                                  inc(Counter);
                                  GearS := DIYMemo.Lines[Counter];
                                  TransferS := Copy(GearS, 4,
                                    Length(GearS));
                                  val(TransferS, DIYX2Y2.X, Code);
                                    Codefx(TransferS, Code);
                                  inc(Counter);
                                  GearS := DIYMemo.Lines[Counter];
                                  TransferS := Copy(GearS, 4,
                                    Length(GearS));
                                  val(TransferS, DIYX2Y2.Y, Code);
                                    Codefx(TransferS, Code);
                                  inc(Counter);
                                  GearS := DIYMemo.Lines[Counter];
                                  TransferS := Copy(GearS, 4,
                                    Length(GearS));
                                  val(TransferS, V1Color, Code);
                                    Codefx(TransferS, Code);
                                  inc(Counter);
                                  GearS := DIYMemo.Lines[Counter];
                                  TransferS := Copy(GearS, 4,
                                    Length(GearS));
                                  val(TransferS, V2Color, Code);
                                    Codefx(TransferS, Code);
                                  inc(Counter);
{read file name and PROCESS}
                                  GearS := DIYMemo.Lines[Counter];
                                  inc(Counter);
{        If (not(GearS=''))then begin
Collage
        NumbOpen(GearS);LivingDead(NumbLevel);Numb3DImage;
        end;}
                                  Pen.Color := V1Color;
                                  Brush.Color := V1Color;
                                  Brush.Style := bsFDiagonal;
                                  MainForm.DrawingTool :=
                                    dtVCRectangle;
                                  MainForm.DrawShape(DIYXY, DIYX2Y2,
                                    DIYX3Y3, DIYX4Y4, pmCopy);
                                end else {bsFDiagonal}
                                  if ((GearS = 'dtVARectangle')) then
                                    begin {}
                                    inc(Counter);
                                    GearS := DIYMemo.Lines[Counter];
                                    TransferS := Copy(GearS, 4,
                                      Length(GearS));
                                    val(TransferS, DIYXY.X, Code);
                                      Codefx(TransferS, Code);
                                    inc(Counter);
                                    GearS := DIYMemo.Lines[Counter];
                                    TransferS := Copy(GearS, 4,
                                      Length(GearS));
                                    val(TransferS, DIYXY.Y, Code);
                                      Codefx(TransferS, Code);
                                    inc(Counter);
                                    GearS := DIYMemo.Lines[Counter];
                                    TransferS := Copy(GearS, 4,
                                      Length(GearS));
                                    val(TransferS, DIYX2Y2.X, Code);
                                      Codefx(TransferS, Code);
                                    inc(Counter);
                                    GearS := DIYMemo.Lines[Counter];
                                    TransferS := Copy(GearS, 4,
                                      Length(GearS));
                                    val(TransferS, DIYX2Y2.Y, Code);
                                      Codefx(TransferS, Code);
                                    inc(Counter);
                                    GearS := DIYMemo.Lines[Counter];
                                    TransferS := Copy(GearS, 4,
                                      Length(GearS));
                                    val(TransferS, V1Color, Code);
                                      Codefx(TransferS, Code);
                                    inc(Counter);
                                    GearS := DIYMemo.Lines[Counter];
                                    TransferS := Copy(GearS, 4,
                                      Length(GearS));
                                    val(TransferS, V2Color, Code);
                                      Codefx(TransferS, Code);
                                    inc(Counter);
{read file name and PROCESS}
                                    GearS := DIYMemo.Lines[Counter];
                                    inc(Counter);
{        If (not(GearS=''))then begin
Turtle
        NumbOpen(GearS);LivingDead(NumbLevel);Numb3DImage;
        end;}
                                    Pen.Color := V1Color;
                                    Brush.Color := V1Color;
                                    Brush.Style := bsFDiagonal;
                                    MainForm.DrawingTool :=
                                      dtVARectangle;
                                    MainForm.DrawShape(DIYXY,
                                      DIYX2Y2, DIYX3Y3, DIYX4Y4,
                                      pmCopy);
                                  end else {bsFDiagonal}
                                    if ((GearS = 'dtVMRectangle'))
                                      then begin {}
                                      inc(Counter);
                                      GearS :=
                                        DIYMemo.Lines[Counter];
                                      TransferS := Copy(GearS, 4,
                                        Length(GearS));
                                      val(TransferS, DIYXY.X, Code);
                                        Codefx(TransferS, Code);
                                      inc(Counter);
                                      GearS :=
                                        DIYMemo.Lines[Counter];
                                      TransferS := Copy(GearS, 4,
                                        Length(GearS));
                                      val(TransferS, DIYXY.Y, Code);
                                        Codefx(TransferS, Code);
                                      inc(Counter);
                                      GearS :=
                                        DIYMemo.Lines[Counter];
                                      TransferS := Copy(GearS, 4,
                                        Length(GearS));
                                      val(TransferS, DIYX2Y2.X,
                                        Code); Codefx(TransferS, Code);
                                      inc(Counter);
                                      GearS :=
                                        DIYMemo.Lines[Counter];
                                      TransferS := Copy(GearS, 4,
                                        Length(GearS));
                                      val(TransferS, DIYX2Y2.Y,
                                        Code); Codefx(TransferS, Code);
                                      inc(Counter);
                                      GearS :=
                                        DIYMemo.Lines[Counter];
                                      TransferS := Copy(GearS, 4,
                                        Length(GearS));
                                      val(TransferS, V1Color, Code);
                                        Codefx(TransferS, Code);
                                      inc(Counter);
                                      GearS :=
                                        DIYMemo.Lines[Counter];
                                      TransferS := Copy(GearS, 4,
                                        Length(GearS));
                                      val(TransferS, V2Color, Code);
                                        Codefx(TransferS, Code);
                                      inc(Counter);
{read file name and PROCESS}
                                      GearS :=
                                        DIYMemo.Lines[Counter];
                                      inc(Counter);
{        If (not(GearS=''))then begin
Fractal
        NumbOpen(GearS);LivingDead(NumbLevel);Numb3DImage;
        end;}
                                      Pen.Color := V1Color;
                                      Brush.Color := V1Color;
                                      Brush.Style := bsFDiagonal;
                                      MainForm.DrawingTool :=
                                        dtVMRectangle;
                                      MainForm.DrawShape(DIYXY,
                                        DIYX2Y2, DIYX3Y3, DIYX4Y4,
                                        pmCopy);
                                    end else {bsFDiagonal}
                                      if ((GearS = 'dtVERectangle'))
                                        then begin {}
                                        inc(Counter);
                                        GearS :=
                                          DIYMemo.Lines[Counter];
                                        TransferS := Copy(GearS, 4,
                                          Length(GearS));
                                        val(TransferS, DIYXY.X,
                                          Code); Codefx(TransferS,
                                          Code);
                                        inc(Counter);
                                        GearS :=
                                          DIYMemo.Lines[Counter];
                                        TransferS := Copy(GearS, 4,
                                          Length(GearS));
                                        val(TransferS, DIYXY.Y,
                                          Code); Codefx(TransferS,
                                          Code);
                                        inc(Counter);
                                        GearS :=
                                          DIYMemo.Lines[Counter];
                                        TransferS := Copy(GearS, 4,
                                          Length(GearS));
                                        val(TransferS, DIYX2Y2.X,
                                          Code); Codefx(TransferS,
                                          Code);
                                        inc(Counter);
                                        GearS :=
                                          DIYMemo.Lines[Counter];
                                        TransferS := Copy(GearS, 4,
                                          Length(GearS));
                                        val(TransferS, DIYX2Y2.Y,
                                          Code); Codefx(TransferS,
                                          Code);
                                        inc(Counter);
                                        GearS :=
                                          DIYMemo.Lines[Counter];
                                        TransferS := Copy(GearS, 4,
                                          Length(GearS));
                                        val(TransferS, V1Color,
                                          Code); Codefx(TransferS,
                                          Code);
                                        inc(Counter);
                                        GearS :=
                                          DIYMemo.Lines[Counter];
                                        TransferS := Copy(GearS, 4,
                                          Length(GearS));
                                        val(TransferS, V2Color,
                                          Code); Codefx(TransferS,
                                          Code);
                                        inc(Counter);
                                        GearS :=
                                          DIYMemo.Lines[Counter];
                                        TransferS := Copy(GearS, 4,
                                          Length(GearS));
                                        val(TransferS, Color256S,
                                          Code); Codefx(TransferS,
                                          Code);
                                        inc(Counter);
                                        MainForm.FalseSets;
{Ramped Blank}
                                        Pen.Color := V1Color;
                                        Brush.Color := V4Color;
                                        dCounter := 0;
                                        Ramper := (255 / (1 +
                                          abs(DIYXY.Y - DIYX2Y2.Y)));
                                        for Code := DIYXY.Y to
                                          DIYX2Y2.Y do begin
                                          V4Color := RGB(Colors[0,
                                            round(dCounter)],
                                            Colors[1,
                                              round(dCounter)],
                                            Colors[2,
                                              round(dCounter)]);
                                          dCounter := dCounter +
                                            Ramper;
                                          Pen.Color := V4Color;
                                          Moveto(DIYXY.X, Code);
                                          Lineto(DIYX2Y2.X, Code);
                                        end;
                                        V4Color := Brush.Color;
                                      end else {bsFDiagonal}
                                        if ((GearS = 'dtVPRectangle'))
                                          then begin {}
                                          inc(Counter);
                                          GearS :=
                                            DIYMemo.Lines[Counter];
                                          TransferS := Copy(GearS, 4,
                                            Length(GearS));
                                          val(TransferS, DIYXY.X,
                                            Code); Codefx(TransferS,
                                            Code);
                                          inc(Counter);
                                          GearS :=
                                            DIYMemo.Lines[Counter];
                                          TransferS := Copy(GearS, 4,
                                            Length(GearS));
                                          val(TransferS, DIYXY.Y,
                                            Code); Codefx(TransferS,
                                            Code);
                                          inc(Counter);
                                          GearS :=
                                            DIYMemo.Lines[Counter];
                                          TransferS := Copy(GearS, 4,
                                            Length(GearS));
                                          val(TransferS, DIYX2Y2.X,
                                            Code); Codefx(TransferS,
                                            Code);
                                          inc(Counter);
                                          GearS :=
                                            DIYMemo.Lines[Counter];
                                          TransferS := Copy(GearS, 4,
                                            Length(GearS));
                                          val(TransferS, DIYX2Y2.Y,
                                            Code); Codefx(TransferS,
                                            Code);
                                          inc(Counter);
                                          GearS :=
                                            DIYMemo.Lines[Counter];
                                          TransferS := Copy(GearS, 4,
                                            Length(GearS));
                                          val(TransferS, V1Color,
                                            Code); Codefx(TransferS,
                                            Code);
                                          inc(Counter);
                                          GearS :=
                                            DIYMemo.Lines[Counter];
                                          TransferS := Copy(GearS, 4,
                                            Length(GearS));
                                          val(TransferS, V2Color,
                                            Code); Codefx(TransferS,
                                            Code);
                                          inc(Counter);
{from Pandora sprang forth demons}
                                          GearS :=
                                            DIYMemo.Lines[Counter];
                                          TransferS := Copy(GearS, 4,
                                            Length(GearS));
                                          val(TransferS, V3Color,
                                            Code); Codefx(TransferS,
                                            Code);
                                          inc(Counter);
                                          GearS :=
                                            DIYMemo.Lines[Counter];
                                          TransferS := Copy(GearS, 4,
                                            Length(GearS));
                                          val(TransferS, V4Color,
                                            Code); Codefx(TransferS,
                                            Code);
                                          inc(Counter);
                                          GearS :=
                                            DIYMemo.Lines[Counter];
                                          TransferS := Copy(GearS, 4,
                                            Length(GearS));
                                          val(TransferS, dCounter,
                                            Code); Codefx(TransferS,
                                            Code);
                                          inc(Counter);
{Pandora dCounter}
                                          Ramper := (255 / (1 +
                                            abs(DIYXY.Y - DIYX2Y2.Y)));
                                          for Code := DIYXY.Y to
                                            DIYX2Y2.Y do begin
                                            for ColdX := DIYXY.X to
                                              DIYX2Y2.X do begin
                                              if (random(abs(DIYXY.Y
                                                - DIYX2Y2.Y))
                                                <= random(round(Ramper
                                                  * dCounter))) then
                                                Pixels[ColdX, Code]
                                                  := V1Color;
                                              if (random(abs(DIYXY.Y
                                                - DIYX2Y2.Y))
                                                <= random(round(Ramper
                                                  * (dCounter / 8))))
                                                  then begin
                                                Pixels[ColdX + 1,
                                                  Code] := V2Color;
                                                Pixels[ColdX, Code]
                                                  := V3Color;
                                                Pixels[ColdX, Code +
                                                  1] := V2Color;
                                                Pixels[ColdX - 1,
                                                  Code] := V2Color;
                                                Pixels[ColdX, Code -
                                                  1] := V2Color; end;
if
(random(abs(DIYXY.Y- DIYX2Y2.Y))<= random(round(Ramper* dCounter)))
then Pixels[ColdX, Code]:= V4Color;
end; end;
end else {bsFDiagonal}
if ((GearS ='dtVNRectangle')) then
begin {}
inc(Counter);
GearS :=DIYMemo.Lines[Counter];
TransferS := Copy(GearS,4, Length(GearS));
val(TransferS, DIYXY.X,Code);
Codefx(TransferS,Code);
inc(Counter);
GearS :=DIYMemo.Lines[Counter];
TransferS := Copy(GearS,4, Length(GearS));
val(TransferS, DIYXY.Y,Code);
Codefx(TransferS,Code);
inc(Counter);
GearS :=DIYMemo.Lines[Counter];
TransferS := Copy(GearS,4, Length(GearS));
val(TransferS, DIYX2Y2.X,Code);
Codefx(TransferS,Code);
inc(Counter);
GearS :=DIYMemo.Lines[Counter];
TransferS := Copy(GearS,4, Length(GearS));
val(TransferS, DIYX2Y2.Y,Code);
Codefx(TransferS,Code);
inc(Counter);
GearS :=DIYMemo.Lines[Counter];
TransferS := Copy(GearS,4, Length(GearS));
val(TransferS, V1Color,Code);
Codefx(TransferS,Code);
inc(Counter);
GearS :=DIYMemo.Lines[Counter];
TransferS := Copy(GearS,4, Length(GearS));
val(TransferS, V2Color,Code);
Codefx(TransferS,Code);
inc(Counter);
{read file name and PROCESS}
GearS :=DIYMemo.Lines[Counter];
inc(Counter);
{        If (not(GearS=''))then begin
        NumbOpen(GearS);
        LivingDead(NumbLevel);
        Numb3DImage;
        end;}
        Pen.Color := V1Color;
        Brush.Color := V1Color;
        Brush.Style :=bsFDiagonal;
        MainForm.DrawingTool :=dtVNRectangle;
        MainForm.DrawShape(DIYXY,DIYX2Y2, DIYX3Y3,DIYX4Y4, pmCopy);
        end else {bsFDiagonal}
        if ((GearS ='dtRectangle')) then
        begin {}
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS, DIYXY.X,Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS, DIYXY.Y,Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS,DIYX2Y2.X, Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS,DIYX2Y2.Y, Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS, V1Color,Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS, V2Color,Code);
        Codefx(TransferS,Code);
        inc(Counter);
        Pen.Color := V1Color;
        Brush.Color := V1Color;
        Brush.Style := bsSolid{Solid};
        MainForm.DrawingTool :=dtRectangle;
        MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3, DIYX4Y4, pmCopy);
        end else {bsSolid}
        if ((GearS ='dtRoundRect')) then
        begin {}
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS,DIYXY.X, Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS,DIYXY.Y, Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS,DIYX2Y2.X, Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS,DIYX2Y2.Y, Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS,V1Color, Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS,V2Color, Code);
        Codefx(TransferS,Code);
        inc(Counter);
        Pen.Color := V1Color;
        Brush.Color :=V1Color;
        Brush.Style := bsSolid{Solid};
        MainForm.DrawingTool:= dtRoundRect;
        MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3, DIYX4Y4, pmCopy);
        end else {bsSolid}
        if ((GearS ='dtVBRectangle')) then
        begin {}
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS,DIYXY.X, Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS,DIYXY.Y, Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS,DIYX2Y2.X, Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS,DIYX2Y2.Y, Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS,V1Color, Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS,V2Color, Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        inc(Counter);
{read file name and PROCESS DIYFilename}
        {BitmapBlotto(GearS); }
        Pen.Color :=V1Color;
        Brush.Color :=V1Color;
        Brush.Style :=bsCross;
        MainForm.DrawingTool:= dtVBRectangle;
        MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3, DIYX4Y4, pmCopy);
        {DIYFilename (Bitmap)}
        end else {bsCross}
        if ((GearS ='dtLine')) then
        begin {}
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS,DIYXY.X, Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS,DIYXY.Y, Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS,DIYX2Y2.X, Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS,DIYX2Y2.Y, Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS,V1Color, Code);
        Codefx(TransferS,Code);
        inc(Counter);
        GearS :=DIYMemo.Lines[Counter];
        TransferS :=Copy(GearS, 4,Length(GearS));
        val(TransferS,V2Color, Code);
        Codefx(TransferS,Code);
        inc(Counter);
        Pen.Color :=V1Color;
        Brush.Color :=V1Color;
        Brush.Style :=bsFDiagonal {Solid};
        MainForm.DrawingTool:= dtLine;
        MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3, DIYX4Y4, pmCopy);
        end else {no fill}
        if ((GearS = 'Annotation'))
        then begin
        inc(Counter);
       { F_File:File of TFont;}
       GearS :=DIYMemo.Lines[Counter];
       AssignFile(F_File, GearS);
       Reset(F_File);
{       Read(F_File,FractalFont);}
       CloseFile(F_File);
       inc(Counter);
       GearS :=DIYMemo.Lines[Counter];
       DIYTextStorage :=GearS;
       inc(Counter);
        {X}
        GearS := DIYMemo.Lines[Counter];
        TransferS := Copy(GearS, 4,Length(GearS));
        val(TransferS,DIYXY.X, Code);
        Codefx(TransferS, Code);
        inc(Counter);
        {Y}
        GearS :=  DIYMemo.Lines[Counter];
        TransferS := Copy(GearS, 4,Length(GearS));
        val(TransferS,DIYXY.Y, Code);
        Codefx(TransferS, Code);
        inc(Counter);
        {to next symbol}
        TextOut(DIYXY.X, DIYXY.Y, DIYTextStorage);
        end else begin
        ShowMessage('State of Confusion' +#13#10 +
        'Unknown Object found' + #13#10
        + GearS + #13#10
        +'while running Gear Symbolizer');
        inc(Counter);
        end;
      end;
    end;
    Color256S := ReSets; MainForm.FalseSets;
{Mainform.DoImageDone;}
  end;
end;

procedure TMathForm.DIYRunClick(Sender: TObject);

var
  TransferS, GearS: string;
  ColdX, ReSets, Code, Counter: Integer;
  dCounter, Ramper: Double;
  F_File: file of TFont;

begin

  ReSets := Color256S;
  if ((not bVista)) then
    DoMessages(30202)else
     {Run the objects to place on Main Display}
  begin
{ (dtSCEllipse, dtEllipse, dtCSEllipse,
  dtVTriangle,  dtTriangle, dtTSTriangle,
  dtVDRectangle, dtVRectangle,  dtVTRectangle,  dtVFRectangle,
  dtVNRectangle, dtRectangle, dtVBRectangle, dtRoundRect,
  dtLine);}
    if (ZoomingOn) then MainForm.UnZoom;
    with MainForm.Image2.Canvas do begin
{ Brush.Color:=FBackGroundColor;
 Brush.Style:=bsSolid;
 FillRect(Rect(0,0,FYImageX-1,FYImageY-1));
 Pen.Color :=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
 Font.Color:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
 MainForm.Show;}
      Randomize;
      Counter := 1; {Skip first DIY List title}
      while (DIYMemo.Lines.Count > Counter) do begin
        GearS := DIYMemo.Lines[Counter];
       {Read from Memo and decipher-remake into data
        and then call DrawShape(Origin, Point(X, Y), pmCopy);}
        if ((GearS = 'DIY List')) then begin
          inc(Counter); {just in case any were added}
        end else
          if ((GearS = 'dtEllipse')) then begin
            inc(Counter);
            GearS := DIYMemo.Lines[Counter];
            TransferS := Copy(GearS, 4, Length(GearS));
            val(TransferS, DIYXY.X, Code); Codefx(TransferS, Code);
            inc(Counter);
            GearS := DIYMemo.Lines[Counter];
            TransferS := Copy(GearS, 4, Length(GearS));
            val(TransferS, DIYXY.Y, Code); Codefx(TransferS, Code);
            inc(Counter);
            GearS := DIYMemo.Lines[Counter];
            TransferS := Copy(GearS, 4, Length(GearS));
            val(TransferS, DIYX2Y2.X, Code); Codefx(TransferS, Code);
            inc(Counter);
            GearS := DIYMemo.Lines[Counter];
            TransferS := Copy(GearS, 4, Length(GearS));
            val(TransferS, DIYX2Y2.Y, Code); Codefx(TransferS, Code);
            inc(Counter);
            GearS := DIYMemo.Lines[Counter];
            TransferS := Copy(GearS, 4, Length(GearS));
            val(TransferS, V1Color, Code); Codefx(TransferS, Code);
            inc(Counter);
            GearS := DIYMemo.Lines[Counter];
            TransferS := Copy(GearS, 4, Length(GearS));
            val(TransferS, V2Color, Code); Codefx(TransferS, Code);
            inc(Counter);
            Pen.Color := V1Color;
            Brush.Color := V1Color;
            Brush.Style := bsSolid;
            MainForm.DrawingTool := dtEllipse;
            MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3, DIYX4Y4,
              pmCopy);
          end else
            if ((GearS = 'dtCSEllipse')) then begin
              inc(Counter);
              GearS := DIYMemo.Lines[Counter];
              TransferS := Copy(GearS, 4, Length(GearS));
              val(TransferS, DIYXY.X, Code); Codefx(TransferS, Code);
              inc(Counter);
              GearS := DIYMemo.Lines[Counter];
              TransferS := Copy(GearS, 4, Length(GearS));
              val(TransferS, DIYXY.Y, Code); Codefx(TransferS, Code);
              inc(Counter);
              GearS := DIYMemo.Lines[Counter];
              TransferS := Copy(GearS, 4, Length(GearS));
              val(TransferS, DIYX2Y2.X, Code); Codefx(TransferS,
                Code);
              inc(Counter);
              GearS := DIYMemo.Lines[Counter];
              TransferS := Copy(GearS, 4, Length(GearS));
              val(TransferS, DIYX2Y2.Y, Code); Codefx(TransferS,
                Code);
              inc(Counter); {DIYZ,DIYL:Integer;}
              GearS := DIYMemo.Lines[Counter];
              TransferS := Copy(GearS, 4, Length(GearS));
              val(TransferS, DIYL.X, Code); Codefx(TransferS, Code);
              inc(Counter);
              GearS := DIYMemo.Lines[Counter];
              TransferS := Copy(GearS, 4, Length(GearS));
              val(TransferS, DIYL.Y, Code); Codefx(TransferS, Code);
              inc(Counter);
              GearS := DIYMemo.Lines[Counter];
              TransferS := Copy(GearS, 4, Length(GearS));
              val(TransferS, V1Color, Code); Codefx(TransferS, Code);
              inc(Counter);
              GearS := DIYMemo.Lines[Counter];
              TransferS := Copy(GearS, 4, Length(GearS));
              val(TransferS, V2Color, Code); Codefx(TransferS, Code);
              inc(Counter);
{        Pen.Color := V1Color;
        Brush.Color:=V1Color;
        Brush.Style:=bsBDiagonal;
        MainForm.DrawingTool := dtCSEllipse;
        MainForm.DrawShape(DIYXY,DIYX2Y2,DIYX3Y3,DIYX4Y4, pmCopy);}
{RanfillOval... Convert the coordinates into parameters}
{ranFillOval(x,y,b: integer;
color: TColor;aspect: Extended);}
        {Given that it is a circle...
convert to space time continuuhm
DIYXY.X-(MainForm.Image1.Width div 2),
(MainForm.Image1.Height div 2)-DIYXY.Y,        }
              ranFillOval(
                ((DIYXY.X + ((DIYX2Y2.X - DIYXY.X) div 2)) -
                  (MainForm.Image2.Width div 2)), {x=x1-x2}
                ((MainForm.Image2.Height div 2) - ((DIYXY.Y) +
                  ((DIYX2Y2.Y - DIYXY.Y) div 2))),
                ((DIYX2Y2.X - DIYXY.X) div 2), {b:= x/2}
                V1Color,
{        (((DIYXY.X - DIYX2Y2.X)/(DIYXY.Y - DIYX2Y2.Y))
         / (640/480))}
                1.0); {aspect:=( (x/y) / (640/480)}
            end else {DIYZ,DIYL:Integer;}
              if ((GearS = 'dtSCEllipse')) then begin
                inc(Counter);
                GearS := DIYMemo.Lines[Counter];
                TransferS := Copy(GearS, 4, Length(GearS));
                val(TransferS, DIYXY.X, Code); Codefx(TransferS,
                  Code);
                inc(Counter);
                GearS := DIYMemo.Lines[Counter];
                TransferS := Copy(GearS, 4, Length(GearS));
                val(TransferS, DIYXY.Y, Code); Codefx(TransferS,
                  Code);
                inc(Counter);
                GearS := DIYMemo.Lines[Counter];
                TransferS := Copy(GearS, 4, Length(GearS));
                val(TransferS, DIYX2Y2.X, Code); Codefx(TransferS,
                  Code);
                inc(Counter);
                GearS := DIYMemo.Lines[Counter];
                TransferS := Copy(GearS, 4, Length(GearS));
                val(TransferS, DIYX2Y2.Y, Code); Codefx(TransferS,
                  Code);
                inc(Counter);
                GearS := DIYMemo.Lines[Counter];
                TransferS := Copy(GearS, 4, Length(GearS));
                val(TransferS, V1Color, Code); Codefx(TransferS,
                  Code);
                inc(Counter);
                GearS := DIYMemo.Lines[Counter];
                TransferS := Copy(GearS, 4, Length(GearS));
                val(TransferS, V2Color, Code); Codefx(TransferS,
                  Code);
                inc(Counter);
{read file name and PROCESS}
                GearS := DIYMemo.Lines[Counter];
                inc(Counter);
                if (not (GearS = '')) then begin
                  SkyLoaderDo(GearS);
                  SkyDiver(SkyLevel);
                  SkyImage;
                end else begin
                  Pen.Color := V1Color;
                  Brush.Color := V1Color;
                  Brush.Style := bsFDiagonal {Solid};
                  MainForm.DrawingTool := dtVSRectangle;
                  MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3,
                    DIYX4Y4, pmCopy);
                end;
              end else
                if ((GearS = 'dtVTriangle')) then begin
                  inc(Counter);
                  GearS := DIYMemo.Lines[Counter];
                  TransferS := Copy(GearS, 4, Length(GearS));
                  val(TransferS, DIYXY.X, Code); Codefx(TransferS,
                    Code);
                  inc(Counter);
                  GearS := DIYMemo.Lines[Counter];
                  TransferS := Copy(GearS, 4, Length(GearS));
                  val(TransferS, DIYXY.Y, Code); Codefx(TransferS,
                    Code);
                  inc(Counter);
                  GearS := DIYMemo.Lines[Counter];
                  TransferS := Copy(GearS, 4, Length(GearS));
                  val(TransferS, DIYX2Y2.X, Code); Codefx(TransferS,
                    Code);
                  inc(Counter);
                  GearS := DIYMemo.Lines[Counter];
                  TransferS := Copy(GearS, 4, Length(GearS));
                  val(TransferS, DIYX2Y2.Y, Code); Codefx(TransferS,
                    Code);
                  inc(Counter);
                  GearS := DIYMemo.Lines[Counter];
                  TransferS := Copy(GearS, 4, Length(GearS));
                  val(TransferS, DIYX3Y3.X, Code); Codefx(TransferS,
                    Code);
                  inc(Counter);
                  GearS := DIYMemo.Lines[Counter];
                  TransferS := Copy(GearS, 4, Length(GearS));
                  val(TransferS, DIYX3Y3.Y, Code); Codefx(TransferS,
                    Code);
                  inc(Counter);
                  GearS := DIYMemo.Lines[Counter];
                  TransferS := Copy(GearS, 4, Length(GearS));
                  val(TransferS, DIYZ, Code); Codefx(TransferS,
                    Code);
                  inc(Counter);
                  GearS := DIYMemo.Lines[Counter];
                  TransferS := Copy(GearS, 4, Length(GearS));
                  val(TransferS, DIYE, Code); Codefx(TransferS,
                    Code);
                  inc(Counter);
                  GearS := DIYMemo.Lines[Counter];
                  TransferS := Copy(GearS, 4, Length(GearS));
                  val(TransferS, V1Color, Code); Codefx(TransferS,
                    Code);
                  inc(Counter);
                  GearS := DIYMemo.Lines[Counter];
                  TransferS := Copy(GearS, 4, Length(GearS));
                  val(TransferS, V2Color, Code); Codefx(TransferS,
                    Code);
                  inc(Counter);
{        Pen.Color := V1Color;
        Brush.Color:=V1Color;
        Brush.Style:=bsDiagCross;
        MainForm.DrawingTool := dtVTriangle;
        ITriangling:=10;
 MainForm.DrawShape(DIYXY,DIYX2Y2,DIYX3Y3,DIYX4Y4, pmCopy);}
{procedure generate(x1,y1,x2,y2,x3,y3,level,DIYE: integer;
        color1,color2: TColor);}
{generate(DIYXY.X,DIYXY.Y,DIYX2Y2.X,DIYX2Y2.Y,DIYX3Y3.X,DIYX3Y3.Y,
        DIYZ,DIYE,V1Color,V2Color);}
{str(( x-(Image1.Width div 2)),Xs);
str(((Image1.Height div 2)-y),Ys);}
                  generate(
                    DIYXY.X - (MainForm.Image2.Width div 2),
                    (MainForm.Image2.Height div 2) - DIYXY.Y,
                    DIYX2Y2.X - (MainForm.Image2.Width div 2),
                    (MainForm.Image2.Height div 2) - DIYX2Y2.Y,
                    DIYX3Y3.X - (MainForm.Image2.Width div 2),
                    (MainForm.Image2.Height div 2) - DIYX3Y3.Y,
                    DIYZ, DIYE, V1Color, V2Color);
                end else {bsDiagCross DIYXY,DIYX2Y2 DIYX3Y3, DIYX4Y4: TPoint;}
                  if (GearS = 'dtTSTriangle') then begin
                    inc(Counter);
                    GearS := DIYMemo.Lines[Counter];
                    TransferS := Copy(GearS, 4, Length(GearS));
                    val(TransferS, DIYXY.X, Code); Codefx(TransferS,
                      Code);
                    inc(Counter);
                    GearS := DIYMemo.Lines[Counter];
                    TransferS := Copy(GearS, 4, Length(GearS));
                    val(TransferS, DIYXY.Y, Code); Codefx(TransferS,
                      Code);
                    inc(Counter);
                    GearS := DIYMemo.Lines[Counter];
                    TransferS := Copy(GearS, 4, Length(GearS));
                    val(TransferS, DIYX2Y2.X, Code);
                      Codefx(TransferS, Code);
                    inc(Counter);
                    GearS := DIYMemo.Lines[Counter];
                    TransferS := Copy(GearS, 4, Length(GearS));
                    val(TransferS, DIYX2Y2.Y, Code);
                      Codefx(TransferS, Code);
                    inc(Counter);
                    GearS := DIYMemo.Lines[Counter];
                    TransferS := Copy(GearS, 4, Length(GearS));
                    val(TransferS, DIYX3Y3.X, Code);
                      Codefx(TransferS, Code);
                    inc(Counter);
                    GearS := DIYMemo.Lines[Counter];
                    TransferS := Copy(GearS, 4, Length(GearS));
                    val(TransferS, DIYX3Y3.Y, Code);
                      Codefx(TransferS, Code);
                    inc(Counter);
                    GearS := DIYMemo.Lines[Counter];
                    TransferS := Copy(GearS, 4, Length(GearS));
                    val(TransferS, DIYL.X, Code); Codefx(TransferS,
                      Code);
                    inc(Counter);
                    GearS := DIYMemo.Lines[Counter];
                    TransferS := Copy(GearS, 4, Length(GearS));
                    val(TransferS, DIYL.Y, Code); Codefx(TransferS,
                      Code);
                    inc(Counter);
                    GearS := DIYMemo.Lines[Counter];
                    TransferS := Copy(GearS, 4, Length(GearS));
                    val(TransferS, V1Color, Code); Codefx(TransferS,
                      Code);
                    inc(Counter);
                    GearS := DIYMemo.Lines[Counter];
                    TransferS := Copy(GearS, 4, Length(GearS));
                    val(TransferS, V2Color, Code); Codefx(TransferS,
                      Code);
                    inc(Counter);
{        Pen.Color := V1Color;
        Brush.Color:=V1Color;
        Brush.Style:=bsBDiagonal;
        MainForm.DrawingTool := dtTSTriangle;
        MainForm.DrawShape(DIYXY,DIYX2Y2,DIYX3Y3,DIYX4Y4, pmCopy);}
{change to do for a triangle
        ranFillOval(x,y,b: integer;
                color: TColor;aspect: Extended);
RanFillTri(Pt1,Pt2,Pt3,LtPt:TPoint;color: TColor)}
                    RanFillTri(DIYXY, DIYX2Y2, DIYX3Y3, DIYL,
                      V1Color);
                  end else {bsBDiagonal DIYXY,DIYX2Y2 DIYX3Y3, DIYX4Y4: TPoint;}
                    if (GearS = 'dtTriangle') then begin
                      inc(Counter);
                      GearS := DIYMemo.Lines[Counter];
                      TransferS := Copy(GearS, 4, Length(GearS));
                      val(TransferS, DIYXY.X, Code);
                        Codefx(TransferS, Code);
                      inc(Counter);
                      GearS := DIYMemo.Lines[Counter];
                      TransferS := Copy(GearS, 4, Length(GearS));
                      val(TransferS, DIYXY.Y, Code);
                        Codefx(TransferS, Code);
                      inc(Counter);
                      GearS := DIYMemo.Lines[Counter];
                      TransferS := Copy(GearS, 4, Length(GearS));
                      val(TransferS, DIYX2Y2.X, Code);
                        Codefx(TransferS, Code);
                      inc(Counter);
                      GearS := DIYMemo.Lines[Counter];
                      TransferS := Copy(GearS, 4, Length(GearS));
                      val(TransferS, DIYX2Y2.Y, Code);
                        Codefx(TransferS, Code);
                      inc(Counter);
                      GearS := DIYMemo.Lines[Counter];
                      TransferS := Copy(GearS, 4, Length(GearS));
                      val(TransferS, DIYX3Y3.X, Code);
                        Codefx(TransferS, Code);
                      inc(Counter);
                      GearS := DIYMemo.Lines[Counter];
                      TransferS := Copy(GearS, 4, Length(GearS));
                      val(TransferS, DIYX3Y3.Y, Code);
                        Codefx(TransferS, Code);
                      inc(Counter);
                      GearS := DIYMemo.Lines[Counter];
                      TransferS := Copy(GearS, 4, Length(GearS));
                      val(TransferS, V1Color, Code);
                        Codefx(TransferS, Code);
                      inc(Counter);
                      GearS := DIYMemo.Lines[Counter];
                      TransferS := Copy(GearS, 4, Length(GearS));
                      val(TransferS, V2Color, Code);
                        Codefx(TransferS, Code);
                      inc(Counter);
                      Pen.Color := V1Color;
                      Brush.Color := V1Color;
                      Brush.Style := bsSolid;
                      MainForm.DrawingTool := dtTriangle;
                      MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3,
                        DIYX4Y4, pmCopy);
                    end else {no fill DIYXY,DIYX2Y2 DIYX3Y3, DIYX4Y4: TPoint;}
                      if ((GearS = 'dtVRectangle')) then begin {}
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, DIYXY.X, Code);
                          Codefx(TransferS, Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, DIYXY.Y, Code);
                          Codefx(TransferS, Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, DIYX2Y2.X, Code);
                          Codefx(TransferS, Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, DIYX2Y2.Y, Code);
                          Codefx(TransferS, Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, DIYX3Y3.X, Code);
                          Codefx(TransferS, Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, DIYX3Y3.Y, Code);
                          Codefx(TransferS, Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, DIYX4Y4.X, Code);
                          Codefx(TransferS, Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, DIYX4Y4.Y, Code);
                          Codefx(TransferS, Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, DIYZ, Code); Codefx(TransferS,
                          Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, DIYE, Code); Codefx(TransferS,
                          Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, V1Color, Code);
                          Codefx(TransferS, Code);
                        inc(Counter);
                        GearS := DIYMemo.Lines[Counter];
                        TransferS := Copy(GearS, 4, Length(GearS));
                        val(TransferS, V2Color, Code);
                          Codefx(TransferS, Code);
                        inc(Counter);
{        Pen.Color := V1Color;
        Brush.Color:=V1Color;
        Brush.Style:=bsDiagCross;
        MainForm.DrawingTool := dtVRectangle;
        ITriangling:=10;
        MainForm.DrawShape(DIYXY,DIYX2Y2,DIYX3Y3,DIYX4Y4, pmCopy);}
{gen_quad (x1,y1,x2,y2,x3, y3, x4,y4,level,DIYE: integer;
 color1,color2: Tcolor);}
                        gen_quad(
                          DIYXY.X - (MainForm.Image2.Width div 2),
                          (MainForm.Image2.Height div 2) - DIYXY.Y,
                          DIYX2Y2.X - (MainForm.Image2.Width div 2),
                          (MainForm.Image2.Height div 2) - DIYX2Y2.Y,
                          DIYX3Y3.X - (MainForm.Image2.Width div 2),
                          (MainForm.Image2.Height div 2) - DIYX3Y3.Y,
                          DIYX4Y4.X - (MainForm.Image2.Width div 2),
                          (MainForm.Image2.Height div 2) - DIYX4Y4.Y,
                          DIYZ, DIYE, V1Color, V2Color);
                      end else {bsDiagCross}
                        if ((GearS = 'dtVTRectangle')) then begin {}
                          inc(Counter);
                          GearS := DIYMemo.Lines[Counter];
                          TransferS := Copy(GearS, 4, Length(GearS));
                          val(TransferS, DIYXY.X, Code);
                            Codefx(TransferS, Code);
                          inc(Counter);
                          GearS := DIYMemo.Lines[Counter];
                          TransferS := Copy(GearS, 4, Length(GearS));
                          val(TransferS, DIYXY.Y, Code);
                            Codefx(TransferS, Code);
                          inc(Counter);
                          GearS := DIYMemo.Lines[Counter];
                          TransferS := Copy(GearS, 4, Length(GearS));
                          val(TransferS, DIYX2Y2.X, Code);
                            Codefx(TransferS, Code);
                          inc(Counter);
                          GearS := DIYMemo.Lines[Counter];
                          TransferS := Copy(GearS, 4, Length(GearS));
                          val(TransferS, DIYX2Y2.Y, Code);
                            Codefx(TransferS, Code);
                          inc(Counter);
                          GearS := DIYMemo.Lines[Counter];
                          TransferS := Copy(GearS, 4, Length(GearS));
                          val(TransferS, V1Color, Code);
                            Codefx(TransferS, Code);
                          inc(Counter);
                          GearS := DIYMemo.Lines[Counter];
                          TransferS := Copy(GearS, 4, Length(GearS));
                          val(TransferS, V2Color, Code);
                            Codefx(TransferS, Code);
                          inc(Counter);
{read file name and PROCESS}
                          GearS := DIYMemo.Lines[Counter];
                          inc(Counter);
                          if (not (GearS = '')) then begin
                            DoTreeLoader(GearS);
                            treerun;
                          end else begin
                            Pen.Color := V1Color;
                            Brush.Color := V1Color;
                            Brush.Style := bsHorizontal {Solid};
                            MainForm.DrawingTool := dtVTRectangle;
                            MainForm.DrawShape(DIYXY, DIYX2Y2,
                              DIYX3Y3, DIYX4Y4, pmCopy);
                          end;
                        end else {bsHorizontal}
                          if ((GearS = 'dtVFRectangle')) then begin {}
                            inc(Counter);
                            GearS := DIYMemo.Lines[Counter];
                            TransferS := Copy(GearS, 4,
                              Length(GearS));
                            val(TransferS, DIYXY.X, Code);
                              Codefx(TransferS, Code);
                            inc(Counter);
                            GearS := DIYMemo.Lines[Counter];
                            TransferS := Copy(GearS, 4,
                              Length(GearS));
                            val(TransferS, DIYXY.Y, Code);
                              Codefx(TransferS, Code);
                            inc(Counter);
                            GearS := DIYMemo.Lines[Counter];
                            TransferS := Copy(GearS, 4,
                              Length(GearS));
                            val(TransferS, DIYX2Y2.X, Code);
                              Codefx(TransferS, Code);
                            inc(Counter);
                            GearS := DIYMemo.Lines[Counter];
                            TransferS := Copy(GearS, 4,
                              Length(GearS));
                            val(TransferS, DIYX2Y2.Y, Code);
                              Codefx(TransferS, Code);
                            inc(Counter);
                            GearS := DIYMemo.Lines[Counter];
                            TransferS := Copy(GearS, 4,
                              Length(GearS));
                            val(TransferS, V1Color, Code);
                              Codefx(TransferS, Code);
                            inc(Counter);
                            GearS := DIYMemo.Lines[Counter];
                            TransferS := Copy(GearS, 4,
                              Length(GearS));
                            val(TransferS, V2Color, Code);
                              Codefx(TransferS, Code);
                            inc(Counter);
{read file name and PROCESS}
                            GearS := DIYMemo.Lines[Counter];
                            inc(Counter);
                            if (not (GearS = '')) then begin
                              DoFernLoader(GearS);
                              MGFernrun;
                            end else begin
                              Pen.Color := V1Color;
                              Brush.Color := V1Color;
                              Brush.Style := bsHorizontal {Solid};
                              MainForm.DrawingTool := dtVFRectangle;
                              MainForm.DrawShape(DIYXY, DIYX2Y2,
                                DIYX3Y3, DIYX4Y4, pmCopy);
                            end;
                          end else {bsHorizontal}
                            if ((GearS = 'dtVDRectangle')) then
                              begin {}
                              inc(Counter);
                              GearS := DIYMemo.Lines[Counter];
                              TransferS := Copy(GearS, 4,
                                Length(GearS));
                              val(TransferS, DIYXY.X, Code);
                                Codefx(TransferS, Code);
                              inc(Counter);
                              GearS := DIYMemo.Lines[Counter];
                              TransferS := Copy(GearS, 4,
                                Length(GearS));
                              val(TransferS, DIYXY.Y, Code);
                                Codefx(TransferS, Code);
                              inc(Counter);
                              GearS := DIYMemo.Lines[Counter];
                              TransferS := Copy(GearS, 4,
                                Length(GearS));
                              val(TransferS, DIYX2Y2.X, Code);
                                Codefx(TransferS, Code);
                              inc(Counter);
                              GearS := DIYMemo.Lines[Counter];
                              TransferS := Copy(GearS, 4,
                                Length(GearS));
                              val(TransferS, DIYX2Y2.Y, Code);
                                Codefx(TransferS, Code);
                              inc(Counter);
                              GearS := DIYMemo.Lines[Counter];
                              TransferS := Copy(GearS, 4,
                                Length(GearS));
                              val(TransferS, V1Color, Code);
                                Codefx(TransferS, Code);
                              inc(Counter);
                              GearS := DIYMemo.Lines[Counter];
                              TransferS := Copy(GearS, 4,
                                Length(GearS));
                              val(TransferS, V2Color, Code);
                                Codefx(TransferS, Code);
                              inc(Counter);
{read file name and PROCESS}
                              GearS := DIYMemo.Lines[Counter];
                              inc(Counter);
                              if (not (GearS = '')) then begin
                                DoFernLoader(GearS);
                                MGDragon3DO;
                              end else begin
                                Pen.Color := V1Color;
                                Brush.Color := V1Color;
                                Brush.Style := bsFDiagonal {Solid};
                                MainForm.DrawingTool :=
                                  dtVDRectangle;
                                MainForm.DrawShape(DIYXY, DIYX2Y2,
                                  DIYX3Y3, DIYX4Y4, pmCopy);
                              end;
                            end else {bsFDiagonal}
                              if ((GearS = 'dtVWRectangle')) then
                                begin {}
                                inc(Counter);
                                GearS := DIYMemo.Lines[Counter];
                                TransferS := Copy(GearS, 4,
                                  Length(GearS));
                                val(TransferS, DIYXY.X, Code);
                                  Codefx(TransferS, Code);
                                inc(Counter);
                                GearS := DIYMemo.Lines[Counter];
                                TransferS := Copy(GearS, 4,
                                  Length(GearS));
                                val(TransferS, DIYXY.Y, Code);
                                  Codefx(TransferS, Code);
                                inc(Counter);
                                GearS := DIYMemo.Lines[Counter];
                                TransferS := Copy(GearS, 4,
                                  Length(GearS));
                                val(TransferS, DIYX2Y2.X, Code);
                                  Codefx(TransferS, Code);
                                inc(Counter);
                                GearS := DIYMemo.Lines[Counter];
                                TransferS := Copy(GearS, 4,
                                  Length(GearS));
                                val(TransferS, DIYX2Y2.Y, Code);
                                  Codefx(TransferS, Code);
                                inc(Counter);
                                GearS := DIYMemo.Lines[Counter];
                                TransferS := Copy(GearS, 4,
                                  Length(GearS));
                                val(TransferS, V1Color, Code);
                                  Codefx(TransferS, Code);
                                inc(Counter);
                                GearS := DIYMemo.Lines[Counter];
                                TransferS := Copy(GearS, 4,
                                  Length(GearS));
                                val(TransferS, V2Color, Code);
                                  Codefx(TransferS, Code);
                                inc(Counter);
{read file name and PROCESS}
                                GearS := DIYMemo.Lines[Counter];
                                inc(Counter);
{        If (not(GearS=''))then begin
Torso
        NumbOpen(GearS);LivingDead(NumbLevel);Numb3DImage;
        end;}
                                Pen.Color := V1Color;
                                Brush.Color := V1Color;
                                Brush.Style := bsFDiagonal;
                                MainForm.DrawingTool :=
                                  dtVWRectangle;
                                MainForm.DrawShape(DIYXY, DIYX2Y2,
                                  DIYX3Y3, DIYX4Y4, pmCopy);
                              end else {bsFDiagonal}
                                if ((GearS = 'dtVCRectangle')) then
                                  begin {}
                                  inc(Counter);
                                  GearS := DIYMemo.Lines[Counter];
                                  TransferS := Copy(GearS, 4,
                                    Length(GearS));
                                  val(TransferS, DIYXY.X, Code);
                                    Codefx(TransferS, Code);
                                  inc(Counter);
                                  GearS := DIYMemo.Lines[Counter];
                                  TransferS := Copy(GearS, 4,
                                    Length(GearS));
                                  val(TransferS, DIYXY.Y, Code);
                                    Codefx(TransferS, Code);
                                  inc(Counter);
                                  GearS := DIYMemo.Lines[Counter];
                                  TransferS := Copy(GearS, 4,
                                    Length(GearS));
                                  val(TransferS, DIYX2Y2.X, Code);
                                    Codefx(TransferS, Code);
                                  inc(Counter);
                                  GearS := DIYMemo.Lines[Counter];
                                  TransferS := Copy(GearS, 4,
                                    Length(GearS));
                                  val(TransferS, DIYX2Y2.Y, Code);
                                    Codefx(TransferS, Code);
                                  inc(Counter);
                                  GearS := DIYMemo.Lines[Counter];
                                  TransferS := Copy(GearS, 4,
                                    Length(GearS));
                                  val(TransferS, V1Color, Code);
                                    Codefx(TransferS, Code);
                                  inc(Counter);
                                  GearS := DIYMemo.Lines[Counter];
                                  TransferS := Copy(GearS, 4,
                                    Length(GearS));
                                  val(TransferS, V2Color, Code);
                                    Codefx(TransferS, Code);
                                  inc(Counter);
{read file name and PROCESS}
                                  GearS := DIYMemo.Lines[Counter];
                                  inc(Counter);
{        If (not(GearS=''))then begin
Collage
        NumbOpen(GearS);LivingDead(NumbLevel);Numb3DImage;
        end;}
                                  Pen.Color := V1Color;
                                  Brush.Color := V1Color;
                                  Brush.Style := bsFDiagonal;
                                  MainForm.DrawingTool :=
                                    dtVCRectangle;
                                  MainForm.DrawShape(DIYXY, DIYX2Y2,
                                    DIYX3Y3, DIYX4Y4, pmCopy);
                                end else {bsFDiagonal}
                                  if ((GearS = 'dtVARectangle')) then
                                    begin {}
                                    inc(Counter);
                                    GearS := DIYMemo.Lines[Counter];
                                    TransferS := Copy(GearS, 4,
                                      Length(GearS));
                                    val(TransferS, DIYXY.X, Code);
                                      Codefx(TransferS, Code);
                                    inc(Counter);
                                    GearS := DIYMemo.Lines[Counter];
                                    TransferS := Copy(GearS, 4,
                                      Length(GearS));
                                    val(TransferS, DIYXY.Y, Code);
                                      Codefx(TransferS, Code);
                                    inc(Counter);
                                    GearS := DIYMemo.Lines[Counter];
                                    TransferS := Copy(GearS, 4,
                                      Length(GearS));
                                    val(TransferS, DIYX2Y2.X, Code);
                                      Codefx(TransferS, Code);
                                    inc(Counter);
                                    GearS := DIYMemo.Lines[Counter];
                                    TransferS := Copy(GearS, 4,
                                      Length(GearS));
                                    val(TransferS, DIYX2Y2.Y, Code);
                                      Codefx(TransferS, Code);
                                    inc(Counter);
                                    GearS := DIYMemo.Lines[Counter];
                                    TransferS := Copy(GearS, 4,
                                      Length(GearS));
                                    val(TransferS, V1Color, Code);
                                      Codefx(TransferS, Code);
                                    inc(Counter);
                                    GearS := DIYMemo.Lines[Counter];
                                    TransferS := Copy(GearS, 4,
                                      Length(GearS));
                                    val(TransferS, V2Color, Code);
                                      Codefx(TransferS, Code);
                                    inc(Counter);
{read file name and PROCESS}
                                    GearS := DIYMemo.Lines[Counter];
                                    inc(Counter);
                                    if (not (GearS = '')) then begin
{Turtle}
                                      TurtleLoadDo(GearS);
                                      TurtleRun;
                                    end else begin
                                      Pen.Color := V1Color;
                                      Brush.Color := V1Color;
                                      Brush.Style := bsFDiagonal;
                                      MainForm.DrawingTool :=
                                        dtVARectangle;
                                      MainForm.DrawShape(DIYXY,
                                        DIYX2Y2, DIYX3Y3, DIYX4Y4,
                                        pmCopy);
                                    end;
                                  end else {bsFDiagonal}
                                    if ((GearS = 'dtVMRectangle'))
                                      then begin {}
                                      inc(Counter);
                                      GearS :=
                                        DIYMemo.Lines[Counter];
                                      TransferS := Copy(GearS, 4,
                                        Length(GearS));
                                      val(TransferS, DIYXY.X, Code);
                                        Codefx(TransferS, Code);
                                      inc(Counter);
                                      GearS :=
                                        DIYMemo.Lines[Counter];
                                      TransferS := Copy(GearS, 4,
                                        Length(GearS));
                                      val(TransferS, DIYXY.Y, Code);
                                        Codefx(TransferS, Code);
                                      inc(Counter);
                                      GearS :=
                                        DIYMemo.Lines[Counter];
                                      TransferS := Copy(GearS, 4,
                                        Length(GearS));
                                      val(TransferS, DIYX2Y2.X,
                                        Code); Codefx(TransferS, Code);
                                      inc(Counter);
                                      GearS :=
                                        DIYMemo.Lines[Counter];
                                      TransferS := Copy(GearS, 4,
                                        Length(GearS));
                                      val(TransferS, DIYX2Y2.Y,
                                        Code); Codefx(TransferS, Code);
                                      inc(Counter);
                                      GearS :=
                                        DIYMemo.Lines[Counter];
                                      TransferS := Copy(GearS, 4,
                                        Length(GearS));
                                      val(TransferS, V1Color, Code);
                                        Codefx(TransferS, Code);
                                      inc(Counter);
                                      GearS :=
                                        DIYMemo.Lines[Counter];
                                      TransferS := Copy(GearS, 4,
                                        Length(GearS));
                                      val(TransferS, V2Color, Code);
                                        Codefx(TransferS, Code);
                                      inc(Counter);
        {read file name and PROCESS}
                                      GearS :=
                                        DIYMemo.Lines[Counter];
                                      inc(Counter);
                                      if (not (GearS = '')) then begin
{XYZ 3D Fractal DEM}
                                        XYZ3DForm.EleLoadDo(GearS);
                                        XYZ3DForm.EleRunDo;
                                      end else begin
                                        Pen.Color := V1Color;
                                        Brush.Color := V1Color;
                                        Brush.Style := bsFDiagonal;
                                        MainForm.DrawingTool :=
                                          dtVMRectangle;
                                        MainForm.DrawShape(DIYXY,
                                          DIYX2Y2, DIYX3Y3, DIYX4Y4,
                                          pmCopy);
                                      end;
                                    end else {bsFDiagonal}
                                      if ((GearS = 'dtVERectangle'))
                                        then begin {}
                                        inc(Counter);
                                        GearS :=
                                          DIYMemo.Lines[Counter];
                                        TransferS := Copy(GearS, 4,
                                          Length(GearS));
                                        val(TransferS, DIYXY.X,
                                          Code); Codefx(TransferS,
                                          Code);
                                        inc(Counter);
                                        GearS :=
                                          DIYMemo.Lines[Counter];
                                        TransferS := Copy(GearS, 4,
                                          Length(GearS));
                                        val(TransferS, DIYXY.Y,
                                          Code); Codefx(TransferS,
                                          Code);
                                        inc(Counter);
                                        GearS :=
                                          DIYMemo.Lines[Counter];
                                        TransferS := Copy(GearS, 4,
                                          Length(GearS));
                                        val(TransferS, DIYX2Y2.X,
                                          Code); Codefx(TransferS,
                                          Code);
                                        inc(Counter);
                                        GearS :=
                                          DIYMemo.Lines[Counter];
                                        TransferS := Copy(GearS, 4,
                                          Length(GearS));
                                        val(TransferS, DIYX2Y2.Y,
                                          Code); Codefx(TransferS,
                                          Code);
                                        inc(Counter);
                                        GearS :=
                                          DIYMemo.Lines[Counter];
                                        TransferS := Copy(GearS, 4,
                                          Length(GearS));
                                        val(TransferS, V1Color,
                                          Code); Codefx(TransferS,
                                          Code);
                                        inc(Counter);
                                        GearS :=
                                          DIYMemo.Lines[Counter];
                                        TransferS := Copy(GearS, 4,
                                          Length(GearS));
                                        val(TransferS, V2Color,
                                          Code); Codefx(TransferS,
                                          Code);
                                        inc(Counter);
                                        GearS :=
                                          DIYMemo.Lines[Counter];
                                        TransferS := Copy(GearS, 4,
                                          Length(GearS));
                                        val(TransferS, Color256S,
                                          Code); Codefx(TransferS,
                                          Code);
                                        inc(Counter);
                                        MainForm.FalseSets;
{Empty Blank ramped color rectangle}
                                        Pen.Color := V1Color;
                                        Brush.Color := V4Color;
                                        dCounter := 0;
                                        Ramper := (255 / (1 +
                                          abs(DIYXY.Y - DIYX2Y2.Y)));
                                        for Code := DIYXY.Y to
                                          DIYX2Y2.Y do begin
                                          V4Color := RGB(Colors[0,
                                            round(dCounter)],
                                            Colors[1,
                                              round(dCounter)],
                                            Colors[2,
                                              round(dCounter)]);
                                          dCounter := dCounter +
                                            Ramper;
                                          Pen.Color := V4Color;
                                          Moveto(DIYXY.X, Code);
                                          Lineto(DIYX2Y2.X, Code);
                                        end;
                                        V4Color := Brush.Color;
                                      end else {bsFDiagonal}
                                        if ((GearS = 'dtVPRectangle'))
                                          then begin {}
                                          inc(Counter);
                                          GearS :=
                                            DIYMemo.Lines[Counter];
                                          TransferS := Copy(GearS, 4,
                                            Length(GearS));
                                          val(TransferS, DIYXY.X,
                                            Code); Codefx(TransferS,
                                            Code);
                                          inc(Counter);
                                          GearS :=
                                            DIYMemo.Lines[Counter];
                                          TransferS := Copy(GearS, 4,
                                            Length(GearS));
                                          val(TransferS, DIYXY.Y,
                                            Code); Codefx(TransferS,
                                            Code);
                                          inc(Counter);
                                          GearS :=
                                            DIYMemo.Lines[Counter];
                                          TransferS := Copy(GearS, 4,
                                            Length(GearS));
                                          val(TransferS, DIYX2Y2.X,
                                            Code); Codefx(TransferS,
                                            Code);
                                          inc(Counter);
                                          GearS :=
                                            DIYMemo.Lines[Counter];
                                          TransferS := Copy(GearS, 4,
                                            Length(GearS));
                                          val(TransferS, DIYX2Y2.Y,
                                            Code); Codefx(TransferS,
                                            Code);
                                          inc(Counter);
                                          GearS :=
                                            DIYMemo.Lines[Counter];
                                          TransferS := Copy(GearS, 4,
                                            Length(GearS));
                                          val(TransferS, V1Color,
                                            Code); Codefx(TransferS,
                                            Code);
                                          inc(Counter);
                                          GearS :=
                                            DIYMemo.Lines[Counter];
                                          TransferS := Copy(GearS, 4,
                                            Length(GearS));
                                          val(TransferS, V2Color,
                                            Code); Codefx(TransferS,
                                            Code);
                                          inc(Counter);
                                          GearS :=
                                            DIYMemo.Lines[Counter];
                                          TransferS := Copy(GearS, 4,
                                            Length(GearS));
                                          val(TransferS, V3Color,
                                            Code); Codefx(TransferS,
                                            Code);
                                          inc(Counter);
                                          GearS :=
                                            DIYMemo.Lines[Counter];
                                          TransferS := Copy(GearS, 4,
                                            Length(GearS));
                                          val(TransferS, V4Color,
                                            Code); Codefx(TransferS,
                                            Code);
                                          inc(Counter);
                                          GearS :=
                                            DIYMemo.Lines[Counter];
                                          TransferS := Copy(GearS, 4,
                                            Length(GearS));
                                          val(TransferS, dCounter,
                                            Code); Codefx(TransferS,
                                            Code);
                                          inc(Counter);
{Pandora dCounter}
                                          Ramper := (255 / (1 +
                                            abs(DIYXY.Y - DIYX2Y2.Y)));
                                          for Code := DIYXY.Y to
                                            DIYX2Y2.Y do begin
                                            for ColdX := DIYXY.X to
                                              DIYX2Y2.X do begin
                                              if (random(abs(DIYXY.Y
                                                - DIYX2Y2.Y))
                                                <= random(round(Ramper
                                                  * dCounter))) then
                                                Pixels[ColdX, Code]
                                                  := V1Color;
                                              if (random(abs(DIYXY.Y
                                                - DIYX2Y2.Y))
                                                <= random(round(Ramper
                                                  * (dCounter / 8))))
                                                  then begin
                                                Pixels[ColdX + 1,
                                                  Code] := V2Color;
                                                Pixels[ColdX, Code]
                                                  := V3Color;
                                                Pixels[ColdX, Code +
                                                  1] := V2Color;
                                                Pixels[ColdX - 1,
                                                  Code] := V2Color;
                                                Pixels[ColdX, Code -
                                                  1] := V2Color; end;
                                              if (random(abs(DIYXY.Y
                                                - DIYX2Y2.Y))
                                                <= random(round(Ramper
                                                  * dCounter))) then
                                                Pixels[ColdX, Code]
                                                  := V4Color;
                                            end; end;
                                        end else {bsFDiagonal}
                                          if ((GearS =
                                            'dtVNRectangle')) then
                                            begin {}
                                            inc(Counter);
                                            GearS :=
                                              DIYMemo.Lines[Counter];
                                            TransferS := Copy(GearS,
                                              4, Length(GearS));
                                            val(TransferS, DIYXY.X,
                                              Code); Codefx(TransferS,
                                              Code);
                                            inc(Counter);
                                            GearS :=
                                              DIYMemo.Lines[Counter];
                                            TransferS := Copy(GearS,
                                              4, Length(GearS));
                                            val(TransferS, DIYXY.Y,
                                              Code); Codefx(TransferS,
                                              Code);
                                            inc(Counter);
                                            GearS :=
                                              DIYMemo.Lines[Counter];
                                            TransferS := Copy(GearS,
                                              4, Length(GearS));
                                            val(TransferS, DIYX2Y2.X,
                                              Code); Codefx(TransferS,
                                              Code);
                                            inc(Counter);
                                            GearS :=
                                              DIYMemo.Lines[Counter];
                                            TransferS := Copy(GearS,
                                              4, Length(GearS));
                                            val(TransferS, DIYX2Y2.Y,
                                              Code); Codefx(TransferS,
                                              Code);
                                            inc(Counter);
                                            GearS :=
                                              DIYMemo.Lines[Counter];
                                            TransferS := Copy(GearS,
                                              4, Length(GearS));
                                            val(TransferS, V1Color,
                                              Code); Codefx(TransferS,
                                              Code);
                                            inc(Counter);
                                            GearS :=
                                              DIYMemo.Lines[Counter];
                                            TransferS := Copy(GearS,
                                              4, Length(GearS));
                                            val(TransferS, V2Color,
                                              Code); Codefx(TransferS,
                                              Code);
                                            inc(Counter);
{read file name and PROCESS}
                                            GearS :=
                                              DIYMemo.Lines[Counter];
                                            inc(Counter);
                                            if (not (GearS = '')) then
                                              begin
                                              NumbOpen(GearS);
                                              LivingDead(NumbLevel);
                                              Numb3DImage;
                                            end else begin
                                              Pen.Color := V1Color;
                                              Brush.Color := V1Color;
                                              Brush.Style :=
                                                bsFDiagonal;
                                              MainForm.DrawingTool :=
                                                dtVNRectangle;
                                              MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3, DIYX4Y4, pmCopy);
                                            end;
                                          end else {bsFDiagonal}
                                            if ((GearS =
                                              'dtRectangle')) then
                                              begin {}
                                              inc(Counter);
                                              GearS :=
                                                DIYMemo.Lines[Counter];
                                              TransferS :=
                                                Copy(GearS, 4,
                                                Length(GearS));
                                              val(TransferS, DIYXY.X,
                                                Code);
                                                Codefx(TransferS,
                                                Code);
                                              inc(Counter);
                                              GearS :=
                                                DIYMemo.Lines[Counter];
                                              TransferS :=
                                                Copy(GearS, 4,
                                                Length(GearS));
                                              val(TransferS, DIYXY.Y,
                                                Code);
                                                Codefx(TransferS,
                                                Code);
                                              inc(Counter);
                                              GearS :=
                                                DIYMemo.Lines[Counter];
                                              TransferS :=
                                                Copy(GearS, 4,
                                                Length(GearS));
                                              val(TransferS,
                                                DIYX2Y2.X, Code);
                                                Codefx(TransferS,
                                                Code);
                                              inc(Counter);
                                              GearS :=
                                                DIYMemo.Lines[Counter];
                                              TransferS :=
                                                Copy(GearS, 4,
                                                Length(GearS));
                                              val(TransferS,
                                                DIYX2Y2.Y, Code);
                                                Codefx(TransferS,
                                                Code);
                                              inc(Counter);
                                              GearS :=
                                                DIYMemo.Lines[Counter];
                                              TransferS :=
                                                Copy(GearS, 4,
                                                Length(GearS));
                                              val(TransferS, V1Color,
                                                Code);
                                                Codefx(TransferS,
                                                Code);
                                              inc(Counter);
                                              GearS :=
                                                DIYMemo.Lines[Counter];
                                              TransferS :=
                                                Copy(GearS, 4,
                                                Length(GearS));
                                              val(TransferS, V2Color,
                                                Code);
                                                Codefx(TransferS,
                                                Code);
                                              inc(Counter);
                                              Pen.Color := V1Color;
                                              Brush.Color := V1Color;
                                              Brush.Style := bsSolid
                                                {Solid};
                                              MainForm.DrawingTool :=
                                                dtRectangle;
                                              MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3, DIYX4Y4, pmCopy);
                                            end else {bsSolid}
                                              if ((GearS =
                                                'dtRoundRect')) then
                                                begin {}
                                                inc(Counter);
                                                GearS :=
                                                  DIYMemo.Lines[Counter];
                                                TransferS :=
                                                  Copy(GearS, 4,
                                                  Length(GearS));
                                                val(TransferS,
                                                  DIYXY.X, Code);
                                                  Codefx(TransferS,
                                                  Code);
                                                inc(Counter);
                                                GearS :=
                                                  DIYMemo.Lines[Counter];
                                                TransferS :=
                                                  Copy(GearS, 4,
                                                  Length(GearS));
                                                val(TransferS,
                                                  DIYXY.Y, Code);
                                                  Codefx(TransferS,
                                                  Code);
                                                inc(Counter);
                                                GearS :=
                                                  DIYMemo.Lines[Counter];
                                                TransferS :=
                                                  Copy(GearS, 4,
                                                  Length(GearS));
                                                val(TransferS,
                                                  DIYX2Y2.X, Code);
                                                  Codefx(TransferS,
                                                  Code);
                                                inc(Counter);
                                                GearS :=
                                                  DIYMemo.Lines[Counter];
                                                TransferS :=
                                                  Copy(GearS, 4,
                                                  Length(GearS));
                                                val(TransferS,
                                                  DIYX2Y2.Y, Code);
                                                  Codefx(TransferS,
                                                  Code);
                                                inc(Counter);
                                                GearS :=
                                                  DIYMemo.Lines[Counter];
                                                TransferS :=
                                                  Copy(GearS, 4,
                                                  Length(GearS));
                                                val(TransferS,
                                                  V1Color, Code);
                                                  Codefx(TransferS,
                                                  Code);
                                                inc(Counter);
                                                GearS :=
                                                  DIYMemo.Lines[Counter];
                                                TransferS :=
                                                  Copy(GearS, 4,
                                                  Length(GearS));
                                                val(TransferS,
                                                  V2Color, Code);
                                                  Codefx(TransferS,
                                                  Code);
                                                inc(Counter);
                                                Pen.Color := V1Color;
                                                Brush.Color :=
                                                  V1Color;
                                                Brush.Style := bsSolid
                                                  {Solid};
                                                MainForm.DrawingTool
                                                  := dtRoundRect;
                                                MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3, DIYX4Y4, pmCopy);
                                              end else {bsSolid}
                                                if ((GearS =
                                                  'dtVBRectangle')) then
                                                  begin {}
                                                  inc(Counter);
                                                  GearS :=
                                                    DIYMemo.Lines[Counter];
                                                  TransferS :=
                                                    Copy(GearS, 4,
                                                    Length(GearS));
                                                  val(TransferS,
                                                    DIYXY.X, Code);
                                                    Codefx(TransferS,
                                                    Code);
                                                  inc(Counter);
                                                  GearS :=
                                                    DIYMemo.Lines[Counter];
                                                  TransferS :=
                                                    Copy(GearS, 4,
                                                    Length(GearS));
                                                  val(TransferS,
                                                    DIYXY.Y, Code);
                                                    Codefx(TransferS,
                                                    Code);
                                                  inc(Counter);
                                                  GearS :=
                                                    DIYMemo.Lines[Counter];
                                                  TransferS :=
                                                    Copy(GearS, 4,
                                                    Length(GearS));
                                                  val(TransferS,
                                                    DIYX2Y2.X, Code);
                                                    Codefx(TransferS,
                                                    Code);
                                                  inc(Counter);
                                                  GearS :=
                                                    DIYMemo.Lines[Counter];
                                                  TransferS :=
                                                    Copy(GearS, 4,
                                                    Length(GearS));
                                                  val(TransferS,
                                                    DIYX2Y2.Y, Code);
                                                    Codefx(TransferS,
                                                    Code);
                                                  inc(Counter);
                                                  GearS :=
                                                    DIYMemo.Lines[Counter];
                                                  TransferS :=
                                                    Copy(GearS, 4,
                                                    Length(GearS));
                                                  val(TransferS,
                                                    V1Color, Code);
                                                    Codefx(TransferS,
                                                    Code);
                                                  inc(Counter);
                                                  GearS :=
                                                    DIYMemo.Lines[Counter];
                                                  TransferS :=
                                                    Copy(GearS, 4,
                                                    Length(GearS));
                                                  val(TransferS,
                                                    V2Color, Code);
                                                    Codefx(TransferS,
                                                    Code);
                                                  inc(Counter);
                                                  GearS :=
                                                    DIYMemo.Lines[Counter];
                                                  inc(Counter);
{read file name and PROCESS DIYFilename}
                                                  if (not (GearS =
                                                    'NONE.BMP')) then
                                                    begin {DIYFilename (Bitmap)}
                                                    BitmapBlotto(GearS);
                                                  end else begin
                                                    Pen.Color :=
                                                      V1Color;
                                                    Brush.Color :=
                                                      V1Color;
                                                    Brush.Style :=
                                                      bsCross;
                                                    MainForm.DrawingTool
                                                      := dtVBRectangle;
                                                    MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3, DIYX4Y4, pmCopy);
                                                  end;
                                                end else {bsCross}
                                                  if ((GearS =
                                                    'dtLine')) then
                                                    begin {}
                                                    inc(Counter);
                                                    GearS :=
                                                      DIYMemo.Lines[Counter];
                                                    TransferS :=
                                                      Copy(GearS, 4,
                                                      Length(GearS));
                                                    val(TransferS,
                                                      DIYXY.X, Code);
                                                      Codefx(TransferS,
                                                      Code);
                                                    inc(Counter);
                                                    GearS :=
                                                      DIYMemo.Lines[Counter];
                                                    TransferS :=
                                                      Copy(GearS, 4,
                                                      Length(GearS));
                                                    val(TransferS,
                                                      DIYXY.Y, Code);
                                                      Codefx(TransferS,
                                                      Code);
                                                    inc(Counter);
                                                    GearS :=
                                                      DIYMemo.Lines[Counter];
                                                    TransferS :=
                                                      Copy(GearS, 4,
                                                      Length(GearS));
                                                    val(TransferS,
                                                      DIYX2Y2.X, Code);
                                                      Codefx(TransferS,
                                                      Code);
                                                    inc(Counter);
                                                    GearS :=
                                                      DIYMemo.Lines[Counter];
                                                    TransferS :=
                                                      Copy(GearS, 4,
                                                      Length(GearS));
                                                    val(TransferS,
                                                      DIYX2Y2.Y, Code);
                                                      Codefx(TransferS,
                                                      Code);
                                                    inc(Counter);
                                                    GearS :=
                                                      DIYMemo.Lines[Counter];
                                                    TransferS :=
                                                      Copy(GearS, 4,
                                                      Length(GearS));
                                                    val(TransferS,
                                                      V1Color, Code);
                                                      Codefx(TransferS,
                                                      Code);
                                                    inc(Counter);
                                                    GearS :=
                                                      DIYMemo.Lines[Counter];
                                                    TransferS :=
                                                      Copy(GearS, 4,
                                                      Length(GearS));
                                                    val(TransferS,
                                                      V2Color, Code);
                                                      Codefx(TransferS,
                                                      Code);
                                                    inc(Counter);
                                                    Pen.Color :=
                                                      V1Color;
                                                    Brush.Color :=
                                                      V1Color;
                                                    Brush.Style :=
                                                      bsFDiagonal {Solid};
                                                    MainForm.DrawingTool
                                                      := dtLine;
                                                    MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3, DIYX4Y4, pmCopy);
                                                  end else {no fill}
if ((GearS ='Annotation'))then begin
inc(Counter);
       { F_File:File of TFont;}
GearS :=DIYMemo.Lines[Counter];
AssignFile(F_File, GearS);
Reset(F_File);
{       Read(F_File,FractalFont);}
CloseFile(F_File);
inc(Counter);
GearS :=DIYMemo.Lines[Counter];
DIYTextStorage :=GearS;
inc(Counter);
        {X}
                                                      GearS :=
                                                        DIYMemo.Lines[Counter];
                                                      TransferS :=
                                                        Copy(GearS, 4,
                                                        Length(GearS));
                                                      val(TransferS,
                                                        DIYXY.X, Code);
                                                        Codefx(TransferS, Code);
                                                      inc(Counter);
        {Y}
                                                      GearS :=
                                                        DIYMemo.Lines[Counter];
                                                      TransferS :=
                                                        Copy(GearS, 4,
                                                        Length(GearS));
        val(TransferS,DIYXY.Y, Code);
        Codefx(TransferS, Code);
inc(Counter);
        {to next symbol}
TextOut(DIYXY.X, DIYXY.Y, DIYTextStorage);
end else begin
ShowMessage
('State of Confusion' +
#13#10 +
'Unknown Object found' + #13#10
+GearS + #13#10+
'while running DIY');
inc(Counter);
end;
      end;
    end;
{Mainform.DoImageDone;}
  end;
  Color256S := ReSets;
  MainForm.FalseSets;

end;



(**************************************************)
(**************************************************)
(* ColorGrid Color selection
   procedure DIYColorGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

 {TMouseButton = (mbLeft, mbRight, mbMiddle);}
If (Button = mbLeft) then
DIYForegroundColor:=DIYColorGrid.ForegroundColor;
DIYForePanel.Color:=DIYColorGrid.ForegroundColor;
{   property ForegroundColor: TColor read GetForegroundColor;}
If (Button = mbRight) then
DIYBackgroundColor:=DIYColorGrid.BackgroundColor;
DIYBackPanel.Color:=DIYColorGrid.BackgroundColor;
{    property BackgroundColor: TColor read GetBackgroundColor;}
*)
(**************************************************)

procedure TMathForm.SetAllVColors;
begin
  V1Colorbox.Color := V1Color;
  SkyColor1.Color := V1Color;
  TDColor1.Color := V1Color;
  DColor1.Color := V1Color;
  NFColor1.Color := V1Color;
{   FColorPanel.Color:=V1Color;  Fractal Color box}
  V2Colorbox.Color := V2Color;
  SkyColor2.Color := V2Color;
  TDColor2.Color := V2Color;
  DColor2.Color := V2Color;
  NFColor2.Color := V2Color;
  V3Colorbox.Color := V3Color;
  SkyColor3.Color := V3Color;
  TDColor3.Color := V3Color;
  DColor3.Color := V3Color;
  NFColor3.Color := V3Color;
  V4Colorbox.Color := V4Color;
  SkyColor4.Color := V4Color;
  TDColor4.Color := V4Color;
  DColor4.Color := V4Color;
  NFColor4.Color := V4Color;
end;

procedure TMathForm.V1ColorboxClick(Sender: TObject);
begin
  ColorDialog1.Color := MainForm.Image2.Canvas.Brush.Color;
  if ColorDialog1.Execute then begin
    MainForm.Image2.Canvas.Brush.Color := ColorDialog1.Color;
    V1Color := ColorDialog1.Color;
    SetAllVColors;
  end;
end;

procedure TMathForm.V2ColorboxClick(Sender: TObject);
begin
  ColorDialog1.Color := MainForm.Image2.Canvas.Brush.Color;
  if ColorDialog1.Execute then begin
    MainForm.Image2.Canvas.Brush.Color := ColorDialog1.Color;
    V2Color := ColorDialog1.Color;
    SetAllVColors;
  end;
end;

procedure TMathForm.V3ColorboxClick(Sender: TObject);
begin
  ColorDialog1.Color := MainForm.Image2.Canvas.Brush.Color;
  if ColorDialog1.Execute then begin
    MainForm.Image2.Canvas.Brush.Color := ColorDialog1.Color;
    V3Color := ColorDialog1.Color;
    SetAllVColors;
  end;
end;

procedure TMathForm.V4ColorboxClick(Sender: TObject);
begin
  ColorDialog1.Color := MainForm.Image2.Canvas.Brush.Color;
  if ColorDialog1.Execute then begin
    MainForm.Image2.Canvas.Brush.Color := ColorDialog1.Color;
    V4Color := ColorDialog1.Color;
    SetAllVColors;
  end;
end;

procedure TMathForm.DIYMagicLoaderClick(Sender: TObject);
var MyFilesS: string;
begin {does load allow loading more than 1 file??}
  OpenDialog1.Filename := DIYFileFLN;
  OpenDialog1.Filter :=
    'DIY Objects (*.FL?)|*.FLA;*.FLO;*.FLD;*.FLF;*.FLM;*.FLN;*.FLS;*.FLT;*.FLW';
  if OpenDialog1.Execute then begin
    MyFilesS := Uppercase(ExtractFileExt(OpenDialog1.FileName));
    if ((MyFilesS = '.FLA') or (MyFilesS = '.FLO') or (MyFilesS =
      '.FLD') or
      (MyFilesS = '.FLF') or (MyFilesS = '.FLM') or (MyFilesS =
        '.FLN') or
      (MyFilesS = '.FLS') or (MyFilesS = '.FLT') or (MyFilesS =
        '.FLW'))
      then begin
         {Actually load the data into the form}
      if (FileExists(OpenDialog1.FileName)) then begin
        DIYFileFLN := OpenDialog1.FileName;
        MyFilesS := ExtractFileName(OpenDialog1.FileName);
        DIYFileEdit.Text := MyFilesS;
      end;
    end;
  end;
end;



procedure TMathForm.DIYPaintBrushClick(Sender: TObject);
begin
{use this to activate painting
Use style to change bitmap}
  if (MainForm.ImageDraw.Checked) then begin
    MainForm.ImageDraw.Checked := False;
    DIYStyleForm.Hide;
    MainForm.Drawing := False; { clear the Drawing flag }
  end else begin
    if (ZoomingOn) then MainForm.UnZoom;
{    MainForm.DoImageStart; NEVER UNDONE}
    MainForm.ImageDraw.Checked := True;
    DIYStyleForm.Show;
    MainForm.Drawing := True; { clear the Drawing flag }
  end;
end;

procedure TMathForm.PolygonBtnClick(Sender: TObject);
begin {POLYGON and POLYLINE are PAINTING TOOLS
        not available here and now}
end;
{POLYGON and POLYLINE are PAINTING TOOLS
not available here and now}

{procedure Polyline(Points: array of TPoint);
Draws a series of lines on the canvas with the current
 pen, connecting each of the points passed to it in Points.
Description
Use Polyline to connect a set of points on the canvas.
If there are only two points, Polyline draws a single line.
Calling the MoveTo function with the value of the
first point, and then repeatedly calling LineTo
with all subsequent points will draw the same image
 on the canvas. However, unlike LineTo,
 Polyline does not change the value of PenPos.

 procedure Polygon(Points: array of TPoint);
 Draws a series of lines on the canvas connecting
 the points passed in and closing the shape by
 drawing a line from the last point to the first point.
 Description
 Use Polygon to draw a closed, many-sided shape on the
 canvas, using the value of Pen. After drawing the
 complete shape, Polygon fills the shape using the
 value of Brush.
 To draw a polygon on the canvas, without filling it,
 use the Polyline method, specifying the first point
 a second time at the end.
}

(**************************************************)

procedure TMathForm.BitmapBlotto(FileName: string);
var
  ARect: TRect;
begin
  MainForm.DrawShape(DIYXY, DIYX2Y2, DIYX3Y3, DIYX4Y4, pmCopy);
  if (not (FileName = 'NONE.BMP')) then begin
{Load the bitmap, get its size... for undistorted adjustments}
{G_Math_Image.Height;}
    ARect := Rect(DIYXY.X, DIYXY.Y, DIYX2Y2.X, DIYX2Y2.Y);
    MainForm.Image2.Canvas.StretchDraw(ARect,
      G_Math_Image.Picture.Bitmap);
{paste onto/into canvas at the DIY coordinates}
{MainForm.Image1.Canvas.CopyMode := cmSrcCopy;}{ restore the copy mode }

  end;
end;
(**************************************************)



(**************************************************)
(**************************************************)

procedure TMathForm.TurtleRun;
var Input: Integer;
begin
  TurtlePresent(TurtleLevel);
  if (TurtleRG.ItemIndex > -1) then begin {U_Math_P}
    case TurtleRG.ItemIndex of
      0..19: Input := (TurtleRG.ItemIndex + 1);
      20: Input := 25;
      21: Input := 30;
      22: Input := 60;
      23: Input := 90;
      24: Input := 180;
    else Input := 1;
    end; {of case}
  end else Input := 1;
  Plot_System(Input);
{TAxiom:String;
TPI:Extended;
TDegrees:Extended;
TXminI,TmaxI, TYminI, TYmaxI:Integer;
ProductionRules:Array [0..127] of String;}
end;

procedure TMathForm.TurtleOKClick(Sender: TObject);
begin
  MainForm.DoImageStart;
  TurtleRun;
  Mainform.DoImageDone;
end;

procedure TMathForm.TurtleUpDownClick(Sender: TObject; Button:
  TUDBtnType);
begin
  if (Button = btNext) then begin
    if ((TurtleLevel + 1) < 52) then
      SelectNextTurtleFun(Button = btNext)
  end
  else begin if ((TurtleLevel - 1) >= 1) then
      SelectNextTurtleFun(Button = btNext); end;
{ When Button is btPrev, the Down or Left arrow was clicked,
  and the value of Position is about to decrease by Increment.}
end;

procedure TMathForm.SelectNextTurtleFun(Way: Boolean);
begin
{Read Present before changing}
  TurtlePresent(TurtleLevel);
  if Way then inc(TurtleLevel) else dec(TurtleLevel);
{Write Next to display}
  TurtleNext(TurtleLevel);
end;

procedure TMathForm.TurtlePresent(TurtleLevelDo: Integer);
var code: integer; TempS: string;
begin {}
  Axiom := TAxiomEdit.Text;
  TKan := TKanEdit.Text;
  if (TPIEdit.Text <> '0') then begin
    val(TPIEdit.Text, TPI, Code); Codefx(TPIEdit.Text, Code);
    TCircled := (TPI * 2); {TPI  Convert the PI to 2 circled}
    str(TCircled, TempS); TCircledEdit.Text := TempS;
    TDegrees := (360 div TCircled); {TPI  Convert the PI to degrees}
    str(TDegrees, TempS); TDegreeEdit.Text := TempS;
  end else
    if (TCircledEdit.Text <> '0') then begin
      val(TCircledEdit.Text, TCircled, Code);
        Codefx(TCircledEdit.Text, Code);
      TPI := (TCircled div 2); {TPI  Convert the 2PI to degrees}
      val(TPIEdit.Text, TPI, Code); Codefx(TPIEdit.Text, Code);
      TDegrees := (360 div TCircled);
        {TPI  Convert the 2PI to degrees}
      str(TDegrees, TempS); TDegreeEdit.Text := TempS;
    end else
      if (TDegreeEdit.Text <> '0') then begin
        val(TDegreeEdit.Text, TDegrees, Code);
          Codefx(TDegreeEdit.Text, Code);
        TPI := (180 div TDegrees);
          {TDegrees  Convert the degrees to PI }
        str(TPI, TempS); TPIEdit.Text := TempS;
        TCircled := (TPI * 2); {TPI  Convert the PI to 2 circled}
        str(TCircled, TempS); TCircledEdit.Text := TempS;
      end else DoMessages(30204);

{  TurtleDirN:=TPI;     }
  TurtleDirN := TCircled;
  val(TXMinEdit.Text, TXminI, Code); Codefx(TXMinEdit.Text, Code);
  val(TYMinEdit.Text, TYminI, Code); Codefx(TYMinEdit.Text, Code);
  val(TZxmaxEdit.Text, TmaxI, Code); Codefx(TZxmaxEdit.Text, Code);
  val(TZymaxEdit.Text, TYmaxI, Code); Codefx(TZymaxEdit.Text, Code);
{Get Turtle(TurtleLevel);}
  ProductionRules[TurtleLevelDo] := TProjEdit.Text;
end;

procedure TMathForm.TurtleNext(TurtleLevelDo: Integer);
begin {}
{Get Turtle(TurtleLevel);}
  TProjEdit.Text := ProductionRules[TurtleLevelDo];
end;

procedure TMathForm.TClearBtnClick(Sender: TObject);
var i, code: integer;
  SizeStr: string;
begin
  TPIEdit.Text := 'Turtle.FLA';
  TProjEdit.Text := '';
  for i := 1 to 52 do begin
    ProductionRules[i] := '';
    TurtleUpDown.Position := 1;
    TurtleLevel := 1;
    Axiom := '';
    TAxiomEdit.Text := Axiom;
    TKan := '';
    TKanEdit.Text := TKan;
{TPI:Extended;}
    TPIEdit.Text := '0';
    val(TPIEdit.Text, TPI, Code); Codefx(TPIEdit.Text, Code);
{TCircledEdit:Extended;}
    TCircledEdit.Text := '0';
    val(TCircledEdit.Text, TCircled, Code); Codefx(TCircledEdit.Text,
      Code);
{TDegrees:Extended;}
    TDegreeEdit.Text := '0';
    val(TDegreeEdit.Text, TDegrees, Code); Codefx(TDegreeEdit.Text,
      Code);
    TurtleRG.ItemIndex := 0;
{TXminI}
    TXMinEdit.Text := '0';
    val(TXMinEdit.Text, TXminI, Code); Codefx(TXMinEdit.Text, Code);
{TYminI}
    TYMinEdit.Text := '0';
    val(TYMinEdit.Text, TYminI, Code); Codefx(TYMinEdit.Text, Code);
{TmaxI}
    str(FYImageX, SizeStr);
    TZxmaxEdit.Text := SizeStr;
    val(TZxmaxEdit.Text, TmaxI, Code); Codefx(TZxmaxEdit.Text, Code);
{TYmaxI:Integer;}
    str(FYImageY, SizeStr);
    TZymaxEdit.Text := SizeStr;
    val(TZymaxEdit.Text, TYmaxI, Code); Codefx(TZymaxEdit.Text,
      Code);
  end;
end;

procedure TMathForm.TurtleLoadDo(FilesS: string);
var
  fTurtleFile: TextFile;
  TurtleLeveled, i, Code: Integer;
  MyFilesS: string;
begin
  AssignFile(fTurtleFile, FilesS);
  Reset(fTurtleFile);
  if IoResult <> 0 then
  begin
    DoMessages(30102);
  end else
  begin {Readln Turtle files}
{Axiom:String;}
    Readln(fTurtleFile, Axiom);
    TAxiomEdit.Text := Axiom;
{TKan:String;}
    Readln(fTurtleFile, TKan);
    TKanEdit.Text := TKan;
{Get Turtle(TurtleLevel);}
    Readln(fTurtleFile, MyFilesS);
    val(MyFilesS, TurtleLeveled, Code); Codefx(MyFilesS, Code);
    TurtleRG.ItemIndex := TurtleLeveled;
{TurtleLevel:=TurtleLeveled; }
{TPI:Extended;}
    Readln(fTurtleFile, MyFilesS);
    TPIEdit.Text := MyFilesS;
    val(TPIEdit.Text, TPI, Code); Codefx(TPIEdit.Text, Code);
{TCircled:Extended;}
    Readln(fTurtleFile, MyFilesS);
    TCircledEdit.Text := MyFilesS;
    val(TCircledEdit.Text, TCircled, Code); Codefx(TCircledEdit.Text,
      Code);
{TDegrees:Extended;}
    Readln(fTurtleFile, MyFilesS);
    TDegreeEdit.Text := MyFilesS;
    val(TDegreeEdit.Text, TDegrees, Code); Codefx(TDegreeEdit.Text,
      Code);
{TXminI} Readln(fTurtleFile, MyFilesS);
    TXminEdit.Text := MyFilesS;
    val(TXMinEdit.Text, TXminI, Code); Codefx(TXMinEdit.Text, Code);
{TYminI} Readln(fTurtleFile, MyFilesS);
    TYMinEdit.Text := MyFilesS;
    val(TYMinEdit.Text, TYminI, Code); Codefx(TYMinEdit.Text, Code);
{TmaxI} Readln(fTurtleFile, MyFilesS);
    TZxmaxEdit.Text := MyFilesS;
    val(TZxmaxEdit.Text, TmaxI, Code); Codefx(TZxmaxEdit.Text, Code);
{TYmaxI:Integer;} Readln(fTurtleFile, MyFilesS);
    TZymaxEdit.Text := MyFilesS;
    val(TZymaxEdit.Text, TYmaxI, Code); Codefx(TZymaxEdit.Text,
      Code);
    for i := 1 to 52 do begin
{ProductionRules:Array [0..127] of String;}
      Readln(fTurtleFile, MyFilesS);
      ProductionRules[i] := MyFilesS;
    end;
    TurtleUpDown.Position := 1;
    TProjEdit.Text := ProductionRules[1];
  end;
  CloseFile(fTurtleFile);
end;

procedure TMathForm.TFileOpenClick(Sender: TObject);
var MyFilesS: string;
begin { Display Open dialog box }
  OpenDialog1.InitialDir:=FormulasDir;
  OpenDialog1.Filter := 'Turtle (*.FLA)|*.FLA';
  OpenDialog1.Filename := TurtleFileEdit.Text;
  if OpenDialog1.Execute then begin
    MyFilesS := Uppercase(ExtractFileExt(OpenDialog1.FileName));
    FormulasDir:=ExtractFilePath(OpenDialog1.FileName);    
    if MyFilesS = '.FLA' then begin
      MyFilesS := ExtractFileName(OpenDialog1.FileName);
      TurtleFileEdit.Text := MyFilesS;
      TurtleLoadDo(OpenDialog1.FileName);
    end;
  end;
end;

procedure TMathForm.TFileSaveClick(Sender: TObject);
var MyFilesS: string;
  fTurtleFile: TextFile;
  TurtleLeveled, Code: Integer;
begin { Display Open dialog box }
  SaveDialog1.InitialDir:=FormulasDir;
  SaveDialog1.Filter := 'Turtle (*.FLA)|*.FLA';
  SaveDialog1.Filename := TurtleFileEdit.Text;
  if SaveDialog1.Execute then begin
    MyFilesS := Uppercase(ExtractFileExt(SaveDialog1.FileName));
    FormulasDir:=ExtractFilePath(SaveDialog1.FileName);    
    if MyFilesS = '.FLA' then begin
      MyFilesS := ExtractFileName(SaveDialog1.FileName);
      TurtleFileEdit.Text := MyFilesS;
      AssignFile(fTurtleFile, SaveDialog1.FileName);
      Rewrite(fTurtleFile);
      if IoResult <> 0 then
      begin
        DoMessages(30103);
      end else
      begin {Write Turtle files}
{Axiom:String;}
        Axiom := TAxiomEdit.Text;
        Writeln(fTurtleFile, Axiom);
{TKan:String;}
        TKan := TKanEdit.Text;
        Writeln(fTurtleFile, TKan);
{Get Turtle(TurtleLevel);}
        TurtleLeveled := TurtleRG.ItemIndex;
        str(TurtleLeveled, MyFilesS);
        Writeln(fTurtleFile, MyFilesS);
{TurtleLevel:=TurtleLeveled;}
{TPI:Extended;} Writeln(fTurtleFile, TPIEdit.Text);
        val(TPIEdit.Text, TPI, Code);
        Codefx(TPIEdit.Text, Code);
{TCircled:Extended;}
        Writeln(fTurtleFile, TCircledEdit.Text);
        val(TCircledEdit.Text, TCircled, Code);
          Codefx(TCircledEdit.Text, Code);
{TDegrees:Extended;} Writeln(fTurtleFile, TDegreeEdit.Text);
        val(TDegreeEdit.Text, TDegrees, Code);
          Codefx(TDegreeEdit.Text, Code);
{TXminI} Writeln(fTurtleFile, TXMinEdit.Text);
        val(TXMinEdit.Text, TXminI, Code); Codefx(TXMinEdit.Text,
          Code);
{TYminI} Writeln(fTurtleFile, TYMinEdit.Text);
        val(TYMinEdit.Text, TYminI, Code); Codefx(TYMinEdit.Text,
          Code);
{TmaxI} Writeln(fTurtleFile, TZxmaxEdit.Text);
        val(TZxmaxEdit.Text, TmaxI, Code); Codefx(TZxmaxEdit.Text,
          Code);
{TYmaxI:Integer;} Writeln(fTurtleFile, TZymaxEdit.Text);
        val(TZymaxEdit.Text, TYmaxI, Code); Codefx(TZymaxEdit.Text,
          Code);
{Get Turtle(TurtleLevel);}
        ProductionRules[TurtleLevel] := TProjEdit.Text;
        for Code := 1 to 52 do begin
{ProductionRules:Array [0..127] of String;}
          Writeln(fTurtleFile, ProductionRules[Code]);
        end;
      end;
      CloseFile(fTurtleFile);
    end;
  end;
end;
(**************************************************)
(**************************************************)


(**************************************************)
(**************************************************)

(**************************************************)
(**************************************************)


(**************************************************)
(**************************************************)

procedure TMathForm.CollageClearClick(Sender: TObject);
begin
{}
end;
(**************************************************)

procedure TMathForm.CollageOpenClick(Sender: TObject);
begin
{  OpenDialog1.InitialDir:=FormulasDir;
  OpenDialog1.Filter := 'Numb (*.FLN)|*.FLN';
  OpenDialog1.Filename := NumbFileEdit.text;
  if OpenDialog1.Execute then begin}
end;
(**************************************************)

procedure TMathForm.CollageSaveClick(Sender: TObject);
begin
{    FormulasDir:=ExtractFilePath(SaveDialog1.FileName);}
end;
(**************************************************)


procedure TMathForm.CollageRunClick(Sender: TObject);
begin
{MainForm.DoImageStart;
Mainform.DoImageDone;
}
end;
(**************************************************)
(**************************************************)



(**************************************************)




end.
