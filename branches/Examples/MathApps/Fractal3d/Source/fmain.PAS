// ------------------------------------------------
// Fractal3D
// based on Ivan Lee Herring  source code
// -------------------------------------------------

unit fmain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.MMSystem,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Imaging.jpeg,
  Vcl.Clipbrd,
  Vcl.Printers,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ComCtrls,
  Vcl.ExtDlgs,

  Fractal;

type
  TDrawingTool =
    (dtEllipse, dtCSEllipse, dtPolygon,
    dtVTriangle, dtTriangle, dtTSTriangle,
    dtVPRectangle, dtVERectangle, dtVSRectangle,
    dtVDRectangle, dtVTRectangle, dtVFRectangle, dtVNRectangle,
    dtVMRectangle, dtVARectangle, dtVWRectangle, dtVCRectangle,
    dtVBRectangle, dtVRectangle, dtRectangle, dtRoundRect,
    dtLine, dtPBrush);
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileOpenItem: TMenuItem;
    FileSaveItem: TMenuItem;
    FileSaveAsItem: TMenuItem;
    FilePrintItem: TMenuItem;
    FilePrintSetupItem: TMenuItem;
    FileExitItem: TMenuItem;
    EditCutItem: TMenuItem;
    EditCopyItem: TMenuItem;
    EditPasteItem: TMenuItem;
    HelpContentsItem: TMenuItem;
    HelpSearchItem: TMenuItem;
    HelpHowToUseItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    PrintDialog: TPrintDialog;
    PrintSetupDialog: TPrinterSetupDialog;
    SpeedBar: TPanel;
    Options: TMenuItem;
    Fractals1: TMenuItem;
    Math1: TMenuItem;
    ScrollBox1: TScrollBox;
    Image2: TImage;
    ImageSize1: TMenuItem;
    BackgroundColor1: TMenuItem;
    FV_IS640x480: TMenuItem;
    FV_IS800x600: TMenuItem;
    FV_IS1024x768: TMenuItem;
    FV_IS2048x1536: TMenuItem;
    FV_IS1280x960: TMenuItem;
    FV_IS1600x1200: TMenuItem;
    FV_IS1920x1440: TMenuItem;
    FV_IS2400x1800: TMenuItem;
    FV_IS3072x2304: TMenuItem;
    Red1: TMenuItem;
    Green1: TMenuItem;
    Blue1: TMenuItem;
    Black1: TMenuItem;
    White1: TMenuItem;
    Yellow1: TMenuItem;
    HintPanel: TPanel;
    OpenPictureDialog1: TOpenPictureDialog;
    SavePictureDialog1: TSavePictureDialog;
    N6: TMenuItem;
    ImageDraw: TMenuItem;
    ImageAnnotate: TMenuItem;
    ImageSelectFont: TMenuItem;
    FontDialog1: TFontDialog;
    Aqua1: TMenuItem;
    Fuchsia1: TMenuItem;
    Gray1: TMenuItem;
    LimeGreen1: TMenuItem;
    Purple1: TMenuItem;
    Teal1: TMenuItem;
    Silver1: TMenuItem;
    OliveGreen1: TMenuItem;
    NavyBlue1: TMenuItem;
    Maroon1: TMenuItem;
    ImageDoneAlarm: TMenuItem;
    PrintBig1: TMenuItem;
    ForegroundColorSet1: TMenuItem;
    LandformsSet: TMenuItem;
    Win16Set: TMenuItem;
    N5: TMenuItem;
    RedRanger256: TMenuItem;
    GreenGoblin256: TMenuItem;
    BlueMeanie256: TMenuItem;
    PurpleReigns256: TMenuItem;
    YellowSnow256: TMenuItem;
    AquaMarine256: TMenuItem;
    SmogFog256: TMenuItem;
    N7: TMenuItem;
    ColorPicker256: TMenuItem;
    FractalBlack1: TMenuItem;
    PopupMenu1: TPopupMenu;
    UnZoom1to1: TMenuItem;
    PanThis: TMenuItem;
    HiddenFX: TPanel;
    MousePanel: TPanel;
    ProgressBar1: TProgressBar;
    ColorPicker16: TMenuItem;
    RGB16: TMenuItem;
    Mine1: TMenuItem;
    Bomb1: TMenuItem;
    Loaded16: TMenuItem;
    Loaded256: TMenuItem;
    LoadedToo56: TMenuItem;
    ImageDoneVoices: TMenuItem;
    EditCropItem: TMenuItem;
    EditClearAllItem: TMenuItem;
    ZoomToWindow: TMenuItem;
    ZoomMouse: TMenuItem;
    FV_IS2560x1920: TMenuItem;
    FV_IS3200x2400: TMenuItem;
    FV_IS4096x3072: TMenuItem;
    FV_IS4000x3000: TMenuItem;
    FV_IS3840x2880: TMenuItem;
    EditPasteIntoItem: TMenuItem;
    EditCopyAreaItem: TMenuItem;
    ZoomOut1: TMenuItem;
    N12: TMenuItem;
    XYZ3D: TMenuItem;
    ResizeImage: TMenuItem;
    Panel1: TPanel;
    OpenButton: TSpeedButton;
    SaveButton: TSpeedButton;
    SaveAsButton: TSpeedButton;
    PrintButton: TSpeedButton;
    PrintSetupButton: TSpeedButton;
    Croparea: TSpeedButton;
    CutButton: TSpeedButton;
    CopyButton: TSpeedButton;
    PasteButton: TSpeedButton;
    PasteArea: TSpeedButton;
    ImageDrawBtn: TSpeedButton;
    IAnnotateBtn: TSpeedButton;
    SelectFontBtn: TSpeedButton;
    HelpContentsBtn: TSpeedButton;
    TopicSearchBtn: TSpeedButton;
    AboutButton: TSpeedButton;
    ExitButton: TSpeedButton;
    Preview1: TMenuItem;
    MineColor: TPanel;
    BombColor: TPanel;
    ColorDialog1: TColorDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure SetImageSize;
    procedure ShowHint(Sender: TObject);
    procedure FileOpen(Sender: TObject);
    procedure FileSave(Sender: TObject);
    procedure FileSaveAs(Sender: TObject);
    procedure PrintBig1Click(Sender: TObject);
    procedure FilePrint(Sender: TObject);
    procedure FilePrintSetup(Sender: TObject);
    procedure FileExit(Sender: TObject);

    procedure HelpContents(Sender: TObject);
    procedure HelpSearch(Sender: TObject);
    procedure HelpHowToUse(Sender: TObject);
    procedure HelpAbout(Sender: TObject);
    procedure Image2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageDrawClick(Sender: TObject);
    procedure ImageAnnotateClick(Sender: TObject);
    procedure ImageSelectFontClick(Sender: TObject);
    procedure Fractals1Click(Sender: TObject);
    procedure Math1Click(Sender: TObject);
    procedure FalseSize;
    procedure FV_IS640x480Click(Sender: TObject);
    procedure FV_IS800x600Click(Sender: TObject);
    procedure FV_IS1024x768Click(Sender: TObject);
    procedure FV_IS1280x960Click(Sender: TObject);
    procedure FV_IS1600x1200Click(Sender: TObject);
    procedure FV_IS1920x1440Click(Sender: TObject);
    procedure FV_IS2048x1536Click(Sender: TObject);
    procedure FV_IS2400x1800Click(Sender: TObject);
    procedure FV_IS3072x2304Click(Sender: TObject);
    procedure FV_IS4096x3072Click(Sender: TObject);
    procedure FV_IS4000x3000Click(Sender: TObject);
    procedure FV_IS3840x2880Click(Sender: TObject);
    procedure FV_IS3200x2400Click(Sender: TObject);
    procedure FV_IS2560x1920Click(Sender: TObject);

    procedure FalseColors;
    procedure Black1Click(Sender: TObject);
    procedure Maroon1Click(Sender: TObject);
    procedure Green1Click(Sender: TObject);
    procedure OliveGreen1Click(Sender: TObject);
    procedure NavyBlue1Click(Sender: TObject);
    procedure Purple1Click(Sender: TObject);
    procedure Teal1Click(Sender: TObject);
    procedure Gray1Click(Sender: TObject);
    procedure Silver1Click(Sender: TObject);
    procedure Red1Click(Sender: TObject);
    procedure LimeGreen1Click(Sender: TObject);
    procedure Yellow1Click(Sender: TObject);
    procedure Blue1Click(Sender: TObject);
    procedure Fuchsia1Click(Sender: TObject);
    procedure Aqua1Click(Sender: TObject);
    procedure White1Click(Sender: TObject);
    procedure Mine1Click(Sender: TObject);
    procedure Bomb1Click(Sender: TObject);

    procedure FractalBlack1Click(Sender: TObject);

    procedure False16;
    procedure Win16SetClick(Sender: TObject);
    procedure LandformsSetClick(Sender: TObject);
    procedure ColorPicker16Click(Sender: TObject);
    procedure RGB16Click(Sender: TObject);
    procedure Loaded16Click(Sender: TObject);

    procedure FalseSets;
    procedure BlueMeanie256Click(Sender: TObject);
    procedure AquaMarine256Click(Sender: TObject);
    procedure GreenGoblin256Click(Sender: TObject);
    procedure YellowSnow256Click(Sender: TObject);
    procedure PurpleReigns256Click(Sender: TObject);
    procedure RedRanger256Click(Sender: TObject);
    procedure SmogFog256Click(Sender: TObject);
    procedure ColorPicker256Click(Sender: TObject);
    procedure Loaded256Click(Sender: TObject);
    procedure LoadedToo56Click(Sender: TObject);

    procedure ImageDoneVoicesClick(Sender: TObject);
    procedure AlarmBtnClick(Sender: TObject);

    procedure EditCut(Sender: TObject);
    procedure EditCopy(Sender: TObject);
    procedure EditPaste(Sender: TObject);
    procedure EditPasteIntoItemClick(Sender: TObject);
    procedure EditCropItemClick(Sender: TObject);
    procedure EditCopyAreaItemClick(Sender: TObject);
    procedure EditClearAllItemClick(Sender: TObject);
    procedure UnZoom;
    procedure UnZoom1to1Click(Sender: TObject);
    procedure ZoomOut1Click(Sender: TObject);
    procedure ZoomMachine;
    procedure ZoomToWindowClick(Sender: TObject);
    procedure ZoomMouseClick(Sender: TObject);
    procedure ZoomMouseDo;
    procedure Image2Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
      const Msg: string);
    procedure XYZ3DClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ResizeImageClick(Sender: TObject);
    procedure Preview1Click(Sender: TObject);
    procedure BombColorClick(Sender: TObject);
    procedure MineColorClick(Sender: TObject);
    procedure DoMiner;
    procedure DoBomber;
  public
    Drawing: Boolean; { field to track whether button was pressed }
    Origin, MovePt: TPoint; { fields to store points }
    DrawingTool: TDrawingTool;

    procedure DrawShape
    { (TopLeft, BottomRight: TPoint; AMode: TPenMode); }
      (TopLeft, BottomRight, Tri, Quad: TPoint; AMode: TPenMode);

    procedure ChangeFileName;
    procedure WriteDataFile;
    procedure ReadDataFile;
    procedure FileOpener;
    procedure FileSaver;
    procedure Set16Colors;
    procedure SiliconSets;
    procedure OverBit;

    procedure ImageOverDone;
    procedure PlaySound(WavFileName: string);

    procedure EditCropper;
    procedure DoImageDone;
    procedure DoImageStart;
  end;

var
  MainForm: TMainForm;
  BitmapF: TBitmap; { temporary variable to hold the bitmap }

implementation

uses
  FUGlobal,
  FAbout,
  {FmxUtils,}
  FMath,
  FGMath,
  FAnno,
  FGStyle,
  FMJPEG,
  FMResize,
  {FFractalPreviewForm,}
  FXYZ3D,
  Fjul,
  FGlfrm {FGlForm, FMathGL,};

{$R *.DFM}



{---------------------------------------------------------------------}
procedure TMainForm.FileExit(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  isReallyGone := True;
  dtmGlForm.Close;
  { XYZGL.Close; }
  { MathGL.Close; }
  MainFormX := MainForm.left;
  MainFormY := MainForm.top;
  DoSaver;
  { Nil out the array }
  SetLength(ManMatrix, 0, 0);
  { kill bitmap(s) ? }
  BitmapF.Free;
  { if (bBrushHasBitmap = True) then
    begin
    MainForm.Image2.Canvas.Brush.Bitmap := nil;
    DIYSBitmap.Free;
    end; }
end;

(* *********************************************************** *)
procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  { LOAD FRACTALS.F3D and SET these }
  if FileExists(ExtractFilePath(ParamStr(0)) + 'fractal.pof') then
    DoLoader
  else
  begin
    // FractalFont := FontDialog1.Font;
    StartedNameNumber := 'alle alle in free'; { F3197432-460875 }
    Started := Date;
    Colorreg := 3;
    SystemInfoFormX := 113;
    SystemInfoFormY := 113;
    MessageX := 113;
    MessageY := 113;
    DIYStyleFormX := 560;
    DIYStyleFormY := 460;
    AnnotationFormX := 440;
    AnnotationFormY := 440;
    XYZ3DFormX := 488;
    XYZ3DFormY := 0;

    JPEGFormX := 44;
    JPEGFormY := 44;
    ResizeFormX := 44;
    ResizeFormY := 44;
    XYZGLX := 44;
    XYZGLY := 44;
    MathGLX := 44;
    MathGLY := 44;
    // some things do not exist until after created...
    FractalDir := ExtractFilePath(ParamStr(0));
    DemsDir := FractalDir;
    FormulasDir := FractalDir;
    IamDone := 0;
    V1Color := clRed;
    V2Color := clLime;
    V3Color := clBlue;
    V4Color := clYellow;
    BombBackgroundColor := clAqua;
    MineBackgroundColor := clPurple;
    Color16Name := 'COLOR16FILE.1CP';
    Color256Name := 'COLORFILE.2CP';
    Color256TooName := 'COLORFILE.2CP';
    for i := 0 to 15 do
      ColorArray[i] := RGB(0, 0, 255 - (i * 12));
    for i := 0 to 15 do
      ColorArray[i + 16] := RGB(0, 255 - (i * 12), 255 - (i * 12));
    for i := 0 to 15 do
      ColorArray[i + 32] := RGB(0, 255 - (i * 12), 0);
    for i := 0 to 15 do
      ColorArray[i + 48] := RGB(255 - (i * 12), 255 - (i * 12), 0);
    for i := 0 to 15 do
      ColorArray[i + 64] := RGB(255 - (i * 12), 0, 255 - (i * 12));
    for i := 0 to 15 do
      ColorArray[i + 80] := RGB(255 - (i * 12), 0, 0);
    for i := 0 to 15 do
      ColorArray[i + 96] := RGB(255 - (i * 12), 255 - (i * 12), 255 - (i * 12));
    for i := 0 to 15 do
      ColorArray[i + 112] := RGB(0, 0, 255 - (i * 14));
    for i := 0 to 15 do
      ColorArray[i + 128] := RGB(0, 255 - (i * 14), 255 - (i * 14));
    for i := 0 to 15 do
      ColorArray[i + 144] := RGB(0, 255 - (i * 14), 0);
    for i := 0 to 15 do
      ColorArray[i + 160] := RGB(255 - (i * 14), 255 - (i * 14), 0);
    for i := 0 to 15 do
      ColorArray[i + 176] := RGB(255 - (i * 14), 0, 255 - (i * 14));
    for i := 0 to 15 do
      ColorArray[i + 192] := RGB(255 - (i * 14), 0, 0);
    for i := 0 to 15 do
      ColorArray[i + 208] := RGB(0, 255 - (i * 14), 0);
    for i := 0 to 15 do
      ColorArray[i + 224] := RGB(0, 255 - (i * 14), 255 - (i * 14));
    for i := 0 to 15 do
      ColorArray[i + 240] := RGB(255 - (i * 14), 0, 0);
    Color256S := 2;
    MainForm.AquaMarine256.Checked := True;
    { MainForm.SiliconSets; }
    { 2: } for i := 0 to 255 do
    begin
      Colors[0, i] := 0;
      Colors[1, i] := 255 - i;
      Colors[2, i] := 255 - i;
    end;
    FBackGroundColor := clBlue;
    MainForm.Blue1.Checked := True;
    { MainForm.FalseColors; }

    RGBArray[0, 0] := 0;
    RGBArray[1, 0] := 0;
    RGBArray[2, 0] := 128;
    RGBArray[0, 1] := 0;
    RGBArray[1, 1] := 0;
    RGBArray[2, 1] := 255;
    RGBArray[0, 2] := 0;
    RGBArray[1, 2] := 128;
    RGBArray[2, 2] := 128;
    RGBArray[0, 3] := 0;
    RGBArray[1, 3] := 255;
    RGBArray[2, 3] := 255;
    RGBArray[0, 4] := 255;
    RGBArray[1, 4] := 255;
    RGBArray[2, 4] := 0;
    RGBArray[0, 5] := 128;
    RGBArray[1, 5] := 128;
    RGBArray[2, 5] := 0;
    RGBArray[0, 6] := 0;
    RGBArray[1, 6] := 255;
    RGBArray[2, 6] := 0;
    RGBArray[0, 7] := 0;
    RGBArray[1, 7] := 128;
    RGBArray[2, 7] := 0;
    RGBArray[0, 8] := 128;
    RGBArray[1, 8] := 128;
    RGBArray[2, 8] := 128;
    RGBArray[0, 9] := 255;
    RGBArray[1, 9] := 0;
    RGBArray[2, 9] := 255;
    RGBArray[0, 10] := 128;
    RGBArray[1, 10] := 0;
    RGBArray[2, 10] := 128;
    RGBArray[0, 11] := 192;
    RGBArray[1, 11] := 192;
    RGBArray[2, 11] := 192;
    RGBArray[0, 12] := 255;
    RGBArray[1, 12] := 0;
    RGBArray[2, 12] := 0;
    RGBArray[0, 13] := 128;
    RGBArray[1, 13] := 0;
    RGBArray[2, 13] := 0;
    RGBArray[0, 14] := 255;
    RGBArray[1, 14] := 255;
    RGBArray[2, 14] := 255;
    RGBArray[0, 15] := 0;
    RGBArray[1, 15] := 0;
    RGBArray[2, 15] := 0;
    Colorset16 := 3; { MainForm.False16; }
    MainForm.LandformsSet.Checked := True;
    DoSaver;
  end;
  { SET UP VARIABLES ALWAYS }

{
 Left := MainFormX;
 Top := MainFormY;
 Height := 565;
 Width := 679;
}
   FYImageX := 640;
  FYImageY := 480;
  BombColor.Color := BombBackgroundColor;
  MineColor.Color := MineBackgroundColor;
  { FontDialog1.Font:=FractalFont; }
  ImageOverDone; { sets  IamDone Voice or Noise }
  { MainForm.ImageOverDone; }{ sets iamdone ImageDoneAlarmb }
  SetImageSize;
  FalseColors;
  False16;
  FalseSets;
  Application.OnHint := ShowHint;
  Application.HelpFile := ExtractFilePath(ParamStr(0)) + 'FRACTAL3D.HLP';
  OpenPictureDialog1.InitialDir := FractalDir;
  SavePictureDialog1.InitialDir := FractalDir;
  { ForceCurrentDirectory := True; }
  FractalFilename := 'F_000000000.BMP';
  OpenPictureDialog1.FileName := FractalFilename;
  OpenPictureDialog1.Title := 'Fractal File Selection';
  LastKnownFunction := 0;
  bRotatingImage := False;
  bRotateImage := False;
  Julia := False;
  bJuliaBase := False;
  bMan3d := False;
  bJulia3d := False;
  bFractalMaxed := False;
  DoSanMarcosDragon := False;
  Drawing := False;
  Fractaling := False;
  bPointing := False;
  bBrushHasBitmap := False;
  bVista := False;
  iMadeMountains := -1;
  bSaveTheMountains := False;
  bMousing := False;
  bLightning := False;
  bVistaLive := False;
  DIYAnnotate := False;
  ITriangling := 0;
  ImageZoomRatio := 0;
  ImageZoomRatioEx := 0;
  ZoomingOn := False;
  bZoomMousing := False;
  WasZooming := False;
  isReallyGone := False;
  DIYL.X := 0;
  DIYL.Y := 0;
  DIYZ := 0;
  DIYE := 0;
  DrawingTool_PBrush_Width := 10;
  FXMax := 1.2;
  FXMin := -2.0;
  FYMax := 1.2;
  FYMin := -1.2;
  FHQ := -0.003483;
  FVP := 0.268545;
  max_iterations := 64;
  Start_Col := 0;
  Color_Option := 0;
  BitmapF := TBitmap.Create; { construct the bitmap object }
  BitmapF.PixelFormat := MyPixelFormat; { pf24bit; }
  BitmapF.Width := FYImageX; { assign the initial width... }
  BitmapF.Height := FYImageY; { ...and the initial height }
  Image2.Picture.Graphic.assign(BitmapF);
  { VImage.Picture.Graphic.assign(BitmapF); }
  { REST ALL ZOOOOMS }
  { assign the bitmap to the image control }
  SetLength(ManMatrix, FYImageX, FYImageY);

end;

procedure TMainForm.ShowHint(Sender: TObject);
begin
  HintPanel.Caption := Application.Hint;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin { }
  Height := 565;
  Width := 679;
end;

(* *********************************************************** *)
procedure TMainForm.SetImageSize;
begin
  MainForm.FalseSize;
  case FYImageX of
    640:
      MainForm.FV_IS640x480.Checked := True;
    800:
      MainForm.FV_IS800x600.Checked := True;
    1024:
      MainForm.FV_IS1024x768.Checked := True;
    1280:
      MainForm.FV_IS1280x960.Checked := True;
    1600:
      MainForm.FV_IS1600x1200.Checked := True;
    1920:
      MainForm.FV_IS1920x1440.Checked := True;
    2048:
      MainForm.FV_IS2048x1536.Checked := True;
    2400:
      MainForm.FV_IS2400x1800.Checked := True;
    2560:
      MainForm.FV_IS2560x1920.Checked := True;
    3072:
      MainForm.FV_IS3072x2304.Checked := True;
    3200:
      MainForm.FV_IS3200x2400.Checked := True;
    3840:
      MainForm.FV_IS3840x2880.Checked := True;
    4000:
      MainForm.FV_IS4000x3000.Checked := True;
    4096:
      MainForm.FV_IS4096x3072.Checked := True;
  else
    DoMessages(24);
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin { }
  { Check Clipboard:
    if available then enable clipboard items }
  if Clipboard.HasFormat(CF_BITMAP) then
  begin
    PasteButton.Enabled := True;
    PasteArea.Enabled := True;
    EditPasteItem.Enabled := True;
    EditPasteIntoItem.Enabled := True;
  end
  else
  begin
    PasteButton.Enabled := False;
    PasteArea.Enabled := False;
    EditPasteItem.Enabled := False;
    EditPasteIntoItem.Enabled := False;
  end;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  { Check Clipboard:
    if available then enable clipboard items }
  if Clipboard.HasFormat(CF_BITMAP) then
  begin
    PasteButton.Enabled := True;
    PasteArea.Enabled := True;
    EditPasteItem.Enabled := True;
    EditPasteIntoItem.Enabled := True;
  end
  else
  begin
    PasteButton.Enabled := False;
    PasteArea.Enabled := False;
    EditPasteItem.Enabled := False;
    EditPasteIntoItem.Enabled := False;
  end;
end;

(* *********************************************************** *)
{ MovePt must be set to the endpoint of each intermediate line,
  so you can use MovePt and Origin to erase that
  line the next time a line is drawn: }
(* *********************************************************** *)
procedure TMainForm.Image2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Cropping > 0) then
  begin
    DrawingTool := dtRectangle;
    Drawing := True;
    Image2.Canvas.MoveTo(X, Y);
    Origin := Point(X, Y);
    MovePt := Point(X, Y); { keep track of where this move was }
    HintPanel.Caption := Format('Origin: (%d, %d)', [X, Y]);
  end
  else if (bZoomMousing) then
  begin
    DrawingTool := dtRectangle;
    Drawing := True;
    Image2.Canvas.MoveTo(X, Y);
    Origin := Point(X, Y);
    MovePt := Point(X, Y); { keep track of where this move was }
    HintPanel.Caption := Format('Origin: (%d, %d)', [X, Y]);
  end
  else if (ImageDraw.Checked = True) then
  begin
    Drawing := True;
    Image2.Canvas.MoveTo(X, Y);
    Origin := Point(X, Y);
    MovePt := Point(X, Y); { keep track of where this move was }
    HintPanel.Caption := Format('Origin: (%d, %d)', [X, Y]);
  end
  else if (Fractaling = True) then
  begin
    { Choose Box type, Click center and expand at correct ratio, or normal }
    Drawing := True;
    DrawingTool := dtRectangle;
    Image2.Canvas.MoveTo(X, Y);
    Origin := Point(X, Y);
    MovePt := Point(X, Y); { keep track of where this move was }
    HintPanel.Caption := Format('Origin: (%d, %d)', [X, Y]);
  end
  else

    (* If ((bVistaLive= True) and (Drawing= True)) then begin
      Image1.Canvas.MoveTo(X, Y);
      {  Image1.Canvas.MoveTo(Origin.X, Origin.Y);
      Origin := Point(X, Y);} {already going this Triangle}
      MovePt := Point(X, Y);	{ keep track of where this move was }
      HintPanel.Caption:= Format('Origin: (%d, %d)', [X, Y]);
      end; *)
    if ((bVistaLive = True) and (Drawing <> True)) then
    begin
      Drawing := True;
      Image2.Canvas.MoveTo(X, Y);
      Origin := Point(X, Y);
      MovePt := Point(X, Y);
      { keep track of where this move was }
      DIYXY.X := X;
      DIYXY.Y := Y;
      HintPanel.Caption := Format('Origin: (%d, %d)', [X, Y]);
    end;

  { If (Drawing  = True) then Image1.Canvas.Pen.Color:=  clWhite; }
end;

{ Now you get a “rubber band” effect when you draw the line.
  By changing the pen’s mode to pmNotXor, you have it
  combine your line with the background pixels. When you
  go to erase the line, you’re actually setting the pixels back
  to the way they were. By changing the pen mode back to pmCopy
  (its default value) after drawing the lines, you ensure that
  the pen is ready to do its final drawing when you
  release the mouse button. }
(* *********************************************************** *)
procedure TMainForm.Image2MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Xs, Ys: string;
  TXMin, TYMax: Extended;
begin
  if Drawing then
  begin
    if ((DrawingTool = dtVRectangle) or (DrawingTool = dtVTriangle) or
      (DrawingTool = dtTSTriangle) or (DrawingTool = dtTriangle)) then
    begin
      if (ITriangling = 2) then
      begin
        DrawShape(Origin, MovePt, DIYX3Y3, DIYX4Y4, pmNotXor);
        MovePt := Point(X, Y);
        DIYX4Y4.X := X;
        DIYX4Y4.Y := Y;
        DrawShape(Origin, MovePt, DIYX3Y3, DIYX4Y4, pmNotXor);
      end
      else if (ITriangling = 1) then
      begin
        DrawShape(Origin, MovePt, DIYX3Y3, DIYX4Y4, pmNotXor);
        MovePt := Point(X, Y);
        DIYX3Y3.X := X;
        DIYX3Y3.Y := Y;
        DIYX4Y4.X := X;
        DIYX4Y4.Y := Y;
        DrawShape(Origin, MovePt, DIYX3Y3, DIYX4Y4, pmNotXor);
      end
      else if (ITriangling = 0) then
      begin
        DrawShape(Origin, MovePt, DIYX3Y3, DIYX4Y4, pmNotXor);
        MovePt := Point(X, Y);
        DrawShape(Origin, MovePt, DIYX3Y3, DIYX4Y4, pmNotXor);
      end;
    end
    else
    begin
      { Fractal: Choose Box type, Click center and expand at correct ratio, or normal }
      DrawShape(Origin, MovePt, DIYX3Y3, DIYX4Y4, pmNotXor);
      MovePt := Point(X, Y);
      DrawShape(Origin, MovePt, DIYX3Y3, DIYX4Y4, pmNotXor);
    end;
  end;
  { Procedure to display X,Y according to
    graphic coordinate system }
  { Coords(X,Y);
    Case of Active Translation
    FX,FY
    Origin in center of screen
    Origin at bottom left
    Scale - Zoom Resolution
    ... Output to:
    Mouse panel: X,Y
    Hint Panel: Origin.. first moused point
    FX panel: Translated X,Y }

  str(X, Xs);
  str(Y, Ys);
  MousePanel.Caption := '   X: ' + Xs + ', Y: ' + Ys;
  { MousePanel.Caption := Format('  Current: (%d, %d)', [X, Y]); }
  if (Fractaling = True) then
  begin
    { Choose Box type, Click center and expand at correct ratio, or normal }
    bMousing := False;
    TXMin := FXMin + (FXMax - FXMin) / (Image2.Width - 1) * X;
    TYMax := FYMax - (FYMax - FYMin) / (Image2.Height - 1) * Y;
    str(TXMin: 24: 20, Xs);
    str(TYMax: 24: 20, Ys);
    HiddenFX.Caption := 'FX: ' + Xs + ', FY: ' + Ys;
  end
  else if (bPointing = True) then
  begin
    bMousing := False;
    { FVP:= FXMin + (FXMax - FXMin)/(Image2.Width-1) * X ;
      FHQ:= FYMax - (FYMax - FYMin)/(Image2.Height-1) * Y ; }
    TXMin := FXMin + (FXMax - FXMin) / (Image2.Width - 1) * X;
    TYMax := FYMax - (FYMax - FYMin) / (Image2.Height - 1) * Y;
    str(TXMin: 24: 20, Xs);
    str(TYMax: 24: 20, Ys);
    HiddenFX.Caption := 'P: ' + Xs + ', Q: ' + Ys;
  end
  else if (bMousing = True) then
  begin
    str((X - (Image2.Width div 2)), Xs);
    str(((Image2.Height div 2) - Y), Ys);
    HiddenFX.Caption := 'X: ' + Xs + ', Y: ' + Ys;
  end
  else if (bLightning = True) then
  begin
    str((X - (Image2.Width div 2)), Xs);
    str(((Image2.Height div 2) - Y), Ys);
    HiddenFX.Caption := 'Lightning X: ' + Xs + ', Y: ' + Ys;
  end;
end;

(* *********************************************************** *)
procedure TMainForm.Image2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  FiTemp: Integer;
  a, b, c, TXMin, TYMax, TXMax, TYMin: Extended;
  Arect: TRect;
  AnnoS: string;
  { F_File: file of TFont; }
begin
  if ImageDraw.Checked then
  begin
    DrawShape(Origin, Point(X, Y), DIYX3Y3, DIYX4Y4, pmCopy);
    { draw the final shape }
    Drawing := False;
    { clear the Drawing flag for this line, rectangle, etc..
      Mouse down will start another line-rectangle
      will keep drawing till Draw is unchecked
      or Style form is closed }
  end;
  if Fractaling then
  begin
    { Choose Box type, Click center and expand at correct ratio, or normal }
    if (Origin.X > X) then
    begin
      FiTemp := Origin.X;
      Origin.X := X;
      X := FiTemp;
    end;
    if (Origin.Y > Y) then
    begin
      FiTemp := Origin.Y;
      Origin.Y := Y;
      Y := FiTemp;
    end;
    { based on 0,0, upper left, then lower right
      640,480 Numbers provide Ratio not image size just use 4,3 }
    a := ((480 * (X - Origin.X)) / 640);
    b := (((Y - Origin.Y) - a) / 2);
    if (a > (Y - Origin.Y)) then
    begin
      a := Origin.Y - b;
      c := Y + b;
    end
    else
    begin
      a := Origin.Y + b;
      c := Y - b;
    end;
    Origin.Y := round(a);
    Y := round(c);
    with Image2.Canvas do
    begin
      CopyMode := cmDstInvert;
      Arect := Rect(Origin.X, Origin.Y, X, Y);
      CopyRect(Arect, Image2.Canvas, Arect);
      CopyMode := cmSrcCopy; { restore the copy mode }
      { Message to Save?? as Temp File.BMP? }
      Image2.Picture.SaveToFile('TempFile.BMP');
    end;
    TXMin := FXMin + (FXMax - FXMin) / (Image2.Width - 1) * Origin.X;
    { Origins }
    TYMax := FYMax - (FYMax - FYMin) / (Image2.Height - 1) * Origin.Y;
    TXMax := FXMin + (FXMax - FXMin) / (Image2.Width - 1) * X; { 639 }
    TYMin := FYMax - (FYMax - FYMin) / (Image2.Height - 1) * Y; { 479 }
    FXMin := TXMin;
    FYMin := TYMin;
    FXMax := TXMax;
    FYMax := TYMax;
    { FXVPin }
    FVP := FXMin + (FXMax - FXMin) / (Image2.Width - 1) *
      (Origin.X + ((Origin.X - X) div 2));
    { FYHQIn }
    FHQ := FYMax - (FYMax - FYMin) / (Image2.Height - 1) *
      (Origin.Y + ((Origin.Y - Y) div 2));
    Fractaling := False;
    Drawing := False; { clear the Drawing flag }
    LastKnownFunction := 69;
    FractalForm.FractalSet;
  end;
  if bPointing then
  begin { 639 }
    { Convert to Fractal Coordinates }
    { FXVPin := FXMin + (FXMax - FXMin)/639 * xpos ;
      FYHQIn := FYMax - (FYMax - FYMin)/479 * ypos ; }
    { FXVPin } FVP := FXMin + (FXMax - FXMin) / (Image2.Width - 1) * X;
    { FYHQIn } FHQ := FYMax - (FYMax - FYMin) / (Image2.Height - 1) * Y;
    bPointing := False;
    Julia := False; { 479 }
    LastKnownFunction := 69;
    FractalForm.FractalSet;
    { Need to figure what the POINT needs to compute Q and P
      FXVPin := FXMin + (FXMax - FXMin)/639 * xpos ;
      FYHQIn := FYMax - (FYMax - FYMin)/479 * ypos ; }
  end;
  if bVistaLive then
  begin
    if (DrawingTool = dtVRectangle) then
    begin
      if (ITriangling = 2) then
      begin
        DIYX4Y4.X := X;
        DIYX4Y4.Y := Y;
        { DIYX4Y4.X:=DIYXY.X;
          DIYX4Y4.Y:=DIYXY.Y; }
        Application.ProcessMessages;
        str(DIYXY.X, AnnoS);
        MathForm.DIYXEdit.Text := AnnoS;
        str(DIYXY.Y, AnnoS);
        MathForm.DIYYEdit.Text := AnnoS;
        str(DIYX2Y2.X, AnnoS);
        MathForm.DIYWxEdit.Text := AnnoS;
        str(DIYX2Y2.Y, AnnoS);
        MathForm.DIYHyEdit.Text := AnnoS;
        DrawShape(Origin, Point(X, Y), DIYX3Y3, DIYX4Y4, pmCopy);
        DrawShape(Point(X, Y), DIYXY, DIYX3Y3, DIYX4Y4, pmCopy);
        ITriangling := 102;
        DrawShape(Point(X, Y), DIYXY, DIYX3Y3, DIYX4Y4, pmCopy);
        bVistaLive := False;
        Drawing := False; { clear the Drawing flag }
      end
      else if (ITriangling = 1) then
      begin
        { DIYX2Y2.X:= Origin.x;
          DIYX2Y2.Y:=Origin.y; }
        DIYX3Y3.X := X;
        DIYX3Y3.Y := Y;
        DIYX4Y4.X := X;
        DIYX4Y4.Y := Y;
        DrawShape(Origin, Point(X, Y), DIYX3Y3, DIYX4Y4, pmCopy);
        Image2.Canvas.MoveTo(X, Y);
        Origin := Point(X, Y);
        MovePt := Point(X, Y);
        { DrawShape(Origin, MovePt, pmNotXor); }
        inc(ITriangling);
        Drawing := True; { Reset the Drawing flag }
        { DrawShape(Point(X, Y),DIYXY, pmCopy); }
      end
      else if (ITriangling = 0) then
      begin
        { Capture the past points }
        DIYXY.X := Origin.X;
        DIYXY.Y := Origin.Y;
        DIYX2Y2.X := X;
        DIYX2Y2.Y := Y;
        DrawShape(Origin, Point(X, Y), DIYX3Y3, DIYX4Y4, pmCopy);
        Image2.Canvas.MoveTo(X, Y);
        Origin := Point(X, Y);
        MovePt := Point(X, Y);
        { DrawShape(Origin, Point(X, Y), pmNotXor); }
        Application.ProcessMessages;
        inc(ITriangling);
        Drawing := True; { Reset the Drawing flag }
      end;
    end
    else if ((DrawingTool = dtVTriangle) or (DrawingTool = dtTSTriangle) or
      (DrawingTool = dtTriangle)) then
    begin
      if (ITriangling = 1) then
      begin
        DIYX3Y3.X := X;
        DIYX3Y3.Y := Y;
        Application.ProcessMessages;
        str(DIYXY.X, AnnoS);
        MathForm.DIYXEdit.Text := AnnoS;
        str(DIYXY.Y, AnnoS);
        MathForm.DIYYEdit.Text := AnnoS;
        str(DIYX2Y2.X, AnnoS);
        MathForm.DIYWxEdit.Text := AnnoS;
        str(DIYX2Y2.Y, AnnoS);
        MathForm.DIYHyEdit.Text := AnnoS;
        DrawShape(Origin, Point(X, Y), DIYX3Y3, DIYX4Y4, pmCopy);
        DrawShape(Point(X, Y), DIYXY, DIYX3Y3, DIYX4Y4, pmCopy);
        ITriangling := 102;
        DrawShape(Point(X, Y), DIYXY, DIYX3Y3, DIYX4Y4, pmCopy);
        bVistaLive := False;
        Drawing := False; { clear the Drawing flag }
      end
      else if (ITriangling = 0) then
      begin
        { Capture the past points }
        DIYXY.X := Origin.X;
        DIYXY.Y := Origin.Y;
        DIYX2Y2.X := X;
        DIYX2Y2.Y := Y;
        DIYX3Y3.X := X;
        DIYX3Y3.Y := Y;
        DrawShape(Origin, Point(X, Y), DIYX3Y3, DIYX4Y4, pmCopy);
        Image2.Canvas.MoveTo(X, Y);
        Origin := Point(X, Y);
        MovePt := Point(X, Y);
        { DrawShape(Origin, Point(X, Y), pmNotXor); }
        Application.ProcessMessages;
        Drawing := True; { Reset the Drawing flag }
        inc(ITriangling);
      end;
    end
    else
    begin { all other shapes }
      { Capture the data according to type }
      DIYXY.X := Origin.X;
      DIYXY.Y := Origin.Y;
      DIYX2Y2.X := X;
      DIYX2Y2.Y := Y;
      str(Origin.X, AnnoS);
      MathForm.DIYXEdit.Text := AnnoS;
      str(Origin.Y, AnnoS);
      MathForm.DIYYEdit.Text := AnnoS;
      str(X, AnnoS);
      MathForm.DIYWxEdit.Text := AnnoS;
      str(Y, AnnoS);
      MathForm.DIYHyEdit.Text := AnnoS;
      bVistaLive := False;
      Drawing := False; { clear the Drawing flag }
      DrawShape(Origin, Point(X, Y), DIYX3Y3, DIYX4Y4, pmCopy);
    end;
  end;
  if ((ImageAnnotate.Checked) or (DIYAnnotate)) then
  begin
    Image2.Canvas.TextOut(X, Y, AnnotationForm.AnnoEdit.Text);
    ImageAnnotate.Checked := False;
    DIYTextStorage := AnnotationForm.AnnoEdit.Text;
    DIYXY.X := X;
    DIYXY.Y := Y;
    DIYAnnotate := False;
    AnnotationForm.Hide;
    { Place-Record the Annotation AND the Font data
      onto the Vista List }
    begin
      (* MathForm.SaveDialog1.Filename := FractalFont.Name + '*.vft';
        MathForm.SaveDialog1.Filter := 'Vista Font(*.vft)|*.vft';
        if MathForm.SaveDialog1.Execute then { Display Open dialog box }
        begin
        AssignFile(F_File, MathForm.SaveDialog1.Filename);
        ReWrite(F_File);
        if IoResult = 0 then
        begin
        Write(F_File, FractalFont);
        CloseFile(F_File);
      *)
      MathForm.DIYMemo.Lines.Append('Annotation');
      { AnnoS := MathForm.SaveDialog1.Filename;
        MathForm.DIYMemo.Lines.Append(AnnoS); }
      MathForm.DIYMemo.Lines.Append(DIYTextStorage);
      str(X, AnnoS);
      MathForm.DIYMemo.Lines.Append('X: ' + AnnoS);
      str(Y, AnnoS);
      MathForm.DIYMemo.Lines.Append('Y: ' + AnnoS);
      (* end;
        if (IoResult <> 0) then
        Application.Mess ageBox
        ('DID NOT Save Font', 'Font save Error', MB_OKCANCEL);
        end;
      *)
    end;
    Image2.Canvas.Refresh;
    Application.ProcessMessages;
  end;
  if bLightning then
  begin
    DIYL.X := X;
    DIYL.Y := Y;
    str(X, AnnoS);
    MathForm.DIYLxEdit.Text := AnnoS;
    str(Y, AnnoS);
    MathForm.DIYLyEdit.Text := AnnoS;
    bLightning := False;
  end;
  if (Cropping > 0) then
  begin
    DrawingTool := dtLine;
    Drawing := False;
    MovePt := Point(X, Y);
    EditCropper;
  end;

  if (bZoomMousing) then
  begin
    DrawingTool := dtLine;
    Drawing := False;
    MovePt := Point(X, Y);
    ZoomMouseDo;
  end;
  if bRotatingImage then
    bRotateImage := True;
  { in the Drawing...
    bRotateImage:=False;
    Repeat Application.ProcessMessages until (bRotateImage=True) }
end;

(* *********************************************************** *)
procedure TMainForm.DrawShape(TopLeft, BottomRight, Tri, Quad: TPoint;
  AMode: TPenMode);

var
  Code, ColdX, i: Integer;
  TempColor: TColor;
  dCounter, Ramper: Double;

begin

  with Image2.Canvas do
  begin
    Pen.Mode := AMode;
    case DrawingTool of
      dtLine:
        begin
          Image2.Canvas.MoveTo(TopLeft.X, TopLeft.Y);
          Image2.Canvas.LineTo(BottomRight.X, BottomRight.Y);
        end;
      dtVTriangle, dtTSTriangle, dtTriangle:
        begin
          if (ITriangling > 5) then
          begin
            Image2.Canvas.Polygon([DIYXY, DIYX2Y2, DIYX3Y3]);
          end
          else
          begin
            Image2.Canvas.MoveTo(TopLeft.X, TopLeft.Y);
            Image2.Canvas.LineTo(BottomRight.X, BottomRight.Y);
          end;
        end;
      dtVRectangle:
        begin
          if (ITriangling > 5) then
          begin
            Image2.Canvas.Polygon([DIYXY, DIYX2Y2, DIYX3Y3, DIYX4Y4]);
          end
          else
          begin
            Image2.Canvas.MoveTo(TopLeft.X, TopLeft.Y);
            Image2.Canvas.LineTo(BottomRight.X, BottomRight.Y);
          end;
        end;
      dtVDRectangle, dtVTRectangle, dtVFRectangle, dtVNRectangle, dtVMRectangle,
        dtVARectangle, dtVWRectangle, dtVCRectangle, dtVSRectangle,
        dtVBRectangle, dtRectangle:
        Image2.Canvas.Rectangle(TopLeft.X, TopLeft.Y, BottomRight.X,
          BottomRight.Y);
      dtVPRectangle:
        begin
          if bVistaLive then
            Image2.Canvas.Rectangle(TopLeft.X, TopLeft.Y, BottomRight.X,
              BottomRight.Y)
          else
          begin
            { Pandora dCounter }
            val(MathForm.DIYZEdit.Text, dCounter, Code);
            Codefx(MathForm.DIYZEdit.Text, Code);
            Ramper := (255 / (1 + abs(DIYXY.Y - DIYX2Y2.Y)));
            for Code := DIYXY.Y to DIYX2Y2.Y do
            begin
              for ColdX := DIYXY.X to DIYX2Y2.X do
              begin
                if (random(abs(DIYXY.Y - DIYX2Y2.Y)) <=
                  random(round(Ramper * dCounter))) then
                  Pixels[ColdX, Code] := V1Color;
                if (random(abs(DIYXY.Y - DIYX2Y2.Y)) <=
                  random(round(Ramper * (dCounter / 8)))) then
                begin
                  Pixels[ColdX + 1, Code] := V2Color;
                  Pixels[ColdX, Code] := V3Color;
                  Pixels[ColdX, Code + 1] := V2Color;
                  Pixels[ColdX - 1, Code] := V2Color;
                  Pixels[ColdX, Code - 1] := V2Color;
                end;
                if (random(abs(DIYXY.Y - DIYX2Y2.Y)) <=
                  random(round(Ramper * dCounter))) then
                  Pixels[ColdX, Code] := V4Color;
              end;
            end;
          end;
        end;
      dtVERectangle:
        begin
          if bVistaLive then
            Image2.Canvas.Rectangle(TopLeft.X, TopLeft.Y, BottomRight.X,
              BottomRight.Y)
          else
          begin
            dCounter := 0;
            Ramper := (255 / (1 + abs(DIYXY.Y - DIYX2Y2.Y)));
            for i := DIYXY.Y to DIYX2Y2.Y do
            begin
              TempColor := RGB(Colors[0, round(dCounter)],
                Colors[1, round(dCounter)], Colors[2, round(dCounter)]);
              dCounter := dCounter + Ramper;
              Pen.Color := TempColor;
              MoveTo(DIYXY.X, i);
              LineTo(DIYX2Y2.X, i);
            end;
          end;
        end;
      dtCSEllipse, dtEllipse:
        Image2.Canvas.Ellipse(TopLeft.X, TopLeft.Y, BottomRight.X,
          BottomRight.Y);
      dtPBrush:
        Image2.Canvas.Ellipse(TopLeft.X, TopLeft.Y,
          (TopLeft.X + DrawingTool_PBrush_Width),
          (TopLeft.Y + DrawingTool_PBrush_Width));
      dtRoundRect:
        Image2.Canvas.RoundRect(TopLeft.X, TopLeft.Y, BottomRight.X,
          BottomRight.Y, (TopLeft.X - BottomRight.X) div 2,
          (TopLeft.Y - BottomRight.Y) div 2);
    end;
  end;
end;

(* *********************************************************** *)
procedure TMainForm.ImageDrawClick(Sender: TObject);
begin
  { use this to activate painting
    Use style to change bitmap }
  if (ImageDraw.Checked) then
  begin
    ImageDraw.Checked := False;
    DIYStyleForm.Hide;
    Drawing := False; { clear the Drawing flag }
  end
  else
  begin
    ImageDraw.Checked := True;
    DrawingTool := dtLine;
    DIYStyleForm.Show;
    Drawing := True; { clear the Drawing flag }
  end;
  { ImageDraw.Checked := (Not ImageDraw.Checked);
    If ImageDraw.Checked then DrawingTool := dtLine; }
end;

procedure TMainForm.ImageAnnotateClick(Sender: TObject);
begin
  ImageAnnotate.Checked := (not ImageAnnotate.Checked);
  if ImageAnnotate.Checked then
  begin
    { Image2.Canvas.Font := FractalFont; }
    AnnotationForm.Show;
  end;
  {
    Writes a string on the canvas, starting at the point (X,Y),
    and then updates the PenPos to the end of the string.
    procedure TextOut(X, Y: Integer; const Text: string);
    Description
    Use TextOut to write a string onto the canvas.
    The string will be written using the current value of Font.
    Use the TextExtent method to determine the space occupied by
    the text in the image. To write only the text that fits within
    a clipping rectangle, use TextRect instead.
    Writes a string inside a clipping rectangle.
    procedure TextRect(Rect: TRect; X, Y: Integer; const Text: string);
    Description
    Use TextRect to write a string within a limited rectangular region.
    Any portions of the string that fall outside the rectangle passed
    in the Rect parameter are clipped and don't appear. The upper left
    corner of the text is placed at the point (X, Y). }
end;

procedure TMainForm.ImageSelectFontClick(Sender: TObject);
begin
  FontDialog1.Execute;
  Image2.Canvas.Font := FontDialog1.Font;
  HintPanel.Caption := FontDialog1.Font.Name;
end;

(* *********************************************************** *)
function NextFileNumber(F_image: string): string; { File Save }
var
  WorkerName: string;
begin
  WorkerName := ExtractFileName(F_image);
  repeat
    ;
    if FileExists(WorkerName) then
    begin
      inc(WorkerName[11]);
      { $3A is :, the Char after 9 }
      if WorkerName[11] >= char($3A) then
      begin
        WorkerName[11] := char($30);
        { $30 = 0 }
        inc(WorkerName[10]);
      end;
      if WorkerName[10] >= char($3A) then
      begin
        WorkerName[10] := char($30);
        { $30 = 0 }
        inc(WorkerName[9]);
      end;

    end;
  until (not FileExists(WorkerName) or (WorkerName[9] > char($39)));
  { $39 = 9 }
  NextFileNumber := WorkerName;
end; { of NextFileNumber }

(* ********************************************************* *)
procedure TMainForm.ChangeFileName;
var
  SFile, SinFile: string;
begin
  if FileExists(FractalFilename) then
  begin
    SFile := FractalFilename;
    SinFile := NextFileNumber(SFile);
    FractalFilename := ExpandFileName(SinFile);
    if (FileExists(FractalFilename)) then
      DoMessages(27);
  end;
end;

(* *********************************************************** *)
{ Image FILENAME MUST BE CHANGED PRIOR TO CALLING
  to keep aligned with its Image file }
(* *********************************************************** *)
procedure TMainForm.WriteDataFile;
var
  FractalFileRcd: string;
  IterationalEx, Color_RangeEx, Gotto_ColEx: Extended;
  F_File: file of Extended;
begin { Read and Write data files as ALL Extended values }
  { function ExtractFileName(const FileName: string): string;)
    MyFilesExtension := ExtractFileExt(FractalFilename);
    function ChangeFileExt(const FileName, Extension: string): string; }
  if (CallMandelBrot = True) then
  begin
    IterationalEx := max_iterations * 1.0;
    Color_RangeEx := Color_Option * 1.0;
    Gotto_ColEx := Start_Col * 1.0;
    FractalFileRcd := ChangeFileExt(FractalFilename, '.TMP');
    AssignFile(F_File, FractalFileRcd);
    ReWrite(F_File);
    if IoResult = 0 then
    begin
      Write(F_File, FXMax);
      Write(F_File, FXMin);
      Write(F_File, FYMax);
      Write(F_File, FYMin);
      Write(F_File, FHQ);
      Write(F_File, FVP);
      Write(F_File, IterationalEx);
      Write(F_File, Color_RangeEx);
      Write(F_File, Gotto_ColEx);
      { MaximumElevation }
      IterationalEx := MaximumElevation * 1.0;
      Write(F_File, IterationalEx);
      IterationalEx := MinimumElevation * 1.0;
      Write(F_File, IterationalEx);
      IterationalEx := ContourInterval * 1.0;
      Write(F_File, IterationalEx);
      IterationalEx := Fractalgorhythym * 1.0;
      Write(F_File, IterationalEx);
      IterationalEx := FileSizeX * 1.0;
      Write(F_File, IterationalEx);
      IterationalEx := FileSizeY * 1.0;
      Write(F_File, IterationalEx);
      CloseFile(F_File);
      if (IoResult <> 0) then
        DoMessages(39982);
    end
    else
      DoMessages(39981);
  end
  else
  begin
    IterationalEx := max_iterations * 1.0;
    Color_RangeEx := Color_Option * 1.0;
    Gotto_ColEx := Start_Col * 1.0;
    FractalFileRcd := ChangeFileExt(FractalFilename, '.RCD');
    AssignFile(F_File, FractalFileRcd);
    ReWrite(F_File);
    Write(F_File, FXMax);
    Write(F_File, FXMin);
    Write(F_File, FYMax);
    Write(F_File, FYMin);
    Write(F_File, FHQ);
    Write(F_File, FVP);
    Write(F_File, IterationalEx);
    Write(F_File, Color_RangeEx);
    Write(F_File, Gotto_ColEx);
    { MaximumElevation }
    IterationalEx := MaximumElevation * 1.0;
    Write(F_File, IterationalEx);
    IterationalEx := MinimumElevation * 1.0;
    Write(F_File, IterationalEx);
    IterationalEx := ContourInterval * 1.0;
    Write(F_File, IterationalEx);
    IterationalEx := Fractalgorhythym * 1.0;
    Write(F_File, IterationalEx);
    IterationalEx := FileSizeX * 1.0;
    Write(F_File, IterationalEx);
    IterationalEx := FileSizeY * 1.0;
    Write(F_File, IterationalEx);
    CloseFile(F_File);
    if IoResult <> 0 then
      DoMessages(30101);
  end;
  LastKnownFunction := 66;
  FractalForm.FractalSet;
end;

(* *********************************************************** *)
procedure TMainForm.ReadDataFile;
var
  FractalFileRcd: string;
  IterationalEx1, IterationalEx, Color_RangeEx, Gotto_ColEx: Extended;
  F_File: file of Extended;
begin { Try to read RCD file then TMP... failure }
  FractalFileRcd := ChangeFileExt(FractalFilename, '.RCD');
  if (FileExists(FractalFileRcd)) then
  begin
    AssignFile(F_File, FractalFileRcd);
    Reset(F_File);
    if IoResult <> 0 then
    begin
      DoMessages(30102);
    end;
    Read(F_File, FXMax);
    Read(F_File, FXMin);
    Read(F_File, FYMax);
    Read(F_File, FYMin);
    Read(F_File, FHQ);
    Read(F_File, FVP);
    Read(F_File, IterationalEx1);
    Read(F_File, Color_RangeEx);
    Read(F_File, Gotto_ColEx);
    max_iterations := round(IterationalEx1);
    Color_Option := round(Color_RangeEx);
    Start_Col := round(Gotto_ColEx);
    Read(F_File, IterationalEx);
    MaximumElevation := round(IterationalEx);
    Read(F_File, IterationalEx);
    MinimumElevation := round(IterationalEx);
    Read(F_File, IterationalEx);
    ContourInterval := round(IterationalEx);
    Read(F_File, IterationalEx);
    Fractalgorhythym := round(IterationalEx);
    Read(F_File, IterationalEx);
    FileSizeX := round(IterationalEx);
    Read(F_File, IterationalEx);
    FileSizeY := round(IterationalEx);
    CloseFile(F_File);
  end
  else
  begin
    FractalFileRcd := ChangeFileExt(FractalFilename, '.TMP');
    if FileExists(FractalFileRcd) then
    begin
      AssignFile(F_File, FractalFileRcd);
      Reset(F_File);
      if IoResult <> 0 then
      begin
        DoMessages(39987);
        exit;
      end;
      Read(F_File, FXMax);
      Read(F_File, FXMin);
      Read(F_File, FYMax);
      Read(F_File, FYMin);
      Read(F_File, FHQ);
      Read(F_File, FVP);
      Read(F_File, IterationalEx);
      Read(F_File, Color_RangeEx);
      Read(F_File, Gotto_ColEx);
      max_iterations := round(IterationalEx1);
      Color_Option := round(Color_RangeEx);
      Start_Col := round(Gotto_ColEx);
      Read(F_File, IterationalEx);
      MaximumElevation := round(IterationalEx);
      Read(F_File, IterationalEx);
      MinimumElevation := round(IterationalEx);
      Read(F_File, IterationalEx);
      ContourInterval := round(IterationalEx);
      Read(F_File, IterationalEx);
      Fractalgorhythym := round(IterationalEx);
      Read(F_File, IterationalEx);
      FileSizeX := round(IterationalEx);
      Read(F_File, IterationalEx);
      FileSizeY := round(IterationalEx);
      CloseFile(F_File);
      Erase(F_File);
      if IoResult <> 0 then
      begin
        DoMessages(39988);
      end;
    end
    else
    begin
      DoMessages(39989);
    end;
  end;
  LastKnownFunction := 99;
  FractalForm.FractalSet;
end;

(* *********************************************************** *)
procedure TMainForm.FileOpen(Sender: TObject);
begin
  { if then }
  OpenPictureDialog1.Title := 'Fractal File Selection';
  OpenPictureDialog1.FileName := 'Fractals.BMP';
  FileOpener;
end;

procedure TMainForm.FileOpener;
var { PixelString, }
  MyFilesExtension: string;
  { Pixeli: Integer; }
begin
  OpenPictureDialog1.InitialDir := FractalDir;
  if OpenPictureDialog1.Execute then
  begin
    FractalDir := ExtractFilePath(OpenPictureDialog1.FileName);
    MyFilesExtension := Uppercase(ExtractFileExt(OpenPictureDialog1.FileName));
    if MyFilesExtension = '.BMP' then
    begin
      { Add code to open OpenPictureDialog1.FileName }
      FractalFilename := OpenPictureDialog1.FileName;
      Image2.Picture.LoadFromFile(FractalFilename);
      if (BitmapF.PixelFormat <> MyPixelFormat) then
      begin
        DoMessages(12);
        { if (PixelScanSize = 4) then Pixeli := 32 else
          Pixeli := 24;
          str(Pixeli, PixelString); }

      end
      else
      begin
        ReadDataFile; { of this }
        MyFilesExtension := ChangeFileExt(FractalFilename, '.TMP');
        if (not FileExists(MyFilesExtension)) then
          ChangeFileName; { for next }
        MyFilesExtension := ExtractFileName(FractalFilename);
        if (MyFilesExtension[6] = 'P') then
          Julia := True;
        if Julia then
          bPointing := True
        else
          Fractaling := True;
      end;
    end
    else
      DoMessages(30093);
  end;
end;

procedure TMainForm.FileSave(Sender: TObject);
begin
  FileSaver;
end;

procedure TMainForm.FileSaver;
begin
  { Add code to save current file under current name
    SavePictureDialog1 }
  Image2.Picture.SaveToFile(FractalFilename);
  WriteDataFile;
end;

procedure TMainForm.FileSaveAs(Sender: TObject);
var
  jp: TJpegImage; // Requires the "jpeg" unit added to "uses" clause.
  MyFilesExtension: string;
begin
  SavePictureDialog1.InitialDir := FractalDir;
  if SavePictureDialog1.Execute then
  begin
    FractalDir := ExtractFilePath(SavePictureDialog1.FileName);
    MyFilesExtension := Uppercase(ExtractFileExt(SavePictureDialog1.FileName));
    if MyFilesExtension = '.BMP' then
    begin
      Image2.Picture.SaveToFile(SavePictureDialog1.FileName);
    end
    else if MyFilesExtension = '.JPG' then
    begin
      { This example converts a bitmap image to a jpeg file
        by using the Assign method. }

      jp := TJpegImage.Create;
      try
        with jp do
        begin
          assign(Image2.Picture.Bitmap);
          if JPEGForm.ShowModal = mrOK then
            jp.SaveToFile(SavePictureDialog1.FileName);
        end;
      finally
        jp.Free;
      end;
    end;
  end;
end;

(* *********************************************************** *)
procedure TMainForm.PrintBig1Click(Sender: TObject);
begin
  PrintBig1.Checked := (not PrintBig1.Checked);
end;

procedure TMainForm.FilePrint(Sender: TObject);
var
  PrintRect: TRect;
begin
  if PrintDialog.Execute then
  begin
    { Add code to print current file }
    with Printer do
    begin
      BeginDoc;
      { Image1.Picture.Bitmap or FBitmap
        Check to see if enlarged print is available }
      if ((PageWidth > (Image2.Picture.Bitmap.Width * 2)) and
        (PageHeight > (Image2.Picture.Bitmap.Height * 2)) and PrintBig1.Checked)
      then
      begin
        { Rect(0, 0, Image.Width, Image.Height); }
        PrintRect := Rect((PageWidth - (Image2.Picture.Bitmap.Width * 2)) div 2,
          (PageHeight - (Image2.Picture.Bitmap.Height * 2)) div 2,
          (((PageWidth - (Image2.Picture.Bitmap.Width * 2)) div 2) +
          (Image2.Picture.Bitmap.Width * 2)),
          (((PageHeight - (Image2.Picture.Bitmap.Height * 2)) div 2) +
          (Image2.Picture.Bitmap.Height * 2)));
        Canvas.StretchDraw(PrintRect, Image2.Picture.Bitmap);
      end
      else if (((PageWidth - Image2.Picture.Bitmap.Width) > 0) and
        ((PageHeight - Image2.Picture.Bitmap.Height) > 0)) then
        Canvas.Draw((PageWidth - Image2.Picture.Bitmap.Width) div 2,
          (PageHeight - Image2.Picture.Bitmap.Height) div 2,
          Image2.Picture.Bitmap)
      else { Reduce Image to fit page... }
        if DoMessagesOK(10001) = mrYes then
        begin
          PrintRect := Rect((PageWidth - (Image2.Picture.Bitmap.Width div 2))
            div 2, (PageHeight - (Image2.Picture.Bitmap.Height div 2)) div 2,
            (((PageWidth - (Image2.Picture.Bitmap.Width div 2)) div 2) +
            (Image2.Picture.Bitmap.Width div 2)),
            (((PageHeight - (Image2.Picture.Bitmap.Height div 2)) div 2) +
            (Image2.Picture.Bitmap.Height div 2)));
          Canvas.StretchDraw(PrintRect, Image2.Picture.Bitmap);
        end;
      EndDoc;
    end;
  end;
end;

procedure TMainForm.FilePrintSetup(Sender: TObject);
begin
  PrintSetupDialog.Execute;
end;

(* *********************************************************** *)
(* *********************************************************** *)

procedure TMainForm.HelpContents(Sender: TObject);
begin
  { Application.HelpCommand(HELP_CONTENTS, 0); }
  Application.HelpContext(0);
end;

procedure TMainForm.HelpSearch(Sender: TObject);
const
  EmptyString: PChar = '';
begin
  Application.HelpCommand(HELP_PARTIALKEY, Longint(EmptyString));
end;

procedure TMainForm.HelpHowToUse(Sender: TObject);
begin
  Application.HelpCommand(HELP_HELPONHELP, 0);
end;

procedure TMainForm.HelpAbout(Sender: TObject);
begin
  { Add code to show program's About Box }
  AboutBox.Show;
  { With TAboutBox.Create(Self) Do Show; }
end;

(* *********************************************************** *)
procedure TMainForm.Preview1Click(Sender: TObject);
begin
  FMJForm.Show;
  { FractalPreviewForm.Show; }
end;

procedure TMainForm.Fractals1Click(Sender: TObject);
begin
  FractalForm.Show;
end;

procedure TMainForm.XYZ3DClick(Sender: TObject);
begin
  XYZ3DForm.Show;
end;

procedure TMainForm.Math1Click(Sender: TObject);
begin
  MathForm.Show;
end;

(* *********************************************************** *)
procedure TMainForm.FalseSize;
begin
  FV_IS640x480.Checked := False;
  FV_IS800x600.Checked := False;
  FV_IS1024x768.Checked := False;
  FV_IS1280x960.Checked := False;
  FV_IS1600x1200.Checked := False;
  FV_IS1920x1440.Checked := False;
  FV_IS2048x1536.Checked := False;
  FV_IS2400x1800.Checked := False;
  FV_IS2560x1920.Checked := False;
  FV_IS3072x2304.Checked := False;
  FV_IS3200x2400.Checked := False;
  FV_IS3840x2880.Checked := False;
  FV_IS4000x3000.Checked := False;
  FV_IS4096x3072.Checked := False;
end;

procedure TMainForm.FV_IS640x480Click(Sender: TObject);
begin
  if (not FV_IS640x480.Checked) then
  begin
    FalseSize;
    FV_IS640x480.Checked := (not FV_IS640x480.Checked);
    FYImageX := 640;
    FYImageY := 480;
    OverBit;
  end;
end;

procedure TMainForm.FV_IS800x600Click(Sender: TObject);
begin
  if (not FV_IS800x600.Checked) then
  begin
    FalseSize;
    FV_IS800x600.Checked := (not FV_IS800x600.Checked);
    FYImageX := 800;
    FYImageY := 600;
    OverBit;
  end;
end;

procedure TMainForm.FV_IS1024x768Click(Sender: TObject);
begin
  if (not FV_IS1024x768.Checked) then
  begin
    FalseSize;
    FV_IS1024x768.Checked := (not FV_IS1024x768.Checked);
    FYImageX := 1024;
    FYImageY := 768;
    OverBit;
  end;
end;

procedure TMainForm.FV_IS1280x960Click(Sender: TObject);
begin
  if (not FV_IS1280x960.Checked) then
  begin
    FalseSize;
    FV_IS1280x960.Checked := (not FV_IS1280x960.Checked);
    FYImageX := 1280;
    FYImageY := 960;
    OverBit;
  end;
end;

procedure TMainForm.FV_IS1600x1200Click(Sender: TObject);
begin
  if (not FV_IS1600x1200.Checked) then
  begin
    FalseSize;
    FV_IS1600x1200.Checked := (not FV_IS1600x1200.Checked);
    FYImageX := 1600;
    FYImageY := 1200;
    OverBit;
  end;
end;

procedure TMainForm.FV_IS1920x1440Click(Sender: TObject);
begin
  if (not FV_IS1920x1440.Checked) then
  begin
    FalseSize;
    FV_IS1920x1440.Checked := (not FV_IS1920x1440.Checked);
    FYImageX := 1920;
    FYImageY := 1440;
    OverBit;
  end;
end;

procedure TMainForm.FV_IS2048x1536Click(Sender: TObject);
begin
  if (not FV_IS2048x1536.Checked) then
  begin
    FalseSize;
    FV_IS2048x1536.Checked := (not FV_IS2048x1536.Checked);
    FYImageX := 2048;
    FYImageY := 1536;
    OverBit;
  end;
end;

procedure TMainForm.FV_IS2400x1800Click(Sender: TObject);
begin
  if (not FV_IS2400x1800.Checked) then
  begin
    FalseSize;
    FV_IS2400x1800.Checked := (not FV_IS2400x1800.Checked);
    FYImageX := 2400;
    FYImageY := 1800;
    OverBit;
  end;
end;

procedure TMainForm.FV_IS3072x2304Click(Sender: TObject);
begin
  if (not FV_IS3072x2304.Checked) then
  begin
    FalseSize;
    FV_IS3072x2304.Checked := (not FV_IS3072x2304.Checked);
    FYImageX := 3072;
    FYImageY := 2304;
    OverBit;
  end;
end;

procedure TMainForm.FV_IS4096x3072Click(Sender: TObject);
begin
  if (not FV_IS4096x3072.Checked) then
  begin
    FalseSize;
    FV_IS4096x3072.Checked := (not FV_IS4096x3072.Checked);
    FYImageX := 4096;
    FYImageY := 3072;
    OverBit;
  end;
end;

procedure TMainForm.FV_IS4000x3000Click(Sender: TObject);
begin
  if (not FV_IS4000x3000.Checked) then
  begin
    FalseSize;
    FV_IS4000x3000.Checked := (not FV_IS4000x3000.Checked);
    FYImageX := 4000;
    FYImageY := 3000;
    OverBit;
  end;
end;

procedure TMainForm.FV_IS3840x2880Click(Sender: TObject);
begin
  if (not FV_IS3840x2880.Checked) then
  begin
    FalseSize;
    FV_IS3840x2880.Checked := (not FV_IS3840x2880.Checked);
    FYImageX := 3840;
    FYImageY := 2880;
    OverBit;
  end;
end;

procedure TMainForm.FV_IS3200x2400Click(Sender: TObject);
begin
  if (not FV_IS3200x2400.Checked) then
  begin
    FalseSize;
    FV_IS3200x2400.Checked := (not FV_IS3200x2400.Checked);
    FYImageX := 3200;
    FYImageY := 2400;
    OverBit;
  end;
end;

procedure TMainForm.FV_IS2560x1920Click(Sender: TObject);
begin
  if (not FV_IS2560x1920.Checked) then
  begin
    FalseSize;
    FV_IS2560x1920.Checked := (not FV_IS2560x1920.Checked);
    FYImageX := 2560;
    FYImageY := 1920;
    OverBit;
  end;
end;

(* *********************************************************** *)
procedure TMainForm.FalseColors;
begin
  Black1.Checked := False; { clBlack }
  Maroon1.Checked := False; { clMaroon }
  Green1.Checked := False; { clGreen }
  OliveGreen1.Checked := False; { clOlive }
  NavyBlue1.Checked := False; { clNavy }
  Purple1.Checked := False; { clPurple }
  Teal1.Checked := False; { clTeal }
  Gray1.Checked := False; { clGray }
  Silver1.Checked := False; { clSilver }
  Red1.Checked := False; { clRed }
  LimeGreen1.Checked := False; { clime }
  Yellow1.Checked := False; { clYellow }
  Blue1.Checked := False; { clBlue }
  Fuchsia1.Checked := False; { clFuchsia }
  Aqua1.Checked := False; { clAqua }
  White1.Checked := False; { clWhite }
  Bomb1.Checked := False;
  Mine1.Checked := False;
  if FBackGroundColor = clBlack then
    Black1.Checked := True
  else if FBackGroundColor = clWhite then
    White1.Checked := True
  else if FBackGroundColor = clAqua then
    Aqua1.Checked := True
  else if FBackGroundColor = clFuchsia then
    Fuchsia1.Checked := True
  else if FBackGroundColor = clBlue then
    Blue1.Checked := True
  else if FBackGroundColor = clYellow then
    Yellow1.Checked := True
  else if FBackGroundColor = clLime then
    LimeGreen1.Checked := True
  else if FBackGroundColor = clRed then
    Red1.Checked := True
  else if FBackGroundColor = clSilver then
    Silver1.Checked := True
  else if FBackGroundColor = clGray then
    Gray1.Checked := True
  else if FBackGroundColor = clTeal then
    Teal1.Checked := True
  else if FBackGroundColor = clPurple then
    Purple1.Checked := True
  else if FBackGroundColor = clNavy then
    NavyBlue1.Checked := True
  else if FBackGroundColor = clOlive then
    OliveGreen1.Checked := True
  else if FBackGroundColor = clGreen then
    Green1.Checked := True
  else if FBackGroundColor = clMaroon then
    Maroon1.Checked := True
  else if FBackGroundColor = MineBackgroundColor then
    Mine1.Checked := True
  else if FBackGroundColor = BombBackgroundColor then
    Bomb1.Checked := True;
end;

procedure TMainForm.FractalBlack1Click(Sender: TObject);
begin
  if (not FractalBlack1.Checked) then
  begin
    FractalBlack1.Checked := True;
    bFractalMaxed := True;
  end
  else
  begin
    FractalBlack1.Checked := False;
    bFractalMaxed := False;
  end;
end;

procedure TMainForm.Black1Click(Sender: TObject);
begin
  if (not Black1.Checked) then
  begin
    FBackGroundColor := clBlack;
    FalseColors;
  end;
end;

procedure TMainForm.Maroon1Click(Sender: TObject);
begin
  if (not Maroon1.Checked) then
  begin
    FBackGroundColor := clMaroon;
    FalseColors;
    { Maroon1.Checked:=(Not Maroon1.Checked); }
  end;
end;

procedure TMainForm.Green1Click(Sender: TObject);
begin
  if (not Green1.Checked) then
  begin
    FBackGroundColor := clGreen;
    FalseColors;
    { Green1.Checked:=(Not Green1.Checked); }
  end;
end;

procedure TMainForm.OliveGreen1Click(Sender: TObject);
begin
  if (not OliveGreen1.Checked) then
  begin
    FBackGroundColor := clOlive;
    FalseColors;
    { OliveGreen1.Checked:=(Not OliveGreen1.Checked); }
  end;
end;

procedure TMainForm.NavyBlue1Click(Sender: TObject);
begin
  if (not NavyBlue1.Checked) then
  begin
    FBackGroundColor := clNavy;
    FalseColors;
    { NavyBlue1.Checked:=(Not NavyBlue1.Checked); }
  end;
end;

procedure TMainForm.Purple1Click(Sender: TObject);
begin
  if (not Purple1.Checked) then
  begin
    FBackGroundColor := clPurple;
    FalseColors;
    { Purple1.Checked:=(Not Purple1.Checked); }
  end;
end;

procedure TMainForm.Teal1Click(Sender: TObject);
begin
  if (not Teal1.Checked) then
  begin
    FBackGroundColor := clTeal;
    FalseColors;
    { Teal1.Checked:=(Not Teal1.Checked); }

  end;
end;

procedure TMainForm.Gray1Click(Sender: TObject);
begin
  if (not Gray1.Checked) then
  begin
    FBackGroundColor := clGray;
    FalseColors;
    { Gray1.Checked:=(Not Gray1.Checked); }
  end;
end;

procedure TMainForm.Silver1Click(Sender: TObject);
begin
  if (not Silver1.Checked) then
  begin
    FBackGroundColor := clSilver;
    FalseColors;
    { Silver1.Checked:=(Not Silver1.Checked); }
  end;
end;

procedure TMainForm.Red1Click(Sender: TObject);
begin
  if (not Red1.Checked) then
  begin
    FBackGroundColor := clRed;
    FalseColors;
    { Red1.Checked:=(Not Red1.Checked); }
  end;
end;

procedure TMainForm.LimeGreen1Click(Sender: TObject);
begin
  if (not LimeGreen1.Checked) then
  begin
    FBackGroundColor := clLime;
    FalseColors;
    { LimeGreen1.Checked:=(Not LimeGreen1.Checked); }
  end;
end;

procedure TMainForm.Yellow1Click(Sender: TObject);
begin
  if (not Yellow1.Checked) then
  begin
    FBackGroundColor := clYellow;
    FalseColors;
    { Yellow1.Checked:=(Not Yellow1.Checked); }
  end;
end;

procedure TMainForm.Blue1Click(Sender: TObject);
begin
  if (not Blue1.Checked) then
  begin
    FBackGroundColor := clBlue;
    FalseColors;
    { Blue1.Checked:=(Not Blue1.Checked); }
  end;
end;

procedure TMainForm.Fuchsia1Click(Sender: TObject);
begin
  if (not Fuchsia1.Checked) then
  begin
    FBackGroundColor := clFuchsia;
    FalseColors;
    { Fuchsia1.Checked:=(Not Fuchsia1.Checked); }
  end;
end;

procedure TMainForm.Aqua1Click(Sender: TObject);
begin
  if (not Aqua1.Checked) then
  begin
    FBackGroundColor := clAqua;
    FalseColors;
    { Aqua1.Checked:=(Not Aqua1.Checked); }
  end;
end;

procedure TMainForm.White1Click(Sender: TObject);
begin
  if (not White1.Checked) then
  begin
    FBackGroundColor := clWhite;
    FalseColors;
    { White1.Checked:=(Not White1.Checked); }
  end;
end;

procedure TMainForm.Mine1Click(Sender: TObject);
begin
  if (not Mine1.Checked) then
  begin
    FBackGroundColor := MineBackgroundColor;
    FalseColors;
  end;
end;

procedure TMainForm.Bomb1Click(Sender: TObject);
begin
  if (not Bomb1.Checked) then
  begin
    FBackGroundColor := BombBackgroundColor;
    FalseColors;
  end;
end;

procedure TMainForm.BombColorClick(Sender: TObject);
begin
  { BombBackgroundColor, MineBackgroundColor, }
  ColorDialog1.Color := BombBackgroundColor;
  if ColorDialog1.Execute then
  begin
    BombBackgroundColor := ColorDialog1.Color;
    BombColor.Color := ColorDialog1.Color;
    DoBomber;
  end;
end;

procedure TMainForm.MineColorClick(Sender: TObject);
begin
  { BombBackgroundColor, MineBackgroundColor, }
  ColorDialog1.Color := MineBackgroundColor;
  if ColorDialog1.Execute then
  begin
    MineBackgroundColor := ColorDialog1.Color;
    MineColor.Color := ColorDialog1.Color;
    DoMiner;
  end;
end;

procedure TMainForm.DoMiner;
begin { Make a bitmap and paint it background color
    then assign it to the menu bitmap
    save the bitmap as the MINE.BMP }
  MainForm.Mine1.Bitmap.Canvas.Brush.Color := MineBackgroundColor;
  MainForm.Mine1.Bitmap.Canvas.Pen.Color := MineBackgroundColor;
  MainForm.Mine1.Bitmap.Canvas.Brush.Style := bsSolid;
  MainForm.Mine1.Bitmap.Canvas.FillRect
    (Rect(1, 1, (MainForm.Mine1.Bitmap.Width), (MainForm.Mine1.Bitmap.Height)));
  MainForm.Mine1.Bitmap.Canvas.Draw(0, 0, MainForm.Mine1.Bitmap);
  MainForm.Mine1.Bitmap.SaveToFile('MINE.BMP');
end;

procedure TMainForm.DoBomber;
begin
  MainForm.Bomb1.Bitmap.Canvas.Brush.Color := BombBackgroundColor;
  MainForm.Bomb1.Bitmap.Canvas.Pen.Color := BombBackgroundColor;
  MainForm.Bomb1.Bitmap.Canvas.Brush.Style := bsSolid;
  MainForm.Bomb1.Bitmap.Canvas.FillRect
    (Rect(1, 1, (MainForm.Bomb1.Bitmap.Width), (MainForm.Bomb1.Bitmap.Height)));
  MainForm.Bomb1.Bitmap.Canvas.Draw(0, 0, MainForm.Bomb1.Bitmap);
  MainForm.Bomb1.Bitmap.SaveToFile('BOMB.BMP');
end;

(* *********************************************************** *)
procedure TMainForm.Set16Colors;
var
  F_File: file of TColor;
  TempColor: TColor;
  i: Integer;
begin
  case Colorset16 of
    1:
      begin { Win16 }
        Win16Set.Checked := True;
        RGBArray[0, 0] := 0;
        RGBArray[1, 0] := 0;
        RGBArray[2, 0] := 0;
        RGBArray[0, 1] := 128;
        RGBArray[1, 1] := 0;
        RGBArray[2, 1] := 0;
        RGBArray[0, 2] := 0;
        RGBArray[1, 2] := 128;
        RGBArray[2, 2] := 0;
        RGBArray[0, 3] := 128;
        RGBArray[1, 3] := 128;
        RGBArray[2, 3] := 0;
        RGBArray[0, 4] := 0;
        RGBArray[1, 4] := 0;
        RGBArray[2, 4] := 128;
        RGBArray[0, 5] := 128;
        RGBArray[1, 5] := 0;
        RGBArray[2, 5] := 128;
        RGBArray[0, 6] := 0;
        RGBArray[1, 6] := 128;
        RGBArray[2, 6] := 128;
        RGBArray[0, 7] := 192;
        RGBArray[1, 7] := 192;
        RGBArray[2, 7] := 192;
        RGBArray[0, 8] := 128;
        RGBArray[1, 8] := 128;
        RGBArray[2, 8] := 128;
        RGBArray[0, 9] := 255;
        RGBArray[1, 9] := 0;
        RGBArray[2, 9] := 0;
        RGBArray[0, 10] := 0;
        RGBArray[1, 10] := 255;
        RGBArray[2, 10] := 0;
        RGBArray[0, 11] := 255;
        RGBArray[1, 11] := 255;
        RGBArray[2, 11] := 0;
        RGBArray[0, 12] := 0;
        RGBArray[1, 12] := 0;
        RGBArray[2, 12] := 255;
        RGBArray[0, 13] := 255;
        RGBArray[1, 13] := 0;
        RGBArray[2, 13] := 255;
        RGBArray[0, 14] := 0;
        RGBArray[1, 14] := 255;
        RGBArray[2, 14] := 255;
        RGBArray[0, 15] := 255;
        RGBArray[1, 15] := 255;
        RGBArray[2, 15] := 255;
      end;
    2:
      begin { RGB }
        RGB16.Checked := True;
        RGBArray[0, 0] := 255;
        RGBArray[1, 0] := 0;
        RGBArray[2, 0] := 0;
        RGBArray[0, 1] := 225;
        RGBArray[1, 1] := 0;
        RGBArray[2, 1] := 0;
        RGBArray[0, 2] := 210;
        RGBArray[1, 2] := 0;
        RGBArray[2, 2] := 0;
        RGBArray[0, 3] := 195;
        RGBArray[1, 3] := 0;
        RGBArray[2, 3] := 0;
        RGBArray[0, 4] := 180;
        RGBArray[1, 4] := 0;
        RGBArray[2, 4] := 0;
        RGBArray[0, 5] := 0;
        RGBArray[1, 5] := 255;
        RGBArray[2, 5] := 0;
        RGBArray[0, 6] := 0;
        RGBArray[1, 6] := 225;
        RGBArray[2, 6] := 0;
        RGBArray[0, 7] := 0;
        RGBArray[1, 7] := 210;
        RGBArray[2, 7] := 0;
        RGBArray[0, 8] := 0;
        RGBArray[1, 8] := 195;
        RGBArray[2, 8] := 0;
        RGBArray[0, 9] := 0;
        RGBArray[1, 9] := 180;
        RGBArray[2, 9] := 0;
        RGBArray[0, 10] := 0;
        RGBArray[1, 10] := 0;
        RGBArray[2, 10] := 255;
        RGBArray[0, 11] := 0;
        RGBArray[1, 11] := 0;
        RGBArray[2, 11] := 240;
        RGBArray[0, 12] := 0;
        RGBArray[1, 12] := 0;
        RGBArray[2, 12] := 225;
        RGBArray[0, 13] := 0;
        RGBArray[1, 13] := 0;
        RGBArray[2, 13] := 210;
        RGBArray[0, 14] := 0;
        RGBArray[1, 14] := 0;
        RGBArray[2, 14] := 195;
        RGBArray[0, 15] := 0;
        RGBArray[1, 15] := 0;
        RGBArray[2, 15] := 180;
      end;
    3:
      begin { Landforms }
        LandformsSet.Checked := True;
        RGBArray[0, 0] := 0;
        RGBArray[1, 0] := 0;
        RGBArray[2, 0] := 128;
        RGBArray[0, 1] := 0;
        RGBArray[1, 1] := 0;
        RGBArray[2, 1] := 255;
        RGBArray[0, 2] := 0;
        RGBArray[1, 2] := 128;
        RGBArray[2, 2] := 128;
        RGBArray[0, 3] := 0;
        RGBArray[1, 3] := 255;
        RGBArray[2, 3] := 255;
        RGBArray[0, 4] := 255;
        RGBArray[1, 4] := 255;
        RGBArray[2, 4] := 0;
        RGBArray[0, 5] := 128;
        RGBArray[1, 5] := 128;
        RGBArray[2, 5] := 0;
        RGBArray[0, 6] := 0;
        RGBArray[1, 6] := 255;
        RGBArray[2, 6] := 0;
        RGBArray[0, 7] := 0;
        RGBArray[1, 7] := 128;
        RGBArray[2, 7] := 0;
        RGBArray[0, 8] := 128;
        RGBArray[1, 8] := 128;
        RGBArray[2, 8] := 128;
        RGBArray[0, 9] := 255;
        RGBArray[1, 9] := 0;
        RGBArray[2, 9] := 255;
        RGBArray[0, 10] := 128;
        RGBArray[1, 10] := 0;
        RGBArray[2, 10] := 128;
        RGBArray[0, 11] := 192;
        RGBArray[1, 11] := 192;
        RGBArray[2, 11] := 192;
        RGBArray[0, 12] := 255;
        RGBArray[1, 12] := 0;
        RGBArray[2, 12] := 0;
        RGBArray[0, 13] := 128;
        RGBArray[1, 13] := 0;
        RGBArray[2, 13] := 0;
        RGBArray[0, 14] := 255;
        RGBArray[1, 14] := 255;
        RGBArray[2, 14] := 255;
        RGBArray[0, 15] := 0;
        RGBArray[1, 15] := 0;
        RGBArray[2, 15] := 0;
      end;
    4:
      begin { Picker }
        ColorPicker16.Checked := True;
        RGBArray[0, 0] := 0;
        RGBArray[1, 0] := 0;
        RGBArray[2, 0] := 128;
        RGBArray[0, 1] := 0;
        RGBArray[1, 1] := 0;
        RGBArray[2, 1] := 255;
        RGBArray[0, 2] := 0;
        RGBArray[1, 2] := 128;
        RGBArray[2, 2] := 128;
        RGBArray[0, 3] := 0;
        RGBArray[1, 3] := 255;
        RGBArray[2, 3] := 255;
        RGBArray[0, 4] := 255;
        RGBArray[1, 4] := 255;
        RGBArray[2, 4] := 0;
        RGBArray[0, 5] := 128;
        RGBArray[1, 5] := 128;
        RGBArray[2, 5] := 0;
        RGBArray[0, 6] := 0;
        RGBArray[1, 6] := 255;
        RGBArray[2, 6] := 0;
        RGBArray[0, 7] := 0;
        RGBArray[1, 7] := 128;
        RGBArray[2, 7] := 0;
        RGBArray[0, 8] := 128;
        RGBArray[1, 8] := 128;
        RGBArray[2, 8] := 128;
        RGBArray[0, 9] := 255;
        RGBArray[1, 9] := 0;
        RGBArray[2, 9] := 255;
        RGBArray[0, 10] := 128;
        RGBArray[1, 10] := 0;
        RGBArray[2, 10] := 128;
        RGBArray[0, 11] := 192;
        RGBArray[1, 11] := 192;
        RGBArray[2, 11] := 192;
        RGBArray[0, 12] := 255;
        RGBArray[1, 12] := 0;
        RGBArray[2, 12] := 0;
        RGBArray[0, 13] := 128;
        RGBArray[1, 13] := 0;
        RGBArray[2, 13] := 0;
        RGBArray[0, 14] := 255;
        RGBArray[1, 14] := 255;
        RGBArray[2, 14] := 255;
        RGBArray[0, 15] := 0;
        RGBArray[1, 15] := 0;
        RGBArray[2, 15] := 0;
      end;
    5:
      begin { Loader }
        Loaded16.Checked := True;
        { Load the Filename as the selected set??? }
        if (FileExists(Color16Name)) then
        begin
          AssignFile(F_File, Color16Name);
          Reset(F_File);
          if IoResult <> 0 then
          begin
            DoMessages(30091);
          end;
          for i := 0 to 15 do
          begin
            Read(F_File, TempColor);
            RGBArray[0, i] := GetRValue(TempColor);
            RGBArray[1, i] := GetGValue(TempColor);
            RGBArray[2, i] := GetBValue(TempColor);
          end;
        end
        else
          DoMessages(30092);
      end;
  end; { of case }
end;

procedure TMainForm.False16;
begin
  Win16Set.Checked := False;
  RGB16.Checked := False;
  LandformsSet.Checked := False;
  ColorPicker16.Checked := False;
  Loaded16.Checked := False;
  Set16Colors;
end;

procedure TMainForm.Win16SetClick(Sender: TObject);
begin
  if (not Win16Set.Checked) then
  begin
    Colorset16 := 1;
    False16;
  end;
end;

procedure TMainForm.RGB16Click(Sender: TObject);
begin
  if (not RGB16.Checked) then
  begin
    Colorset16 := 2;
    False16;
  end;
end;

procedure TMainForm.LandformsSetClick(Sender: TObject);
begin
  if (not LandformsSet.Checked) then
  begin
    Colorset16 := 3;
    False16;
  end;
end;

procedure TMainForm.ColorPicker16Click(Sender: TObject);
begin
  if (not ColorPicker16.Checked) then
  begin
    Colorset16 := 4;
    False16;
  end;
end;

procedure TMainForm.Loaded16Click(Sender: TObject);
begin
  if (not Loaded16.Checked) then
  begin
    Colorset16 := 5;
    False16;
  end;
end;

(* *********************************************************** *)
procedure TMainForm.SiliconSets;
var
  F_File: file of TColor;
  i: Integer;
begin
  case Color256S of
    1:
      begin
        BlueMeanie256.Checked := True;
        for i := 0 to 255 do
        begin
          Colors[0, i] := 0;
          Colors[1, i] := 0;
          Colors[2, i] := 255 - i;
        end;
      end;
    2:
      begin
        AquaMarine256.Checked := True;
        for i := 0 to 255 do
        begin
          Colors[0, i] := 0;
          Colors[1, i] := 255 - i;
          Colors[2, i] := 255 - i;
        end;
      end;
    3:
      begin
        GreenGoblin256.Checked := True;
        for i := 0 to 255 do
        begin
          Colors[0, i] := 0;
          Colors[1, i] := 255 - i;
          Colors[2, i] := 0;
        end;
      end;
    4:
      begin
        YellowSnow256.Checked := True;
        for i := 0 to 255 do
        begin
          Colors[0, i] := 255 - i;
          Colors[1, i] := 255 - i;
          Colors[2, i] := 0;
        end;
      end;
    5:
      begin
        PurpleReigns256.Checked := True;
        for i := 0 to 255 do
        begin
          Colors[0, i] := 255 - i;
          Colors[1, i] := 0;
          Colors[2, i] := 255 - i;
        end;
      end;
    6:
      begin
        RedRanger256.Checked := True;
        for i := 0 to 255 do
        begin
          Colors[0, i] := 255 - i;
          Colors[1, i] := 0;
          Colors[2, i] := 0;
        end;
      end;
    7:
      begin
        SmogFog256.Checked := True;
        for i := 0 to 255 do
        begin
          Colors[0, i] := 255 - i;
          Colors[1, i] := 255 - i;
          Colors[2, i] := 255 - i;
        end;
      end;
    8:
      begin
        ColorPicker256.Checked := True;
        for i := 0 to 255 do
        begin
          Colors[0, i] := GetRValue(ColorArray[i]);
          Colors[1, i] := GetGValue(ColorArray[i]);
          Colors[2, i] := GetBValue(ColorArray[i]);
        end;
      end;
    9:
      begin { Load filename set }
        if (FileExists(Color256Name)) then
        begin
          Loaded256.Checked := True;
          AssignFile(F_File, Color256Name);
          Reset(F_File);
          if IoResult <> 0 then
          begin
            DoMessages(30091);
          end
          else
          begin
            for i := 0 to 255 do
              Read(F_File, ColorArray[i]);
            CloseFile(F_File);
            for i := 0 to 255 do
            begin
              Colors[0, i] := GetRValue(ColorArray[i]);
              Colors[1, i] := GetGValue(ColorArray[i]);
              Colors[2, i] := GetBValue(ColorArray[i]);
            end;
          end;
        end
        else
          DoMessages(30092);
      end;
    10:
      begin { Load filename set }
        if (FileExists(Color256TooName)) then
        begin
          LoadedToo56.Checked := True;;
          AssignFile(F_File, Color256TooName);
          Reset(F_File);
          if IoResult <> 0 then
          begin
            DoMessages(30091);
          end
          else
          begin
            for i := 0 to 255 do
              Read(F_File, ColorArray[i]);
            CloseFile(F_File);
            for i := 0 to 255 do
            begin
              Colors[0, i] := GetRValue(ColorArray[i]);
              Colors[1, i] := GetGValue(ColorArray[i]);
              Colors[2, i] := GetBValue(ColorArray[i]);
            end;
          end;
        end
        else
          DoMessages(30092);
      end;
  end; { of case }
end;

procedure TMainForm.FalseSets;
begin
  BlueMeanie256.Checked := False;
  AquaMarine256.Checked := False;
  GreenGoblin256.Checked := False;
  YellowSnow256.Checked := False;
  PurpleReigns256.Checked := False;
  RedRanger256.Checked := False;
  SmogFog256.Checked := False;
  ColorPicker256.Checked := False;
  Loaded256.Checked := False;
  LoadedToo56.Checked := False;
  SiliconSets;
end;

procedure TMainForm.BlueMeanie256Click(Sender: TObject);
begin
  if (not BlueMeanie256.Checked) then
  begin
    Color256S := 1;
    FalseSets;
    { BlueMeanie256.Checked:=(Not BlueMeanie256.Checked); }
  end;
end;

procedure TMainForm.AquaMarine256Click(Sender: TObject);
begin
  if (not AquaMarine256.Checked) then
  begin
    Color256S := 2;
    FalseSets;
    { AquaMarine256.Checked:=(Not AquaMarine256.Checked); }
  end;
end;

procedure TMainForm.GreenGoblin256Click(Sender: TObject);
begin
  if (not GreenGoblin256.Checked) then
  begin
    Color256S := 3;
    FalseSets;
    { GreenGoblin256.Checked:=(Not GreenGoblin256.Checked); }
  end;
end;

procedure TMainForm.YellowSnow256Click(Sender: TObject);
begin
  if (not YellowSnow256.Checked) then
  begin
    Color256S := 4;
    FalseSets;
    { YellowSnow256.Checked:=(Not YellowSnow256.Checked); }
  end;
end;

procedure TMainForm.PurpleReigns256Click(Sender: TObject);
begin
  if (not PurpleReigns256.Checked) then
  begin
    Color256S := 5;
    FalseSets;
    { PurpleReigns256.Checked:=(Not PurpleReigns256.Checked); }
  end;
end;

procedure TMainForm.RedRanger256Click(Sender: TObject);
begin
  if (not RedRanger256.Checked) then
  begin
    Color256S := 6;
    FalseSets;
    { RedRanger256.Checked:=(Not RedRanger256.Checked); }
  end;
end;

procedure TMainForm.SmogFog256Click(Sender: TObject);
begin
  if (not SmogFog256.Checked) then
  begin
    Color256S := 7;
    FalseSets;
    { SmogFog256.Checked:=(Not SmogFog256.Checked); }
  end;
end;

procedure TMainForm.ColorPicker256Click(Sender: TObject);
begin
  if (not ColorPicker256.Checked) then
  begin
    Color256S := 8;
    FalseSets;
    { ColorPicker256.Checked:=(Not ColorPicker256.Checked); }
  end;
end;

procedure TMainForm.Loaded256Click(Sender: TObject);
begin
  if (not Loaded256.Checked) then
  begin
    Color256S := 9;
    FalseSets;
    { Loaded256.Checked:=(Not Loaded256.Checked); }
  end;
end;

(* *********************************************************** *)
procedure TMainForm.LoadedToo56Click(Sender: TObject);
begin
  if (not LoadedToo56.Checked) then
  begin
    Color256S := 10;
    FalseSets;
    { LoadedToo56.Checked:=(Not LoadedToo56.Checked); }
  end;
end;

(* *********************************************************** *)
procedure TMainForm.Image2Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Stage = psRunning then
    MainForm.Caption := 'Fractal 3D: Math Graphics working... ' +
      Format('%d%%', [PercentDone])
  else
    MainForm.Caption := 'Fractal 3D: Math Graphics';
end;

procedure TMainForm.AlarmBtnClick(Sender: TObject);
begin { Set Alarm Boolean for access by Image Processors }
  if (not ImageDoneAlarm.Checked) then
  begin
    ImageDoneAlarm.Checked := True;
    ImageDoneVoices.Checked := False;
    ImageDoneAlarmb := True;
    IamDone := 1;
  end
  else
  begin
    ImageDoneAlarm.Checked := False;
    ImageDoneVoices.Checked := False;
    ImageDoneAlarmb := False;
    IamDone := 0;
  end;
end;

procedure TMainForm.ImageDoneVoicesClick(Sender: TObject);
begin { Set Alarm Boolean for access by Image Processors }
  if (not ImageDoneVoices.Checked) then
  begin
    ImageDoneAlarm.Checked := False;
    ImageDoneVoices.Checked := True;
    ImageDoneAlarmb := True;
    IamDone := 2;
  end
  else
  begin
    ImageDoneAlarm.Checked := False;
    ImageDoneVoices.Checked := False;
    ImageDoneAlarmb := False;
    IamDone := 0;
  end;
end;

procedure TMainForm.ImageOverDone;
begin
  case IamDone of
    0:
      begin
        ImageDoneAlarm.Checked := False;
        ImageDoneVoices.Checked := False;
        ImageDoneAlarmb := False;
      end;
    1:
      begin
        ImageDoneAlarm.Checked := True;
        ImageDoneVoices.Checked := False;
        ImageDoneAlarmb := True;
      end;
    2:
      begin
        ImageDoneAlarm.Checked := False;
        ImageDoneVoices.Checked := True;
        ImageDoneAlarmb := True;
      end;
  end; { case }
end;

procedure TMainForm.PlaySound(WavFileName: string);
var
  s: array [0 .. 79] of char;
begin
  { Convert filename to a null-terminated string }
  StrPCopy(s, WavFileName);
  { Play the sound asynchronously }
  sndPlaySound(s, 0); { see mmsystem.hlp for other values }
end;

procedure TMainForm.EditCropItemClick(Sender: TObject);
begin { Crop off edges with mouse }
  Cropping := 1;
end;

procedure TMainForm.EditCut(Sender: TObject);
begin { Add code to Cut out center }
  Cropping := 2;
end;

procedure TMainForm.EditClearAllItemClick(Sender: TObject);
begin
  { erase the screen }
end;

procedure TMainForm.EditCopy(Sender: TObject);
begin { Add code to Copy ALL of Image }
  Clipboard.assign(Image2.Picture);
end;

(* *********************************************************** *)
procedure TMainForm.EditCopyAreaItemClick(Sender: TObject);
begin
  { Copy the moused area, leave the image uncut }
  Cropping := 3;
end;

(* *********************************************************** *)
procedure TMainForm.EditPaste(Sender: TObject);
begin { is there a bitmap on the Clipboard? }
  if Clipboard.HasFormat(CF_BITMAP) then
  begin
    Image2.Picture.Bitmap.assign(Clipboard);
  end;
end;

(* *********************************************************** *)
procedure TMainForm.EditPasteIntoItemClick(Sender: TObject);
begin { Mouse an area to paste the clipboard into }
  Cropping := 4;
end;

(* *********************************************************** *)
procedure TMainForm.EditCropper;
var
  AllRect, Arect: TRect;
  CropImage: TBitmap;
begin
  case Cropping of
    1: { Crop off outside }
      begin
        CropImage := TBitmap.Create;
        CropImage.PixelFormat := MyPixelFormat; { pf24bit; }
        CropImage.Width := Origin.X - MovePt.X;
        CropImage.Height := Origin.Y - MovePt.Y;
        AllRect := Rect(0, 0, CropImage.Width - 1, CropImage.Height - 1);
        Arect := Rect(Origin.X, Origin.Y, MovePt.X, MovePt.Y);
        CropImage.Canvas.CopyMode := cmSrcCopy;
        CropImage.Canvas.CopyRect(AllRect, Image2.Canvas, Arect);
        Image2.Picture.Graphic := CropImage;
        CropImage.Free;
      end;
    2: { Cut out center }
      begin
        CropImage := TBitmap.Create;
        CropImage.PixelFormat := MyPixelFormat; { pf24bit; }
        CropImage.Width := Origin.X - MovePt.X;
        CropImage.Height := Origin.Y - MovePt.Y;
        AllRect := Rect(0, 0, CropImage.Width - 1, CropImage.Height - 1);
        Arect := Rect(Origin.X, Origin.Y, MovePt.X, MovePt.Y);
        CropImage.Canvas.CopyMode := cmSrcCopy;
        CropImage.Canvas.CopyRect(AllRect, Image2.Canvas, Arect);
        Clipboard.assign(CropImage);
        with Image2.Canvas do
        begin
          CopyMode := cmWhiteness; { copy everything as white }
          { get bitmap rectangle
            Origin := Point(X, Y);
            MovePt := Point(X, Y); }
          Arect := Rect(Origin.X, Origin.Y, MovePt.X, MovePt.Y);
          CopyRect(Arect, Image2.Canvas, Arect);
          { copy bitmap over itself }
          CopyMode := cmSrcCopy; { restore normal mode }
        end;
      end;
    3: { Copy area, leave alone }
      begin
        CropImage := TBitmap.Create;
        CropImage.PixelFormat := MyPixelFormat; { pf24bit; }
        CropImage.Width := Origin.X - MovePt.X;
        CropImage.Height := Origin.Y - MovePt.Y;
        AllRect := Rect(0, 0, CropImage.Width - 1, CropImage.Height - 1);
        Arect := Rect(Origin.X, Origin.Y, MovePt.X, MovePt.Y);
        CropImage.Canvas.CopyMode := cmSrcCopy;
        CropImage.Canvas.CopyRect(AllRect, Image2.Canvas, Arect);
        Clipboard.assign(CropImage);
        { Clip owns it... ?   CropImage.Free; }
      end;
    4: { Paste into }
      begin
        CropImage := TBitmap.Create;
        CropImage.assign(Clipboard);
        Arect := Rect(Origin.X, Origin.Y, MovePt.X, MovePt.Y);
        MainForm.Image2.Canvas.StretchDraw(Arect, CropImage);
        CropImage.Free;
      end;
  end; { case }
  Cropping := 0;
  { Check Clipboard:
    if available then enable clipboard items }
  if Clipboard.HasFormat(CF_BITMAP) then
  begin
    PasteButton.Enabled := True;
    PasteArea.Enabled := True;
    EditPasteItem.Enabled := True;
    EditPasteIntoItem.Enabled := True;
  end
  else
  begin
    PasteButton.Enabled := False;
    PasteArea.Enabled := False;
    EditPasteItem.Enabled := False;
    EditPasteIntoItem.Enabled := False;
  end;
end;

(* *********************************************************** *)
{ (UnZoom)  Change Zoom back to 1:1 }
procedure TMainForm.UnZoom1to1Click(Sender: TObject);
begin
  UnZoom;
end;

procedure TMainForm.UnZoom;
begin { assign the bitmap to the image control }
  if (ZoomingOn) then
  begin { RESET All parameters }
    Image2.Picture.Bitmap.Width := FYImageX;
    Image2.Picture.Bitmap.Height := FYImageY;
    Image2.Picture.Graphic.assign(BitmapF);
    ZoomingOn := False;
    { ImageZoomRatio:=0;
      ImageZoomRatioEx:=0;
      ZoomOriginX:=0;
      ZoomOriginY:=0;
      ZoomImageX:= FYImageX;
      ZoomImageY:= FYImageY; }
  end;
end;

(* *********************************************************** *)
procedure TMainForm.ZoomMachine;
begin { Prepare to Zoom }
  ZoomingOn := True;
  ImageZoomRatio := 0;
  ImageZoomRatioEx := 0;
  ZoomImageX := FYImageX;
  ZoomImageY := FYImageY;
  ZoomOriginX := 0;
  ZoomOriginY := 0;
  BitmapF.assign(Image2.Picture.Bitmap);
end;

(* *********************************************************** *)
procedure TMainForm.ZoomOut1Click(Sender: TObject);
begin { Reduce Zoom Ratio }
  if (not ZoomingOn) then
    ZoomMachine;

  { ImageZoomRatio:=(ImageZoomRatio div 2);
    ImageZoomRatioEx:=(ImageZoomRatioEx / 2);
    ZoomImageX:=(ImageZoomRatio * ZoomImageX);
    ZoomImageY:=(ImageZoomRatio * ZoomImageY);
    ZoomOriginX:= (ZoomOriginX div 2);
    ZoomOriginY:= (ZoomOriginY div 2);
  }
  { Image1.Picture.Bitmap.Width := ZoomImageX;
    Image1.Picture.Bitmap.Height := ZoomImageY; }
  { BitmapF :=Image1.Picture.Bitmap; }
end;

(* *********************************************************** *)
procedure TMainForm.ZoomMouseClick(Sender: TObject);
begin
  if (not ZoomingOn) then
    ZoomMachine;
  bZoomMousing := True;
end;

(* *********************************************************** *)
procedure TMainForm.ZoomMouseDo;
var
  FiTemp: Integer;
  a, b, c: Extended;
begin
  { Select area - Make it correct - change bitmap }
  if (Origin.X > MovePt.X) then
  begin
    FiTemp := Origin.X;
    Origin.X := MovePt.X;
    MovePt.X := FiTemp;
  end;
  if (Origin.Y > MovePt.Y) then
  begin
    FiTemp := Origin.Y;
    Origin.Y := MovePt.Y;
    MovePt.Y := FiTemp;
  end;
  { based on 0,0, upper left, then lower right
    640,480 Numbers provide Ratio not image size just use 4,3 }
  a := ((480 * (MovePt.X - Origin.X)) / 640);
  b := (((MovePt.Y - Origin.Y) - a) / 2);
  if (a > (MovePt.Y - Origin.Y)) then
  begin
    a := Origin.Y - b;
    c := MovePt.Y + b;
  end
  else
  begin
    a := Origin.Y + b;
    c := MovePt.Y - b;
  end;
  Origin.Y := round(a);
  MovePt.Y := round(c);
  ZSARect := Rect(Origin.X, Origin.Y, MovePt.X, MovePt.Y);
  ZDAllRect := Rect(0, 0, ScrollBox1.Width - 4 { Image1.Width } ,
    ScrollBox1.Height - 4
    { Image1.Height } );
  Image2.Picture.Bitmap.Width := ScrollBox1.Width - 4;
  Image2.Picture.Bitmap.Height := ScrollBox1.Height - 4;
  ZoomImageX := Image2.Picture.Bitmap.Width;
  ZoomImageY := Image2.Picture.Bitmap.Height;
  Image2.Picture.Bitmap.Canvas.CopyMode := cmSrcCopy;
  { procedure CopyRect(Dest: TRect; Canvas: TCanvas; Source: TRect); }
  Image2.Picture.Bitmap.Canvas.CopyRect(ZDAllRect, BitmapF.Canvas, ZSARect);
  bZoomMousing := False;
end;

(* *********************************************************** *)
procedure TMainForm.ZoomToWindowClick(Sender: TObject);
begin { If (not ZoomingOn) then ZoomMachine; }
  if (not ZoomingOn) then
    ZoomMachine;
  { Get window size,
    ScrollBox1.Width
    ScrollBox1.Height
    change bitmap to closest ratio }
  ZSARect := Rect(0, 0, Image2.Width - 1, Image2.Height - 1);
  ZDAllRect := Rect(0, 0, ScrollBox1.Width - 4 { Image1.Width } ,
    ScrollBox1.Height - 4
    { Image1.Height } );
  Image2.Picture.Bitmap.Width := ScrollBox1.Width - 4;
  Image2.Picture.Bitmap.Height := ScrollBox1.Height - 4;
  ZoomImageX := Image2.Picture.Bitmap.Width;
  ZoomImageY := Image2.Picture.Bitmap.Height;
  Image2.Picture.Bitmap.Canvas.CopyMode := cmSrcCopy;
  Image2.Picture.Bitmap.Canvas.CopyRect(ZDAllRect, BitmapF.Canvas, ZSARect);
end;

(* *********************************************************** *)
procedure TMainForm.DoImageStart;
var
  TempColor: TColor;
  { Save_Cursor:TCursor; }
begin
  Screen.Cursor := crHourglass; { Show hourglass cursor }
  { IF Zooming then Unzoom and remember settings }
  if (ZoomingOn) then
  begin
    UnZoom;
    WasZooming := True;
  end;
  { Set up the Image, procedures set their own title
    so that Vista does NOT have titles all over the place }
  with MainForm.Image2.Canvas do
  begin
    Brush.Color := FBackGroundColor;
    Brush.Style := bsSolid;
    FillRect(Rect(0, 0, FYImageX, FYImageY));
    TempColor := RGB(255 - GetRValue(FBackGroundColor),
      255 - GetGValue(FBackGroundColor), 255 - GetBValue(FBackGroundColor));
    Pen.Color := TempColor;
    Font.Color := TempColor;
    MainForm.Show;
  end;
end;

{ If ImageDoneAlarmb then DoImageDone; }

procedure TMainForm.DoImageDone;
var
  PathS: string;
begin
  if ImageDoneAlarmb then
  begin
    if ImageDoneAlarm.Checked then
    begin
      Beep;
    end
    else if ImageDoneVoices.Checked then
    begin
      PathS := ExtractFilePath(Application.EXEName) + 'DONE.WAV';
      PlaySound(PathS);
    end;
  end;
  BitmapF.assign(Image2.Picture.Bitmap);
  if WasZooming then
  begin
    Image2.Picture.Bitmap.Width := ZoomImageX;
    Image2.Picture.Bitmap.Height := ZoomImageY;
    Image2.Picture.Bitmap.Canvas.CopyMode := cmSrcCopy;
    Image2.Picture.Bitmap.Canvas.CopyRect(ZDAllRect, BitmapF.Canvas, ZSARect);
    { ZSARect := Rect(0,0,
      Image2.Width-1,Image2.Height-1);
      ZDAllRect := Rect(0,0,
      ScrollBox1.Width-4,ScrollBox1.Height-4); }

    WasZooming := False;
    ZoomingOn := True;
  end;
  Screen.Cursor := crDefault; { Always restore to normal }
end;

(* *********************************************************** *)
procedure TMainForm.ResizeImageClick(Sender: TObject);
var
  a, b: Double;
  ResizeX, ResizeY, Code: Integer;
  Arect, AllRect: TRect;
begin
  if (ResizeForm.ShowModal = mrOK) then
  begin
    val(ResizeForm.HeightEdit.Text, ResizeY, Code);
    val(ResizeForm.WidthEdit.Text, ResizeX, Code);
    a := ((3 * (ResizeX)) / 4);
    b := (((ResizeY) - a));
    if (a > (ResizeY)) then
      a := ResizeY + b
    else
      a := ResizeY - b;
    ResizeY := round(a);
    AllRect := Rect(0, 0, Image2.Width - 1, Image2.Height - 1);
    UnZoom;
    BitmapF.assign(Image2.Picture.Bitmap);
    Arect := Rect(0, 0, ResizeX - 1, ResizeY - 1);
    SetStretchBltMode(Image2.Picture.Bitmap.Canvas.Handle, HALFTONE
      { STRETCH_DELETESCANS } );
    { BOOL SetBrushOrgEx(
      HDC hdc,	// handle of device context
      int nXOrg,	// x-coordinate of new origin
      int nYOrg,	// y-coordinate of new origin
      LPPOINT lppt 	// points to previous brush origin
      ); } SetBrushOrgEx(Image2.Picture.Bitmap.Canvas.Handle, 0, 0, nil);
    Image2.Picture.Bitmap.Canvas.CopyMode := cmSrcCopy;
    { procedure CopyRect(Dest: TRect; Canvas: TCanvas; Source: TRect); }
    Image2.Picture.Bitmap.Canvas.CopyRect(Arect, BitmapF.Canvas, AllRect);
    Image2.Picture.Bitmap.Width := ResizeX;
    Image2.Picture.Bitmap.Height := ResizeY;
    BitmapF.assign(Image2.Picture.Bitmap);
    FYImageX := ResizeX;
    FYImageY := ResizeY;
    SetImageSize;
  end;
end;

(* *********************************************************** *)
{ Change Image Size }
procedure TMainForm.OverBit;
begin
  { kill old bitmap(s) ? }
  BitmapF.Free;
  BitmapF := TBitmap.Create; { construct the bitmap object }
  { property PixelFormat: TPixelFormat;
    type TPixelFormat = (pfDevice, pf1bit, pf4bit, pf8bit, pf15bit, pf16bit,
    pf24bit, pf32bit, pfCustom); }
  BitmapF.PixelFormat := MyPixelFormat; { pf24bit; }
  BitmapF.Width := FYImageX; { assign the initial width... }
  BitmapF.Height := FYImageY; { ...and the initial height }
  Image2.Picture.Graphic := BitmapF;
  { VImage.Picture.Graphic := BitmapF; }
  { RESET ZOOM STUFF }
  { assign the bitmap to the image control }
  ImageZoomRatio := 0;
  ImageZoomRatioEx := 0;
  ZoomingOn := False;
  bZoomMousing := False;
  WasZooming := False;
end;

(* *********************************************************** *)

(* *********************************************************** *)
(*
  begin
  Application.Initialize;
  with TSplashScreen.Create(nil) do
  try
  ProgressBar1.Max := 100;
  Show;   // show a splash screen contain ProgressBar control
  Update; // force display of Form5

  Application.Title := 'Fractl3D';
  Application.CreateForm(TMainForm, MainForm);
  ProgressBar1.StepIt;
  Application.CreateForm(TFractalForm, FractalForm);
  ProgressBar1.StepIt;
  Application.CreateForm(TMathForm, MathForm);
  ProgressBar1.StepIt;
  Application.CreateForm(TAnnotationForm, AnnotationForm);

  Application.CreateForm(TDIYStyleForm, DIYStyleForm);
  ProgressBar1.StepIt;
  Application.CreateForm(TJPEGForm, JPEGForm);

  Application.CreateForm(TResizeForm, ResizeForm);
  ProgressBar1.StepIt;
  Application.CreateForm(TSystemInfoForm, SystemInfoForm);
  ProgressBar1.StepIt;
  Application.CreateForm(TXYZ3DForm, XYZ3DForm);

  Application.CreateForm(TAboutBox, AboutBox);
  ProgressBar1.StepIt;
  Application.CreateForm(TFMJForm, FMJForm);
  ProgressBar1.StepIt;
  Application.CreateForm(TXYZGL, XYZGL);
  ProgressBar1.StepIt;
  Application.CreateForm(TMathGL, MathGL);
  ProgressBar1.StepIt;
  finally
  Free;
  end;
  Application.Run;
  end. *)
(* *********************************************************** *)

end.
