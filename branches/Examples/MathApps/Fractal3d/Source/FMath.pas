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
unit FMath;
{MyPixelFormat; pf24bit; PByteArray
PixelLine
        Pixelx:= (colx*3);
pf24bit =3
pf32bit =4}
interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  Printers, ComCtrls, StdCtrls, ExtCtrls,
  Buttons, ExtDlgs, Grids, ColorGrd,
  MPlayer, Clipbrd {, TMultiMP, MMOpen, TMultiP};

type
  TFractalForm = class(TForm)
    PageControl1: TPageControl;
    Mandelbrot: TTabSheet;
    Newton: TTabSheet;
    Dragons: TTabSheet;
    Phoenix: TTabSheet;
    Color16Picker: TTabSheet;
    FMSetRG: TRadioGroup;
    FMFXMaxEdit: TEdit;
    FMFXMinEdit: TEdit;
    FMFYMaxEdit: TEdit;
    FMFYMinEdit: TEdit;
    FMFHQEdit: TEdit;
    FMFVPEdit: TEdit;
    FMSaveEdit: TEdit;
    FMColorRG: TRadioGroup;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    FMFileEdit: TEdit;
    Label9: TLabel;
    FractalMFileOpen: TSpeedButton;
    FMJuliaFileSave: TSpeedButton;
    FMDeleteBtn: TSpeedButton;
    Label11: TLabel;
    Label12: TLabel;
    LobsterSets: TSpeedButton;
    NewtonSets: TSpeedButton;
    Label14: TLabel;
    FractalBFileOpen: TSpeedButton;
    FBFileEdit: TEdit;
    Label15: TLabel;
    FractalBFileSave: TSpeedButton;
    FBSaveEdit: TEdit;
    Label16: TLabel;
    FBFXMaxEdit: TEdit;
    Label17: TLabel;
    FBFXMinEdit: TEdit;
    Label18: TLabel;
    FBFYMaxEdit: TEdit;
    Label19: TLabel;
    FBFYMinEdit: TEdit;
    Label20: TLabel;
    FBFHQEdit: TEdit;
    Label21: TLabel;
    FBFVPEdit: TEdit;
    FNTerminator: TSpeedButton;
    DragonSets: TSpeedButton;
    DragonPoints: TSpeedButton;
    Label23: TLabel;
    FractalDFileOpen: TSpeedButton;
    FDFileEdit: TEdit;
    Label24: TLabel;
    FractalDFileSave: TSpeedButton;
    FDSaveEdit: TEdit;
    Label25: TLabel;
    FDFXMaxEdit: TEdit;
    Label26: TLabel;
    FDFXMinEdit: TEdit;
    Label27: TLabel;
    FDFYMaxEdit: TEdit;
    Label28: TLabel;
    FDFYMinEdit: TEdit;
    Label29: TLabel;
    FDFHQEdit: TEdit;
    Label30: TLabel;
    FDFVPEdit: TEdit;
    FDTerminator: TSpeedButton;
    PhoenixSets: TSpeedButton;
    PhoenixPoints: TSpeedButton;
    Label32: TLabel;
    FractalPFileOpen: TSpeedButton;
    FPFileEdit: TEdit;
    Label33: TLabel;
    FractalPFileSave: TSpeedButton;
    FPSaveEdit: TEdit;
    Label34: TLabel;
    FPFXMaxEdit: TEdit;
    Label35: TLabel;
    FPFXMinEdit: TEdit;
    Label36: TLabel;
    FPFYMaxEdit: TEdit;
    Label37: TLabel;
    FPFYMinEdit: TEdit;
    Label38: TLabel;
    FPFHQEdit: TEdit;
    Label39: TLabel;
    FPFVPEdit: TEdit;
    FPTerminator: TSpeedButton;
    Label42: TLabel;
    Label43: TLabel;
    NewtonHelp: TSpeedButton;
    DragonHelp: TSpeedButton;
    PhoenixHelp: TSpeedButton;
    Label13: TLabel;
    FBEdit: TEdit;
    Label22: TLabel;
    FDEdit: TEdit;
    Label31: TLabel;
    FPEdit: TEdit;
    Label7: TLabel;
    FMEdit: TEdit;
    Label44: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    RecordReader: TTabSheet;
    Memo1: TMemo;
    RecordDeleter: TSpeedButton;
    RecordSaver: TSpeedButton;
    RecordOpener: TSpeedButton;
    Label58: TLabel;
    RecordPrint: TSpeedButton;
    Label60: TLabel;
    ColorPicker: TTabSheet;
    ColorDialog1: TColorDialog;
    DrawGrid1: TDrawGrid;
    ColorSaver: TSpeedButton;
    ColorFill: TSpeedButton;
    ColorClears: TSpeedButton;
    ColorFiler: TSpeedButton;
    RecordHelp: TSpeedButton;
    ColorsHelp: TSpeedButton;
    ColorFileEdit: TEdit;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    PrintDialog1: TPrintDialog;
    Color256Setter: TSpeedButton;
    FRR_CopyToClip: TSpeedButton;
    FRRClearMemo: TSpeedButton;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Out3D: TTabSheet;
    FBSetRG: TRadioGroup;
    FDSetRG: TRadioGroup;
    FPSetRG: TRadioGroup;
    FBColorRG: TRadioGroup;
    FDColorRG: TRadioGroup;
    FPColorRG: TRadioGroup;
    ColorInverter: TSpeedButton;
    ColorSubvert: TSpeedButton;
    DragonBoxer: TSpeedButton;
    PhoenixBoxer: TSpeedButton;
    BBoxer: TSpeedButton;
    BPointer: TSpeedButton;
    DragonPointer: TSpeedButton;
    PhoenixPointer: TSpeedButton;
    SanMarcosDragon: TSpeedButton;
    FDOColorRG: TRadioGroup;
    Label56: TLabel;
    FDOEdit: TEdit;
    Label57: TLabel;
    FDOFileEdit: TEdit;
    FDOSaveEdit: TEdit;
    FractalDOFileSave: TSpeedButton;
    FractalDOFileOpen: TSpeedButton;
    Label59: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label63: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    Label67: TLabel;
    FDOFVPEdit: TEdit;
    FDOFHQEdit: TEdit;
    FDOFYMinEdit: TEdit;
    FDOFYMaxEdit: TEdit;
    FDOFXMinEdit: TEdit;
    FDOFXMaxEdit: TEdit;
    FDOTerminator: TSpeedButton;
    Label68: TLabel;
    Label69: TLabel;
    DrawGrid2: TDrawGrid;
    Color16Edit: TEdit;
    Color16Open: TSpeedButton;
    Color16Save: TSpeedButton;
    Color16Help: TSpeedButton;
    Color16Clear: TSpeedButton;
    Color16Set: TSpeedButton;
    Color16Random: TSpeedButton;
    Color16Subvert: TSpeedButton;
    Color16Invert: TSpeedButton;
    Blanket: TTabSheet;
    Color16SubversionRG: TRadioGroup;
    FDOHelp: TSpeedButton;
    FDOSetRG: TRadioGroup;
    FDOSets: TSpeedButton;
    FDOBoxer: TSpeedButton;
    FDOPoints: TSpeedButton;
    FDOPointer: TSpeedButton;
    Label70: TLabel;
    Label71: TLabel;
    Label41: TLabel;
    Label40: TLabel;
    Label10: TLabel;
    Label72: TLabel;
    Panel1: TPanel;
    BMediaOpen: TSpeedButton;
    BlanketCapture: TSpeedButton;
    BlanketHelp: TSpeedButton;
    Animate1: TAnimate;
    BlobsterSets: TSpeedButton;
    TadpoleSets: TSpeedButton;
    Slobster: TSpeedButton;
    FrogSets: TSpeedButton;
    Clobster: TSpeedButton;
    ToadSets: TSpeedButton;
    Label50: TLabel;
    Label140: TLabel;
    Mandel3DPanel: TPanel;
    MandelbrotSets: TSpeedButton;
    MandelbrotSets3D: TSpeedButton;
    MandelBoxer: TSpeedButton;
    MandelbrotHelp: TSpeedButton;
    JuliaPoints: TSpeedButton;
    JuliaPoints3D: TSpeedButton;
    JuliaPointer: TSpeedButton;
    Label51: TLabel;
    BMediaSave: TSpeedButton;
    MediaPlayer1: TMediaPlayer;
    procedure FractalMFileOpenClick(Sender: TObject);
    procedure FMDeleteBtnClick(Sender: TObject);
    procedure FractalBFileOpenClick(Sender: TObject);
    procedure FractalDFileOpenClick(Sender: TObject);
    procedure FractalPFileOpenClick(Sender: TObject);
    procedure MandelbrotHelpClick(Sender: TObject);
    procedure NewtonHelpClick(Sender: TObject);
    procedure DragonHelpClick(Sender: TObject);
    procedure PhoenixHelpClick(Sender: TObject);
    procedure MandelbrotSetsClick(Sender: TObject);
    procedure JuliaPointsClick(Sender: TObject);
    procedure LobsterSetsClick(Sender: TObject);
    procedure NewtonSetsClick(Sender: TObject);
    procedure DragonPointsClick(Sender: TObject);
    procedure PhoenixPointsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DrawGrid1SelectCell(Sender: TObject; ACol, ARow:
      Integer;
      var CanSelect: Boolean);
    procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure ColorSaverClick(Sender: TObject);
    procedure ColorFillClick(Sender: TObject);
    procedure ColorClearsClick(Sender: TObject);
    procedure ColorFilerClick(Sender: TObject);
    procedure RecordSaverClick(Sender: TObject);
    procedure RecordOpenerClick(Sender: TObject);
    procedure RecordHelpClick(Sender: TObject);
    procedure RecordPrintClick(Sender: TObject);
    procedure Color256SetterClick(Sender: TObject);
    procedure FRR_CopyToClipClick(Sender: TObject);
    procedure FRRClearMemoClick(Sender: TObject);
    procedure DragonSetsClick(Sender: TObject);
    procedure PhoenixSetsClick(Sender: TObject);
    procedure FMJuliaFileSaveClick(Sender: TObject);
    procedure JuliaPointerClick(Sender: TObject);
    procedure MandelBoxerClick(Sender: TObject);
    procedure FractalBFileSaveClick(Sender: TObject);
    procedure FractalDFileSaveClick(Sender: TObject);
    procedure FractalPFileSaveClick(Sender: TObject);
    procedure ColorInverterClick(Sender: TObject);
    procedure ColorSubvertClick(Sender: TObject);
    procedure DrawGrid2DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure DrawGrid2SelectCell(Sender: TObject; ACol, ARow:
      Integer;
      var CanSelect: Boolean);
    procedure Color16OpenClick(Sender: TObject);
    procedure Color16SaveClick(Sender: TObject);
    procedure Color16ClearClick(Sender: TObject);
    procedure Color16HelpClick(Sender: TObject);
    procedure Color16RandomClick(Sender: TObject);
    procedure Color16SubvertClick(Sender: TObject);
    procedure Color16SetClick(Sender: TObject);
    procedure Color16InvertClick(Sender: TObject);
    procedure ColorsHelpClick(Sender: TObject);
    procedure BlanketHelpClick(Sender: TObject);
    procedure BMediaOpenClick(Sender: TObject);
    procedure MediaPlayer1Click(Sender: TObject; Button: TMPBtnType;
      var DoDefault: Boolean);
    procedure FractalDOFileOpenClick(Sender: TObject);
    procedure FractalDOFileSaveClick(Sender: TObject);
    procedure SanMarcosDragonClick(Sender: TObject);
    procedure MandelbrotSets3DClick(Sender: TObject);
    procedure JuliaPoints3DClick(Sender: TObject);
    procedure FDOHelpClick(Sender: TObject);
    procedure FDOSetsClick(Sender: TObject);
    procedure FDOPointsClick(Sender: TObject);
    procedure BlanketCaptureClick(Sender: TObject);
    procedure BlobsterSetsClick(Sender: TObject);
    procedure ClobsterClick(Sender: TObject);
    procedure SlobsterClick(Sender: TObject);
    procedure TadpoleSetsClick(Sender: TObject);
    procedure FrogSetsClick(Sender: TObject);
    procedure ToadSetsClick(Sender: TObject);
    procedure Roots(A, B, C: Extended);
    procedure BMediaSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure FractalSet;
  end;

var
  FractalForm: TFractalForm;
  CallMandelBrot: Boolean;

implementation

uses fUGlobal,fMain,fAbout;

{$R *.DFM}
procedure TFractalForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FractalFormX := FractalForm.left;
  FractalFormY := FractalForm.top;
  DoSaver;
end;

procedure TFractalForm.FormCreate(Sender: TObject);
begin
  left := FractalFormX;{  left := 488;}
  top := FractalFormY;{  top := 44;}
  OpenDialog1.InitialDir := FractalDir;
  SaveDialog1.InitialDir := FractalDir;
  Color16Edit.Text := Color16Name;
  ColorFileEdit.Text := Color256Name;
{  Color256Edit2.Text := Color256TooName; }  
end;

procedure TFractalForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  CallMandelBrot := True;
end;

procedure TFractalForm.MandelbrotHelpClick(Sender: TObject);
begin
  Application.HelpContext(1100);
{Julia Dragon
P= 0.383725
Q=0.147851}
end;

procedure TFractalForm.DragonHelpClick(Sender: TObject);
begin
  Application.HelpContext(1200); {}
end;

procedure TFractalForm.FDOHelpClick(Sender: TObject);
begin
  Application.HelpContext(1300);
end;

procedure TFractalForm.PhoenixHelpClick(Sender: TObject);
begin
  Application.HelpContext(1400); {}
end;

procedure TFractalForm.NewtonHelpClick(Sender: TObject);
begin
  Application.HelpContext(1500); {}
end;

procedure TFractalForm.BlanketHelpClick(Sender: TObject);
begin
  Application.HelpContext(1600);
end;

procedure TFractalForm.RecordHelpClick(Sender: TObject);
begin
  Application.HelpContext(1700); {HELP}
end;

procedure TFractalForm.Color16HelpClick(Sender: TObject);
begin
  Application.HelpContext(1800); {}
end;

procedure TFractalForm.ColorsHelpClick(Sender: TObject);
begin
  Application.HelpContext(1900);
end;

procedure TFractalForm.FMDeleteBtnClick(Sender: TObject);
var
  DupS, s: string; F: file; I: Integer; Whatsit: Word;
begin
  Whatsit := mrYes;
  OpenDialog1.Options := [ofAllowMultiSelect, ofFileMustExist];
  OpenDialog1.Title := 'Delete File Selection';
  OpenDialog1.Filter := 'BMP files|*.BMP';
  OpenDialog1.FileName := '*.BMP';
  if (OpenDialog1.Execute) then begin
    with OpenDialog1.Files do
      for I := 0 to Count - 1 do begin
        DupS := Strings[I];
        try
          AssignFile(F, DupS);
          Reset(F);
{        MessageDlg.mbYesToAll:=mrAll;}
          if ((Whatsit = mrYes) or (Whatsit = mrNo)) then
            Whatsit := MessageDlg('Erase ' + DupS + '?',
              mtConfirmation, [mbYes, mbYesToAll, mbNo, mbNoToAll
                { mbCancel}], 0);
          if ((Whatsit = mrYes) or (Whatsit = mrYesToAll {mrAll}))
            then
          begin
            CloseFile(F);
            Erase(F);
        {Erase the bin, tmp and rcd files too}
            s := ChangeFileExt(DupS, '.TMP');
            if FileExists(s) then begin
              AssignFile(F, s); Reset(F);
              CloseFile(F); Erase(F); end;
            s := ChangeFileExt(DupS, '.RCD');
            if FileExists(s) then begin
              AssignFile(F, s); Reset(F);
              CloseFile(F); Erase(F); end;
            s := ChangeFileExt(DupS, '.BIN');
            if FileExists(s) then begin
              AssignFile(F, s); Reset(F);
              CloseFile(F); Erase(F); end;
          end;
        except
          on EInOutError do
            MessageDlg('File I/O error.' + DupS, mtError, [mbOk], 0);
        end;
      end; end; end;


{Make the file open and save be generic by changing the
dialog name prior to calling the routine}

procedure TFractalForm.FMJuliaFileSaveClick(Sender: TObject);
begin
  FractalFilename := FMSaveEdit.text;
  MainForm.Image2.Picture.SaveToFile(FractalFilename);
  FMFileEdit.Text := FractalFilename;
  FMSaveEdit.Text := FractalFilename;
end;

procedure TFractalForm.FractalMFileOpenClick(Sender: TObject);
begin {change the dialog title}
  MainForm.OpenPictureDialog1.FileName := 'F_MANSET000.BMP';
  MainForm.OpenPictureDialog1.Title :=
    'Open a Julia Mandel BMP File';
  if (FMSetRG.Itemindex = 4) then Julia := True;
  MainForm.FileOpener;
  FMFileEdit.Text := FractalFilename;
  FMSaveEdit.Text := FractalFilename;
(* inside opener
    If (FMSetRG.Itemindex = 3)then MainForm.ReadDataFile;
        {Read the data File and Check
        if File is finished
        Mandelbrot or Julian or what (file name)}
    If (FMSetRG.Itemindex = 2)then
        begin MainForm.ReadDataFile; Fractaling:=True; end;
                            {Get the Coordinates}
*)
end;

procedure TFractalForm.FractalBFileSaveClick(Sender: TObject);
begin
  FractalFilename := FBSaveEdit.text;
  MainForm.Image2.Picture.SaveToFile(FractalFilename);
  FBFileEdit.Text := FractalFilename;
  FBSaveEdit.Text := FractalFilename;
end;

procedure TFractalForm.FractalBFileOpenClick(Sender: TObject);
begin {change the dialog title}
  MainForm.OpenPictureDialog1.Title := 'Newton File Selection';
  MainForm.OpenPictureDialog1.FileName := 'F_NEWTON000.BMP';
  if (FBSetRG.Itemindex = 4) then Julia := True;
  MainForm.FileOpener;
  FBFileEdit.Text := FractalFilename;
  FBSaveEdit.Text := FractalFilename;
end;

procedure TFractalForm.FractalDFileSaveClick(Sender: TObject);
begin
  FractalFilename := FDSaveEdit.text;
  MainForm.Image2.Picture.SaveToFile(FractalFilename);
  FDFileEdit.Text := FractalFilename;
  FDSaveEdit.Text := FractalFilename;
end;

procedure TFractalForm.FractalDFileOpenClick(Sender: TObject);
begin {change the dialog title}
  MainForm.OpenPictureDialog1.Title := 'Dragon File Selection';
  MainForm.OpenPictureDialog1.FileName := 'F_DRGSET000.BMP';
  if (FDSetRG.Itemindex = 4) then Julia := True;
  MainForm.FileOpener;
  FDFileEdit.Text := FractalFilename;
  FDSaveEdit.Text := FractalFilename;
end;

procedure TFractalForm.FractalPFileSaveClick(Sender: TObject);
begin
  FractalFilename := FPSaveEdit.text;
  MainForm.Image2.Picture.SaveToFile(FractalFilename);
  FPFileEdit.Text := FractalFilename;
  FPSaveEdit.Text := FractalFilename;
end;

procedure TFractalForm.FractalPFileOpenClick(Sender: TObject);
begin {change the dialog title}
  MainForm.OpenPictureDialog1.Title := 'Phoenix File Selection';
  MainForm.OpenPictureDialog1.FileName := 'F_PHESET000.BMP';
  if (FPSetRG.Itemindex = 4) then Julia := True;
  MainForm.FileOpener;
  FPFileEdit.Text := FractalFilename;
  FPSaveEdit.Text := FractalFilename;
end;


{TMainForm.Image1.Hint:= 'Click to rotate';}
{To draw on a bitmap, use the image control’s canvas and attach the mouse-event handlers to the appropriate events in the image control. Typically you would use region operations (fills, rectangles, polylines, and so on). These are fast and efficient methods of drawing.
An efficient way to draw images when you need to access individual pixels is to use the bitmap ScanLine property. For general-purpose usage, you can set up the bitmap pixel format to 24 bits and then treat the pointer returned from ScanLine as an array of RGB. Otherwise, you will need to know the native format of the ScanLine property. This example shows how to use ScanLine to get pixels one line at a time.
procedure TForm1.Button1Click(Sender: TObject);
// This example shows drawing directly to the Bitmap}

procedure TFractalForm.FractalSet;
var S: string;
begin
  case LastKnownFunction of
    0: begin
        S := 'Base Unknown Caller'; Memo1.Lines.Add(S);
        Memo1.Lines.Add(FractalFilename);
        Str(FXMax: 24: 20, S); Memo1.Lines.Add(S);
        Str(FXMin: 24: 20, S); Memo1.Lines.Add(S);
        Str(FYMax: 24: 20, S); Memo1.Lines.Add(S);
        Str(FYMin: 24: 20, S); Memo1.Lines.Add(S);
        Str(FHQ: 24: 20, S); Memo1.Lines.Add(S);
        Str(FVP: 24: 20, S); Memo1.Lines.Add(S);
        Str(Max_Iterations, S); Memo1.Lines.Add(S);
        Str(Color_Option, S); S := S + '  : Color Option';
        Memo1.Lines.Add(S);
      end;
    10..12: begin
        if LastKnownFunction = 10 then S := 'MandelBrot Caller'
        else S := 'Julia Caller';
        Memo1.Lines.Add(S);
        FMFileEdit.Text := FractalFilename;
        FMSaveEdit.Text := FractalFilename;
        Memo1.Lines.Add(FractalFilename);
        Str(FXMax: 24: 20, S); FMFXMaxEdit.Text := S;
          Memo1.Lines.Add(S);
        Str(FXMin: 24: 20, S); FMFXMinEdit.Text := S;
          Memo1.Lines.Add(S);
        Str(FYMax: 24: 20, S); FMFYMaxEdit.Text := S;
          Memo1.Lines.Add(S);
        Str(FYMin: 24: 20, S); FMFYMinEdit.Text := S;
          Memo1.Lines.Add(S);
        Str(FHQ: 24: 20, S); FMFHQEdit.Text := S; Memo1.Lines.Add(S);
        Str(FVP: 24: 20, S); FMFVPEdit.Text := S; Memo1.Lines.Add(S);
        Str(Max_Iterations, S); FMEdit.Text := S; Memo1.Lines.Add(S);
        FMColorRG.ItemIndex := Color_Option;
        Str(Color_Option, S); S := S + '  : Color Option';
        Memo1.Lines.Add(S);
        Str(FMSetRG.Itemindex, S); S := S + '  : Set Selection';
        Memo1.Lines.Add(S);
      end;
    20..22: begin {Newton}
        if LastKnownFunction = 20 then S := 'Newton Lobster Caller'
        else S := 'Newton Frog Caller';
        Memo1.Lines.Add(S);
        FBFileEdit.Text := FractalFilename;
        FBSaveEdit.Text := FractalFilename;
        Memo1.Lines.Add(FractalFilename);
        Str(FXMax: 24: 20, S); FBFXMaxEdit.Text := S;
          Memo1.Lines.Add(S);
        Str(FXMin: 24: 20, S); FBFXMinEdit.Text := S;
          Memo1.Lines.Add(S);
        Str(FYMax: 24: 20, S); FBFYMaxEdit.Text := S;
          Memo1.Lines.Add(S);
        Str(FYMin: 24: 20, S); FBFYMinEdit.Text := S;
          Memo1.Lines.Add(S);
        Str(FHQ: 24: 20, S); FBFHQEdit.Text := S; Memo1.Lines.Add(S);
        Str(FVP: 24: 20, S); FBFVPEdit.Text := S; Memo1.Lines.Add(S);
        Str(Max_Iterations, S); FBEdit.Text := S; Memo1.Lines.Add(S);
        FBColorRG.ItemIndex := Color_Option;
        Str(Color_Option, S); S := S + '  : Color Option';
        Memo1.Lines.Add(S);
        Str(FBSetRG.Itemindex, S); S := S + '  : Set Selection';
        Memo1.Lines.Add(S);
      end;
    30..32: begin {Dragon}
        if LastKnownFunction = 30 then S := 'Dragon Caller'
        else S := 'Dragon point Caller';
        Memo1.Lines.Add(S);
        FDFileEdit.Text := FractalFilename;
        FDSaveEdit.Text := FractalFilename;
        Memo1.Lines.Add(FractalFilename);
        Str(FXMax: 24: 20, S); FDFXMaxEdit.Text := S;
          Memo1.Lines.Add(S);
        Str(FXMin: 24: 20, S); FDFXMinEdit.Text := S;
          Memo1.Lines.Add(S);
        Str(FYMax: 24: 20, S); FDFYMaxEdit.Text := S;
          Memo1.Lines.Add(S);
        Str(FYMin: 24: 20, S); FDFYMinEdit.Text := S;
          Memo1.Lines.Add(S);
        Str(FHQ: 24: 20, S); FDFHQEdit.Text := S; Memo1.Lines.Add(S);
        Str(FVP: 24: 20, S); FDFVPEdit.Text := S; Memo1.Lines.Add(S);
        Str(Max_Iterations, S); FDEdit.Text := S; Memo1.Lines.Add(S);
        FDColorRG.ItemIndex := Color_Option;
        Str(Color_Option, S); S := S + '  : Color Option';
        Memo1.Lines.Add(S);
        Str(FDSetRG.Itemindex, S); S := S + '  : Set Selection';
        Memo1.Lines.Add(S);
      end;
    40..42: begin {Phoenix}
        if LastKnownFunction = 40 then S := 'Phoenix Caller'
        else S := 'Phoenix point Caller';
        Memo1.Lines.Add(S);
        FPFileEdit.Text := FractalFilename;
        FPSaveEdit.Text := FractalFilename;
        Memo1.Lines.Add(FractalFilename);
        Str(FXMax: 24: 20, S); FPFXMaxEdit.Text := S;
          Memo1.Lines.Add(S);
        Str(FXMin: 24: 20, S); FPFXMinEdit.Text := S;
          Memo1.Lines.Add(S);
        Str(FYMax: 24: 20, S); FPFYMaxEdit.Text := S;
          Memo1.Lines.Add(S);
        Str(FYMin: 24: 20, S); FPFYMinEdit.Text := S;
          Memo1.Lines.Add(S);
        Str(FHQ: 24: 20, S); FPFHQEdit.Text := S; Memo1.Lines.Add(S);
        Str(FVP: 24: 20, S); FPFVPEdit.Text := S; Memo1.Lines.Add(S);
        Str(Max_Iterations, S); FPEdit.Text := S; Memo1.Lines.Add(S);
        FPColorRG.ItemIndex := Color_Option;
        Str(Color_Option, S); S := S + '  : Color Option';
        Memo1.Lines.Add(S);
        Str(FPSetRG.Itemindex, S); S := S + '  : Set Selection';
        Memo1.Lines.Add(S);
      end;
    66: begin
        S := 'File Write Caller'; Memo1.Lines.Add(S);
{ FMFileEdit.Text:=FractalFilename;}
        Memo1.Lines.Add(FractalFilename);
        Str(FXMax: 24: 20, S); Memo1.Lines.Add(S);
        Str(FXMin: 24: 20, S); Memo1.Lines.Add(S);
        Str(FYMax: 24: 20, S); Memo1.Lines.Add(S);
        Str(FYMin: 24: 20, S); Memo1.Lines.Add(S);
        Str(FHQ: 24: 20, S); Memo1.Lines.Add(S);
        Str(FVP: 24: 20, S); Memo1.Lines.Add(S);
        Str(Max_Iterations, S); Memo1.Lines.Add(S);
        Str(Color_Option, S); S := S + '  : Color Option';
        Memo1.Lines.Add(S);
      end;
    69: begin
        S := 'Mouse Caller'; Memo1.Lines.Add(S);
{ FMFileEdit.Text:=FractalFilename;}
        Memo1.Lines.Add(FractalFilename);
        Str(FXMax: 24: 20, S); Memo1.Lines.Add(S);
        Str(FXMin: 24: 20, S); Memo1.Lines.Add(S);
        Str(FYMax: 24: 20, S); Memo1.Lines.Add(S);
        Str(FYMin: 24: 20, S); Memo1.Lines.Add(S);
        Str(FHQ: 24: 20, S); Memo1.Lines.Add(S);
        Str(FVP: 24: 20, S); Memo1.Lines.Add(S);
        Str(Max_Iterations, S); Memo1.Lines.Add(S);
        Str(Color_Option, S); S := S + '  : Color Option';
        Memo1.Lines.Add(S);
      end;
    99: begin
        S := 'File Loader Caller'; Memo1.Lines.Add(S);
{ FMFileEdit.Text:=FractalFilename;}
        Memo1.Lines.Add(FractalFilename);
        Str(FXMax: 24: 20, S); Memo1.Lines.Add(S);
        Str(FXMin: 24: 20, S); Memo1.Lines.Add(S);
        Str(FYMax: 24: 20, S); Memo1.Lines.Add(S);
        Str(FYMin: 24: 20, S); Memo1.Lines.Add(S);
        Str(FHQ: 24: 20, S); Memo1.Lines.Add(S);
        Str(FVP: 24: 20, S); Memo1.Lines.Add(S);
        Str(Max_Iterations, S); Memo1.Lines.Add(S);
        Str(Color_Option, S); S := S + '  : Color Option';
        Memo1.Lines.Add(S);
      end;
  else begin
      S := 'Really Unknown Caller'; Memo1.Lines.Add(S);
      Memo1.Lines.Add(FractalFilename);
      Str(FXMax: 24: 20, S); Memo1.Lines.Add(S);
      Str(FXMin: 24: 20, S); Memo1.Lines.Add(S);
      Str(FYMax: 24: 20, S); Memo1.Lines.Add(S);
      Str(FYMin: 24: 20, S); Memo1.Lines.Add(S);
      Str(FHQ: 24: 20, S); Memo1.Lines.Add(S);
      Str(FVP: 24: 20, S); Memo1.Lines.Add(S);
      Str(Max_Iterations, S); Memo1.Lines.Add(S);
      Str(Color_Option, S); S := S + '  : Color Option';
      Memo1.Lines.Add(S);
    end;
  end; {of case}
end;

procedure TFractalForm.MandelBoxerClick(Sender: TObject);
var MyFilesExtension: string;
begin
  if (FractalFilename = 'F_000000000.BMP') then
    DoMessages(30061) else
  begin
    MyFilesExtension := ExtractFileName(FractalFilename);
    if (MyFilesExtension[6] = 'S') then
    begin
      Julia := False; bPointing := False;
      Fractaling := True; {On the Fly starter}
    end;
  end;
end;

procedure TFractalForm.MandelbrotSetsClick(Sender: TObject);
var
  Code, i, SetSelection, Pixelx,
    maxcolx, maxrowy, FMcolor, rowy, colx: integer;
  cos_theta, sin_theta, theta,
    Xtemp, YTemp, Temp, X, Y, Xsquare, Ysquare,
    max_size, deltaP, deltaQ, P: Extended;
  FractalFileMatrix, S: string;
  F_File: file of Smallint; {Integer;}
  Q: array[0..3072] of Extended;
  Bitmap: TBitmap;
  PixelLine: PByteArray;
begin
  MainForm.DoImageStart;
  MainForm.ProgressBar1.Visible := True;
  Application.ProcessMessages;
  CallMandelBrot := FALSE;
  LastKnownFunction := 10;
  Randomize;
  Bitmap := TBitmap.create;
  try
    Bitmap.Assign(MainForm.Image2.Picture.Bitmap);
    Bitmap.PixelFormat := MyPixelFormat; {pf24bit;}
      {   FYImageX:= 640;FYImageY:= 480;}
    Bitmap.Width := FYImageX; { assign the initial width... }
    Bitmap.Height := FYImageY; { ...and the initial height }
    SetSelection := FMSetRG.Itemindex;
    case SetSelection of
      0: begin
          FXMax := 1.6; {1.0;}
          FXMin := -1.6; {-2.20 ;} {32/24=1.33}
          FYMax := 1.2;
          FYMin := -1.2;
          FHQ := -0.003483;
          FVP := 0.268545;
          Color_Option := FMColorRG.ItemIndex;
          val(FMEdit.Text, Max_Iterations, Code);
          Start_Col := 0;
          FractalFilename := 'F_MANSET000.BMP'
        end;
      1: begin
          val(FMFXMaxEdit.Text, FXMax, Code);
          Codefx(FMFXMaxEdit.Text, Code);
          val(FMFXMinEdit.Text, FXMin, Code);
          Codefx(FMFXMinEdit.Text, Code);
          val(FMFYMaxEdit.Text, FYMax, Code);
          Codefx(FMFYMaxEdit.Text, Code);
          val(FMFYMinEdit.Text, FYMin, Code);
          Codefx(FMFYMinEdit.Text, Code);
          val(FMFHQEdit.Text, FHQ, Code);
          Codefx(FMFHQEdit.Text, Code);
          val(FMFVPEdit.Text, FVP, Code);
          Codefx(FMFVPEdit.Text, Code);
          val(FMEdit.Text, Max_Iterations, Code);
          Codefx(FMEdit.Text, Code);
          Start_Col := 0;
          Color_Option := FMColorRG.ItemIndex;
          FractalFilename := 'F_MANSET000.BMP';
          MainForm.ChangeFileName;
            {to get a correct name from no input}
        end;
      2: begin
{Must have image loaded and data read...Mouse a Rectangle}
          if (FractalFilename = 'F_000000000.BMP') then
          begin
            DoMessages(30061);
            FractalFilename := 'F_000000000.BMP';
          end else
          begin
            val(FMEdit.Text, Max_Iterations, Code);
            Codefx(FMEdit.Text, Code);
            Start_Col := 0;
            Color_Option := FMColorRG.ItemIndex;
          end;
        end;
      3: begin
{Must have image loaded and data read to complete the image generation}
          if (FractalFilename = 'F_000000000.BMP') then
            DoMessages(30062);
          FractalFilename := 'F_000000000.BMP';
        end;
      4: begin
          DoMessages(30063);
        end;
    end;
    if (bJuliaBase) then begin
      FXMax := 1.6;
      FXMin := -1.6;
      FYMax := 1.2;
      FYMin := -1.2;
      FHQ := -0.003483;
      FVP := 0.268545;
      Color_Option := FMColorRG.ItemIndex;
      val(FMEdit.Text, Max_Iterations, Code);
      Start_Col := 0;
      FractalFilename := 'F_JULP__000.BMP';
    end;
{Check the X and y's to make sure the area is right side up}
    if (FXMax < FXMin) then
      begin Temp := FXMax; FXMax := FXMin; FXMin := Temp; end;
    if (FYMax < FYMin) then
      begin Temp := FYMax; FYMax := FYMin; FYMin := Temp; end;
{Begin here}
    Str(FXMax: 24: 20, S); FMFXMaxEdit.Text := S;
    Str(FXMin: 24: 20, S); FMFXMinEdit.Text := S;
    Str(FYMax: 24: 20, S); FMFYMaxEdit.Text := S;
    Str(FYMin: 24: 20, S); FMFYMinEdit.Text := S;
    Str(FHQ: 24: 20, S); FMFHQEdit.Text := S;
    Str(FVP: 24: 20, S); FMFVPEdit.Text := S;
    if (bMan3d and (not (SetSelection = 3))) then
    begin
{Set up 3D Array  ManMatrix :  array of array of Integer;}
      SetLength(ManMatrix, FYImageX, FYImageY);
      MaximumElevation := Max_Iterations;
      MinimumElevation := 0;
      ContourInterval := 0;
      Fractalgorhythym := 0;
      FileSizeX := FYImageX;
      FileSizeY := FYImageY;
    end;
    max_size := 4.0;
    maxcolx := (FYImageX - 1); {x := 0 to 639}
    maxrowy := (FYImageY - 1);
    deltaP := (FXMax - FXMin) / maxcolx;
    deltaQ := (FYMax - FYMin) / maxrowy;
    Q[0] := FYMax;
    for rowy := 1 to maxrowy do Q[rowy] := Q[rowy - 1] - deltaQ;
    P := (FXMin + (start_col * deltaP));
    for colx := start_col to maxcolx do begin
      if CallMandelBrot = True then begin
        Start_Col := colx;
        FVP := P;
{			FHQ := Q;}
        Mainform.FileSaver;
                        {Exit;} Abort;
      end;
      Mainform.ProgressBar1.Position :=
        Round((colx / (FYImageX - 1))* 100);
      Application.ProcessMessages;
      for rowy := 0 to maxrowy do begin
        PixelLine := Bitmap.ScanLine[rowy];
        X := 0;
        Y := 0;
        Xsquare := 0;
        Ysquare := 0;
        FMcolor := 1;
        while ((FMcolor < max_iterations) and
          ((Xsquare + Ysquare) < max_size)) do
        begin
          Xsquare := X * X;
          Ysquare := Y * Y;
          Y := 2 * X * Y + Q[rowy];
          X := Xsquare - Ysquare + P;
          inc(FMcolor);
        end;

        if (bMan3d and (not (SetSelection = 3))) then begin
          ManMatrix[colx, rowy] := FMcolor;
        end;
        if Color_Option = 0 then
        begin
          if ((FMcolor = (max_iterations)) and (bFractalMaxed)) then
            begin
            if (MainForm.FractalBlack1.Checked) then begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := GetRValue(FBackGroundColor);
              PixelLine[(Pixelx + 1)] := GetGValue(FBackGroundColor);
              PixelLine[(Pixelx + 2)] := GetBValue(FBackGroundColor);
            end;
          end else begin {Actual 16 color selection}
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := RGBArray[2, FMcolor mod 16];
            PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor mod 16];
            PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor mod 16];
          end; end
        else if Color_Option = 1 then
        begin
          if ((FMcolor = (max_iterations)) and (bFractalMaxed)) then
            begin
            if (MainForm.FractalBlack1.Checked) then begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := GetRValue(FBackGroundColor);
              PixelLine[(Pixelx + 1)] := GetGValue(FBackGroundColor);
              PixelLine[(Pixelx + 2)] := GetBValue(FBackGroundColor);
            end;
          end else begin {Actual 256 color selection}
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := Colors[2, FMcolor mod 256];
            PixelLine[(Pixelx + 1)] := Colors[1, FMcolor mod 256];
            PixelLine[(Pixelx + 2)] := Colors[0, FMcolor mod 256];
          end; end
        else if Color_Option = 2 then
        begin
          if ((FMcolor = (max_iterations)) and (bFractalMaxed)) then
            begin
            if (MainForm.FractalBlack1.Checked) then begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := GetRValue(FBackGroundColor);
              PixelLine[(Pixelx + 1)] := GetGValue(FBackGroundColor);
              PixelLine[(Pixelx + 2)] := GetBValue(FBackGroundColor);
            end;
          end else begin {Actual Rotating color selection}
            if FMcolor mod 2 = 0 then
            begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := Colors[2, FMcolor mod 256];
              PixelLine[(Pixelx + 1)] := Colors[1, FMcolor mod 256];
              PixelLine[(Pixelx + 2)] := Colors[0, FMcolor mod 256];
            end
            else begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := RGBArray[2, FMcolor mod 16];
              PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor mod 16];
              PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor mod 16];
            end;
          end; end
        else if Color_Option = 3 then
        begin
          if X = 0 then
            FMcolor := 2
          else
          begin
            cos_theta := abs(X) / (sqrt(X * X + Y * Y));
            sin_theta := abs(Y) / (sqrt(X * X + Y * Y));
            theta := arctan(sin_theta / cos_theta);
            if (X < 0) and (Y >= 0) then
              theta := theta + 1.5707963
            else if (X < 0) and (Y < 0) then
              theta := theta + 3.14159625
            else if (X > 0) and (Y < 0) then
              theta := theta + 4.7123889;
            if (theta >= 0) and (theta <= 3.14159625) then
              FMcolor := 2
            else
              FMcolor := 1;
          end;
          if (FMcolor = 1) then begin
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := GetBValue(FBackGroundColor);
            PixelLine[(Pixelx + 1)] := GetGValue(FBackGroundColor);
            PixelLine[(Pixelx + 2)] := GetRValue(FBackGroundColor);
          end else if (FMcolor = 2) then begin
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := Colors[2, 0];
            PixelLine[(Pixelx + 1)] := Colors[1, 0];
            PixelLine[(Pixelx + 2)] := Colors[0, 0];
          end
        end;
      end; { of row }
      P := P + deltaP;
    end; { of col }
    MainForm.Image2.Picture.Bitmap.Assign(Bitmap);
    MainForm.Image2.canvas.draw(0, 0,
      MainForm.Image2.Picture.Bitmap);
  finally
    Bitmap.free;
  end;
  if (not CallMandelBrot) then Mainform.FileSaver;
  Mainform.DoImageDone;
(*If (bMan3d and (not  (SetSelection= 3))) then Begin
{Set up 3D Array  ManMatrix :  array of array of Integer;}
SetLength(ManMatrix, FYImageX,FYImageY);
End;
If (bMan3d and (not  (SetSelection= 3))) then Begin
ManMatrix[colx,rowy]:=FMcolor;
end;
*)
  if (bMan3d and (not (SetSelection = 3)) and (not CallMandelBrot))
    then begin
    MainForm.HintPanel.Caption := 'Saving Mandelbrot 3D to disk';
    Application.ProcessMessages;
    FractalFileMatrix := ChangeFileExt(FractalFilename, '.BIN');
    AssignFile(F_File, FractalFileMatrix);
    ReWrite(F_File);
    if IoResult = 0 then begin
      for rowy := 0 to maxrowy do begin
        Mainform.ProgressBar1.Position :=
        Round((rowy / (FYImageX - 1)) * 100);
        Application.ProcessMessages;
        for colx := 0 to maxcolx do begin
        {Save to disk ManMatrix[colx,rowy];  }
          Write(F_File, ManMatrix[colx, rowy]);
        end; end;
      CloseFile(F_File);
      if (IoResult <> 0) then
        DoMessages(30064);
    end else DoMessages(30065);
{Nil out the array}
    SetLength(ManMatrix, 0, 0);
  end;
  Mainform.ProgressBar1.Position := 0;
  MainForm.ProgressBar1.Visible := False;
  Application.ProcessMessages;
  FractalSet;
end;

procedure TFractalForm.MandelbrotSets3DClick(Sender: TObject);
begin
  if bMan3d then bMan3d := false else bMan3d := True;
  if (bMan3d or bJulia3d) then Mandel3DPanel.Color := clActiveCaption
  else Mandel3DPanel.Color := clBtnFace;
end;

procedure TFractalForm.JuliaPoints3DClick(Sender: TObject);
begin
  if bJulia3d then bJulia3d := false else bJulia3d := True;
  if bJulia3d then Mandel3DPanel.Color := clActiveCaption
  else Mandel3DPanel.Color := clBtnFace;
end;
    {Beep(1000,20); ...
    BP Delphi replaced it with Beep; (no choice... no matter)
    Beep(
      Specifies the frequency, in hertz, of the sound.
                This parameter must be in the range 37 through 32,767
                (0x25 through 0x7FFF),
      Specifies the duration, in milliseconds, of the sound.}

procedure TFractalForm.JuliaPointerClick(Sender: TObject);
var MyFilesExtension: string;
begin
  if (FractalFilename = 'F_000000000.BMP') then
      DoMessages(30066) else
  begin
    MyFilesExtension := ExtractFileName(FractalFilename);
    if (MyFilesExtension[6] = 'P') then
    begin
      Julia := True;
      bPointing := True; {On the Fly starter}
    end;
  end;
end;

procedure TFractalForm.JuliaPointsClick(Sender: TObject);
var
  Code, i, SetSelection, Pixelx,
    maxcolx, maxrowy, FMcolor, rowy, colx: integer;
  cos_theta, sin_theta, theta,
    Xtemp, YTemp, Temp, X, Y, Xsquare, Ysquare, deltaX, deltaY,
    max_size, deltaP, deltaQ, Q, P: Extended;
{  Q: array[0..2305] of Extended; }
  Bitmap: TBitmap;
  PixelLine: PByteArray;
{Set up 3D Array ManMatrix :  array of array of Integer;}
  FractalFileMatrix, S: string;
  F_File: file of Smallint; {Integer;}
begin
  SetSelection := FMSetRG.Itemindex;
  if (SetSelection = 0) then
  begin
    bJuliaBase := True;
    MandelbrotSetsClick(Sender);
  end else begin
    if (FractalFilename = 'F_000000000.BMP') then
      DoMessages(30066) else
    begin
      MainForm.DoImageStart;
      MainForm.ProgressBar1.Visible := True;
      Application.ProcessMessages;
      CallMandelBrot := FALSE;
      Julia := False; bPointing := False; {We should be done by now}
      LastKnownFunction := 11;
      Randomize;
      Bitmap := TBitmap.create;
      try
        Bitmap.Assign(MainForm.Image2.Picture.Bitmap);
        Bitmap.PixelFormat := MyPixelFormat; {pf24bit;}
          {   FYImageX:= 640;FYImageY:= 480;}
        Bitmap.Width := FYImageX; { assign the initial width... }
        Bitmap.Height := FYImageY; { ...and the initial height }
        case SetSelection of
          0: begin
              FXMax := 1.6;
              FXMin := -1.6;
              FYMax := 1.2;
              FYMin := -1.2;
              FHQ := -0.003483;
              FVP := 0.268545;
              Color_Option := FMColorRG.ItemIndex;
              val(FMEdit.Text, Max_Iterations, Code);
              Start_Col := 0;
              FractalFilename := 'F_JULP__000.BMP';
              DoMessages(30067);
            end;
          1: begin
              val(FMFXMaxEdit.Text, FXMax, Code);
              CodeFX(FMFXMaxEdit.Text,Code);
              val(FMFXMinEdit.Text, FXMin, Code);
              CodeFX(FMFXMinEdit.Text,Code);
              val(FMFYMaxEdit.Text, FYMax, Code);
              CodeFX(FMFYMaxEdit.Text,Code);
              val(FMFYMinEdit.Text, FYMin, Code);
              CodeFX(FMFYMinEdit.Text,Code);
              val(FMFHQEdit.Text, FHQ, Code);
              CodeFX(FMFHQEdit.Text,Code);
              val(FMFVPEdit.Text, FVP, Code);
              CodeFX(FMFVPEdit.Text,Code);
              val(FMEdit.Text, Max_Iterations, Code);
              CodeFX(FMEdit.Text,Code);
              Start_Col := 0;
              Color_Option := FMColorRG.ItemIndex;
              FractalFilename := 'F_JULPT_000.BMP';
              MainForm.ChangeFileName;
                {to get a correct name from no input}
            end;
          2: begin
{Must have image loaded and data read...Mouse a Point}
              val(FMEdit.Text, Max_Iterations, Code);
              CodeFX(FMEdit.Text,Code);
              Start_Col := 0;
              Color_Option := FMColorRG.ItemIndex;
            end;
          3: begin
{Must have image loaded and data read to complete the image generation}
            end;
          4: begin
{Julia Base done here? what was #0?}
              val(FMEdit.Text, Max_Iterations, Code);
              CodeFX(FMEdit.Text,Code);
              Start_Col := 0;
              Color_Option := FMColorRG.ItemIndex;
              FractalFilename := 'F_JULP__000.BMP'
            end;
        end;
{Check the X and y's to make sure the area is right side up}
        if (FXMax < FXMin) then
          begin Temp := FXMax; FXMax := FXMin; FXMin := Temp; end;
        if (FYMax < FYMin) then
          begin Temp := FYMax; FYMax := FYMin; FYMin := Temp; end;
{Begin here}
        Str(FXMax: 24: 20, S); FMFXMaxEdit.Text := S;
        Str(FXMin: 24: 20, S); FMFXMinEdit.Text := S;
        Str(FYMax: 24: 20, S); FMFYMaxEdit.Text := S;
        Str(FYMin: 24: 20, S); FMFYMinEdit.Text := S;
        Str(FHQ: 24: 20, S); FMFHQEdit.Text := S;
        Str(FVP: 24: 20, S); FMFVPEdit.Text := S;

        if (bMan3d and (not (SetSelection = 3))) then
        begin
{Set up 3D Array  ManMatrix :  array of array of Integer;}
          SetLength(ManMatrix, FYImageX, FYImageY);
          MaximumElevation := Max_Iterations;
          MinimumElevation := 0;
          ContourInterval := 0;
          Fractalgorhythym := 0;
          FileSizeX := FYImageX;
          FileSizeY := FYImageY;
        end;
        P := FVP;
        Q := FHQ;
        max_size := 4.0;
        maxcolx := (FYImageX - 1); {x := 0 to 639}
        maxrowy := (FYImageY - 1);
        {P} deltaX := (FXMax - FXMin) / maxcolx;
        {Q} deltaY := (FYMax - FYMin) / maxrowy;
        for colx := start_col to maxcolx do begin
          if CallMandelBrot = True then begin
            Start_Col := colx;
            FVP := P;
            FHQ := Q;
            Mainform.FileSaver;
                        {Exit;} Abort;
          end;
          Mainform.ProgressBar1.Position :=
          Round((colx / (FYImageX - 1)) * 100);
          Application.ProcessMessages;
          for rowy := 0 to maxrowy do begin
            PixelLine := Bitmap.ScanLine[rowy];
            X := FXMin + colx * deltaX;
            Y := FYMax - rowy * deltaY;
            Xsquare := 0;
            Ysquare := 0;
            FMcolor := 0;
            while (FMcolor < max_iterations) and
              ((Xsquare + Ysquare) < max_size) do
            begin
              YTemp := 2 * X * Y;
              Xsquare := X * X;
              Ysquare := Y * Y;
              Y := YTemp + Q;
              X := Xsquare - Ysquare + P;
              inc(FMcolor);
            end;
            if (bJulia3d and (not (SetSelection = 3))) then
            begin
              ManMatrix[colx, rowy] := FMcolor;
            end;
            if Color_Option = 0 then
            begin
              if (FMcolor >= (max_iterations)) then
                begin {Actual 16 color selection}
                FMcolor := (Round((Xsquare + Ysquare) * 14) mod 14) +
                  1;
                Pixelx := (colx * PixelScanSize);
                PixelLine[Pixelx] := RGBArray[2, FMcolor];
                PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor];
                PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor];
              end else
                if ((bFractalMaxed)) then begin
                  if (MainForm.FractalBlack1.Checked) then begin
                    Pixelx := (colx * PixelScanSize);
                    PixelLine[Pixelx] := GetRValue(FBackGroundColor);
                    PixelLine[(Pixelx + 1)] :=
                      GetGValue(FBackGroundColor);
                    PixelLine[(Pixelx + 2)] :=
                      GetBValue(FBackGroundColor);
                  end;
                end
                else begin
                  FMcolor := 0;
                  Pixelx := (colx * PixelScanSize);
                  PixelLine[Pixelx] := RGBArray[2, FMcolor];
                  PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor];
                  PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor];
                end; end
            else if Color_Option = 4 then
            begin
              if ((FMcolor < (max_iterations)) and (bFractalMaxed))
                then begin
                if (MainForm.FractalBlack1.Checked) then begin
                  Pixelx := (colx * PixelScanSize);
                  PixelLine[Pixelx] := GetRValue(FBackGroundColor);
                  PixelLine[(Pixelx + 1)] :=
                    GetGValue(FBackGroundColor);
                  PixelLine[(Pixelx + 2)] :=
                    GetBValue(FBackGroundColor);
                end; end
{else If (FMcolor < (max_iterations)) then begin
        FMcolor := 0;
        Pixelx:= (colx*3);
        PixelLine[Pixelx] := RGBArray[2, FMcolor];
        PixelLine[(Pixelx+1)] := RGBArray[1, FMcolor];
        PixelLine[(Pixelx+2)] := RGBArray[0, FMcolor];

end;}
              else
              begin {Actual 16 color selection}
                FMcolor := (Round((Xsquare + Ysquare) * 14) mod 14) +
                  1;
                Pixelx := (colx * PixelScanSize);
                PixelLine[Pixelx] := RGBArray[2, FMcolor];
                PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor];
                PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor];
              end; end

            else if Color_Option = 1 then
            begin
              if ((FMcolor < (max_iterations)) and (bFractalMaxed))
                then begin
                if (MainForm.FractalBlack1.Checked) then begin
                  Pixelx := (colx * PixelScanSize);
                  PixelLine[Pixelx] := GetRValue(FBackGroundColor);
                  PixelLine[(Pixelx + 1)] :=
                    GetGValue(FBackGroundColor);
                  PixelLine[(Pixelx + 2)] :=
                    GetBValue(FBackGroundColor);
                end; end
              else begin {Actual 256 color selection}
                Pixelx := (colx * PixelScanSize);
                PixelLine[Pixelx] := Colors[2, FMcolor mod 256];
                PixelLine[(Pixelx + 1)] := Colors[1, FMcolor mod
                  256];
                PixelLine[(Pixelx + 2)] := Colors[0, FMcolor mod
                  256];
              end; end
            else if Color_Option = 2 then begin
              if ((FMcolor < (max_iterations)) and (bFractalMaxed))
                then begin
                if (MainForm.FractalBlack1.Checked) then begin
                  Pixelx := (colx * PixelScanSize);
                  PixelLine[Pixelx] := GetRValue(FBackGroundColor);
                  PixelLine[(Pixelx + 1)] :=
                    GetGValue(FBackGroundColor);
                  PixelLine[(Pixelx + 2)] :=
                    GetBValue(FBackGroundColor);
                end; end else
              begin {Actual Rotating color selection}
                if FMcolor mod 2 = 0 then begin
                  Pixelx := (colx * PixelScanSize);
                  PixelLine[Pixelx] := Colors[2, FMcolor mod 256];
                  PixelLine[(Pixelx + 1)] := Colors[1, FMcolor mod
                    256];
                  PixelLine[(Pixelx + 2)] := Colors[0, FMcolor mod
                    256];
                end
                else begin
                  Pixelx := (colx * PixelScanSize);
                  PixelLine[Pixelx] := RGBArray[2, FMcolor mod 16];
                  PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor mod
                    16];
                  PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor mod
                    16];
                end; end; end
            else if Color_Option = 3 then
            begin
              if X = 0 then
                FMcolor := 2
              else
              begin
                cos_theta := abs(X) / (sqrt(X * X + Y * Y));
                sin_theta := abs(Y) / (sqrt(X * X + Y * Y));
                theta := arctan(sin_theta / cos_theta);
                if (X < 0) and (Y >= 0) then
                  theta := theta + 1.5707963
                else if (X < 0) and (Y < 0) then
                  theta := theta + 3.14159625
                else if (X > 0) and (Y < 0) then
                  theta := theta + 4.7123889;
                if (theta >= 0) and (theta <= 3.14159625) then
                  FMcolor := 2
                else
                  FMcolor := 1;
              end;
              if (FMcolor = 1) then begin
                Pixelx := (colx * PixelScanSize);
                PixelLine[Pixelx] := GetBValue(FBackGroundColor);
                PixelLine[(Pixelx + 1)] :=
                  GetGValue(FBackGroundColor);
                PixelLine[(Pixelx + 2)] :=
                  GetRValue(FBackGroundColor);
              end else if (FMcolor = 2) then begin
                Pixelx := (colx * PixelScanSize);
                PixelLine[Pixelx] := Colors[2, 255];
                PixelLine[(Pixelx + 1)] := Colors[1, 255];
                PixelLine[(Pixelx + 2)] := Colors[0, 255];
              end
            end;
          end; { of row }
                        {FVP := P; FHQ := Q;}
        {	P := P + deltaP;}
        end; { of col }
        MainForm.Image2.Picture.Bitmap.Assign(Bitmap);
        MainForm.Image2.canvas.draw(0, 0,
          MainForm.Image2.Picture.Bitmap);
      finally
        Bitmap.free;
      end;
      if (not CallMandelBrot) then Mainform.FileSaver;
      Mainform.DoImageDone;
(*If (bMan3d and (not  (SetSelection= 3))) then Begin
{Set up 3D Array  ManMatrix :  array of array of Integer;
FractalFileMatrix}
SetLength(ManMatrix, FYImageX,FYImageY);
End;
If (bMan3d and (not  (SetSelection= 3))) then Begin
ManMatrix[colx,rowy]:=FMcolor;
end;
*)
      if (bMan3d and (not (SetSelection = 3)) and (not
        CallMandelBrot)) then begin
        MainForm.HintPanel.Caption := 'Saving Mandelbrot 3D to disk';
        Application.ProcessMessages;
        FractalFileMatrix := ChangeFileExt(FractalFilename, '.BIN');
        AssignFile(F_File, FractalFileMatrix);
        ReWrite(F_File);
        if IoResult = 0 then begin
          for rowy := 0 to maxrowy do begin
            Mainform.ProgressBar1.Position := Round((rowy / (FYImageX
              - 1)) * 100);
            Application.ProcessMessages;
            for colx := 0 to maxcolx do begin
        {Save to disk ManMatrix[colx,rowy];  }
              Write(F_File, ManMatrix[colx, rowy]);
            end; end;
          CloseFile(F_File);
          if (IoResult <> 0) then
            DoMessages(30064);
        end else DoMessages(30065);
        {Nil out the array}
        SetLength(ManMatrix, 0, 0);
      end;
      Mainform.ProgressBar1.Position := 0;
      MainForm.ProgressBar1.Visible := False;
      Application.ProcessMessages;
      FractalSet;
    end; end; end;




procedure TFractalForm.LobsterSetsClick(Sender: TObject);
var
  Code, i, SetSelection, Pixelx, flag,
    maxcolx, maxrowy, FMcolor, rowy, colx: integer;
  cos_theta, sin_theta, theta,
    Xold, Yold, denom, deltaX, deltaY,
    Xtemp, YTemp, Temp, X, Y, Xsquare, Ysquare,
    max_size {, deltaP, deltaQ,  P}: Double; {Extended;}
  S: string;
{  Q: array[0..2305] of Extended;  }
  Bitmap: TBitmap;
  PixelLine: PByteArray;
begin
  MainForm.DoImageStart;
  MainForm.ProgressBar1.Visible := True;
  Application.ProcessMessages;
  CallMandelBrot := FALSE;
  LastKnownFunction := 20;
  Randomize;
  Bitmap := TBitmap.create;
  try
    Bitmap.Assign(MainForm.Image2.Picture.Bitmap);
    Bitmap.PixelFormat := MyPixelFormat; {pf24bit;}
      {   FYImageX:= 640;FYImageY:= 480;}
    Bitmap.Width := FYImageX; { assign the initial width... }
    Bitmap.Height := FYImageY; { ...and the initial height }
    SetSelection := FBSetRG.Itemindex;
    case SetSelection of
      0: begin
{Newt1	XMax := 3.5;
 XMin := -3.5;
 YMax := 2.50;
 YMin := -2.50;}
          FXMax := 3.6;
          FXMin := -3.5; {71/53.25=1.33}
          FYMax := 2.7;
          FYMin := -2.625;
{FHQ   := -0.003483;
FVP := 0.268545;}
          Color_Option := FBColorRG.ItemIndex;
          val(FBEdit.Text, Max_Iterations, Code);
          Start_Col := 0;
          FractalFilename := 'F_LOBSTR000.BMP'
        end;
{Set_String:='F_MANSET???.BMP';
NN := 'F_LOBPTR000.BMP}
      1: begin
          val(FBFXMaxEdit.Text, FXMax, Code);
          CodeFX(FBFXMaxEdit.Text,Code);
          val(FBFXMinEdit.Text, FXMin, Code);
          CodeFX(FBFXMinEdit.Text,Code);
          val(FBFYMaxEdit.Text, FYMax, Code);
          CodeFX(FBFYMaxEdit.Text,Code);
          val(FBFYMinEdit.Text, FYMin, Code);
          CodeFX(FBFYMinEdit.Text,Code);
          val(FBFHQEdit.Text, FHQ, Code);
          CodeFX(FBFHQEdit.Text,Code);
          val(FBFVPEdit.Text, FVP, Code);
          CodeFX(FBFVPEdit.Text,Code);
          val(FBEdit.Text, Max_Iterations, Code);
          CodeFX(FBEdit.Text,Code);
          Start_Col := 0;
          Color_Option := FBColorRG.ItemIndex;
          FractalFilename := 'F_LOBSTR000.BMP';
          MainForm.ChangeFileName;
            {to get a correct name from no input}
        end;
      2: begin
{Must have image loaded and data read...Mouse a Rectangle}
          val(FBEdit.Text, Max_Iterations, Code);
          CodeFX(FBEdit.Text,Code);
          Start_Col := 0;
          Color_Option := FBColorRG.ItemIndex;
        end;
      3: begin
{Must have image loaded and data read to complete the image generation}
        end;
    end;
{Check the X and y's to make sure the area is right side up}
    if (FXMax < FXMin) then
      begin Temp := FXMax; FXMax := FXMin; FXMin := Temp; end;
    if (FYMax < FYMin) then
      begin Temp := FYMax; FYMax := FYMin; FYMin := Temp; end;
{Begin here}
    max_size := 4.0;
    maxcolx := (FYImageX - 1); {x := 0 to 639}
    maxrowy := (FYImageY - 1);
{Newt1	deltaX := (XMax - XMin)/(maxcol);
 deltaY := (YMax - YMin)/(maxrow);}
    deltaX := (FXMax - FXMin) / maxcolx;
    deltaY := (FYMax - FYMin) / maxrowy;
{old this		Q[0] := FYMax;
  for rowy:=1 to maxrowy do Q[rowy] := Q[rowy-1] - deltaQ;
  P := (FXMin + (start_col * deltaP));}
    for colx := start_col to maxcolx do begin
      if CallMandelBrot = True then
      begin
        Start_Col := colx;
        Mainform.FileSaver;
       {Exit;} Abort;
      end;
      Mainform.ProgressBar1.Position :=
      Round((colx / (FYImageX - 1))* 100);
      Application.ProcessMessages;
      for rowy := 0 to maxrowy do begin
        PixelLine := Bitmap.ScanLine[rowy];
        X := FXMin + colx * deltaX;
        Y := FYMax - rowy * deltaY;
        Xold := 42;
        Yold := 42;
        FMcolor := 0;
        flag := 0;
        while (FMcolor <= max_iterations) and (flag = 0) do
        begin
          Xsquare := X * X;
          Ysquare := Y * Y;
          denom := 3 * ((Xsquare - Ysquare) * (Xsquare -
            Ysquare) + 4 * Xsquare * Ysquare);
          if denom = 0 then
            denom := 0.00000001;
          X := 0.6666667 * X + (Xsquare - Ysquare) / denom;

          Y := 0.6666667 * Y - 2 * X * Y / denom;
          if (Xold = X) and (Yold = Y) then
            flag := 1;
          Xold := X;
          Yold := Y;
          inc(FMcolor);
        end;
        if Color_Option = 0 then
        begin
               {Actual 16 color selection}{FMcolor mod 255;}
          if x > 0 then begin
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := ((FMcolor mod 16) * 15);
              {RGBArray[2, FMcolor mod 5];}
            PixelLine[(Pixelx + 1)] := 0;
              {RGBArray[1, FMcolor mod 5];}
            PixelLine[(Pixelx + 2)] := 0;
              {RGBArray[0, FMcolor mod 5];}
          end
          else begin
            if ((x < -0.3) and (y > 0)) then begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := 0;
                {RGBArray[2, ((FMcolor mod 5)+5)];}
              PixelLine[(Pixelx + 1)] := 0;
                {RGBArray[1, ((FMcolor mod 5)+5)];}
              PixelLine[(Pixelx + 2)] := ((FMcolor mod 16) * 15);
                {RGBArray[0, ((FMcolor mod 5)+5)];}
            end
            else begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := 0;
                { RGBArray[2, ((FMcolor mod 6)+10)];}
              PixelLine[(Pixelx + 1)] := ((FMcolor mod 16) * 15);
                {RGBArray[1, ((FMcolor mod 6)+10)];}
              PixelLine[(Pixelx + 2)] := 0;
                {RGBArray[0, ((FMcolor mod 6)+10)];}
            end
          end; end
        else if Color_Option = 1 then
        begin
{Actual 256 color selection}
          Pixelx := (colx * PixelScanSize);
          PixelLine[Pixelx] := Colors[2, FMcolor mod 255];
          PixelLine[(Pixelx + 1)] := Colors[1, FMcolor mod 255];
          PixelLine[(Pixelx + 2)] := Colors[0, FMcolor mod 255];
        end
        else if Color_Option = 2 then
        begin {Actual Rotating color selection}
          if FMcolor mod 2 = 0 then
          begin
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := Colors[2, FMcolor mod 256];
            PixelLine[(Pixelx + 1)] := Colors[1, FMcolor mod 256];
            PixelLine[(Pixelx + 2)] := Colors[0, FMcolor mod 256];
          end
          else begin
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := RGBArray[2, FMcolor mod 16];
            PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor mod 16];
            PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor mod 16];
          end; end
        else if Color_Option = 3 then
        begin
          if X = 0 then
            FMcolor := 2
          else
          begin
            cos_theta := abs(X) / (sqrt(X * X + Y * Y));
            sin_theta := abs(Y) / (sqrt(X * X + Y * Y));
            theta := arctan(sin_theta / cos_theta);
            if (X < 0) and (Y >= 0) then
              theta := theta + 1.5707963
            else if (X < 0) and (Y < 0) then
              theta := theta + 3.14159625
            else if (X > 0) and (Y < 0) then
              theta := theta + 4.7123889;
            if (theta >= 0) and (theta <= 3.14159625) then
              FMcolor := 2
            else
              FMcolor := 1;
          end;
          if (FMcolor = 1) then begin
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := GetBValue(FBackGroundColor);
            PixelLine[(Pixelx + 1)] := GetGValue(FBackGroundColor);
            PixelLine[(Pixelx + 2)] := GetRValue(FBackGroundColor);
          end else if (FMcolor = 2) then begin
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := Colors[2, 0];
            PixelLine[(Pixelx + 1)] := Colors[1, 0];
            PixelLine[(Pixelx + 2)] := Colors[0, 0];
          end
        end;
      end; { of row }
{			P := P + deltaP;}
    end; { of col }
    MainForm.Image2.Picture.Bitmap.Assign(Bitmap);
    MainForm.Image2.canvas.draw(0, 0,
      MainForm.Image2.Picture.Bitmap);
  finally
    Bitmap.free;
  end;
  if (not CallMandelBrot) then Mainform.FileSaver;
  Mainform.DoImageDone;
  Mainform.ProgressBar1.Position := 0;
  MainForm.ProgressBar1.Visible := False;
  Application.ProcessMessages;
  FractalSet;
end;



procedure TFractalForm.BlobsterSetsClick(Sender: TObject);
begin
{  MainForm.DoImageStart;
Mainform.DoImageDone;}
end;

procedure TFractalForm.ClobsterClick(Sender: TObject);
begin
{  MainForm.DoImageStart;
Mainform.DoImageDone;}
end;

procedure TFractalForm.SlobsterClick(Sender: TObject);
begin
{  MainForm.DoImageStart;
Mainform.DoImageDone;}
end;

procedure TFractalForm.NewtonSetsClick(Sender: TObject);
var
  Code, i, SetSelection, Pixelx, flag,
    maxcolx, maxrowy, FMcolor, rowy, colx: integer;
  Xnew, Xold, Ynew, Yold: Single;
  cos_theta, sin_theta, theta,
    denom, deltaX, deltaY, temp1, temp2, temp3,
    Xtemp, YTemp, Temp, X, Y, Xsquare, Ysquare,
    max_size {, deltaP, deltaQ,  P}: Extended;
  S: string;
{  Q: array[0..2305] of Extended;  }
  Bitmap: TBitmap;
  PixelLine: PByteArray;
begin
  MainForm.DoImageStart;
  MainForm.ProgressBar1.Visible := True;
  Application.ProcessMessages;
  CallMandelBrot := FALSE;
  LastKnownFunction := 20;
  Randomize;
  Bitmap := TBitmap.create;
  try
    Bitmap.Assign(MainForm.Image2.Picture.Bitmap);
    Bitmap.PixelFormat := MyPixelFormat; {pf24bit;}
      {   FYImageX:= 640;FYImageY:= 480;}
    Bitmap.Width := FYImageX; { assign the initial width... }
    Bitmap.Height := FYImageY; { ...and the initial height }
    SetSelection := FBSetRG.Itemindex;
    case SetSelection of
      0: begin
{Newt1	XMax := 3.5;
 XMin := -3.5;
 YMax := 2.50;
 YMin := -2.50;}
          FXMax := 3.6;
          FXMin := -3.5; {71/53.25=1.33}
          FYMax := 2.7;
          FYMin := -2.625;
{FHQ   := -0.003483;
FVP := 0.268545;}
          Color_Option := FBColorRG.ItemIndex;
          val(FBEdit.Text, Max_Iterations, Code);
          Start_Col := 0;
          FractalFilename := 'F_NWTSON000.BMP'
        end;
{Set_String:='F_NWTSON000.BMP';
NN := 'F_NWTPTR000.BMP}
      1: begin
          val(FBFXMaxEdit.Text, FXMax, Code);
          CodeFX(FBFXMaxEdit.Text,Code);
          val(FBFXMinEdit.Text, FXMin, Code);
          CodeFX(FBFXMinEdit.Text,Code);
          val(FBFYMaxEdit.Text, FYMax, Code);
          CodeFX(FBFYMaxEdit.Text,Code);
          val(FBFYMinEdit.Text, FYMin, Code);
          CodeFX(FBFYMinEdit.Text,Code);
          val(FBFHQEdit.Text, FHQ, Code);
          CodeFX(FBFHQEdit.Text,Code);
          val(FBFVPEdit.Text, FVP, Code);
          CodeFX(FBFVPEdit.Text,Code);
          val(FBEdit.Text, Max_Iterations, Code);
          CodeFX(FBEdit.Text,Code);
          Start_Col := 0;
          Color_Option := FBColorRG.ItemIndex;
          FractalFilename := 'F_NWTSON000.BMP';
          MainForm.ChangeFileName;
            {to get a correct name from no input}
        end;
      2: begin
{Must have image loaded and data read...Mouse a Rectangle}
          val(FBEdit.Text, Max_Iterations, Code);
          CodeFX(FBEdit.Text,Code);
          Start_Col := 0;
          Color_Option := FBColorRG.ItemIndex;
        end;
      3: begin
{Must have image loaded and data read to complete the image generation}
        end;
    end;
{Check the X and y's to make sure the area is right side up}
    if (FXMax < FXMin) then
      begin Temp := FXMax; FXMax := FXMin; FXMin := Temp; end;
    if (FYMax < FYMin) then
      begin Temp := FYMax; FYMax := FYMin; FYMin := Temp; end;
{Begin here}
    max_size := 4.0;
    maxcolx := (FYImageX - 1); {x := 0 to 639}
    maxrowy := (FYImageY - 1);
{Newt1	deltaX := (XMax - XMin)/(maxcol);
 deltaY := (YMax - YMin)/(maxrow);}
    deltaX := (FXMax - FXMin) / maxcolx;
    deltaY := (FYMax - FYMin) / maxrowy;
{old this		Q[0] := FYMax;
  for rowy:=1 to maxrowy do Q[rowy] := Q[rowy-1] - deltaQ;
  P := (FXMin + (start_col * deltaP));}
    for colx := start_col to maxcolx do
    begin
      if CallMandelBrot = True then
      begin
        Start_Col := colx;
        Mainform.FileSaver;
       {Exit;} Abort;
      end;
      Mainform.ProgressBar1.Position :=
      Round((colx / (FYImageX - 1)) * 100);
      Application.ProcessMessages;
      for rowy := 0 to maxrowy do
      begin
        PixelLine := Bitmap.ScanLine[rowy];
        X := FXMin + colx * deltaX;
        Y := FYMax - rowy * deltaY;
{			Xsquare := 0;
   Ysquare := 0;}
        Xold := 42;
        Yold := 42;
        FMcolor := 0;
        flag := 0;
        while (FMcolor <= max_iterations) and (flag = 0) do
        begin
          Xsquare := X * X;
          Ysquare := Y * Y;
          denom := (3 * Xsquare - 3 * Ysquare - 2);
          denom := denom * denom + 36 * Xsquare * Ysquare;
          if denom = 0 then
            denom := 0.00000001;
          temp1 := X * Xsquare - 3 * X * Ysquare - 2 * X - 5;
          temp2 := 3 * Xsquare - 3 * Ysquare - 2;
          temp3 := 3 * Xsquare * Y - Ysquare * Y - 2 * Y;
          X := X - (temp1 * temp2 - 6 * X * Y * temp3) / denom;
          Y := Y - (temp1 * (-6 * X * Y) + temp3 * temp2)
            / denom;
          Xnew := X;
          Ynew := Y;
          if (Xold = Xnew) and (Yold = Ynew) then
            flag := 1;
          Xold := X;
          Yold := Y;
          inc(FMcolor);
        end;
        if Color_Option = 0 then
        begin
 {Actual 16 color selection}
          if x > 0 then begin
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := ((FMcolor mod 16) * 16);
              {RGBArray[0, FMcolor mod 5];}
            PixelLine[(Pixelx + 1)] := 0;
              {RGBArray[1, FMcolor mod 5];}
            PixelLine[(Pixelx + 2)] := 0;
              {RGBArray[2, FMcolor mod 5];}
          end
          else begin
            if ((x < -0.3) and (y > 0)) then begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := 0;
                {RGBArray[0, ((FMcolor mod 5)+5)];}
              PixelLine[(Pixelx + 1)] := 0;
                {RGBArray[1, ((FMcolor mod 5)+5)];}
              PixelLine[(Pixelx + 2)] := ((FMcolor mod 16) * 16);
                { RGBArray[2, ((FMcolor mod 5)+5)];}
            end
            else begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := 0;
                { RGBArray[0, ((FMcolor mod 6)+10)];}
              PixelLine[(Pixelx + 1)] := ((FMcolor mod 16) * 16);
                { RGBArray[1, ((FMcolor mod 6)+10)];}
              PixelLine[(Pixelx + 2)] := 0;
                { RGBArray[2, ((FMcolor mod 6)+10)];}
            end
          end; end
        else if Color_Option = 1 then
        begin
 {Actual 16 color selection}
          if x > 0 then begin
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := Colors[0, FMcolor mod 8];
            PixelLine[(Pixelx + 1)] := Colors[1, FMcolor mod 8];
            PixelLine[(Pixelx + 2)] := Colors[2, FMcolor mod 8];
          end
          else begin
            if ((x < -0.3) and (y > 0)) then begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := Colors[0, ((FMcolor mod 8) + 15)];
              PixelLine[(Pixelx + 1)] := Colors[1, ((FMcolor mod 8) +
                15)];
              PixelLine[(Pixelx + 2)] := Colors[2, ((FMcolor mod 8) +
                15)];
            end
            else begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := Colors[0, ((FMcolor mod 8) + 32)];
              PixelLine[(Pixelx + 1)] := Colors[1, ((FMcolor mod 8) +
                32)];
              PixelLine[(Pixelx + 2)] := Colors[2, ((FMcolor mod 8) +
                32)];
            end
          end; end
        else if Color_Option = 2 then
        begin {Actual Rotating color selection}
          if FMcolor mod 2 = 0 then
          begin
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := Colors[0, FMcolor mod 256];
            PixelLine[(Pixelx + 1)] := Colors[1, FMcolor mod 256];
            PixelLine[(Pixelx + 2)] := Colors[2, FMcolor mod 256];
          end
          else begin
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := RGBArray[0, FMcolor mod 16];
            PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor mod 16];
            PixelLine[(Pixelx + 2)] := RGBArray[2, FMcolor mod 16];
          end; end
        else if Color_Option = 3 then
        begin
          if X = 0 then
            FMcolor := 2
          else
          begin
            cos_theta := abs(X) / (sqrt(X * X + Y * Y));
            sin_theta := abs(Y) / (sqrt(X * X + Y * Y));
            theta := arctan(sin_theta / cos_theta);
            if (X < 0) and (Y >= 0) then
              theta := theta + 1.5707963
            else if (X < 0) and (Y < 0) then
              theta := theta + 3.14159625
            else if (X > 0) and (Y < 0) then
              theta := theta + 4.7123889;
            if (theta >= 0) and (theta <= 3.14159625) then
              FMcolor := 2
            else
              FMcolor := 1;
          end;
          if (FMcolor = 1) then begin
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := GetBValue(FBackGroundColor);
            PixelLine[(Pixelx + 1)] := GetGValue(FBackGroundColor);
            PixelLine[(Pixelx + 2)] := GetRValue(FBackGroundColor);
          end else if (FMcolor = 2) then begin
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := Colors[2, 0];
            PixelLine[(Pixelx + 1)] := Colors[1, 0];
            PixelLine[(Pixelx + 2)] := Colors[0, 0];
          end
        end;
      end; { of row }
{			P := P + deltaP;}
    end; { of col }
    MainForm.Image2.Picture.Bitmap.Assign(Bitmap);
    MainForm.Image2.canvas.draw(0, 0,
      MainForm.Image2.Picture.Bitmap);
  finally
    Bitmap.free;
  end;
  if (not CallMandelBrot) then Mainform.FileSaver;
  Mainform.DoImageDone;
  Mainform.ProgressBar1.Position := 0;
  MainForm.ProgressBar1.Visible := False;
  Application.ProcessMessages;
  FractalSet;
end;




procedure TFractalForm.TadpoleSetsClick(Sender: TObject);
begin
{  MainForm.DoImageStart;
Mainform.DoImageDone;}
end;

procedure TFractalForm.FrogSetsClick(Sender: TObject);
begin
{  MainForm.DoImageStart;
Mainform.DoImageDone;}
end;

procedure TFractalForm.ToadSetsClick(Sender: TObject);
begin
{  MainForm.DoImageStart;
Mainform.DoImageDone;}
end;


procedure TFractalForm.SanMarcosDragonClick(Sender: TObject);
begin
{set and Color for the San Marcos Dragon}
  DoSanMarcosDragon := True;
  DragonPointsClick(Sender);
  DoSanMarcosDragon := False;
end;

procedure TFractalForm.DragonSetsClick(Sender: TObject);
var
  Code, i, SetSelection, Pixelx,
    maxcolx, maxrowy, FMcolor, rowy, colx: integer;
  cos_theta, sin_theta, theta,
    Xtemp, YTemp, Temp, X, Y, Xsquare, Ysquare,
    max_size, deltaP, deltaQ, P: Extended;
  S: string;
  Q: array[0..3072] of Extended;
  Bitmap: TBitmap;
  PixelLine: PByteArray;
begin
  MainForm.DoImageStart;
  MainForm.ProgressBar1.Visible := True;
  Application.ProcessMessages;
  CallMandelBrot := FALSE;
  LastKnownFunction := 30;
  Randomize;
  Bitmap := TBitmap.create;
  try
    Bitmap.Assign(MainForm.Image2.Picture.Bitmap);
    Bitmap.PixelFormat := MyPixelFormat; {pf24bit;}
      {   FYImageX:= 640;FYImageY:= 480;}
    Bitmap.Width := FYImageX; { assign the initial width... }
    Bitmap.Height := FYImageY; { ...and the initial height }
    SetSelection := FDSetRG.Itemindex;
    case SetSelection of
      0: begin
          FXMax := 4.2;
          FXMin := -2.20; {64/48=1.33}
          FYMax := 2.4;
          FYMin := -2.4;
          FHQ := -0.924069;
          FVP := 2.447261;
          Color_Option := FDColorRG.ItemIndex;
          val(FDEdit.Text, Max_Iterations, Code);
          Start_Col := 0;
          FractalFilename := 'F_DRGSET000.BMP'
        end;
{Set_String:='F_DRGSET???.BMP';
NN := 'F_DRGSET000.BMP';}
      1: begin
          val(FDFXMaxEdit.Text, FXMax, Code);
          CodeFX(FDFXMaxEdit.Text,Code);
          val(FDFXMinEdit.Text, FXMin, Code);
          CodeFX(FDFXMinEdit.Text,Code);
          val(FDFYMaxEdit.Text, FYMax, Code);
          CodeFX(FDFYMaxEdit.Text,Code);
          val(FDFYMinEdit.Text, FYMin, Code);
          CodeFX(FDFYMinEdit.Text,Code);
          val(FDFHQEdit.Text, FHQ, Code);
          CodeFX(FDFHQEdit.Text,Code);
          val(FDFVPEdit.Text, FVP, Code);
          CodeFX(FDFVPEdit.Text,Code);
          val(FDEdit.Text, Max_Iterations, Code);
          CodeFX(FDEdit.Text,Code);
          Start_Col := 0;
          Color_Option := FDColorRG.ItemIndex;
          FractalFilename := 'F_DRGSET000.BMP';
          MainForm.ChangeFileName;
            {to get a correct name from no input}
        end;
      2: begin
{Must have image loaded and data read...Mouse a Rectangle}
          val(FDEdit.Text, Max_Iterations, Code);
          CodeFX(FDEdit.Text,Code);
          Start_Col := 0;
          Color_Option := FDColorRG.ItemIndex;
        end;
      3: begin
{Must have image loaded and data read to complete the image generation}
        end;
    end;

{Check the X and y's to make sure the area is right side up}
    if (FXMax < FXMin) then
      begin Temp := FXMax; FXMax := FXMin; FXMin := Temp; end;
    if (FYMax < FYMin) then
      begin Temp := FYMax; FYMax := FYMin; FYMin := Temp; end;
{Begin here}
    max_size := 4.0;
    maxcolx := (FYImageX - 1); {x := 0 to 639}
    maxrowy := (FYImageY - 1);
    deltaP := (FXMax - FXMin) / maxcolx;
    deltaQ := (FYMax - FYMin) / maxrowy;
    Q[0] := FYMax;
    for rowy := 1 to maxrowy do Q[rowy] := Q[rowy - 1] - deltaQ;
    P := (FXMin + (start_col * deltaP));
    for colx := start_col to maxcolx do begin
      if CallMandelBrot = True then begin
        Start_Col := colx;
        Mainform.FileSaver;
        {Exit;} Abort;
      end;
      Mainform.ProgressBar1.Position :=
      Round((colx / (FYImageX - 1))* 100);
      Application.ProcessMessages;
      for rowy := 0 to maxrowy do begin
        PixelLine := Bitmap.ScanLine[rowy];
        X := 0.5;
        Y := 0.0;
    {Xsquare := 0;
    Ysquare := 0;}
        FMcolor := 1;
        while ((FMcolor < max_iterations) and
          ((X * X + Y * Y) {(Xsquare + Ysquare)} < max_size)) do
        begin
{                                	Xsquare := X*X;
     Ysquare := Y*Y;
     Y:= 2*X*Y + Q[rowy];
     X := Xsquare - Ysquare + P;
     inc(FMcolor);}
          Xtemp := (Y - X) * (Y + X) + X;
          Ytemp := X * Y;
          Ytemp := Ytemp + Ytemp - Y;
          X := P * Xtemp + Q[rowy] * Ytemp;
          Y := Q[rowy] * Xtemp - P * Ytemp;
          inc(FMcolor);
        end;

        if Color_Option = 0 then
        begin
          if ((FMcolor = (max_iterations)) and (bFractalMaxed)) then
            begin
            if (MainForm.FractalBlack1.Checked) then begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := GetRValue(FBackGroundColor);
              PixelLine[(Pixelx + 1)] := GetGValue(FBackGroundColor);
              PixelLine[(Pixelx + 2)] := GetBValue(FBackGroundColor);
            end;
          end else begin {Actual 16 color selection}
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := RGBArray[2, FMcolor mod 16];
            PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor mod 16];
            PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor mod 16];
          end; end
        else if Color_Option = 1 then
        begin
          if ((FMcolor = (max_iterations)) and (bFractalMaxed)) then
            begin
            if (MainForm.FractalBlack1.Checked) then begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := GetRValue(FBackGroundColor);
              PixelLine[(Pixelx + 1)] := GetGValue(FBackGroundColor);
              PixelLine[(Pixelx + 2)] := GetBValue(FBackGroundColor);
            end;
          end else begin {Actual 256 color selection}
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := Colors[2, FMcolor mod 256];
            PixelLine[(Pixelx + 1)] := Colors[1, FMcolor mod 256];
            PixelLine[(Pixelx + 2)] := Colors[0, FMcolor mod 256];
          end; end
        else if Color_Option = 2 then
        begin
          if ((FMcolor = (max_iterations)) and (bFractalMaxed)) then
            begin
            if (MainForm.FractalBlack1.Checked) then begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := GetRValue(FBackGroundColor);
              PixelLine[(Pixelx + 1)] := GetGValue(FBackGroundColor);
              PixelLine[(Pixelx + 2)] := GetBValue(FBackGroundColor);
            end;
          end else begin {Actual Rotating color selection}
            if FMcolor mod 2 = 0 then
            begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := Colors[2, FMcolor mod 256];
              PixelLine[(Pixelx + 1)] := Colors[1, FMcolor mod 256];
              PixelLine[(Pixelx + 2)] := Colors[0, FMcolor mod 256];
            end
            else begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := RGBArray[2, FMcolor mod 16];
              PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor mod 16];
              PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor mod 16];
            end; end; end
        else if Color_Option = 3 then
        begin
          if X = 0 then
            FMcolor := 2
          else
          begin
            cos_theta := abs(X) / (sqrt(X * X + Y * Y));
            sin_theta := abs(Y) / (sqrt(X * X + Y * Y));
            theta := arctan(sin_theta / cos_theta);
            if (X < 0) and (Y >= 0) then
              theta := theta + 1.5707963
            else if (X < 0) and (Y < 0) then
              theta := theta + 3.14159625
            else if (X > 0) and (Y < 0) then
              theta := theta + 4.7123889;
            if (theta >= 0) and (theta <= 3.14159625) then
              FMcolor := 2
            else
              FMcolor := 1;
          end;
          if (FMcolor = 1) then begin
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := GetBValue(FBackGroundColor);
            PixelLine[(Pixelx + 1)] := GetGValue(FBackGroundColor);
            PixelLine[(Pixelx + 2)] := GetRValue(FBackGroundColor);
          end else if (FMcolor = 2) then begin
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := Colors[2, 255];
            PixelLine[(Pixelx + 1)] := Colors[1, 255];
            PixelLine[(Pixelx + 2)] := Colors[0, 255];
          end
        end;
      end; { of row }
      P := P + deltaP;
    end; { of col }
    MainForm.Image2.Picture.Bitmap.Assign(Bitmap);
    MainForm.Image2.canvas.draw(0, 0,
      MainForm.Image2.Picture.Bitmap);
  finally
    Bitmap.free;
  end;
  if (not CallMandelBrot) then Mainform.FileSaver;
  Mainform.DoImageDone;
  Mainform.ProgressBar1.Position := 0;
  MainForm.ProgressBar1.Visible := False;
  Application.ProcessMessages;
  FractalSet;
end;

procedure TFractalForm.DragonPointsClick(Sender: TObject);
var
  Code, i, SetSelection, Pixelx,
    maxcolx, maxrowy, FMcolor, rowy, colx: integer;
  cos_theta, sin_theta, theta,
    temp_sq, temp_xy,
    Xtemp, YTemp, Temp, X, Y, Xsquare, Ysquare, deltaX, deltaY,
    max_size, deltaP, deltaQ, Q, P: Extended;
  S: string; {  Q: array[0..2305] of Extended; }
  Bitmap: TBitmap;
  PixelLine: PByteArray;
begin
  if ((FractalFilename = 'F_000000000.BMP')
       and(not DoSanMarcosDragon)) then DoMessages(30068) else
  begin
    MainForm.DoImageStart;
    MainForm.ProgressBar1.Visible := True;
    Application.ProcessMessages;
    CallMandelBrot := FALSE;
    Julia := False; bPointing := False; {We should be done by now}
    LastKnownFunction := 31;
    Randomize;
    Bitmap := TBitmap.create;
    try
      Bitmap.Assign(MainForm.Image2.Picture.Bitmap);
      Bitmap.PixelFormat := MyPixelFormat; {pf24bit;}
        {   FYImageX:= 640;FYImageY:= 480;}
      Bitmap.Width := FYImageX; { assign the initial width... }
      Bitmap.Height := FYImageY; { ...and the initial height }
      SetSelection := FDSetRG.Itemindex;
      case SetSelection of
        0: begin
            FXMax := 4.2;
            FXMin := -2.20;
            FYMax := 2.4;
            FYMin := -2.4;
            FHQ := -0.924069;
            FVP := 2.447261;
            Color_Option := FDColorRG.ItemIndex;
            val(FDEdit.Text, Max_Iterations, Code);
            Start_Col := 0;
            FractalFilename := 'F_DRGP__000.BMP';
            DoMessages(30069);
          end;
        1: begin
            val(FDFXMaxEdit.Text, FXMax, Code);
            CodeFX(FDFXMaxEdit.Text,Code);
            val(FDFXMinEdit.Text, FXMin, Code);
            CodeFX(FDFXMinEdit.Text,Code);
            val(FDFYMaxEdit.Text, FYMax, Code);
            CodeFX(FDFYMaxEdit.Text,Code);
            val(FDFYMinEdit.Text, FYMin, Code);
            CodeFX(FDFYMinEdit.Text,Code);
            val(FDFHQEdit.Text, FHQ, Code);
            CodeFX(FDFHQEdit.Text,Code);
            val(FDFVPEdit.Text, FVP, Code);
            CodeFX(FDFVPEdit.Text,Code);
            val(FDEdit.Text, Max_Iterations, Code);
            CodeFX(FDEdit.Text,Code);
            Start_Col := 0;
            Color_Option := FDColorRG.ItemIndex;
            FractalFilename := 'F_DRGPT_000.BMP';
            MainForm.ChangeFileName;
              {to get a correct name from no input}
          end;
        2: begin
{Must have image loaded and data read...Mouse a Point}
            val(FDEdit.Text, Max_Iterations, Code);
            CodeFX(FDEdit.Text,Code);
            Start_Col := 0;
            Color_Option := FDColorRG.ItemIndex;
          end;
        3: begin
{Must have image loaded and data read to complete the image generation}
          end;
        4: begin
{Julia Base done here? what was #0?}
            Color_Option := FDColorRG.ItemIndex;
            val(FDEdit.Text, Max_Iterations, Code);
            Start_Col := 0;
            FractalFilename := 'F_DRGPT_000.BMP'
          end;
      end;
      if DoSanMarcosDragon then begin
        FXMax := 4.2; {San Marcos Coordinates}
        FXMin := -2.20;
        FYMax := 2.4;
        FYMin := -2.4;
        FHQ := -0.924069;
        FVP := 2.447261;
        Color_Option := 6;
        Max_Iterations := 64;
        Start_Col := 0;
        FractalFilename := 'SAN_MARCOS.BMP'
      end;
{Check the X and y's to make sure the area is right side up}
      if (FXMax < FXMin) then
        begin Temp := FXMax; FXMax := FXMin; FXMin := Temp; end;
      if (FYMax < FYMin) then
        begin Temp := FYMax; FYMax := FYMin; FYMin := Temp; end;
{Begin here}
      P := FVP;
      Q := FHQ;
      max_size := 4.0;
      maxcolx := (FYImageX - 1); {x := 0 to 639}
      maxrowy := (FYImageY - 1);
        {P} deltaX := (FXMax - FXMin) / maxcolx;
        {Q} deltaY := (FYMax - FYMin) / maxrowy;
      for colx := start_col to maxcolx do begin
        if CallMandelBrot = True then begin
          Start_Col := colx;
          FVP := P;
          FHQ := Q;
          Mainform.FileSaver;
         {Exit;} Abort;
        end;
        Mainform.ProgressBar1.Position :=
        Round((colx / (FYImageX - 1)) * 100);
        Application.ProcessMessages;
        for rowy := 0 to maxrowy do begin
          PixelLine := Bitmap.ScanLine[rowy];
          X := FXMin + colx * deltaX;
          Y := FYMax - rowy * deltaY;
          Xsquare := 0;
          Ysquare := 0;
          FMcolor := 0;
          while (FMcolor < max_iterations) and
            ((Xsquare + Ysquare) < max_size) do
          begin
{    YTemp :=2*X*Y;
    Xsquare := X*X;
    Ysquare := Y*Y;
     Y :=  YTemp + Q;
     X := Xsquare - Ysquare + P;}
            Xsquare := X * X;
            Ysquare := Y * Y;
            temp_sq := Ysquare - Xsquare;
            temp_xy := X * Y;
            temp_xy := temp_xy + temp_xy;
            Ytemp := Q * (temp_sq + X) - P * (temp_xy - Y);
            X := P * (temp_sq + X) + Q * (temp_xy - Y);
            Y := Ytemp;
            inc(FMcolor);
          end;
          if Color_Option = 6 then
          begin
{San Marcos Colors}
            if (FMcolor < (max_iterations)) then FMcolor := 0
            else
              FMcolor := (Round((Xsquare + Ysquare) * 100) mod 6) + 1;
            if (rowy < (FYImageY div 2)) then FMcolor := FMcolor + 8;
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := RGBArray[2, FMcolor];
            PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor];
            PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor];
          end else
            if Color_Option = 0 then
            begin
              if (FMcolor >= (max_iterations)) then
                begin {Actual 16 color selection}
                FMcolor := (Round((Xsquare + Ysquare) * 14) mod 14) +
                  1;
                Pixelx := (colx * PixelScanSize);
                PixelLine[Pixelx] := RGBArray[2, FMcolor];
                PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor];
                PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor];
              end else
                if ((FMcolor < (max_iterations)) and (bFractalMaxed))
                  then begin
                  if (MainForm.FractalBlack1.Checked) then begin
                    Pixelx := (colx * PixelScanSize);
                    PixelLine[Pixelx] := GetRValue(FBackGroundColor);
                    PixelLine[(Pixelx + 1)] :=
                      GetGValue(FBackGroundColor);
                    PixelLine[(Pixelx + 2)] :=
                      GetBValue(FBackGroundColor);
                  end; end
                else begin
                  FMcolor := 0;
                  Pixelx := (colx * PixelScanSize);
                  PixelLine[Pixelx] := RGBArray[2, FMcolor];
                  PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor];
                  PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor];

                end
                  ; end
            else if Color_Option = 4 then
            begin
              if ((FMcolor < (max_iterations)) and (bFractalMaxed))
                then begin
                if (MainForm.FractalBlack1.Checked) then begin
                  Pixelx := (colx * PixelScanSize);
                  PixelLine[Pixelx] := GetRValue(FBackGroundColor);
                  PixelLine[(Pixelx + 1)] :=
                    GetGValue(FBackGroundColor);
                  PixelLine[(Pixelx + 2)] :=
                    GetBValue(FBackGroundColor);
                end;
              end
{else If (FMcolor < (max_iterations)) then begin
        FMcolor := 0;
        Pixelx:= (colx*3);
        PixelLine[Pixelx] := RGBArray[2, FMcolor];
        PixelLine[(Pixelx+1)] := RGBArray[1, FMcolor];
        PixelLine[(Pixelx+2)] := RGBArray[0, FMcolor];

end}
              else begin {Actual 16 color selection}
                FMcolor := (Round((Xsquare + Ysquare) * 14) mod 14) +
                  1;
                Pixelx := (colx * PixelScanSize);
                PixelLine[Pixelx] := RGBArray[2, FMcolor];
                PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor];
                PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor];
              end; end

            else if Color_Option = 1 then
            begin
              if ((FMcolor < (max_iterations)) and (bFractalMaxed))
                then begin
                if (MainForm.FractalBlack1.Checked) then begin
                  Pixelx := (colx * PixelScanSize);
                  PixelLine[Pixelx] := GetRValue(FBackGroundColor);
                  PixelLine[(Pixelx + 1)] :=
                    GetGValue(FBackGroundColor);
                  PixelLine[(Pixelx + 2)] :=
                    GetBValue(FBackGroundColor);
                end;
              end
              else begin {Actual 256 color selection}
                Pixelx := (colx * PixelScanSize);
                PixelLine[Pixelx] := Colors[2, FMcolor mod 256];
                PixelLine[(Pixelx + 1)] := Colors[1, FMcolor mod
                  256];
                PixelLine[(Pixelx + 2)] := Colors[0, FMcolor mod
                  256];
              end; end
            else if Color_Option = 2 then
            begin
              if ((FMcolor < (max_iterations)) and (bFractalMaxed))
                then begin
                if (MainForm.FractalBlack1.Checked) then begin
                  Pixelx := (colx * PixelScanSize);
                  PixelLine[Pixelx] := GetRValue(FBackGroundColor);
                  PixelLine[(Pixelx + 1)] :=
                    GetGValue(FBackGroundColor);
                  PixelLine[(Pixelx + 2)] :=
                    GetBValue(FBackGroundColor);
                end;
              end
              else begin {Actual Rotating color selection}
                if FMcolor mod 2 = 0 then
                begin
                  Pixelx := (colx * PixelScanSize);
                  PixelLine[Pixelx] := Colors[2, FMcolor mod 256];
                  PixelLine[(Pixelx + 1)] := Colors[1, FMcolor mod
                    256];
                  PixelLine[(Pixelx + 2)] := Colors[0, FMcolor mod
                    256];
                end
                else begin
                  Pixelx := (colx * PixelScanSize);
                  PixelLine[Pixelx] := RGBArray[2, FMcolor mod 16];
                  PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor mod
                    16];
                  PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor mod
                    16];
                end; end; end
            else if Color_Option = 3 then
            begin
              if X = 0 then
                FMcolor := 2
              else
              begin
                cos_theta := abs(X) / (sqrt(X * X + Y * Y));
                sin_theta := abs(Y) / (sqrt(X * X + Y * Y));
                theta := arctan(sin_theta / cos_theta);
                if (X < 0) and (Y >= 0) then
                  theta := theta + 1.5707963
                else if (X < 0) and (Y < 0) then
                  theta := theta + 3.14159625
                else if (X > 0) and (Y < 0) then
                  theta := theta + 4.7123889;
                if (theta >= 0) and (theta <= 3.14159625) then
                  FMcolor := 2
                else
                  FMcolor := 1;
              end;
              if (FMcolor = 1) then begin
                Pixelx := (colx * PixelScanSize);
                PixelLine[Pixelx] := GetBValue(FBackGroundColor);
                PixelLine[(Pixelx + 1)] :=
                  GetGValue(FBackGroundColor);
                PixelLine[(Pixelx + 2)] :=
                  GetRValue(FBackGroundColor);
              end else if (FMcolor = 2) then begin
                Pixelx := (colx * PixelScanSize);
                PixelLine[Pixelx] := Colors[2, 255];
                PixelLine[(Pixelx + 1)] := Colors[1, 255];
                PixelLine[(Pixelx + 2)] := Colors[0, 255];
              end
            end;
        end; { of row }
                        {FVP := P; FHQ := Q;}
        {	P := P + deltaP;}
      end; { of col }
      MainForm.Image2.Picture.Bitmap.Assign(Bitmap);
      MainForm.Image2.canvas.draw(0, 0,
        MainForm.Image2.Picture.Bitmap);
    finally
      Bitmap.free;
    end;
    if (not CallMandelBrot) then Mainform.FileSaver;
    Mainform.DoImageDone;
    Mainform.ProgressBar1.Position := 0;
    MainForm.ProgressBar1.Visible := False;
    Application.ProcessMessages;
    FractalSet;
  end; end;




procedure TFractalForm.PhoenixSetsClick(Sender: TObject);
var
  Code, i, SetSelection, Pixelx,
    maxcolx, maxrowy, FMcolor, rowy, colx: integer;
  Yi, Xi, Xisquare, Xitemp, cos_theta, sin_theta, theta,
    Xtemp, YTemp, Temp, X, Y, Xsquare, Ysquare,
    max_size, deltaP, deltaQ, P: Extended;
  S: string;
  Q: array[0..3072] of Extended;
  Bitmap: TBitmap;
  PixelLine: PByteArray;

begin
  MainForm.DoImageStart;
  MainForm.ProgressBar1.Visible := True;
  Application.ProcessMessages;
  CallMandelBrot := FALSE;
  LastKnownFunction := 40;
  Randomize;
  Bitmap := TBitmap.create;
  try
    Bitmap.Assign(MainForm.Image2.Picture.Bitmap);
    Bitmap.PixelFormat := MyPixelFormat; {pf24bit;}
      {   FYImageX:= 640;FYImageY:= 480;}
    Bitmap.Width := FYImageX; { assign the initial width... }
    Bitmap.Height := FYImageY; { ...and the initial height }
    SetSelection := FPSetRG.Itemindex;
    case SetSelection of
      0: begin
          FXMax := 2.67;
          FXMin := -2.67; {5.34/4=1.335}
          FYMax := 2.0;
          FYMin := -2.0;
          FHQ := -1.209169;
          FVP := 0.356338;
          Color_Option := FPColorRG.ItemIndex;
          val(FPEdit.Text, Max_Iterations, Code);
          Start_Col := 0;
          FractalFilename := 'F_PHESET000.BMP'
        end;
{Set_String:='F_PHESET???.BMP';
NN := 'F_PHESET000.BMP';}
      1: begin
          val(FPFXMaxEdit.Text, FXMax, Code);
          CodeFX(FPFXMaxEdit.Text,Code);
          val(FPFXMinEdit.Text, FXMin, Code);
          CodeFX(FPFXMinEdit.Text,Code);
          val(FPFYMaxEdit.Text, FYMax, Code);
          CodeFX(FPFYMaxEdit.Text,Code);
          val(FPFYMinEdit.Text, FYMin, Code);
          CodeFX(FPFYMinEdit.Text,Code);
          val(FPFHQEdit.Text, FHQ, Code);
          CodeFX(FPFHQEdit.Text,Code);
          val(FPFVPEdit.Text, FVP, Code);
          CodeFX(FPFVPEdit.Text,Code);
          val(FPEdit.Text, Max_Iterations, Code);
          CodeFX(FPEdit.Text,Code);
          Start_Col := 0;
          Color_Option := FPColorRG.ItemIndex;
          FractalFilename := 'F_PHESET000.BMP';
          MainForm.ChangeFileName;
            {to get a correct name from no input}
        end;
      2: begin
{Must have image loaded and data read...Mouse a Rectangle}
          val(FPEdit.Text, Max_Iterations, Code);
          CodeFX(FPEdit.Text,Code);
          Start_Col := 0;
          Color_Option := FPColorRG.ItemIndex;
        end;
      3: begin
{Must have image loaded and data read to complete the image generation}
        end;
    end;
{Check the X and y's to make sure the area is right side up}
    if (FXMax < FXMin) then
      begin Temp := FXMax; FXMax := FXMin; FXMin := Temp; end;
    if (FYMax < FYMin) then
      begin Temp := FYMax; FYMax := FYMin; FYMin := Temp; end;
{Begin here}
    max_size := 4.0;
    maxcolx := (FYImageX - 1); {x := 0 to 639}
    maxrowy := (FYImageY - 1);
    deltaP := (FXMax - FXMin) / maxcolx;
    deltaQ := (FYMax - FYMin) / maxrowy;
    Q[0] := FYMax;
    for rowy := 1 to maxrowy do Q[rowy] := Q[rowy - 1] - deltaQ;
    P := (FXMin + (start_col * deltaP));
    for colx := start_col to maxcolx do begin
      if CallMandelBrot = True then begin
        Start_Col := colx;
        Mainform.FileSaver;
          {Exit;} Abort;
      end;
      Mainform.ProgressBar1.Position :=
      Round((colx / (FYImageX - 1))* 100);
      Application.ProcessMessages;
      for rowy := 0 to maxrowy do begin
        PixelLine := Bitmap.ScanLine[rowy];
        Y := 0;
        Yi := 0;
        X := 0;
        Xi := 0;
        FMcolor := 0;
        Xsquare := 0;
        Xisquare := 0;
        while (FMcolor < max_iterations) and
          (Xsquare + Xisquare < max_size) do
        begin
          Xsquare := X * X;
          Xisquare := Xi * Xi;
          Xtemp := Xsquare - Xisquare + P
            + Q[rowy] * Y;
          Xitemp := 2 * X * Xi + Q[rowy] * Yi;
          Y := X;
          Yi := Xi;
          X := Xtemp;
          Xi := Xitemp;
          inc(FMcolor);
        end;

{		for rowy:=0 to maxrowy do begin
              PixelLine := Bitmap.ScanLine[rowy];
    X := 0;
    Y := 0;
    Xsquare := 0;
    Ysquare := 0;
    FMcolor := 1;
    while   ((FMcolor < max_iterations) and
     ((Xsquare + Ysquare) < max_size)) do
                                        begin
                                 Xsquare := X*X;
     Ysquare := Y*Y;
     Y:= 2*X*Y + Q[rowy];
     X := Xsquare - Ysquare + P;
     inc(FMcolor);
                                        end;      }
        if Color_Option = 0 then
        begin
          if ((FMcolor = (max_iterations)) and (bFractalMaxed)) then
            begin
            if (MainForm.FractalBlack1.Checked) then begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := GetRValue(FBackGroundColor);
              PixelLine[(Pixelx + 1)] := GetGValue(FBackGroundColor);
              PixelLine[(Pixelx + 2)] := GetBValue(FBackGroundColor);
            end;
          end else begin {Actual 16 color selection}
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := RGBArray[2, FMcolor mod 16];
            PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor mod 16];
            PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor mod 16];
          end; end
        else if Color_Option = 1 then
        begin
          if ((FMcolor = (max_iterations)) and (bFractalMaxed)) then
            begin
            if (MainForm.FractalBlack1.Checked) then begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := GetRValue(FBackGroundColor);
              PixelLine[(Pixelx + 1)] := GetGValue(FBackGroundColor);
              PixelLine[(Pixelx + 2)] := GetBValue(FBackGroundColor);
            end;
          end else begin {Actual 256 color selection}
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := Colors[2, FMcolor mod 256];
            PixelLine[(Pixelx + 1)] := Colors[1, FMcolor mod 256];
            PixelLine[(Pixelx + 2)] := Colors[0, FMcolor mod 256];
          end; end
        else if Color_Option = 2 then
        begin
          if ((FMcolor = (max_iterations)) and (bFractalMaxed)) then
            begin
            if (MainForm.FractalBlack1.Checked) then begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := GetRValue(FBackGroundColor);
              PixelLine[(Pixelx + 1)] := GetGValue(FBackGroundColor);
              PixelLine[(Pixelx + 2)] := GetBValue(FBackGroundColor);
            end;
          end else begin {Actual Rotating color selection}
            if FMcolor mod 2 = 0 then
            begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := Colors[2, FMcolor mod 256];
              PixelLine[(Pixelx + 1)] := Colors[1, FMcolor mod 256];
              PixelLine[(Pixelx + 2)] := Colors[0, FMcolor mod 256];
            end
            else begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := RGBArray[2, FMcolor mod 16];
              PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor mod 16];
              PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor mod 16];
            end; end; end
        else if Color_Option = 3 then
        begin
          if X = 0 then
            FMcolor := 2
          else
          begin
            cos_theta := abs(X) / (sqrt(X * X + Y * Y));
            sin_theta := abs(Y) / (sqrt(X * X + Y * Y));
            theta := arctan(sin_theta / cos_theta);
            if (X < 0) and (Y >= 0) then
              theta := theta + 1.5707963
            else if (X < 0) and (Y < 0) then
              theta := theta + 3.14159625
            else if (X > 0) and (Y < 0) then
              theta := theta + 4.7123889;
            if (theta >= 0) and (theta <= 3.14159625) then
              FMcolor := 2
            else
              FMcolor := 1;
          end;
          if (FMcolor = 1) then begin
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := GetBValue(FBackGroundColor);
            PixelLine[(Pixelx + 1)] := GetGValue(FBackGroundColor);
            PixelLine[(Pixelx + 2)] := GetRValue(FBackGroundColor);
          end else if (FMcolor = 2) then begin
            Pixelx := (colx * PixelScanSize);
            PixelLine[Pixelx] := Colors[2, 255];
            PixelLine[(Pixelx + 1)] := Colors[1, 255];
            PixelLine[(Pixelx + 2)] := Colors[0, 255];
          end
        end;
      end; { of row }
      P := P + deltaP;
    end; { of col }
    MainForm.Image2.Picture.Bitmap.Assign(Bitmap);
    MainForm.Image2.canvas.draw(0, 0,
      MainForm.Image2.Picture.Bitmap);
  finally
    Bitmap.free;
  end;
  if (not CallMandelBrot) then Mainform.FileSaver;
  Mainform.DoImageDone;
  Mainform.ProgressBar1.Position := 0;
  MainForm.ProgressBar1.Visible := False;
  Application.ProcessMessages;
  FractalSet;
end;

procedure TFractalForm.PhoenixPointsClick(Sender: TObject);
var
  Code, i, SetSelection, Pixelx,
    maxcolx, maxrowy, FMcolor, rowy, colx: integer;
  cos_theta, sin_theta, theta,
    Xi, Yi, Xisquare, Xitemp, deltaXi,
    Xtemp, YTemp, Temp, X, Y, Xsquare, Ysquare, deltaX, deltaY,
    max_size, deltaP, deltaQ, Q, P: Extended;
  S: string; {  Q: array[0..2305] of Extended; }
  Bitmap: TBitmap;
  PixelLine: PByteArray;
begin
  if (FractalFilename = 'F_000000000.BMP') then
    DoMessages(30070) else
  begin
    MainForm.DoImageStart;
    MainForm.ProgressBar1.Visible := True;
    Application.ProcessMessages;
    CallMandelBrot := FALSE;
    Julia := False; bPointing := False; {We should be done by now}
    LastKnownFunction := 41;
    Randomize;
    Bitmap := TBitmap.create;
    try
      Bitmap.Assign(MainForm.Image2.Picture.Bitmap);
      Bitmap.PixelFormat := MyPixelFormat; {pf24bit;}
        {   FYImageX:= 640;FYImageY:= 480;}
      Bitmap.Width := FYImageX; { assign the initial width... }
      Bitmap.Height := FYImageY; { ...and the initial height }
      SetSelection := FPSetRG.Itemindex;
      case SetSelection of
        0: begin
            FXMax := 2.67;
            FXMin := -2.67;
            FYMax := 2.0;
            FYMin := -2.0;
            FHQ := -1.209169;
            FVP := 0.356338;
            Color_Option := FPColorRG.ItemIndex;
            val(FPEdit.Text, Max_Iterations, Code);
            Start_Col := 0;
            FractalFilename := 'F_PHEP__000.BMP';
            DoMessages(30071);
          end;
        1: begin
            val(FPFXMaxEdit.Text, FXMax, Code);
            CodeFX(FPFXMaxEdit.Text,Code);
            val(FPFXMinEdit.Text, FXMin, Code);
            CodeFX(FPFXMinEdit.Text,Code);
            val(FPFYMaxEdit.Text, FYMax, Code);
            CodeFX(FPFYMaxEdit.Text,Code);
            val(FPFYMinEdit.Text, FYMin, Code);
            CodeFX(FPFYMinEdit.Text,Code);
            val(FPFHQEdit.Text, FHQ, Code);
            CodeFX(FPFHQEdit.Text,Code);
            val(FPFVPEdit.Text, FVP, Code);
            CodeFX(FPFVPEdit.Text,Code);
            val(FPEdit.Text, Max_Iterations, Code);
            CodeFX(FPEdit.Text,Code);
            Start_Col := 0;
            Color_Option := FPColorRG.ItemIndex;
            FractalFilename := 'F_PHEPT_000.BMP';
            MainForm.ChangeFileName;
              {to get a correct name from no input}
          end;
        2: begin
{Must have image loaded and data read...Mouse a Point}
            val(FPEdit.Text, Max_Iterations, Code);
            CodeFX(FPEdit.Text,Code);
            Start_Col := 0;
            Color_Option := FPColorRG.ItemIndex;
          end;
        3: begin
{Must have image loaded and data read to complete the image generation}
          end;
        4: begin
{Julia Base done here? what was #0?}
            Color_Option := FPColorRG.ItemIndex;
            val(FPEdit.Text, Max_Iterations, Code);
            Start_Col := 0;
            FractalFilename := 'F_PHEPT_000.BMP'
          end;
      end;
{Check the X and y's to make sure the area is right side up}
      if (FXMax < FXMin) then
        begin Temp := FXMax; FXMax := FXMin; FXMin := Temp; end;
      if (FYMax < FYMin) then
        begin Temp := FYMax; FYMax := FYMin; FYMin := Temp; end;
{Begin here}
      P := FVP;
      Q := FHQ;
      max_size := 4.0;
      maxcolx := (FYImageX - 1); {x := 0 to 639}
      maxrowy := (FYImageY - 1);
(*	       {P}deltaX:= (FXMax - FXMin)/maxcolx;
        {Q}deltaY := (FYMax - FYMin)/maxrowy;
  deltaX := (FYMax - FYMin)/(maxrow - 1);
     deltaXi := (FXMax - FXMin)/(maxcol -1);*)
      deltaX := (FYMax - FYMin) / (maxrowy);
      deltaXi := (FXMax - FXMin) / (maxcolx);
      for colx := start_col to maxcolx do begin
        if CallMandelBrot = True then begin
          Start_Col := colx;
          FVP := P;
          FHQ := Q;
          Mainform.FileSaver;
          {Exit;} Abort;
        end;
        Mainform.ProgressBar1.Position :=
        Round((colx / (FYImageX - 1)) * 100);
        Application.ProcessMessages;
        for rowy := 0 to maxrowy do begin
          PixelLine := Bitmap.ScanLine[rowy];
{				X := FXMin + colx * deltaX;
    Y := FYMax - rowy * deltaY;
    Xsquare := 0;
    Ysquare := 0;
    FMcolor := 0;}
          Y := 0;
          Yi := 0;
          X := FYMax - rowy * deltaX;
          Xi := FXMin + colx * deltaXi;
          Xsquare := 0;
          Xisquare := 0;
          FMcolor := 0;
          while (FMcolor < max_iterations) and
            ((Xsquare + Xisquare) {(Xsquare + Ysquare) } < max_size)
              do
          begin
{                                        YTemp :=2*X*Y;
     Xsquare := X*X;
     Ysquare := Y*Y;
     Y :=  YTemp + Q;
     X := Xsquare - Ysquare + P;
     inc(FMcolor); }
            Xsquare := X * X;
            Xisquare := Xi * Xi;
            Xtemp := Xsquare - Xisquare + P + Q * Y;
            Xitemp := 2 * X * Xi + Q * Yi;
            Y := X;
            Yi := Xi;
            X := Xtemp;
            Xi := Xitemp;
            inc(FMcolor);
          end;
          if Color_Option = 0 then
          begin
            if ((FMcolor < (max_iterations)) and (bFractalMaxed)) then
              begin
              if (MainForm.FractalBlack1.Checked) then begin
                Pixelx := (colx * PixelScanSize);
                PixelLine[Pixelx] := GetRValue(FBackGroundColor);
                PixelLine[(Pixelx + 1)] :=
                  GetGValue(FBackGroundColor);
                PixelLine[(Pixelx + 2)] :=
                  GetBValue(FBackGroundColor);
              end; end
{else If (FMcolor < (max_iterations))then begin
        FMcolor := 0;
        Pixelx:= (colx*3);
        PixelLine[Pixelx] := RGBArray[2, FMcolor];
        PixelLine[(Pixelx+1)] := RGBArray[1, FMcolor];
        PixelLine[(Pixelx+2)] := RGBArray[0, FMcolor];

end}
            else begin {Actual 16 color selection}
{        FMcolor := (Round((Xsquare + Xisquare)*14) mod 14) + 1;
        Pixelx:= (colx*3);
        PixelLine[Pixelx] := RGBArray[2, FMcolor];
        PixelLine[(Pixelx+1)] := RGBArray[1, FMcolor];
        PixelLine[(Pixelx+2)] := RGBArray[0, FMcolor];}
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := RGBArray[2, FMcolor mod 16];
              PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor mod 16];
              PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor mod 16];

            end; end

          else if Color_Option = 1 then
          begin
            if ((FMcolor < (max_iterations)) and (bFractalMaxed)) then
              begin
              if (MainForm.FractalBlack1.Checked) then begin
                Pixelx := (colx * PixelScanSize);
                PixelLine[Pixelx] := GetRValue(FBackGroundColor);
                PixelLine[(Pixelx + 1)] :=
                  GetGValue(FBackGroundColor);
                PixelLine[(Pixelx + 2)] :=
                  GetBValue(FBackGroundColor);
              end;
            end
            else begin {Actual 256 color selection}
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := Colors[2, FMcolor mod 256];
              PixelLine[(Pixelx + 1)] := Colors[1, FMcolor mod 256];
              PixelLine[(Pixelx + 2)] := Colors[0, FMcolor mod 256];
            end; end
          else if Color_Option = 2 then
          begin
            if ((FMcolor < (max_iterations)) and (bFractalMaxed)) then
              begin
              if (MainForm.FractalBlack1.Checked) then begin
                Pixelx := (colx * PixelScanSize);
                PixelLine[Pixelx] := GetRValue(FBackGroundColor);
                PixelLine[(Pixelx + 1)] :=
                  GetGValue(FBackGroundColor);
                PixelLine[(Pixelx + 2)] :=
                  GetBValue(FBackGroundColor);
              end;
            end
            else begin {Actual Rotating color selection}
              if FMcolor mod 2 = 0 then
              begin
                Pixelx := (colx * PixelScanSize);
                PixelLine[Pixelx] := Colors[2, FMcolor mod 256];
                PixelLine[(Pixelx + 1)] := Colors[1, FMcolor mod
                  256];
                PixelLine[(Pixelx + 2)] := Colors[0, FMcolor mod
                  256];
              end
              else begin
                Pixelx := (colx * PixelScanSize);
                PixelLine[Pixelx] := RGBArray[2, FMcolor mod 16];
                PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor mod
                  16];
                PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor mod
                  16];
              end; end; end
          else if Color_Option = 4 then
          begin
            if ((FMcolor < (max_iterations)) and (bFractalMaxed)) then
              begin
              if (MainForm.FractalBlack1.Checked) then begin
                Pixelx := (colx * PixelScanSize);
                PixelLine[Pixelx] := GetRValue(FBackGroundColor);
                PixelLine[(Pixelx + 1)] :=
                  GetGValue(FBackGroundColor);
                PixelLine[(Pixelx + 2)] :=
                  GetBValue(FBackGroundColor);
              end;
            end
{else If (FMcolor < (max_iterations)) then begin
        FMcolor := 0;
        Pixelx:= (colx*3);
        PixelLine[Pixelx] := RGBArray[2, FMcolor];
        PixelLine[(Pixelx+1)] := RGBArray[1, FMcolor];
        PixelLine[(Pixelx+2)] := RGBArray[0, FMcolor];

end}
            else begin {Actual 16 color selection}
              FMcolor := (Round((Xsquare + Xisquare {Ysquare}) * 14)
                mod 14) + 1;
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := RGBArray[2, FMcolor];
              PixelLine[(Pixelx + 1)] := RGBArray[1, FMcolor];
              PixelLine[(Pixelx + 2)] := RGBArray[0, FMcolor];
            end; end

          else if Color_Option = 3 then
          begin
            if X = 0 then
              FMcolor := 2
            else
            begin
              cos_theta := abs(X) / (sqrt(X * X + Y * Y));
              sin_theta := abs(Y) / (sqrt(X * X + Y * Y));
              theta := arctan(sin_theta / cos_theta);
              if (X < 0) and (Y >= 0) then
                theta := theta + 1.5707963
              else if (X < 0) and (Y < 0) then
                theta := theta + 3.14159625
              else if (X > 0) and (Y < 0) then
                theta := theta + 4.7123889;
              if (theta >= 0) and (theta <= 3.14159625) then
                FMcolor := 2
              else
                FMcolor := 1;
            end;
            if (FMcolor = 1) then begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := GetBValue(FBackGroundColor);
              PixelLine[(Pixelx + 1)] := GetGValue(FBackGroundColor);
              PixelLine[(Pixelx + 2)] := GetRValue(FBackGroundColor);
            end else if (FMcolor = 2) then begin
              Pixelx := (colx * PixelScanSize);
              PixelLine[Pixelx] := Colors[2, 255];
              PixelLine[(Pixelx + 1)] := Colors[1, 255];
              PixelLine[(Pixelx + 2)] := Colors[0, 255];
            end
          end;
        end; { of row }
                        {FVP := P; FHQ := Q;}
        {	P := P + deltaP;}
      end; { of col }
      MainForm.Image2.Picture.Bitmap.Assign(Bitmap);
      MainForm.Image2.canvas.draw(0, 0,
        MainForm.Image2.Picture.Bitmap);
    finally
      Bitmap.free;
    end;
    if (not CallMandelBrot) then Mainform.FileSaver;
    Mainform.DoImageDone;
    Mainform.ProgressBar1.Position := 0;
    MainForm.ProgressBar1.Visible := False;
    Application.ProcessMessages;
    FractalSet;
  end; end;




(*    B_Dragon
Procedure DragOut(P,Q,Scale : Extended);
{          Writeln('1.646009 0.967049');	Readln(P,Q);}

Procedure gen3ddrg(alpha, beta, gamma,
                   scale, x_offset, y_offset, QVal: Extended);
{

*)

procedure TFractalForm.DrawGrid1SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var index: integer;
var RS, GS, BS: string;
begin
  index := ARow * DrawGrid1.ColCount + ACol;
  if ColorDialog1.Execute then
    ColorArray[index] := ColorDialog1.Color;
  {ColorArray[(ACol*ARow)]:=ColorDialog1.Color;}
              {(ACol*ARow) ... Mixes and duplicates values}
  Str(GetRValue(ColorArray[index]), RS);
  Str(GetGValue(ColorArray[index]), GS);
  Str(GetBValue(ColorArray[index]), BS);
  MainForm.MousePanel.Caption :=
    '  Red: ' + RS + ', Green: ' + GS + ', Blue: ' + BS;
end;

procedure TFractalForm.DrawGrid1DrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  index: integer;
begin
  index := ARow * DrawGrid1.ColCount + ACol;
  with Sender as TDrawGrid do
  begin {(ACol*ARow)}
    DrawGrid1.Canvas.Brush.Color := ColorArray[index];
      { clBackGround;}
    DrawGrid1.Canvas.FillRect(Rect);
{    DrawGrid1.Draw(Canvas,Rect.Left,Rect.Top,index);}
    if gdFocused in State then
      Canvas.DrawFocusRect(Rect);
    Canvas.Refresh;
  end;
end;

(*
  OpenDialog1.Title := '16 Colors File Selection';
  OpenDialog1.Filter := 'COLOR16FILE.1CP|*.1CP';
  OpenDialog1.FileName := Color16Edit.Text;
  if OpenDialog1.Execute then begin
  {MyFilesExtension:=ExtractFileName(FractalFilename);}
    Color16Edit.Text := ExtractFileName(OpenDialog1.FileName);
  end;

  OpenDialog1.Title := '256 Colors File Selection';
  OpenDialog1.FileName := Color256Edit.Text;
  OpenDialog1.Filter := 'COLOR256FILE.2CP|*.2CP';
  if OpenDialog1.Execute then begin
  {MyFilesExtension:=ExtractFileName(FractalFilename);}
    Color256Edit.Text := ExtractFileName(OpenDialog1.FileName);
  end;
*)
procedure TFractalForm.ColorFilerClick(Sender: TObject);
var F_File: file of TColor;
  i: Integer;
  FileS: string;
begin
  OpenDialog1.InitialDir := FractalDir;
  FileS := ColorFileEdit.Text;
  OpenDialog1.FileName := FileS;
  if OpenDialog1.Execute then
  { Display Open dialog box }
  begin
    if (FileExists(OpenDialog1.Filename)) then begin
      AssignFile(F_File, OpenDialog1.Filename);
      Reset(F_File);
      if IoResult <> 0 then
        begin
        DoMessages(30091);
        Exit;
        end;
      for i := 0 to 255 do Read(F_File, ColorArray[i]);
      CloseFile(F_File);
    end else DoMessages(30092);
    PageControl1.ActivePage := RecordReader;
    PageControl1.ActivePage := ColorPicker;
  end; end;

{Save the array to disk}

procedure TFractalForm.ColorSaverClick(Sender: TObject);
var F_File: file of TColor;
  i: Integer;
  FileS: string;
begin
  FileS := ColorFileEdit.Text;
{  If (FileExists(FileS)) THEN Begin}
  AssignFile(F_File, FileS);
  ReWrite(F_File);
  if IoResult <> 0 then
  begin
    DoMessages(30091);
    Exit;
  end;
  for i := 0 to 255 do Write(F_File, ColorArray[i]);
  CloseFile(F_File);
end;

procedure TFractalForm.ColorFillClick(Sender: TObject);
var i: Integer;
begin
  Randomize;
  for i := 0 to 255 do ColorArray[i] := Random(16000000);
  PageControl1.ActivePage := RecordReader;
  PageControl1.ActivePage := ColorPicker;
  SetFocus;
end;

procedure TFractalForm.ColorClearsClick(Sender: TObject);
var i: Integer;
begin
  for i := 0 to 255 do ColorArray[i] := RGB(255, 255, 255);
  PageControl1.ActivePage := RecordReader;
  PageControl1.ActivePage := ColorPicker;
  SetFocus;
end;

procedure TFractalForm.Color256SetterClick(Sender: TObject);
var i: Integer;
Colors: array[0..2, 0..255] of Byte;
begin
  case Color256S of
    1: for i := 0 to 255 do
      begin Colors[0, i] := 0; Colors[1, i] := 0; Colors[2, i] := 255 -
      i;
        ColorArray[i] := RGB(Colors[0, i], Colors[1, i], Colors[2,
          i]);
      end;
    2: for i := 0 to 255 do
      begin Colors[0, i] := 0; Colors[1, i] := 255 - i; Colors[2, i] :=
      255 - i;
        ColorArray[i] := RGB(Colors[0, i], Colors[1, i], Colors[2,
          i]);
      end;
    3: for i := 0 to 255 do
      begin Colors[0, i] := 0; Colors[1, i] := 255 - i; Colors[2, i] :=
      0;
        ColorArray[i] := RGB(Colors[0, i], Colors[1, i], Colors[2,
          i]);
      end;
    4: for i := 0 to 255 do
      begin Colors[0, i] := 255 - i; Colors[1, i] := 255 - i; Colors[2,
      i] := 0;
        ColorArray[i] := RGB(Colors[0, i], Colors[1, i], Colors[2,
          i]);
      end;
    5: for i := 0 to 255 do
      begin Colors[0, i] := 255 - i; Colors[1, i] := 0; Colors[2, i] :=
      255 - i;
        ColorArray[i] := RGB(Colors[0, i], Colors[1, i], Colors[2,
          i]);
      end;
    6: for i := 0 to 255 do
      begin Colors[0, i] := 255 - i; Colors[1, i] := 0; Colors[2, i] :=
      0;
        ColorArray[i] := RGB(Colors[0, i], Colors[1, i], Colors[2,
          i]);
      end;
    7: for i := 0 to 255 do
      begin Colors[0, i] := 255 - i; Colors[1, i] := 255 - i; Colors[2,
      i] := 255 - i;
        ColorArray[i] := RGB(Colors[0, i], Colors[1, i], Colors[2,
          i]);
      end;
  end; {of case}
  PageControl1.ActivePage := RecordReader;
  PageControl1.ActivePage := ColorPicker;
  SetFocus;
end;


procedure TFractalForm.ColorInverterClick(Sender: TObject);
var i: Integer; Colors: array[0..2, 0..255] of Byte;
begin
{ColorArray[0]:=RGB(Colors[0,0],Colors[1,0],Colors[2,0]);}
  if (GetRValue(ColorArray[0]) = 255) then
  begin for i := 0 to 255 do Colors[0, i] := i end
  else begin for i := 0 to 255 do Colors[0, i] := 0 end;
  if (GetGValue(ColorArray[0]) = 255) then
  begin for i := 0 to 255 do Colors[1, i] := i end
  else begin for i := 0 to 255 do Colors[1, i] := 0 end;
  if (GetBValue(ColorArray[0]) = 255) then
  begin for i := 0 to 255 do Colors[2, i] := i end
  else begin for i := 0 to 255 do Colors[2, i] := 0 end;
  for i := 0 to 255 do
    ColorArray[i] := RGB(Colors[0, i], Colors[1, i], Colors[2, i]);
  PageControl1.ActivePage := RecordReader;
  PageControl1.ActivePage := ColorPicker;
  SetFocus;
end;

procedure TFractalForm.ColorSubvertClick(Sender: TObject);
var i: Integer;
begin
  for i := 0 to 15 do ColorArray[i] := RGB(0, 0, 255 - (i * 12));
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
    ColorArray[i + 96] := RGB(255 - (i * 12), 255 - (i * 12), 255 - (i
    * 12));

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
  PageControl1.ActivePage := RecordReader;
  PageControl1.ActivePage := ColorPicker;
  SetFocus;
end;

procedure TFractalForm.RecordSaverClick(Sender: TObject);
begin {SaveDialog1}
  SaveDialog1.InitialDir := FractalDir;
  if SaveDialog1.Execute then { Display Open dialog box }
  begin
    Memo1.Lines.SaveToFile(SaveDialog1.Filename);
  end;
end;

procedure TFractalForm.RecordOpenerClick(Sender: TObject);
begin
  OpenDialog1.InitialDir := FractalDir;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    Memo1.Lines.LoadFromFile(OpenDialog1.Filename);
  end;
end;


procedure TFractalForm.FRR_CopyToClipClick(Sender: TObject);
begin
  Memo1.SelectAll;
  Memo1.CopyToClipboard;
end;

procedure TFractalForm.FRRClearMemoClick(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TFractalForm.RecordPrintClick(Sender: TObject);
begin
  with Printer do
  begin
    BeginDoc;
    Memo1.PaintTo(Handle, 10, 10);
    EndDoc;
  end;
end;
{      BeginDoc;
      Canvas.Draw((PageWidth - Bmp.Width) div 2,
                  (PageHeight - Bmp.Height) div 2,
                  Bmp);
      EndDoc;
 begin
with PrintDialog1 do
  begin
  Options := [poPrintToFile];
  PrintToFile := True;
  if Execute then
    begin
    if PrintToFile then
      begin
      SaveDialog1.Title := 'Print to File: ';
      if SaveDialog1.Execute then
        Memo1.Lines.SaveToFile(SaveDialog1.FileName);
      end
    else
      Memo1.Text.Print('');
    end;
  end;
end;
}


procedure TFractalForm.DrawGrid2DrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  index: integer;
begin
  index := ARow * DrawGrid2.ColCount + ACol;
  with Sender as TDrawGrid do
  begin {(ACol*ARow)}
    DrawGrid2.Canvas.Brush.Color :=
      RGB(RGBArray[0, index], RGBArray[1, index], RGBArray[2,
        index]);
    {ColorArray[index];}{ clBackGround;}
    DrawGrid2.Canvas.FillRect(Rect);
{    DrawGrid1.Draw(Canvas,Rect.Left,Rect.Top,index);}
    if gdFocused in State then
      Canvas.DrawFocusRect(Rect);
    Canvas.Refresh;
  end;
end;

procedure TFractalForm.DrawGrid2SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var index: integer;
var RS, GS, BS: string; TempColor: TColor;
begin
  index := ARow * DrawGrid2.ColCount + ACol;
  if ColorDialog1.Execute then begin
    TempColor := ColorDialog1.Color;
    RGBArray[0, index] := GetRValue(TempColor);
    RGBArray[1, index] := GetGValue(TempColor);
    RGBArray[2, index] := GetBValue(TempColor);
  end;
  Str(GetRValue(RGBArray[0, index]), RS);
  Str(GetGValue(RGBArray[1, index]), GS);
  Str(GetBValue(RGBArray[2, index]), BS);
  MainForm.MousePanel.Caption :=
    '  Red: ' + RS + ', Green: ' + GS + ', Blue: ' + BS;
end;


procedure TFractalForm.Color16OpenClick(Sender: TObject);
var F_File: file of TColor; TempColor: TColor;
  i: Integer;
  FileS: string;
begin
  OpenDialog1.InitialDir := FractalDir;
  FileS := Color16Edit.Text;
  OpenDialog1.FileName := FileS;
  if OpenDialog1.Execute then { Display Open dialog box }
  begin
    if (FileExists(OpenDialog1.Filename)) then begin
      AssignFile(F_File, OpenDialog1.Filename);
      Reset(F_File);
      if IoResult <> 0 then
      begin
        DoMessages(30091);
        Exit;
      end;
      for i := 0 to 15 do begin
        Read(F_File, TempColor);
        RGBArray[0, i] := GetRValue(TempColor);
        RGBArray[1, i] := GetGValue(TempColor);
        RGBArray[2, i] := GetBValue(TempColor);
      end;
      Color16Edit.Text := OpenDialog1.FileName;
    end else DoMessages(30092);
    PageControl1.ActivePage := RecordReader;
    PageControl1.ActivePage := Color16Picker;
  end; end;


procedure TFractalForm.Color16SaveClick(Sender: TObject);
var F_File: file of TColor; Temp: TColor;
  i: Integer;
  FileS: string;
begin
  FileS := Color16Edit.Text;
  AssignFile(F_File, FileS);
  ReWrite(F_File);
  if IoResult <> 0 then
  begin
        DoMessages(30091);
        Exit;
  end;
  for i := 0 to 15 do begin
    Temp := RGB(RGBArray[0, i], RGBArray[1, i], RGBArray[2, i]);
    Write(F_File, Temp);
  end;
  CloseFile(F_File);
end;

procedure TFractalForm.Color16ClearClick(Sender: TObject);
var i: byte;
begin
  for i := 0 to 15 do begin
    RGBArray[0, i] := 255;
    RGBArray[1, i] := 255;
    RGBArray[2, i] := 255;
  end;
  PageControl1.ActivePage := RecordReader;
  PageControl1.ActivePage := Color16Picker;
  SetFocus;
end;


procedure TFractalForm.Color16RandomClick(Sender: TObject);
var i: Integer;
begin
  Randomize;
  for i := 0 to 15 do begin
    RGBArray[0, i] := Random(255);
    RGBArray[1, i] := Random(255);
    RGBArray[2, i] := Random(255);
  end;
  PageControl1.ActivePage := RecordReader;
  PageControl1.ActivePage := Color16Picker;
  SetFocus;
end;

procedure TFractalForm.Color16SubvertClick(Sender: TObject);
var Subversion: Integer; i: Byte; TempColor: Tcolor;
begin
  Subversion := Color16SubversionRG.ItemIndex;
  case Subversion of
    0: for i := 0 to 15 do
      begin TempColor := RGB(0, 0, 255 - (i * 12));
        RGBArray[0, i] := GetRValue(TempColor);
        RGBArray[1, i] := GetGValue(TempColor);
        RGBArray[2, i] := GetBValue(TempColor);
      end;
    1: for i := 0 to 15 do
      begin TempColor := RGB(0, 255 - (i * 12), 255 - (i * 12));
        RGBArray[0, i] := GetRValue(TempColor);
        RGBArray[1, i] := GetGValue(TempColor);
        RGBArray[2, i] := GetBValue(TempColor);
      end;
    2: for i := 0 to 15 do
      begin TempColor := RGB(0, 255 - (i * 12), 0);
        RGBArray[0, i] := GetRValue(TempColor);
        RGBArray[1, i] := GetGValue(TempColor);
        RGBArray[2, i] := GetBValue(TempColor);
      end;
    3: for i := 0 to 15 do
      begin TempColor := RGB(255 - (i * 12), 255 - (i * 12), 0);
        RGBArray[0, i] := GetRValue(TempColor);
        RGBArray[1, i] := GetGValue(TempColor);
        RGBArray[2, i] := GetBValue(TempColor);
      end;
    4: for i := 0 to 15 do
      begin TempColor := RGB(255 - (i * 12), 0, 255 - (i * 12));
        RGBArray[0, i] := GetRValue(TempColor);
        RGBArray[1, i] := GetGValue(TempColor);
        RGBArray[2, i] := GetBValue(TempColor);
      end;
    5: for i := 0 to 15 do
      begin TempColor := RGB(255 - (i * 12), 0, 0);
        RGBArray[0, i] := GetRValue(TempColor);
        RGBArray[1, i] := GetGValue(TempColor);
        RGBArray[2, i] := GetBValue(TempColor);
      end;
    6: for i := 0 to 15 do
      begin TempColor := RGB(255 - (i * 12), 255 - (i * 12), 255 - (i
        * 12));
        RGBArray[0, i] := GetRValue(TempColor);
        RGBArray[1, i] := GetGValue(TempColor);
        RGBArray[2, i] := GetBValue(TempColor);
      end;
  end; {Case}
  PageControl1.ActivePage := RecordReader;
  PageControl1.ActivePage := Color16Picker;
  SetFocus;
end;

procedure TFractalForm.Color16SetClick(Sender: TObject);
begin
{Win16Set.Checked ColorPicker16.Checked LandformsSet.Checked}
  if MainForm.Win16Set.Checked then begin
    RGBArray[0, 0] := 0; RGBArray[1, 0] := 0; RGBArray[2, 0] := 0;
    RGBArray[0, 1] := 128; RGBArray[1, 1] := 0; RGBArray[2, 1] := 0;
    RGBArray[0, 2] := 0; RGBArray[1, 2] := 128; RGBArray[2, 2] := 0;
    RGBArray[0, 3] := 128; RGBArray[1, 3] := 128; RGBArray[2, 3] :=
      0;
    RGBArray[0, 4] := 0; RGBArray[1, 4] := 0; RGBArray[2, 4] := 128;
    RGBArray[0, 5] := 128; RGBArray[1, 5] := 0; RGBArray[2, 5] :=
      128;
    RGBArray[0, 6] := 0; RGBArray[1, 6] := 128; RGBArray[2, 6] :=
      128;
    RGBArray[0, 7] := 192; RGBArray[1, 7] := 192; RGBArray[2, 7] :=
      192;
    RGBArray[0, 8] := 128; RGBArray[1, 8] := 128; RGBArray[2, 8] :=
      128;
    RGBArray[0, 9] := 255; RGBArray[1, 9] := 0; RGBArray[2, 9] := 0;
    RGBArray[0, 10] := 0; RGBArray[1, 10] := 255; RGBArray[2, 10] :=
      0;
    RGBArray[0, 11] := 255; RGBArray[1, 11] := 255; RGBArray[2, 11]
      := 0;
    RGBArray[0, 12] := 0; RGBArray[1, 12] := 0; RGBArray[2, 12] :=
      255;
    RGBArray[0, 13] := 255; RGBArray[1, 13] := 0; RGBArray[2, 13] :=
      255;
    RGBArray[0, 14] := 0; RGBArray[1, 14] := 255; RGBArray[2, 14] :=
      255;
    RGBArray[0, 15] := 255; RGBArray[1, 15] := 255; RGBArray[2, 15]
      := 255;
  end else
    if MainForm.LandformsSet.Checked then begin
      RGBArray[0, 0] := 0; RGBArray[1, 0] := 0; RGBArray[2, 0] :=
        128;
      RGBArray[0, 1] := 0; RGBArray[1, 1] := 0; RGBArray[2, 1] :=
        255;
      RGBArray[0, 2] := 0; RGBArray[1, 2] := 128; RGBArray[2, 2] :=
        128;
      RGBArray[0, 3] := 0; RGBArray[1, 3] := 255; RGBArray[2, 3] :=
        255;
      RGBArray[0, 4] := 255; RGBArray[1, 4] := 255; RGBArray[2, 4] :=
        0;
      RGBArray[0, 5] := 128; RGBArray[1, 5] := 128; RGBArray[2, 5] :=
        0;
      RGBArray[0, 6] := 0; RGBArray[1, 6] := 255; RGBArray[2, 6] :=
        0;
      RGBArray[0, 7] := 0; RGBArray[1, 7] := 128; RGBArray[2, 7] :=
        0;
      RGBArray[0, 8] := 128; RGBArray[1, 8] := 128; RGBArray[2, 8] :=
        128;
      RGBArray[0, 9] := 255; RGBArray[1, 9] := 0; RGBArray[2, 9] :=
        255;
      RGBArray[0, 10] := 128; RGBArray[1, 10] := 0; RGBArray[2, 10]
        := 128;
      RGBArray[0, 11] := 192; RGBArray[1, 11] := 192; RGBArray[2, 11]
        := 192;
      RGBArray[0, 12] := 255; RGBArray[1, 12] := 0; RGBArray[2, 12]
        := 0;
      RGBArray[0, 13] := 128; RGBArray[1, 13] := 0; RGBArray[2, 13]
        := 0;
      RGBArray[0, 14] := 255; RGBArray[1, 14] := 255; RGBArray[2, 14]
        := 255;
      RGBArray[0, 15] := 0; RGBArray[1, 15] := 0; RGBArray[2, 15] :=
        0;
    end else
      if MainForm.RGB16.Checked then begin
        RGBArray[0, 0] := 255; RGBArray[1, 0] := 0; RGBArray[2, 0] :=
          0;
        RGBArray[0, 1] := 225; RGBArray[1, 1] := 0; RGBArray[2, 1] :=
          0;
        RGBArray[0, 2] := 210; RGBArray[1, 2] := 0; RGBArray[2, 2] :=
          0;
        RGBArray[0, 3] := 195; RGBArray[1, 3] := 0; RGBArray[2, 3] :=
          0;
        RGBArray[0, 4] := 180; RGBArray[1, 4] := 0; RGBArray[2, 4] :=
          0;
        RGBArray[0, 5] := 0; RGBArray[1, 5] := 255; RGBArray[2, 5] :=
          0;
        RGBArray[0, 6] := 0; RGBArray[1, 6] := 225; RGBArray[2, 6] :=
          0;
        RGBArray[0, 7] := 0; RGBArray[1, 7] := 210; RGBArray[2, 7] :=
          0;
        RGBArray[0, 8] := 0; RGBArray[1, 8] := 195; RGBArray[2, 8] :=
          0;
        RGBArray[0, 9] := 0; RGBArray[1, 9] := 180; RGBArray[2, 9] :=
          0;
        RGBArray[0, 10] := 0; RGBArray[1, 10] := 0; RGBArray[2, 10]
          := 255;
        RGBArray[0, 11] := 0; RGBArray[1, 11] := 0; RGBArray[2, 11]
          := 240;
        RGBArray[0, 12] := 0; RGBArray[1, 12] := 0; RGBArray[2, 12]
          := 225;
        RGBArray[0, 13] := 0; RGBArray[1, 13] := 0; RGBArray[2, 13]
          := 210;
        RGBArray[0, 14] := 0; RGBArray[1, 14] := 0; RGBArray[2, 14]
          := 195;
        RGBArray[0, 15] := 0; RGBArray[1, 15] := 0; RGBArray[2, 15]
          := 180;
      end; {BGR would be reversed/...change R and B numbers}
  PageControl1.ActivePage := RecordReader;
  PageControl1.ActivePage := Color16Picker;
  SetFocus;
end;

procedure TFractalForm.Color16InvertClick(Sender: TObject);
var i: Integer; TR, TG, TB: Byte;
begin
  for i := 0 to 15 do begin
    TR := RGBArray[0, i];
    TG := RGBArray[1, i];
    TB := RGBArray[2, i];
    RGBArray[0, i] := 255 - TR;
    RGBArray[1, i] := 255 - TG;
    RGBArray[2, i] := 255 - TB;
  end;
  PageControl1.ActivePage := RecordReader;
  PageControl1.ActivePage := Color16Picker;
  SetFocus;
end;


procedure TFractalForm.BMediaOpenClick(Sender: TObject);
begin
  OpenDialog1.InitialDir := FractalDir;
  OpenDialog1.DefaultExt := 'AVI';
  OpenDialog1.Filter := '*.avi;*.mid;*.wav';
    {Open a video file}
  if OpenDialog1.Execute then
  begin
    MediaPlayer1.Frames := 1;
    MediaPlayer1.FileName := OpenDialog1.Filename;
    MediaPlayer1.Open;
  end;
end;

procedure TFractalForm.BlanketCaptureClick(Sender: TObject);
begin {Capture a video frame and display it}
{     PMultiImage1.VideoToPicture(PMultiMedia1.Handle);
     PMultiImage1.Visible:=True;}
end;

procedure TFractalForm.BMediaSaveClick(Sender: TObject);
begin
(*  SaveDialog1.DefaultExt := 'PNG';
  SaveDialog1.Filter := '*.png';
  if SaveDialog1.Execute then
  begin
{  PMultiImage1.ImageWriteRes:=sColor256;  }
  PMultiImage1.SaveAsPNG(SaveDialog1.Filename);
  end;
*)
end;

procedure TFractalForm.MediaPlayer1Click(Sender: TObject;
  Button: TMPBtnType; var DoDefault: Boolean);
begin
  if Button = btBack then
    begin MediaPlayer1.Frames := 1; MediaPlayer1.Back; end;
  if Button = btNext then
    begin MediaPlayer1.Frames := 1; MediaPlayer1.Next; end;
end;

procedure TFractalForm.FractalDOFileOpenClick(Sender: TObject);
begin
 {change the dialog title}
  MainForm.OpenPictureDialog1.Title := 'FDO File Selection';
  MainForm.OpenPictureDialog1.FileName := 'F_DOOSET000.BMP';
  if (FDSetRG.Itemindex = 4) then Julia := True;
  MainForm.FileOpener;
  FDOFileEdit.Text := FractalFilename;
  FDOSaveEdit.Text := FractalFilename;
end;

procedure TFractalForm.FractalDOFileSaveClick(Sender: TObject);
begin
  FractalFilename := FDOSaveEdit.text;
  MainForm.Image2.Picture.SaveToFile(FractalFilename);
end;

procedure TFractalForm.FDOSetsClick(Sender: TObject);
begin
{  MainForm.DoImageStart;
Mainform.DoImageDone;}
end;

procedure TFractalForm.FDOPointsClick(Sender: TObject);
begin
{  MainForm.DoImageStart;
Mainform.DoImageDone;}
end;
(*
        begin
        val( FXEdit.Text,V1e,Code);
        val( FYEdit.Text,V2e,Code);
        val( FZxEdit.Text,V3e,Code);
        Roots(V1e,V2e,V3e);
        end
*)

procedure TFractalForm.Roots(A, B, C: Extended);
var TextS: string;
{A,B,C,} DETERM, Root1, Root2: Extended;
begin
  MainForm.DoImageStart;
  with MainForm.Image2.Canvas do begin
{ClrScr;}
{Writeln('Solve for X in A*X^2 + B*X + C = 0');
Writeln;}
{Write('Enter A,B,C ');Readln(A,B,C);}
{A:=1; B:=2; C:=3;}
    Determ := B * B - 4 * A * C;
    if Determ > 0 then begin
      Root1 := (B + SQRT(DETERM)) / 2 / A;
      Root2 := (B - SQRT(DETERM)) / 2 / A;
      TextS := ('Root 1 = ' + C2S(Root1) + ' Root 2 = ' +
        C2S(Root2));
      TextOut(10, 10, TextS);
    end
    else if Determ = 0 then begin
      Root1 := B / 2 / A;
      TextS := ('Root 1: ' + C2S(Root1));
      TextOut(10, 10, TextS);
    end
    else begin
      Root1 := B / 2 / A;
      Root2 := SQRT(-Determ) / 2 / A;
      TextS := ('Root 1 = ' + C2S(Root1) + ' +- -i ' + C2S(Root2));
      TextOut(10, 10, TextS);
    end; end;
  Mainform.DoImageDone;
end;
(**************************************************)




end.
