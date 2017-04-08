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
unit fXYZ3D;
{MyPixelFormat; pf24bit;}

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, Printers,
  ComCtrls, StdCtrls, ExtCtrls, Gauges, Buttons, Grids;

type
  TXYZ3DForm = class(TForm)
    PageControl1: TPageControl;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Tiling: TTabSheet;
    TileHelp: TBitBtn;
    TileRun: TSpeedButton;
    ElevationTS: TTabSheet;
    ELoad: TSpeedButton;
    EFileNameEdit: TEdit;
    ESave: TSpeedButton;
    EMouse: TSpeedButton;
    EHelp: TBitBtn;
    EleRun: TSpeedButton;
    EColorsRG: TRadioGroup;
    ECI1Edit: TEdit;
    DemCIE: TLabel;
    DemMinE: TLabel;
    DemMaxE: TLabel;
    XYZDEMRG: TRadioGroup;
    DEMImport: TTabSheet;
    DemiFileEdit: TEdit;
    DemiImport: TSpeedButton;
    DemiHelp: TBitBtn;
    DemiMemo: TMemo;
    DemiColumns: TEdit;
    DemiRows: TEdit;
    Label24: TLabel;
    Label25: TLabel;
    DemiLeftX1: TEdit;
    Label36: TLabel;
    Label37: TLabel;
    DemiTopY1: TEdit;
    DemiRightX2: TEdit;
    Label38: TLabel;
    Label39: TLabel;
    DemiBottomY2: TEdit;
    DemiRG: TRadioGroup;
    BinFileList: TListBox;
    TileOpen1: TSpeedButton;
    TileRG: TRadioGroup;
    DemiColumnsX: TLabel;
    ECI2Edit: TEdit;
    DemiRowsY: TLabel;
    ECI3Edit: TEdit;
    Label4: TLabel;
    EXEdit: TEdit;
    Label5: TLabel;
    EYEdit: TEdit;
    Label19: TLabel;
    EZxEdit: TEdit;
    Label20: TLabel;
    EZyEdit: TEdit;
    Label21: TLabel;
    EREdit: TEdit;
    Label34: TLabel;
    EDEdit: TEdit;
    Label146: TLabel;
    EGEdit: TEdit;
    Label43: TLabel;
    EHEdit: TEdit;
    Label44: TLabel;
    EIEdit: TEdit;
    Label147: TLabel;
    EKEdit: TEdit;
    Label10: TLabel;
    TileOpen2: TSpeedButton;
    TileOpen3: TSpeedButton;
    TileOpen4: TSpeedButton;
    TileOpen5: TSpeedButton;
    TileOpen6: TSpeedButton;
    TileOpen9: TSpeedButton;
    TileOpen8: TSpeedButton;
    TileOpen7: TSpeedButton;
    Label8: TLabel;
    TileRowStrip: TCheckBox;
    TileColumnStrip: TCheckBox;
    TileThinEdit: TEdit;
    ECI6Edit: TEdit;
    ECI5Edit: TEdit;
    ECI4Edit: TEdit;
    ECI8Edit: TEdit;
    ECI7Edit: TEdit;
    DemCIEEdit: TEdit;
    Label6: TLabel;
    DehHeader: TSpeedButton;
    TabSheet1: TTabSheet;
    MtnsRG: TRadioGroup;
    MtnsClear: TSpeedButton;
    MtnsFileEdit: TEdit;
    MtnsSave: TSpeedButton;
    MtnsHelp: TBitBtn;
    MtnsFLSBtn: TSpeedButton;
    MtnsXEdit: TEdit;
    Label130: TLabel;
    MtnsYEdit: TEdit;
    Label131: TLabel;
    Label132: TLabel;
    MtnsZxEdit: TEdit;
    Label133: TLabel;
    MtnsZyEdit: TEdit;
    Label134: TLabel;
    MtnsREdit: TEdit;
    Label135: TLabel;
    MtnsDEdit: TEdit;
    Label1: TLabel;
    MtnsGEdit: TEdit;
    Label136: TLabel;
    MtnsHEdit: TEdit;
    Label137: TLabel;
    MtnsIEdit: TEdit;
    Label2: TLabel;
    MtnsKEdit: TEdit;
    TileSizeEdit: TEdit;
    Label3: TLabel;
    TileSizeRGNS: TRadioGroup;
    TileSizeRGEW: TRadioGroup;
    Label7: TLabel;
    NullDemValueEdit: TEdit;
    Label9: TLabel;
    Label11: TLabel;
    NullValueEdit: TEdit;
    FlmHeader: TSpeedButton;
    CopyClipBtn: TSpeedButton;
    MtnsRLSCB: TComboBox;
    TileSmooth: TCheckBox;
    TabSheet2: TTabSheet;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    DehMemoPrintBtn: TSpeedButton;
    FlmMemoPrintBtn: TSpeedButton;
    MtnsRLSRG: TRadioGroup;
    MtnsRLSMouseBtn: TSpeedButton;
    FileSeedEdit: TEdit;
    FileSeedDemBtn: TSpeedButton;
    FileSeedRG: TRadioGroup;
    MtnsProgressBar: TProgressBar;
    Label18: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    MtnsRLSBtn: TSpeedButton;
    TodoTransRG: TRadioGroup;
    TodoTransEdit: TEdit;
    SpeedButton1: TSpeedButton;
    TodoTNameEdit: TEdit;
    RgbToDemEdit: TEdit;
    RgbToDemBtn: TSpeedButton;
    Label17: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

{Dem Import.. collated here for all}
    procedure FlmHeaderClick(Sender: TObject);
Function ReadFlmFileData(FlmFile:String):Boolean;
procedure DisplayFlmData;
procedure WriteFlmFile(FlmFile:String);
    procedure FlmMemoPrintBtnClick(Sender: TObject);

    procedure DehHeaderClick(Sender: TObject);
Function ReadDehFileData(DehFile:String):Boolean;
procedure DisplayDehData;
procedure WriteDehFile(DehFile:String);
    procedure DehMemoPrintBtnClick(Sender: TObject);


    procedure EHelpClick(Sender: TObject);
    procedure ESaveClick(Sender: TObject);
    procedure ELoadClick(Sender: TObject);
    procedure EleLoadDo(FilesS: string);
    procedure EleRunClick(Sender: TObject);
    procedure EleRunDo;
    procedure ElevationDisplay;
    procedure ContourDisplay;
    procedure WireDisplay;


    procedure DemiHelpClick(Sender: TObject);
    procedure DemiImportClick(Sender: TObject);
    procedure DoEtopo;
    procedure DoNoaaGlobe;
    procedure DoGtopo;
    procedure DoUSGS;
    procedure CopyClipBtnClick(Sender: TObject);

    procedure TileHelpClick(Sender: TObject);
    procedure TileOpen1Click(Sender: TObject);
    procedure TileOpen2Click(Sender: TObject);
    procedure TileOpen3Click(Sender: TObject);
    procedure TileOpen4Click(Sender: TObject);
    procedure TileOpen5Click(Sender: TObject);
    procedure TileOpen6Click(Sender: TObject);
    procedure TileOpen7Click(Sender: TObject);
    procedure TileOpen8Click(Sender: TObject);
    procedure TileOpen9Click(Sender: TObject);
    procedure TileRunClick(Sender: TObject);
    procedure WriteTileHdr(InFileString: string; Whatsit: Integer);


    procedure MtnsHelpClick(Sender: TObject);
    procedure MtnsSaveClick(Sender: TObject);
    procedure MtnsClearClick(Sender: TObject);
    procedure MtnsFLSBtnClick(Sender: TObject);
    procedure MtnsRLSCBChange(Sender: TObject);
    procedure MtnsRLSBtnClick(Sender: TObject);
    procedure MtnsDisplayDo(Vre, Vde: Extended;
             MtnDo, Vx, Vy, Vzx, Vzy, Vg, Vh, Vi,Viki, MtnSize: Integer);
    procedure RgbToDemBtnClick(Sender: TObject);
    procedure FileSeedDemBtnClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }

  end;

var
  XYZ3DForm: TXYZ3DForm;
implementation

uses fUGlobal,fMain, fAbout, fuMathF,
{fGlfrm fGeometry,} fSysInfo,{ fGlForm,} fGlfrm;

{$R *.DFM}
var {For changing 9 tiles}
  Min1, Min2, Min3, Min4, Min5, Min6, Min7, Min8, Min9,
  Max1, Max2, Max3, Max4, Max5, Max6, Max7, Max8, Max9: Integer;

(*************************************************************)
procedure TXYZ3DForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  XYZ3DFormX := XYZ3DForm.left;
  XYZ3DFormY := XYZ3DForm.top;
  DoSaver;
end;

procedure TXYZ3DForm.FormCreate(Sender: TObject);
begin
  top := XYZ3DFormY;{0;}
  left := XYZ3DFormX;{488;}
  OpenDialog1.InitialDir := FractalDir;
  SaveDialog1.InitialDir := FractalDir;
  ForceCurrentDirectory := True;
  IsDemLoaded:=False;
  EMouse.Enabled:=False;
  EleRun.Enabled:=False;
  ESave.Enabled:=False;
  MtnsRLSCB.ItemIndex:=0;
end;
(*************************************************************)
(*************************************************************)
procedure TXYZ3DForm.FlmHeaderClick(Sender: TObject);
begin {Just read the header into the memo}
  OpenDialog1.Filter := 'flm Headers|*.flm';
  OpenDialog1.Filename := 'flm Headers.flm';
  OpenDialog1.InitialDir:=DemsDir;
  if (OpenDialog1.Execute) then
  begin
    DemsDir:=ExtractFilePath(OpenDialog1.FileName);
    If ReadFlmFileData(OpenDialog1.FileName)
    then DisplayFlmData;
  end;
end;

Function TXYZ3DForm.ReadFlmFileData(FlmFile:String):Boolean;
var
 MtnString:String;
 CodeVx:Integer;
 fMtnFile: TextFile;
Begin
  FlmFile := ChangeFileExt(FlmFile, '.flm');
  if (fileExists(FlmFile)) then
  begin
    DemsDir:=ExtractFilePath(FlmFile);
    {Set up file: Read flm to get X and Y size}
    AssignFile(fMtnFile, FlmFile);
    Reset(fMtnFile);
    if (IoResult <> 0) then
    begin
      DoMessages(30014);
      CloseFile(fMtnFile);
      Result:=False;
      Exit;
    end else
    begin
      Readln(fMtnFile, MtnString); {which Mountains were made}
      val(MtnString, iMadeMountains, CodeVx);
        Codefx(MtnString, CodeVx);
      Readln(fMtnFile, MtnString); {Null value}
      val(MtnString, NullDemValue, CodeVx);
        Codefx(MtnString, CodeVx);
      {store later in FLMs per file }
      Readln(fMtnFile, FractalFileMatrix);
      {file name holder for 3D}
      if (FractalFileMatrix <> ChangeFileExt(FlmFile, '.bin'))
          then DoMessages(40);
          {Confirm message to keep or replace name}
      Readln(fMtnFile, MtnString);
      val(MtnString, MinimumElevation, CodeVx);
         Codefx(MtnString,CodeVx);
      Readln(fMtnFile, MtnString);
      val(MtnString, MaximumElevation, CodeVx);
         Codefx(MtnString,CodeVx);
      Readln(fMtnFile, MtnString);
      val(MtnString, ContourInterval, CodeVx);
         Codefx(MtnString, CodeVx);
      Readln(fMtnFile, MtnString);
      val(MtnString, Fractalgorhythym, CodeVx);
         Codefx(MtnString,CodeVx);
      Readln(fMtnFile, MtnString);
      val(MtnString, FileSizeX, CodeVx);
         Codefx(MtnString, CodeVx);
      Readln(fMtnFile, MtnString);
      val(MtnString, FileSizeY, CodeVx);
         Codefx(MtnString, CodeVx);
      Readln(fMtnFile, MtnString);
      val(MtnString, Vx, CodeVx);
         Codefx(MtnString, CodeVx);
      Readln(fMtnFile, MtnString);
      val(MtnString, Vy, CodeVx);
         Codefx(MtnString, CodeVx);
      Readln(fMtnFile, MtnString);
      val(MtnString, Vzx, CodeVx);
         Codefx(MtnString, CodeVx);
      Readln(fMtnFile, MtnString);
      val(MtnString, Vzy, CodeVx);
         Codefx(MtnString, CodeVx);
      Readln(fMtnFile, MtnString);
      val(MtnString, Vre, CodeVx);
         Codefx(MtnString, CodeVx);
      Readln(fMtnFile, MtnString);
      val(MtnString, Vde, CodeVx);
         Codefx(MtnString, CodeVx);
      Readln(fMtnFile, MtnString);
      val(MtnString, Vg, CodeVx);
         Codefx(MtnString, CodeVx);
      Readln(fMtnFile, MtnString);
      val(MtnString, Vh, CodeVx);
         Codefx(MtnString, CodeVx);
      Readln(fMtnFile, MtnString);
      val(MtnString, Vi, CodeVx);
         Codefx(MtnString, CodeVx);
      Readln(fMtnFile, MtnString);
      val(MtnString, Viki, CodeVx);
         Codefx(MtnString, CodeVx);
      Readln(fMtnFile, MtnString);
      val(MtnString, VDEM, CodeVx);
         Codefx(MtnString, CodeVx);
      CloseFile(fMtnFile);
              Result:=True;
    end;
  end else Result:=False;
End;

procedure TXYZ3DForm.DisplayFlmData;
var MtnString:String;
begin
      DemiMemo.Lines.Clear;
      str(iMadeMountains, MtnString);
        DemiMemo.Lines.Append(MtnString);
        If iMadeMountains<12 then
          begin
          If (iMadeMountains>2) then
               MtnsRLSRG.ItemIndex:= iMadeMountains-3
          else MtnsRG.ItemIndex:= iMadeMountains;
          end;
      str(NullDemValue, MtnString);
        DemiMemo.Lines.Append(MtnString);
        DemiMemo.Lines.Append(FractalFileMatrix);
      str(MinimumElevation, MtnString);
        DemiMemo.Lines.Append(MtnString);
        DemMinE.Caption := 'Minimum Elevation:  ' + MtnString;
      str(MaximumElevation, MtnString);
        DemiMemo.Lines.Append(MtnString);
        DemMaxE.Caption :=
          'Maximum Elevation:  ' + IntToStr(MaximumElevation);
      str(ContourInterval, MtnString);
        DemiMemo.Lines.Append(MtnString);
      if (ContourInterval = 0) then
      begin
        ContourInterval :=
         ((MaximumElevation - MinimumElevation) div 7);
        str(ContourInterval, MtnString);
        DemCIE.Caption := '7 Contour Intervals:  ' + MtnString;
      end else
      begin
        DemCIE.Caption := '7 Contour Intervals:  ' + MtnString;
      end;
      str(Fractalgorhythym, MtnString);
        DemiMemo.Lines.Append(MtnString);
  EColorsRG.ItemIndex:=Fractalgorhythym;
      str(FileSizeX, MtnString);
        DemiMemo.Lines.Append(MtnString);
          DemiColumns.Text:=MtnString;
      str(FileSizeY, MtnString);
        DemiMemo.Lines.Append(MtnString);
          DemiRows.Text:=MtnString;
      str(Vx, MtnString);
        DemiMemo.Lines.Append(MtnString);
         ExEdit.text:= MtnString;
         MtnsxEdit.text:= MtnString;
      str(Vy, MtnString);
        DemiMemo.Lines.Append(MtnString);
         EyEdit.text:= MtnString;
         MtnsyEdit.text:= MtnString;
      str(Vzx, MtnString);
        DemiMemo.Lines.Append(MtnString);
         EZxEdit.text:= MtnString;
         MtnsZxEdit.text:= MtnString;
      str(Vzy, MtnString);
        DemiMemo.Lines.Append(MtnString);
         EZyEdit.text:= MtnString;
         MtnsZyEdit.text:= MtnString;
      str(Vre, MtnString);
        DemiMemo.Lines.Append(MtnString);
         EREdit.text:= MtnString;
         MtnsREdit.text:= MtnString;
      str(Vde, MtnString);
        DemiMemo.Lines.Append(MtnString);
         EDEdit.text:= MtnString;
         MtnsDEdit.text:= MtnString;
      str(Vg, MtnString);
        DemiMemo.Lines.Append(MtnString);
         EGEdit.text:= MtnString;
         MtnsGEdit.text:= MtnString;
      str(Vh, MtnString);
        DemiMemo.Lines.Append(MtnString);
         EHEdit.text:= MtnString;
         MtnsHEdit.text:= MtnString;
      str(Vi, MtnString);
        DemiMemo.Lines.Append(MtnString);
         EIEdit.text:= MtnString;
         MtnsIEdit.text:= MtnString;
      str(Viki, MtnString);
        DemiMemo.Lines.Append(MtnString);
         EKEdit.text:= MtnString;
         MtnsKEdit.text:= MtnString;
  XYZDEMRG.ItemIndex:=VDEM;
end;

procedure TXYZ3DForm.WriteFlmFile(FlmFile:String);
var  fMtnFile: TextFile;
MtnString:String;
begin
  AssignFile(fMtnFile, FlmFile);
  Rewrite(fMtnFile);
  if (IoResult <> 0) then
  begin
    DoMessages(30002);
    CloseFile(fMtnFile);
  end else
  begin
    {which Mountains were made}
    str(iMadeMountains, MtnString);
    Writeln(fMtnFile, MtnString);
    {NullDemValue:=StrtoInt(NullValueEdit.text);}
    Writeln(fMtnFile, IntToStr(NullDemValue));
    {file name holder for 3D}
Writeln(fMtnFile, FractalFileMatrix);
    str(MinimumElevation, MtnString);
    Writeln(fMtnFile, MtnString);
    str(MaximumElevation, MtnString);
    Writeln(fMtnFile, MtnString);
    str(ContourInterval, MtnString);
    Writeln(fMtnFile, MtnString);
Fractalgorhythym := EColorsRG.ItemIndex;
    str(Fractalgorhythym, MtnString);
    Writeln(fMtnFile, MtnString);
    str(FileSizeX, MtnString);
    Writeln(fMtnFile, MtnString);
    str(FileSizeY, MtnString);
    Writeln(fMtnFile, MtnString);
    str(Vx, MtnString);
    Writeln(fMtnFile, MtnString);
    str(Vy, MtnString);
    Writeln(fMtnFile, MtnString);
    str(Vzx, MtnString);
    Writeln(fMtnFile, MtnString);
    str(Vzy, MtnString);
    Writeln(fMtnFile, MtnString);
    str(Vre, MtnString);
    Writeln(fMtnFile, MtnString);
    str(Vde, MtnString);
    Writeln(fMtnFile, MtnString);
    str(Vg, MtnString);
    Writeln(fMtnFile, MtnString);
    str(Vh, MtnString);
    Writeln(fMtnFile, MtnString);
    str(Vi, MtnString);
    Writeln(fMtnFile, MtnString);
    str(Viki, MtnString);
    Writeln(fMtnFile, MtnString);
VDEM := XYZDEMRG.ItemIndex;
    str(VDEM, MtnString);
    Writeln(fMtnFile, MtnString);
    CloseFile(fMtnFile);
  end;
end;

procedure TXYZ3DForm.FlmMemoPrintBtnClick(Sender: TObject);
var
  MyFile: TextFile;
begin
  AssignPrn(MyFile);
  Rewrite(MyFile);
  Writeln(MyFile, 'Dem Mountains ID '+ IntToStr(iMadeMountains));
  Writeln(MyFile, 'NullDemValue '+ IntToStr(NullDemValue));
  Writeln(MyFile, 'bin File name '+ FractalFileMatrix);
  Writeln(MyFile, 'Minimum Elevation '+ IntToStr(MinimumElevation));
  Writeln(MyFile, 'Maximum Elevation '+ IntToStr(MaximumElevation));
  Writeln(MyFile, 'Contour Interval '+ IntToStr(ContourInterval));
  Writeln(MyFile, 'Fractal Tint type '+ IntToStr(Fractalgorhythym));
  Writeln(MyFile, 'File Size X '+ IntToStr(FileSizeX));
  Writeln(MyFile, 'File Size Y '+ IntToStr(FileSizeY));
  Writeln(MyFile, 'Vx '+ IntToStr(Vx));
  Writeln(MyFile, 'Vy '+ IntToStr(Vy));
  Writeln(MyFile, 'Vzx '+ IntToStr(Vzx));
  Writeln(MyFile, 'Vzy '+ IntToStr(Vzy));
  Writeln(MyFile, 'Vre '+ FloatToStr(Vre));
  Writeln(MyFile, 'Vde '+ FloatToStr(Vde));
  Writeln(MyFile, 'Vg '+ IntToStr(Vg));
  Writeln(MyFile, 'Vh '+ IntToStr(Vh));
  Writeln(MyFile, 'Vi '+ IntToStr(Vi));
  Writeln(MyFile, 'Vk '+ IntToStr(Viki));
  Writeln(MyFile, 'Digital Elevation Map type '+ IntToStr(VDEM));
  {VDEM   XYZDEMRG.itemindex}
  System.CloseFile(MyFile);
end;

{iMadeMountains:=  ?1..9..?;    Mountains  -32767}
{iMadeMountains:=  20 21 22 23  Fractals   -32767
             Mandel, Julia, Phoenix, Egg}
{iMadeMountains:=  110;  ETopo  DD         -32767}
{iMadeMountains:=  106;  GTopo  DD          -9999
                   105   Globe  DD           -500
                   200          RGB Import -32767}
{iMadeMountains:=  666;
                   103? USGS 250k
                   103?  100k
                   102?  50k
                   101  24k
5minute... 10            ETopo
1Km.. 3arc second: 5     GTopo   Globe
Dted 1x1 degree 3
Dted level 1}
(*************************************************************)
procedure TXYZ3DForm.DehHeaderClick(Sender: TObject);
begin {Just read the header into the memo}
  OpenDialog1.Filter := 'Header files|*.deh';
  OpenDialog1.Filename := 'Header files.deh';
  OpenDialog1.InitialDir:=DemsDir;
  if (OpenDialog1.Execute) then
  begin
    DemsDir:=ExtractFilePath(OpenDialog1.FileName);
    If ReadDehFileData(OpenDialog1.FileName) then
    DisplayDehData;
  end;
end;

Function TXYZ3DForm.ReadDehFileData(DehFile:String):Boolean;
var
  MtnString: string;
  MyFile: TextFile;
Begin
  Result:=False;
  AssignFile(MyFile, DehFile);
  Reset(MyFile);
  if (IoResult = 0) then
  begin
    Readln(MyFile, MtnString);
      VDEM:=StrToInt(MtnString);
    Readln(MyFile,MtnString);
      NullDemValue:=  StrToInt(MtnString);
    Readln(MyFile, ILorUGridString);
    Readln(MyFile, ProjectionString);
    Readln(MyFile, UtmZoneString);
    Readln(MyFile, DatumString);
    Readln(MyFile, SpheroidString);
    Readln(MyFile, ZunitsString);
    Readln(MyFile, MtnString);
      MinimumElevation:=StrToInt(MtnString);
    Readln(MyFile, MtnString);
      MaximumElevation:=StrToInt(MtnString);
    Readln(MyFile, GridUnitString);
    Readln(MyFile, MtnString);
      FileSizeX:=StrToInt(MtnString);
    Readln(MyFile, MtnString);
      FileSizeY:=StrToInt(MtnString);
    Readln(MyFile, MtnString);
      CellSizeX:=StrToFloat(MtnString);
    Readln(MyFile, MtnString);
      CellSizeY:=StrToFloat(MtnString);
    Readln(MyFile, MtnString);
      DemiLeftX1e:=StrToFloat(MtnString);
    Readln(MyFile, MtnString);
      DemiTopY1e:=StrToFloat(MtnString);
    Readln(MyFile, MtnString);
      DemiRightX2e:=StrToFloat(MtnString);
    Readln(MyFile, MtnString);
      DemiBottomY2e:=StrToFloat(MtnString);
      if (DemiLeftX1e >= DemiRightX2e) then DoMessages(30010);
      If ((GridUnitString='DD')and (DemiBottomY2e >= DemiTopY1e))
       then DoMessages(30010);
    Readln(MyFile, GridOriginString);
    Readln(MyFile, CellOriginString);
    CloseFile(MyFile);
    Result:=True;
  end else DoMessages(30012);
end;


procedure TXYZ3DForm.DisplayDehData;
begin
  DemiMemo.Lines.Append(IntToStr(VDEM));
  DemiMemo.Lines.Append(IntToStr(NullDemValue));
  DemiMemo.Lines.Append(ILorUGridString);
  DemiMemo.Lines.Append(ProjectionString);
  DemiMemo.Lines.Append(UtmZoneString);
  DemiMemo.Lines.Append(DatumString);
  DemiMemo.Lines.Append(SpheroidString);
  DemiMemo.Lines.Append(ZunitsString);
  DemiMemo.Lines.Append(IntToStr(MinimumElevation));
  DemiMemo.Lines.Append(IntToStr(MaximumElevation));
  DemiMemo.Lines.Append(GridUnitString);
  DemiMemo.Lines.Append(IntToStr(FileSizeX));
      DemiColumns.Text:=(IntToStr(FileSizeX));
  DemiMemo.Lines.Append(IntToStr(FileSizeY));
      DemiRows.Text:=(IntToStr(FileSizeY));
  DemiMemo.Lines.Append(FloatToStr(CellSizeX));
  DemiMemo.Lines.Append(FloatToStr(CellSizeY));
  DemiMemo.Lines.Append(FloatToStr(DemiLeftX1e));
    DemiLeftX1.Text:=FloatToStr(DemiLeftX1e);
  DemiMemo.Lines.Append(FloatToStr(DemiTopY1e));
    DemiTopY1.Text:=FloatToStr(DemiTopY1e);
  DemiMemo.Lines.Append(FloatToStr(DemiRightX2e));
    DemiRightX2.Text:=FloatToStr(DemiRightX2e);
  DemiMemo.Lines.Append(FloatToStr(DemiBottomY2e));
    DemiBottomY2.Text:=FloatToStr(DemiBottomY2e);
  DemiMemo.Lines.Append(GridOriginString);
  DemiMemo.Lines.Append(CellOriginString);
End;


procedure TXYZ3DForm.WriteDehFile(DehFile:String);
var
{  MtnString: string;}
  MyFile: TextFile;
Begin
{  Result:=False;}
  AssignFile(MyFile, DehFile);
  rewrite(MyFile);
  if (IoResult = 0) then
  begin
  Writeln(MyFile,  IntToStr(VDEM));
  Writeln(MyFile,  IntToStr(NullDemValue));
  Writeln(MyFile,  ILorUGridString);
  Writeln(MyFile,  ProjectionString);
  Writeln(MyFile,  UtmZoneString);
  Writeln(MyFile,  DatumString);
  Writeln(MyFile,  SpheroidString);
  Writeln(MyFile,  ZunitsString);
  Writeln(MyFile,  IntToStr(MinimumElevation));
  Writeln(MyFile,  IntToStr(MaximumElevation));
  Writeln(MyFile,  GridUnitString);
  Writeln(MyFile,  IntToStr(FileSizeX));
  Writeln(MyFile,  IntToStr(FileSizeY));
  Writeln(MyFile,  FloatToStr(CellSizeX));
  Writeln(MyFile,  FloatToStr(CellSizeY));
  Writeln(MyFile,  FloatToStr(DemiLeftX1e));
  Writeln(MyFile,  FloatToStr(DemiTopY1e));
  Writeln(MyFile,  FloatToStr(DemiRightX2e));
  Writeln(MyFile,  FloatToStr(DemiBottomY2e));
      if (DemiLeftX1e >= DemiRightX2e) then DoMessages(30010);
      If ((GridUnitString='DD')and (DemiBottomY2e >= DemiTopY1e))
       then DoMessages(30010);
  Writeln(MyFile,  GridOriginString);
  Writeln(MyFile,  CellOriginString);
    CloseFile(MyFile);
{    Result:=True;}
  end else DoMessages(30012);
end;


procedure TXYZ3DForm.DehMemoPrintBtnClick(Sender: TObject);
var
  MyFile: TextFile;
begin
  AssignPrn(MyFile);
  Rewrite(MyFile);
  Writeln(MyFile, 'VR GIS DEM Header type '+ IntToStr(VDEM));
  Writeln(MyFile, 'Null Value '+ IntToStr(NullDemValue));
  Writeln(MyFile, 'Image, Latlong, or UTM '+ ILorUGridString);
  Writeln(MyFile, 'Projection '+ ProjectionString);
  Writeln(MyFile, 'UTM ZONE for UTM projection '+ UtmZoneString);
  Writeln(MyFile, 'Datum '+  DatumString);
  Writeln(MyFile, 'Spheroid '+ SpheroidString);
  Writeln(MyFile, 'Zunits '+ ZunitsString);
  Writeln(MyFile, 'Z Min '+IntToStr(MinimumElevation));
  Writeln(MyFile, 'Z Max '+IntToStr(MaximumElevation));
  Writeln(MyFile, 'Grid Unit '+GridUnitString);
  Writeln(MyFile, 'ColumnsX '+IntToStr(FileSizeX));
  Writeln(MyFile, 'RowsY '+ IntToStr(FileSizeY));
  Writeln(MyFile, 'X Size '+FloatToStr(CellSizeX));
  Writeln(MyFile, 'Y Size '+FloatToStr(CellSizeY));
  Writeln(MyFile, 'LeftX1 '+FloatToStr(DemiLeftX1e));
  Writeln(MyFile, 'TopY1 '+FloatToStr(DemiTopY1e));
  Writeln(MyFile, 'RightX2 '+FloatToStr(DemiRightX2e));
  Writeln(MyFile, 'BottomY2 '+FloatToStr(DemiBottomY2e));
  Writeln(MyFile, 'Grid Origin '+GridOriginString);
        {'Center NW NE SW SE' }
  Writeln(MyFile, 'Cell Origin '+CellOriginString);
  System.CloseFile(MyFile);
end;
(*************************************************************)
(*************************************************************)


(*************************************************************)
{Tiling}
(*************************************************************)
procedure TXYZ3DForm.TileHelpClick(Sender: TObject);
begin
  Application.HelpContext(4400); {4400}
end;

procedure TXYZ3DForm.TileOpen1Click(Sender: TObject);
begin {}
  OpenDialog1.Filename := '*.bin';
  OpenDialog1.InitialDir:=DemsDir;
  OpenDialog1.Filter := 'Elevation files|*.bin';
  if (OpenDialog1.Execute) then
  begin
    DemsDir:=ExtractFilePath(OpenDialog1.Filename);
    BinFileList.Items[1] := ExtractFileName(OpenDialog1.Filename);
  end;
end;

procedure TXYZ3DForm.TileOpen2Click(Sender: TObject);
begin
  OpenDialog1.Filename := '*.bin';
  OpenDialog1.InitialDir:=DemsDir;
  OpenDialog1.Filter := 'Elevation files|*.bin';
  if (OpenDialog1.Execute) then
  begin
    DemsDir:=ExtractFilePath(OpenDialog1.Filename);
    BinFileList.Items[2] := ExtractFileName(OpenDialog1.Filename);
  end;
end;

procedure TXYZ3DForm.TileOpen3Click(Sender: TObject);
begin
  OpenDialog1.Filename := '*.bin';
  OpenDialog1.InitialDir:=DemsDir;
  OpenDialog1.Filter := 'Elevation files|*.bin';
  if (OpenDialog1.Execute) then
  begin
    DemsDir:=ExtractFilePath(OpenDialog1.Filename);
    BinFileList.Items[3] := ExtractFileName(OpenDialog1.Filename);
  end;
end;

procedure TXYZ3DForm.TileOpen4Click(Sender: TObject);
begin
  OpenDialog1.Filename := '*.bin';
  OpenDialog1.InitialDir:=DemsDir;
  OpenDialog1.Filter := 'Elevation files|*.bin';
  if (OpenDialog1.Execute) then
  begin
    DemsDir:=ExtractFilePath(OpenDialog1.Filename);
    BinFileList.Items[4] := ExtractFileName(OpenDialog1.Filename);
  end;
end;

procedure TXYZ3DForm.TileOpen5Click(Sender: TObject);
begin
  OpenDialog1.Filename := '*.bin';
  OpenDialog1.InitialDir:=DemsDir;
  OpenDialog1.Filter := 'Elevation files|*.bin';
  if (OpenDialog1.Execute) then
  begin
    DemsDir:=ExtractFilePath(OpenDialog1.Filename);
    BinFileList.Items[5] := ExtractFileName(OpenDialog1.Filename);
  end;
end;

procedure TXYZ3DForm.TileOpen6Click(Sender: TObject);
begin
  OpenDialog1.Filename := '*.bin';
  OpenDialog1.InitialDir:=DemsDir;
  OpenDialog1.Filter := 'Elevation files|*.bin';
  if (OpenDialog1.Execute) then
  begin
    DemsDir:=ExtractFilePath(OpenDialog1.Filename);
    BinFileList.Items[6] := ExtractFileName(OpenDialog1.Filename);
  end;
end;

procedure TXYZ3DForm.TileOpen7Click(Sender: TObject);
begin
  OpenDialog1.Filename := '*.bin';
  OpenDialog1.InitialDir:=DemsDir;
  OpenDialog1.Filter := 'Elevation files|*.bin';
  if (OpenDialog1.Execute) then
  begin
    DemsDir:=ExtractFilePath(OpenDialog1.Filename);
    BinFileList.Items[7] := ExtractFileName(OpenDialog1.Filename);
  end;
end;

procedure TXYZ3DForm.TileOpen8Click(Sender: TObject);
begin
  OpenDialog1.Filename := '*.bin';
  OpenDialog1.InitialDir:=DemsDir;
  OpenDialog1.Filter := 'Elevation files|*.bin';
  if (OpenDialog1.Execute) then
  begin
    DemsDir:=ExtractFilePath(OpenDialog1.Filename);
    BinFileList.Items[8] := ExtractFileName(OpenDialog1.Filename);
  end;
end;

procedure TXYZ3DForm.TileOpen9Click(Sender: TObject);
begin
  OpenDialog1.Filename := '*.bin';
  OpenDialog1.InitialDir:=DemsDir;
  OpenDialog1.Filter := 'Elevation files|*.bin';
  if (OpenDialog1.Execute) then
  begin
    DemsDir:=ExtractFilePath(OpenDialog1.Filename);
    BinFileList.Items[9] := ExtractFileName(OpenDialog1.Filename);
  end;
end;


{Currently SKIPS writing .deh file...}
procedure TXYZ3DForm.TileRunClick(Sender: TObject);
var
  MtnString, FileString, File0String,
  File1String, File2String, File3String: string;
{  DTM3_File,   DTM4_File,}
  DTM1_File,   DTM2_File, DTM_File: TextFile;
  ElevationCount: Smallint;
  FileSizeX2,FileSizeY2,
  ElevationTotal, Xcount2, Ycount2,
  Xcount, Ycount, WorkWithMe,  ThinnerSQ,
  X1size, X2size, X3size, Y1size, Y2size, Y3size, CodeVx: Integer;

In_Bin_File2,  In_Bin_File,
Bin_File1, Bin_File2, Bin_File3: file of Smallint;
{TStackX: array of Smallint;}
begin
{Actually DO the Combine Divisions}
{REALLY NEED a VLM, HDR, DTM information writer...
the created file(s) have new size, stats, name, boundaries, etc...}
  if (TileRG.Itemindex > -1) then
  begin
  NullDemValue:=StrToInt(NullDemValueEdit.Text);
          Thinner:=StrToInt(TileThinEdit.text);
          ThinnerSQ:= (Thinner*Thinner);
    case TileRG.Itemindex of
    0:
    begin {Thin # 1}
          FileString := DemsDir+BinFileList.Items[1];
          {Read header of dem}
          If (not ReadFlmFileData(FileString)) then exit;
            MainForm.ProgressBar1.Visible := True;
            Application.ProcessMessages;
          {Set up files: Write  txt->flm to set X and Y sizes}
              X1size := (FileSizeX div Thinner);
              Y1size := (FileSizeY div Thinner);
              MtnString := ChangeFileExt(FileString, '.txt');
              AssignFile(DTM_File, MtnString);
              Rewrite(DTM_File);
              str(X1size, MtnString);
                writeln(DTM_File, MtnString);
              str(Y1size, MtnString);
                writeln(DTM_File, MtnString);
              CloseFile(DTM_File);

          {Read dem}
          MainForm.HintPanel.Caption :=
          'Reading Fractal Mountains from disk';
          Application.ProcessMessages;
          File0String := ExtractFileName(FileString);
          WorkWithMe := Length(File0String);
          File0String := Copy(File0String, 1, WorkWithMe - 4);
          File1String := DemsDir + File0String + '_1.bin';
              AssignFile(In_Bin_File, FileString);
              Reset(In_Bin_File);
          FractalFileMatrix := FileString;
      if (IoResult <> 0) then
{3}   begin
          DoMessages(30016);
          CloseFile(In_Bin_File);
          Exit;
{-3}  end else
{3}   begin
        SetLength(ManMatrix, FileSizeX, FileSizeY);
        for Ycount := 0 to FileSizeY - 1 do
        begin
          MainForm.ProgressBar1.Visible := True;
          Mainform.ProgressBar1.Position :=
              Round((Ycount / (FileSizeY - 1)) * 100);
          Application.ProcessMessages;
          for Xcount := 0 to FileSizeX - 1 do
          begin
            Read(In_Bin_File, ManMatrix[Xcount, Ycount]);
          end;
        end;
        CloseFile(In_Bin_File);
        Mainform.ProgressBar1.Position := 0;
        MainForm.ProgressBar1.Visible := False;
        MainForm.HintPanel.Caption := 'Hints for Sail';
        Application.ProcessMessages;
      end;
      {Thin dem and write new dem file}
      AssignFile(Bin_File1, File1String);
      Rewrite(Bin_File1);
      if (IoResult <> 0) then
{3}   begin
          DoMessages(30017);
          CloseFile(Bin_File1);
          Exit;
{-3}  end else
{3}   begin
              Max1 := -2147483647;
              Min1 := 2147483646;
        for Ycount := 0 to Y1size - 1 do
        begin
          MainForm.ProgressBar1.Visible := True;
          Mainform.ProgressBar1.Position :=
               Round((Ycount / (Y1size- 1)) * 100);
          Application.ProcessMessages;
          for Xcount := 0 to X1size - 1 do
          begin
          {Add the thinned data and divide by
          thinner * Thinner}
            ElevationTotal:=0;
            for Ycount2 := 0 to Thinner - 1 do
            begin
            for Xcount2 := 0 to Thinner - 1 do
              begin
              ElevationTotal:=
              ElevationTotal+
              ManMatrix[((Xcount*Thinner)+Xcount2),
                                     ((Ycount*Thinner)+Ycount2)];
              end;
            end;
            ElevationCount:=
                round(ElevationTotal / ThinnerSQ );
                  if (ElevationCount > (NullDemValue)) then
            begin
                    if (Max1 <= ElevationCount) then
                      Max1 := ElevationCount;
                    if (Min1 >= ElevationCount) then
                      Min1 := ElevationCount;
            end;
            write(Bin_File1,ElevationCount );
          end;
        end;
        CloseFile(Bin_File1);
        Mainform.ProgressBar1.Position := 0;
        MainForm.ProgressBar1.Visible := False;
        MainForm.HintPanel.Caption := 'Hints for Sail';
        Application.ProcessMessages;
      end;
      {Gather data then write new header}
          WriteTileHdr(File1String, TileRG.Itemindex);
    end;

      1: {}
    begin {Combine #1,2 into 1 new file
  File1String:=MtnString+File0String+ '-12.BIN';}
If TileColumnStrip.Checked then
   Thinner:=StrtoInt(TileThinEdit.text)+1
   else Thinner:=1;
          File1String :=DemsDir+BinFileList.Items[1];
          File1String :=ChangeFileExt(File1String, '.flm');
          File2String := DemsDir+BinFileList.Items[2];
          File2String := ChangeFileExt(File2String, '.flm');
{Read header of dem}
          if ((fileExists(File1String))
          and (fileExists(File2String))) then
          begin
            MainForm.ProgressBar1.Visible := True;
            Application.ProcessMessages;
            {Set up file: Read flm to get X and Y size}
          If (not ReadFlmFileData(File2String)) then exit;
FileSizeX2:=FileSizeX;
FileSizeY2:=FileSizeY;
          If (not ReadFlmFileData(File1String)) then exit;
 If (FileSizeY<>FileSizeY2) then
 begin
 DoMessages(30018);
 exit;
 end;
{Set up files: Write flm to set X and Y sizes}
              X1size := (FileSizeX + FileSizeX2);
              Y1size := (FileSizeY + FileSizeY2);
          FileString := DemsDir+BinFileList.Items[1];
          File0String := ExtractFileName(FileString);
          WorkWithMe := Length(File0String);
          File0String := Copy(File0String, 1, WorkWithMe - 4);
          File0String := DemsDir + File0String + '-12.bin';
              MtnString := ChangeFileExt(File0String, '.txt');
              AssignFile(DTM_File, MtnString);
              Rewrite(DTM_File);
              str(X1size, MtnString);
                writeln(DTM_File, MtnString);
              str(Y1size, MtnString);
                 writeln(DTM_File, MtnString);
              CloseFile(DTM_File);

{Read dem}
    MainForm.HintPanel.Caption :=
      'Reading data from file1';
    Application.ProcessMessages;
              AssignFile(In_Bin_File2, File2String);
              Reset(In_Bin_File2);
      if (IoResult <> 0) then
{3}      begin
          DoMessages(30016);
          CloseFile(In_Bin_File2);
          Exit;
{-3}     end else AssignFile(In_Bin_File, File1String);
              Reset(In_Bin_File);
      if (IoResult <> 0) then
{3}      begin
          DoMessages(30016);
          CloseFile(In_Bin_File);
          Exit;
{-3}     end else
{3}      begin
        SetLength(ManMatrix, FileSizeX, FileSizeY);
        SetLength(ManMatrix2, FileSizeX2, FileSizeY2);
        for Ycount := 0 to FileSizeY - 1 do
        begin
          MainForm.ProgressBar1.Visible := True;
          Mainform.ProgressBar1.Position :=
              Round((Ycount / (FileSizeY - 1)) * 100);
          Application.ProcessMessages;
          for Xcount := 0 to FileSizeX - 1 do
          begin
            Read(In_Bin_File, ManMatrix[Xcount, Ycount]);
          end;
        end;
        CloseFile(In_Bin_File);
        Mainform.ProgressBar1.Position := 0;
{Read dem2}
    MainForm.HintPanel.Caption :=
      'Reading data from file 2';
        for Ycount := 0 to FileSizeY2 - 1 do
        begin
          MainForm.ProgressBar1.Visible := True;
          Mainform.ProgressBar1.Position :=
              Round((Ycount / (FileSizeY2 - 1)) * 100);
          Application.ProcessMessages;
          for Xcount := 0 to FileSizeX2 - 1 do
          begin
            Read(In_Bin_File2, ManMatrix2[Xcount, Ycount]);
          end;
        end;
        CloseFile(In_Bin_File2);
        Mainform.ProgressBar1.Position := 0;

        MainForm.ProgressBar1.Visible := False;
        MainForm.HintPanel.Caption := 'Hints for Sail';
        Application.ProcessMessages;

{Thin dem and write new dem file}
              AssignFile(Bin_File1, File0String);
              Rewrite(Bin_File1);
      if (IoResult <> 0) then
{3}      begin
          DoMessages(30017);
          CloseFile(Bin_File1);
          Exit;
{-3}     end else
{3}      begin
          MainForm.HintPanel.Caption := 'Writing new dem file';
          Mainform.ProgressBar1.Position :=0;
          MainForm.ProgressBar1.Visible := True;
          Application.ProcessMessages;
              Max1 := -2147483647;
              Min1 := 2147483646;
        for Ycount := 0 to FileSizeY - 1 do
        begin
          Mainform.ProgressBar1.Position :=
               Round((Ycount / (FileSizeY- 1)) * 100);
          Application.ProcessMessages;
          for Xcount := 0 to X1size - Thinner do
          begin
            ElevationCount:=ManMatrix[Xcount,Ycount];
                  if (ElevationCount > (NullDemValue)) then
                  begin
                    if (Max1 <= ElevationCount) then
                      Max1 := ElevationCount;
                    if (Min1 >= ElevationCount) then
                      Min1 := ElevationCount;
                  end;
            write(Bin_File1,ElevationCount );
          end;
          for Xcount := 0 to FileSizeX2 - 1 do
          begin
            ElevationCount:=ManMatrix2[Xcount,Ycount];
                  if (ElevationCount > (NullDemValue)) then
                  begin
                    if (Max1 <= ElevationCount) then
                      Max1 := ElevationCount;
                    if (Min1 >= ElevationCount) then
                      Min1 := ElevationCount;
                  end;
            write(Bin_File1,ElevationCount );
          end;
        end;
        CloseFile(Bin_File1);
        SetLength(ManMatrix, 0, 0);
        SetLength(ManMatrix2, 0, 0);
        Mainform.ProgressBar1.Position := 0;
        MainForm.ProgressBar1.Visible := False;
        MainForm.HintPanel.Caption := 'Hints for Sail';
        Application.ProcessMessages;
        {3}end;
       end;
       end;
      WriteTileHdr(File0String, TileRG.Itemindex);
    end;
      2: begin {Combine #1,4 Vertical into 1 file
 File1String:=MtnString+File0String+ '-14.BIN';}
          File1String := DemsDir+BinFileList.Items[1];
          File2String := DemsDir+BinFileList.Items[4];

          FileString := DemsDir+BinFileList.Items[1];
          File0String := ExtractFileName(FileString);
          WorkWithMe := Length(File0String);
          File0String := Copy(File0String, 1, WorkWithMe - 4);
          File0String := DemsDir + File0String + '-14.bin';

          WriteTileHdr(File0String, TileRG.Itemindex);
        end;
      3: begin {Combine #1,2,4,5 into 1 files
 File1String:=MtnString+File0String+ '-4.BIN';}
          File1String := DemsDir+BinFileList.Items[1];
          File2String := DemsDir+BinFileList.Items[2];
{copy 1 and 2 then 4 and 5}
          {File1String := DemsDir+BinFileList.Items[4];
          File2String := DemsDir+BinFileList.Items[5];}

          FileString := DemsDir+BinFileList.Items[1];
          File0String := ExtractFileName(FileString);
          WorkWithMe := Length(File0String);
          File0String := Copy(File0String, 1, WorkWithMe - 4);
          File0String := DemsDir + File0String + '-4.bin';

          WriteTileHdr(File0String, TileRG.Itemindex);
        end;
      4: begin {Combine #1..9 into 1 files : 3by3by3
 File1String:=MtnString+File0String+ '-9.BIN';}
          File1String := DemsDir+BinFileList.Items[1];
          File2String := DemsDir+BinFileList.Items[2];
          File3String := DemsDir+BinFileList.Items[3];

          FileString := DemsDir+BinFileList.Items[1];
          File0String := ExtractFileName(FileString);
          WorkWithMe := Length(File0String);
          File0String := Copy(File0String, 1, WorkWithMe - 4);
          File0String := DemsDir + File0String + '-9.bin';
          WriteTileHdr(FileString, TileRG.Itemindex);
        end;
      5: begin {Divide #5 into 5,6 files
 File1String:=MtnString+File0String+ '_E _W.BIN';}
          FileString := DemsDir+BinFileList.Items[5];
          WriteTileHdr(FileString, TileRG.Itemindex);
        end;
      6: begin {Divide #5 into 5,8 files
 File1String:=MtnString+File0String+ '_N _S.BIN';}
          FileString := DemsDir+BinFileList.Items[5];
          WriteTileHdr(FileString, TileRG.Itemindex);
        end;
      7: begin {Divide #5 into 5,6,8,9 files
 File1String:=MtnString+File0String+ '_NE _SE _NW _SW.BIN';}
          FileString := DemsDir+BinFileList.Items[5];
          WriteTileHdr(FileString, TileRG.Itemindex);
        end;
      8:
begin {Divide #5 into 1..9 files
 File1String:=MtnString+File0String+ '_1.._9.BIN';}
  FileString := DemsDir+BinFileList.Items[5];
  MtnString := ChangeFileExt(FileString, '.flm');
  if (fileExists(MtnString)) then
  begin
    Mainform.ProgressBar1.Position :=0;
    MainForm.ProgressBar1.Visible := True;
    Application.ProcessMessages;
    {Set up file: Read flm to get X and Y size}
    If (not ReadFlmFileData(MtnString)) then exit;
    {Set up files: Write flm to set X and Y sizes}
    X1size := (FileSizeX div 3);
    X2size := X1size;
    X3size := (X1size + (FileSizeX mod 3)); {X3 is the rest}
    Y1size := (FileSizeY div 3);
    Y2size := Y1size;
    Y3size := (Y1size + (FileSizeY mod 3)); {Y3 is the rest}
    {SetLength(TStackX, FileSizeX); }
    {Set up 3 files and read the first third}
    MtnString := ChangeFileExt(FileString, '.txt');
    AssignFile(DTM_File, MtnString);
    Rewrite(DTM_File);
    str(X1size, MtnString);
    writeln(DTM_File, MtnString);
    str(X3size, MtnString);
    writeln(DTM_File, MtnString);
    str(Y1size, MtnString);
    writeln(DTM_File, MtnString);
    str(Y3size, MtnString);
    writeln(DTM_File, MtnString);
    CloseFile(DTM_File);

    MtnString := ExtractFilePath(FileString);
    File0String := ExtractFileName(FileString);
    WorkWithMe := Length(File0String);
    File0String := Copy(File0String, 1, WorkWithMe - 4);

    File1String := MtnString + File0String + '_1.bin';
    File2String := MtnString + File0String + '_2.bin';
    File3String := MtnString + File0String + '_3.bin';
    AssignFile(Bin_File1, File1String);
    Rewrite(Bin_File1);
    AssignFile(Bin_File2, File2String);
    Rewrite(Bin_File2);
    AssignFile(Bin_File3, File3String);
    Rewrite(Bin_File3);
    Max1 := -2147483647; Max2 := Max1; Max3 := Max1;
    Max4 := Max1; Max5 := Max1; Max6 := Max1;
    Max7 := Max1; Max8 := Max1; Max9 := Max1;
    Min1 := 2147483646; Min2 := Min1; Min3 := Min1;
    Min4 := Min1; Min5 := Min1; Min6 := Min1;
    Min7 := Min1; Min8 := Min1; Min9 := Min1;
    {READ the In bin file}
    SetLength(ManMatrix, FileSizeX, FileSizeY);
    AssignFile(In_Bin_File, FileString);
    Reset(In_Bin_File);
    for Ycount := 0 to FileSizeY - 1 do
    begin
      MainForm.ProgressBar1.Visible := True;
      Mainform.ProgressBar1.Position :=
              Round((Ycount / (FileSizeY - 1)) * 100);
      Application.ProcessMessages;
      for Xcount := 0 to FileSizeX - 1 do
      begin
        Read(In_Bin_File, ManMatrix[Xcount, Ycount]);
      end;
    end;
    CloseFile(In_Bin_File);
    Mainform.ProgressBar1.Position := 0;
    {Write 3 out files}
    for Ycount := 0 to Y1size - 1 do
    begin
      Mainform.ProgressBar1.Position :=
              Round((Ycount /(FileSizeY - 1)) * 100);
      Application.ProcessMessages;
      for Xcount := 0 to X1size - 1 do
      begin
        ElevationCount:=ManMatrix[Xcount,Ycount];
        {Read(In_Bin_File, ElevationCount);}
        if (ElevationCount > (NullDemValue)) then
        begin
          if (Max1 <= ElevationCount) then
                Max1 := ElevationCount;
          if (Min1 >= ElevationCount) then
               Min1 := ElevationCount;
        end;
        write(Bin_File1, ElevationCount);
      end;
      for Xcount := X1size to X2size - 1 do
      begin
        ElevationCount:=ManMatrix[Xcount,Ycount];
        {Read(In_Bin_File, ElevationCount);}
        if (ElevationCount > (NullDemValue)) then
        begin
          if (Max2 <= ElevationCount) then
                  Max2 := ElevationCount;
          if (Min2 >= ElevationCount) then
                  Min2 := ElevationCount;
        end;
        write(Bin_File2, ElevationCount);
      end;
      for Xcount := X2size to X3size - 1 do
      begin
        ElevationCount:=ManMatrix[Xcount,Ycount];
        {Read(In_Bin_File, ElevationCount);}
        if (ElevationCount > (NullDemValue)) then
        begin
          if (Max3 <= ElevationCount) then
                 Max3 := ElevationCount;
          if (Min3 >= ElevationCount) then
                 Min3 := ElevationCount;
        end;
        write(Bin_File3, ElevationCount);
      end;
    end;
   {Close 3 files}{Rename}
    CloseFile(Bin_File1);
    CloseFile(Bin_File2);
    CloseFile(Bin_File3);
   {Set up 3 files and read the second third}
    File1String := MtnString + File0String + '_4.bin';
    File2String := MtnString + File0String + '_5.bin';
    File3String := MtnString + File0String + '_6.bin';
    AssignFile(Bin_File1, File1String);
    Rewrite(Bin_File1);
    AssignFile(Bin_File2, File2String);
    Rewrite(Bin_File2);
    AssignFile(Bin_File3, File3String);
    Rewrite(Bin_File3);
    for Ycount := Y1size to Y2size - 1 do
    begin
      Mainform.ProgressBar1.Position :=
                   Round((Ycount /(FileSizeY - 1)) * 100);
      Application.ProcessMessages;
      for Xcount := 0 to X1size - 1 do
      begin
        ElevationCount:=ManMatrix[Xcount,Ycount];
        if (ElevationCount > (NullDemValue)) then
        begin
          if (Max4 <= ElevationCount) then
                      Max4 := ElevationCount;
          if (Min4 >= ElevationCount) then
                     Min4 := ElevationCount;
        end;
        write(Bin_File1, ElevationCount);
      end;
      for Xcount := X1size to X2size - 1 do
      begin
        ElevationCount:=ManMatrix[Xcount,Ycount];
        if (ElevationCount > (NullDemValue)) then
        begin
          if (Max5 <= ElevationCount) then
                      Max5 := ElevationCount;
          if (Min5 >= ElevationCount) then
                      Min5 := ElevationCount;
        end;
        write(Bin_File2, ElevationCount);
      end;
      for Xcount := X2size to X3size - 1 do
      begin
        ElevationCount:=ManMatrix[Xcount,Ycount];
        if (ElevationCount > (NullDemValue)) then
        begin
          if (Max6 <= ElevationCount) then
                      Max6 := ElevationCount;
          if (Min6 >= ElevationCount) then
                      Min6 := ElevationCount;
      end;
      write(Bin_File3, ElevationCount);
    end;
   end;
  {Close 3 files}{Rename}
  CloseFile(Bin_File1);
  CloseFile(Bin_File2);
  CloseFile(Bin_File3);
  {Set up 3 files and read the rest}
  File1String := MtnString + File0String + '_7.bin';
  File2String := MtnString + File0String + '_8.bin';
  File3String := MtnString + File0String + '_9.bin';
  AssignFile(Bin_File1, File1String);
  Rewrite(Bin_File1);
  AssignFile(Bin_File2, File2String);
  Rewrite(Bin_File2);
  AssignFile(Bin_File3, File3String);
  Rewrite(Bin_File3);
  for Ycount := Y2size to Y3size - 1 do
  begin
    Mainform.ProgressBar1.Position :=
                   Round((Ycount /(FileSizeY - 1)) * 100);
    Application.ProcessMessages;
    for Xcount := 0 to X1size - 1 do
    begin
      ElevationCount:=ManMatrix[Xcount,Ycount];
      if (ElevationCount > (NullDemValue)) then
      begin
        if (Max7 <= ElevationCount) then
                      Max7 := ElevationCount;
        if (Min7 >= ElevationCount) then
                      Min7 := ElevationCount;
      end;
      write(Bin_File1, ElevationCount);
    end;
    for Xcount := X1size to X2size - 1 do
    begin
      ElevationCount:=ManMatrix[Xcount,Ycount];
      if (ElevationCount > (NullDemValue)) then
      begin
        if (Max8 <= ElevationCount) then
                      Max8 := ElevationCount;
        if (Min8 >= ElevationCount) then
                      Min8 := ElevationCount;
      end;
      write(Bin_File2, ElevationCount);
    end;
    for Xcount := X2size to X3size - 1 do
    begin
      ElevationCount:=ManMatrix[Xcount,Ycount];
      if (ElevationCount > (NullDemValue)) then
      begin
        if (Max9 <= ElevationCount) then
                      Max9 := ElevationCount;
        if (Min9 >= ElevationCount) then
                      Min9 := ElevationCount;
      end;
      write(Bin_File3, ElevationCount);
    end;
  end;
  CloseFile(Bin_File1);
  CloseFile(Bin_File2);
  CloseFile(Bin_File3);

SetLength(ManMatrix, 0, 0);
WriteTileHdr(FileString, TileRG.Itemindex);
end else {message no file to divide}
                  DoMessages(30006);
end;
      9: {Divide 5 in Set size files ?? Number of files?
      DemsDir+};
    end; {Case}
  end else DoMessages(30019);
  Mainform.ProgressBar1.Position := 0;
  MainForm.ProgressBar1.Visible := False;
  Application.ProcessMessages;
end;
(*************************************************************)
{Write the headers for the just created files}
procedure TXYZ3DForm.WriteTileHdr(InFileString: string; Whatsit:
  Integer);
var
  FileString, File0String,
    File1String, File2String, File3String,
    File4String, File5String, File6String,
    File7String, File8String, File9String,
    MtnString: string;
{  DTM_Out1_File, DTM_Out2_File, DTM_Out3_File,
    DTM_Out4_File, DTM_Out5_File, DTM_Out6_File,
    DTM_Out7_File, DTM_Out8_File, DTM_Out9_File,}
    DTM_File: TextFile;
{ElevationCount:Smallint;} SoWhatb: Boolean;
  WorkWithMe, X1size, X3size, Y1size, Y3size, CodeVx: Integer;
  Tempex2, Tempex3, Tempex4, Tempex5,
  Tempey2, Tempey3, Tempey4, Tempey5,
   DemiLeftX1_, DemiTopY1e_,
   DemiRightX2e_, DemiBottomY2e_ : Double;
begin
{Read the DTM info and MAKE ? HDR files for Import}
  case Whatsit of
    0:  {Thin 1 Thinner}
    Begin
       FileString := ChangeFileExt(InFileString, '.txt');
        AssignFile(DTM_File, FileString);
        Reset(DTM_File);
        if (IoResult = 0) then begin
          Readln(DTM_File, MtnString);
          val(MtnString, X1size, CodeVx);
          Readln(DTM_File, MtnString);
          val(MtnString, Y1size, CodeVx);
          CloseFile(DTM_File);
          SoWhatb := DeleteFile(FileString);
          if (SoWhatb = False) then   DoMessages(30031);

          MtnString := ChangeFileExt(InFileString, '.deh');
          If (not ReadDehFileData(MtnString)) then exit;
          FileString := ExtractFilePath(InFileString);
          File0String := ExtractFileName(InFileString);
          WorkWithMe := Length(File0String);
          File0String := Copy(File0String, 1, WorkWithMe - 4);
          File1String := FileString + File0String + '_1.deh';
          ContourInterval:=0;
          FileSizeX:=X1size;
          FileSizeY:=Y1size;
          CellSizeX:= CellSizeX * Thinner;
          CellSizeY:= CellSizeY * Thinner;
          Vre := 0; Vde := 0; Vzx := 0;
          Vzy := 0; Vx := 0; Vy := 0;
          Vi  := 0; Vh := 0;
          XYZDEMRG.ItemIndex := 0;
          EColorsRG.ItemIndex := 0;
          MaximumElevation := Max1;
          MinimumElevation := Min1;
          ContourInterval :=
            ((MaximumElevation - MinimumElevation) div 7);
          WriteDehFile(File1String);
          MtnString := ChangeFileExt(File1String, '.flm');
          WriteFlmFile(MtnString);
        end;
      end; {7}

{Mask 1 by 2}
    1: {Combine  1&2 side}
    Begin
        FileString := ChangeFileExt(InFileString, '.txt');
        AssignFile(DTM_File, FileString);
        Reset(DTM_File);
        if (IoResult = 0) then
        begin
          Readln(DTM_File, MtnString);
          val(MtnString, X1size, CodeVx);
          Readln(DTM_File, MtnString);
          val(MtnString, Y1size, CodeVx);
          CloseFile(DTM_File);
          SoWhatb := DeleteFile(FileString);
          if (SoWhatb = False) then DoMessages(30031);
          MtnString := ChangeFileExt(InFileString, '.deh');
          If (not ReadDehFileData(MtnString)) then exit;

          FileString := ExtractFilePath(InFileString);
          File0String := ExtractFileName(InFileString);
          WorkWithMe := Length(File0String);
          File0String := Copy(File0String, 1, WorkWithMe - 4);
          File1String := FileString + File0String + '_1.deh';
          {read once write to all }
          FileSizeX:=X1size;
          FileSizeX:=Y1size;
          DemiBottomY2e:= (DemiTopY1e + (Y1size * CellSizeY));
          DemiRightX2e:= (DemiLeftX1e + (X1size * CellSizeX));
          Vre := 0; Vde := 0; Vzx := 0; Vzy := 0;
          Vx := 0; Vy := 0; Vi:= 0; Vh := 0;
          XYZDEMRG.ItemIndex := 0;
          EColorsRG.ItemIndex := 0;
          MaximumElevation := Max1;
          MinimumElevation := min1;
          ContourInterval :=
          ((MaximumElevation - MinimumElevation) div 7);

          WriteDehFile(File1String);
          MtnString := ChangeFileExt(File1String, '.flm');
          WriteFlmFile(MtnString);
        end;
      end; {7}
    2: {Combine  1&4 top};
    3: {Combine  1,2,4,5 side and top};
    4: {Combine  1..9};
    5: {Divide #5 into 5,6 files side};
    6: {Divide #5 into 5,8 files top};
    7: {Divide #5 into 5,6,8,9 files side and top};
    8: begin {Divide #5 into 9 files}
{Read the DTM info and MAKE 9 DEH and FLM files}
        FileString := ChangeFileExt(InFileString, '.txt');
        AssignFile(DTM_File, FileString);
        Reset(DTM_File);
        if (IoResult = 0) then
        begin
          Readln(DTM_File, MtnString);
          val(MtnString, X1size, CodeVx);
          Readln(DTM_File, MtnString);
          val(MtnString, X3size, CodeVx);
          Readln(DTM_File, MtnString);
          val(MtnString, Y1size, CodeVx);
          Readln(DTM_File, MtnString);
          val(MtnString, Y3size, CodeVx);
          CloseFile(DTM_File);
          SoWhatb := DeleteFile(FileString);
          if (SoWhatb = False) then DoMessages(30031);
          MtnString := ChangeFileExt(InFileString, '.deh');
          If (not ReadDehFileData(MtnString)) then exit;

          FileString := ExtractFilePath(InFileString);
          File0String := ExtractFileName(InFileString);
          WorkWithMe := Length(File0String);
          File0String := Copy(File0String, 1, WorkWithMe - 4);
          File1String := FileString + File0String + '_1.deh';
          File2String := FileString + File0String + '_2.deh';
          File3String := FileString + File0String + '_3.deh';
          File4String := FileString + File0String + '_4.deh';
          File5String := FileString + File0String + '_5.deh';
          File6String := FileString + File0String + '_6.deh';
          File7String := FileString + File0String + '_7.deh';
          File8String := FileString + File0String + '_8.deh';
          File9String := FileString + File0String + '_9.deh';

  {DemiLeftX1e,DemiTopY1e,DemiRightX2e,DemiBottomY2e
  xcounte Ycounte CellSizeY }
          Tempex2 := (DemiLeftX1e + (CellSizeX * (X1size - 1)));
          Tempex3 := (Tempex2 + CellSizeX);
          Tempex4 := (Tempex3 + (CellSizeX * (X1size - 1)));
          Tempex5 := (Tempex4 + CellSizeX);
          if (DemiRightX2e <> ((Tempex5 + (CellSizeX * (X3size - 1)))))
            then
            MessageDlg('Corruption X size', mtInformation, [mbOk], 0);
          Tempey2 := (DemiTopY1e - (CellSizeY * (Y1size - 1)));
          Tempey3 := (Tempey2 - CellSizeY);
          Tempey4 := (Tempey3 - (CellSizeY * (Y1size - 1)));
          Tempey5 := (Tempey4 - CellSizeY);
          if (DemiBottomY2e <> ((Tempey5 - (CellSizeY * (Y3size - 1)))))
            then
            MessageDlg('Corruption Y size', mtInformation, [mbOk], 0);
DemiBottomY2e_ :=DemiBottomY2e;
DemiLeftX1_ :=DemiLeftX1e;
DemiTopY1e_ :=DemiTopY1e;
DemiRightX2e_ :=DemiRightX2e;
(*     DemiLeftX1_, DemiTopY1e_, DemiRightX2e_, DemiBottomY2e_
  DemiLeftX1e:=DemiLeftX1_; 147
   DemiLeftX1e :=Tempex3; 258
   DemiLeftX1e :=Tempex5; 369
DemiTopY1e:=DemiTopY1e_; 123
  DemiTopY1e :=Tempey3; 456
  DemiTopY1e :=Tempey5; 789
  DemiRightX2e:=DemiRightX2e_;  369
    DemiRightX2e :=Tempex2;147
    DemiRightX2e :=Tempex4; 258
DemiBottomY2e:=DemiBottomY2e_;789
DemiBottomY2e:=Tempey2;123
   DemiBottomY2e:=Tempey4; 456
*)


{read once write to all        iMadeMountains:=  66;    ETopo}
          Vre := 0; Vde := 0; Vzx := 0;
          Vzy := 0; Vx := 0; Vy := 0;
          Vi  := 0; Vh := 0;
          XYZDEMRG.ItemIndex := 0;
          EColorsRG.ItemIndex := 0;
    {1} MaximumElevation := Max1;
          MinimumElevation := min1;
          ContourInterval :=
          ((MaximumElevation - MinimumElevation)  div 7);
  FileSizeX:=   X1size;     FileSizeY:= Y1size;
  DemiLeftX1e:=DemiLeftX1_;DemiTopY1e:=DemiTopY1e_;
    DemiRightX2e :=Tempex2;DemiBottomY2e:=Tempey2;
          MtnString := ChangeFileExt(File1String, '.flm');
          WriteFlmFile(MtnString);
    {2} MaximumElevation := Max2;
          MinimumElevation := min2;
          ContourInterval :=
          ((MaximumElevation - MinimumElevation) div 7);
  FileSizeX:=   X1size;     FileSizeY:= Y1size;
   DemiLeftX1e :=Tempex3;DemiTopY1e:=DemiTopY1e_;
    DemiRightX2e :=Tempex4;DemiBottomY2e:=Tempey2;
          MtnString := ChangeFileExt(File2String, '.flm');
          WriteFlmFile(MtnString);
    {3} MaximumElevation := Max3;
          MinimumElevation := min3;
          ContourInterval :=
          ((MaximumElevation - MinimumElevation) div 7);
    FileSizeX:=   X3size;     FileSizeY:= Y1size;
   DemiLeftX1e :=Tempex5;DemiTopY1e:=DemiTopY1e_;
DemiRightX2e:=DemiRightX2e_;DemiBottomY2e:=Tempey2;
          MtnString := ChangeFileExt(File3String, '.flm');
          WriteFlmFile(MtnString);
    {4} MaximumElevation := Max4;
          MinimumElevation := min4;
          ContourInterval :=
          ((MaximumElevation - MinimumElevation) div 7);
  FileSizeX:=   X1size;     FileSizeY:= Y1size;
  DemiLeftX1e:=DemiLeftX1_;  DemiTopY1e :=Tempey3;
    DemiRightX2e :=Tempex2;   DemiBottomY2e:=Tempey4;
          MtnString := ChangeFileExt(File4String, '.flm');
          WriteFlmFile(MtnString);
    {5} MaximumElevation := Max5;
          MinimumElevation := min5;
          ContourInterval :=
          ((MaximumElevation - MinimumElevation) div 7);
   FileSizeX:=   X1size;     FileSizeY:= Y1size;
   DemiLeftX1e :=Tempex3;  DemiTopY1e :=Tempey3;
    DemiRightX2e :=Tempex4;   DemiBottomY2e:=Tempey4;
          MtnString := ChangeFileExt(File5String, '.flm');
          WriteFlmFile(MtnString);
    {6} MaximumElevation := Max6;
          MinimumElevation := min6;
          ContourInterval :=
          ((MaximumElevation - MinimumElevation) div 7);
    FileSizeX:=   X3size;     FileSizeY:= Y1size;
   DemiLeftX1e :=Tempex5;    DemiTopY1e :=Tempey3;
DemiRightX2e:=DemiRightX2e_;   DemiBottomY2e:=Tempey4;
          MtnString := ChangeFileExt(File6String, '.flm');
          WriteFlmFile(MtnString);
    {7} MaximumElevation := Max7;
          MinimumElevation := min7;
          ContourInterval :=
          ((MaximumElevation - MinimumElevation) div 7);
   FileSizeX:=   X1size;     FileSizeY:= Y3size;
   DemiLeftX1e:=DemiLeftX1_;  DemiTopY1e :=Tempey5;
    DemiRightX2e :=Tempex2;DemiBottomY2e:=DemiBottomY2e_;
          MtnString := ChangeFileExt(File7String, '.flm');
          WriteFlmFile(MtnString);
    {8} MaximumElevation := Max8;
          MinimumElevation := min8;
          ContourInterval :=
          ((MaximumElevation - MinimumElevation) div 7);
  FileSizeX:=   X1size;     FileSizeY:= Y3size;
   DemiLeftX1e :=Tempex3;     DemiTopY1e :=Tempey5;
    DemiRightX2e :=Tempex4;DemiBottomY2e:=DemiBottomY2e_;
          MtnString := ChangeFileExt(File8String, '.flm');
          WriteFlmFile(MtnString);
    {9} MaximumElevation := Max9;
          MinimumElevation := min9;
          ContourInterval :=
          ((MaximumElevation - MinimumElevation) div 7);
    FileSizeX:=   X3size;    FileSizeY:= Y3size;
   DemiLeftX1e :=Tempex5;     DemiTopY1e :=Tempey5;
DemiRightX2e:=DemiRightX2e_;DemiBottomY2e:=DemiBottomY2e_;
          MtnString := ChangeFileExt(File9String, '.flm');
          WriteFlmFile(MtnString);
        end;
      end; {7}
    9: begin {Divide #5 into set size files
              : Figure out how MANY files}
       end;
  end; {case}
end;
(*************************************************************)
(*************************************************************)


(***************************************************)
(***************************************************)
procedure TXYZ3DForm.EHelpClick(Sender: TObject);
begin
  Application.HelpContext(4700); {D Elevation map 4700}
end;

procedure TXYZ3DForm.ELoadClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Fractal Mountain (flm or rcd)=bin|*.flm;*.rcd;*.bin';
  OpenDialog1.Filename := EFileNameEdit.Text;
  OpenDialog1.InitialDir:=DemsDir;
  if (OpenDialog1.Execute) then
  begin
    DemsDir:=ExtractFilePath(OpenDialog1.Filename);
    EleLoadDo(OpenDialog1.FileName);
  end;
end;

procedure TXYZ3DForm.EleLoadDo(FilesS: string);
var
  MtnString, MyFilesS: string;
{  fMtnFile: TextFile; }{i,Code:Integer;}
{DEM_Int:Smallint;
Dem_File: File of Smallint;}
  F_File: file of Smallint; {Integer;}
{V1e,V2e:Extended;
Vzx,Vzy,Vx,Vy,Vi,Vh,CodeVzx,CodeVzy,}
  Xcount, Ycount, FileCode: Integer;
{1}begin { Display Open dialog box }
  FileCode := -1;
  if((fileExists(ChangeFileExt(FilesS, '.bin')))and
  ( (fileExists(ChangeFileExt(FilesS, '.flm')))
       or (fileExists(ChangeFileExt(FilesS, '.rcd'))))) then
{1+}  begin
    Mainform.ProgressBar1.Position := 0;
    MainForm.ProgressBar1.Visible := True;
  {Switch}
  MyFilesS := ChangeFileExt(FilesS, '.flm');
  if (fileExists(MyFilesS)) then
{2 flm}  begin
    FileCode := 0;
    EFileNameEdit.Text := Extractfilename(MyFilesS);
    Application.ProcessMessages;
    FractalFileName := FilesS;
    DemsDir:=ExtractFilePath(MyFilesS);
    If ReadFlmFileData(MyFilesS)
    then DisplayFlmData;
  MyFilesS := ChangeFileExt(FilesS, '.deh');
  if (fileExists(MyFilesS)) then
    If ReadDehFileData(MyFilesS) then
    DisplayDehData;
{    LoadFlmFile}
{Need a Display varibles procedure}
    Application.ProcessMessages;
{-2}   end else
{2 rcd}  begin {Switch}
    FileCode := 2;
    MyFilesS := ChangeFileExt(FilesS, '.rcd');
    if (fileExists(MyFilesS)) then
{3}    begin
      FileCode := 0;
      EFileNameEdit.Text := MyFilesS;
      FractalFileName := FilesS;
      DemsDir:=ExtractFilePath(FilesS);
      MainForm.ReadDataFile;
      EColorsRG.ItemIndex := Fractalgorhythym;
      str(MaximumElevation, MtnString);
      DemMaxE.Caption := 'Maximum Elevation:  ' + MtnString;
      {EMaxEdit.Text:=MtnString;}
      str(MinimumElevation, MtnString);
      DemMinE.Caption := 'Minimum Elevation:  ' + MtnString;
      {EMinEdit.Text:=MtnString;}
      str(ContourInterval, MtnString);
      if (ContourInterval = 0) then
{4}      begin
        ContourInterval :=
          ((MaximumElevation - MinimumElevation) div 7);
        str(ContourInterval, MtnString);
        DemCIE.Caption := 'Contour Interval:  ' + MtnString;
{-4}      end else DemCIE.Caption := 'Contour Interval:  ' + MtnString;
      iMadeMountains := 50;
      NullDemValue:=-32767;
      Vre := 0; Vde := 0;
      Vzx := 0; Vzy := 0; Vx := 0; Vy := 0; Vi := 0; Vh := 0;
{Need a Display varibles procedure}
    Application.ProcessMessages;
{-3}    end;
{-2}end;
{1+}end else DoMessages(30020);
      {ShowMessage('Bin file not yet processed');}
  if (FileCode > 0) then
{2}  begin
    if (FileCode = 1) then  DoMessages(30021);
      {ShowMessage('Fractal MetaData file is bad'); }
    if (FileCode = 2) then DoMessages(30022);
      {ShowMessage('Mountain MetaData file is bad'); }
{-2}  end else if (FileCode = 0) then
{2 read dem}  begin
    MainForm.HintPanel.Caption :=
      'Reading Fractal Mountains from disk';
    Application.ProcessMessages;
    FractalFileMatrix := ChangeFileExt(FractalFilename, '.bin');

      AssignFile(F_File, FractalFileMatrix);
      Reset(F_File);
      if (IoResult <> 0) then
{3}      begin
        DoMessages(30016);
        CloseFile(F_File);
{-3}      end else
{3}      begin
        SetLength(ManMatrix, FileSizeX, FileSizeY);
        for Ycount := 0 to FileSizeY - 1 do
        begin
          MainForm.ProgressBar1.Visible := True;
          Mainform.ProgressBar1.Position :=
          Round((Ycount / (FileSizeY- 1)) * 100);
          Application.ProcessMessages;
          for Xcount := 0 to FileSizeX - 1 do
          begin
            Read(F_File, ManMatrix[Xcount, Ycount]);
          end;
        end;
        CloseFile(F_File);
        IsDemLoaded:=True;
        EMouse.Enabled:=True;
        EleRun.Enabled:=True;
        ESave.Enabled:=True;
        Mainform.ProgressBar1.Position := 0;
        MainForm.ProgressBar1.Visible := False;
        MainForm.HintPanel.Caption := 'Hints for Sail';
        Application.ProcessMessages;
{-3}      end;
{-2}    end;
{    if (FileCode <> 1) then ;}{? Compile shutup or bad logic?}
    Mainform.ProgressBar1.Position := 0;
    MainForm.ProgressBar1.Visible := False;
    Application.ProcessMessages;
{-1}end;


procedure TXYZ3DForm.ESaveClick(Sender: TObject);
var MyFilesS: string;
begin
  SaveDialog1.Filter := 'Fractal Mountain files|*.rcd;*.flm';
  SaveDialog1.Filename := EFileNameEdit.Text;
  SaveDialog1.InitialDir:=DemsDir;
  if (SaveDialog1.Execute) then
  begin
    DemsDir:=ExtractFilePath(SaveDialog1.FileName);
{save DEM ?}
    MyFilesS := Uppercase(ExtractFileExt(SaveDialog1.FileName));
    if (MyFilesS = '.FLM') then
    begin
      FractalFileName := SaveDialog1.FileName;
      EFileNameEdit.Text := SaveDialog1.FileName;
      WriteFlmFile(SaveDialog1.FileName);
    end else
    if MyFilesS = '.RCD' then
    begin
      FractalFileName := SaveDialog1.FileName;
      EFileNameEdit.Text := SaveDialog1.FileName;
      Fractalgorhythym := EColorsRG.ItemIndex;
      MainForm.WriteDataFile;
    end;
  end;
end;


procedure TXYZ3DForm.EleRunClick(Sender: TObject);
var
Demsize, holdstill:Integer;
   f: file of Byte;
begin {Make it happen image maker}
  holdstill:=XYZDEMRG.ItemIndex;

If IsDemLoaded then
begin
     SystemInfoForm.LoadEdits;
     AssignFile(f, ChangeFileExt(OpenDialog1.FileName, '.bin'));
     Reset(f);
     Demsize := FileSize(f);
     CloseFile(f);
     If ((Demsize*3) > MemoryAvailable) then
     begin
     If DoMessagesOK(10002)<>mrYes then exit;
     {Info.. Warning..Error   Confirmation requires result
     Too big for physical memory. try anyway?  Yes No}
     end;
     If holdstill < 5 then
     begin
     if ((FYImageX <> FileSizeX) or (FYImageY <> FileSizeY)) then
     begin
     FYImageX := (FileSizeX);
     FYImageY := (FileSizeY);
     MainForm.OverBit;
     end;
     MainForm.DoImageStart;

     case holdstill  of
     0: ElevationDisplay;
     1: ; {Slope}
     2: ContourDisplay;
     3: ; {LOS}
     4: WireDisplay; {Masked area}
{     5: WireDisplay;
     6: Globe;}
     end; {case}
     Mainform.DoImageDone;
     end
     else If ((holdstill= 5)or(holdstill= 6)) then
     begin
       If holdstill= 5 then
       begin
{         WireDisplay;}
         {XYZGL}dtmGlForm.FormShowDown;
         {XYZGL}dtmGlForm.show;
       end else
{       If holdstill= 6 then  Globe};
     end;
end;
end;

procedure TXYZ3DForm.EleRunDo;
begin
  case XYZDEMRG.ItemIndex of
    0: ElevationDisplay;
    1: ; {Slope}
    2: ContourDisplay;
    3: ; {LOS}
    4: ; {Masked area}
    5: WireDisplay;
    6: ; {Globe}
  end; {case}
end;

procedure TXYZ3DForm.ElevationDisplay;
var {Placemat,}
  Whatis1i, Whatis2i, Whatis3i, Whatis4i,
    Whatis5i, Whatis6i, Whatis7i, Whatis8i,
    Elevator, maxcolx, maxrowy, I, K, CodeVx: Integer;
  Whatis1, Whatis2, Whatis3, Whatis4,
    Whatis5, Whatis6, Whatis7, Whatis8, Whatis9: Double;
  TempColor: TColor; MtnString: string;
begin
  MainForm.ProgressBar1.Position:=0;
  MainForm.ProgressBar1.Visible := True;
  Application.ProcessMessages;
{Range:=(MaximumElevation-MinimumElevation);}
{ContourInterval:=(Range div 7);}
{Step:=(Range * ContourInterval);}
  if (ContourInterval = 0) then begin
    ContourInterval := ((MaximumElevation - MinimumElevation) div 7);
    str(ContourInterval, MtnString);
    DemCIE.Caption := '7 Contour Intervals:  ' + MtnString;
  end;
{val( EMaxEdit.Text,MaximumElevation,CodeVx);Codefx(EMaxEdit.Text,CodeVx);
val( EMinEdit.Text,MinimumElevation,CodeVx);Codefx(EMinEdit.Text,CodeVx);
val( ECIEdit.Text,ContourInterval,CodeVx);Codefx(ECIEdit.Text,CodeVx);   }
  Fractalgorhythym := EColorsRG.ItemIndex;
  maxcolx := (FileSizeX - 1);
  maxrowy := (FileSizeY - 1);
{        Placemat:=trunc(FYImageY / FileSizeY);
        If (Placemat = 0) then Placemat := 1;}
  with MainForm.Image2.Canvas do begin

    if (Fractalgorhythym = 0) then begin
{Predetermine 9 Terrain RANGES having 255 units}
      Whatis1 := ((MinimumElevation + 7001) / 255);
      Whatis2 := (999 / 255);
      Whatis3 := (3999 / 255);
      Whatis4 := (2001 / 255);
      Whatis5 := (399 / 255);
      Whatis6 := (599 / 255);
      Whatis9 := ((MaximumElevation - 3001) / 255);
      for K := 0 to maxrowy do begin
        Mainform.ProgressBar1.Position :=
        Round((K / (FYImageY - 1)) * 100);
        Application.ProcessMessages;
        for I := 0 to maxcolx do begin
          Elevator := ManMatrix[I, K];
          If (NullDemValue = Elevator)then Pixels[I, K] := clBlue else
          case Elevator of
            -50000.. - 7001: {Smog 255,255,255}
              begin
                TempColor := RGB(255 - abs(round(Elevator / Whatis1)
                  mod 255),
                  255 - abs(round(Elevator / Whatis1) mod 255),
                  255 - abs(round(Elevator / Whatis1) mod 255));
                Pixels[I, K] := TempColor;
              end;
            -7000.. - 6001: { Purple 255,0,255}
              begin
                TempColor := RGB(255 - abs(round(Elevator / Whatis2)
                  mod 255),
                  0,
                  255 - abs(round(Elevator / Whatis2) mod 255));
                Pixels[I, K] := TempColor;
              end;
            -6000.. - 2001: {Blue 0,0,255 }
              begin
                TempColor := RGB(0,
                  0,
                  255 - abs(round(Elevator / Whatis3) mod 255));
                Pixels[I, K] := TempColor;
              end;
            -2000..0: {Aqua 0,255,255 }
              begin
                TempColor := RGB(0,
                  255 - abs(round(Elevator / Whatis4) mod 255),
                  255 - abs(round(Elevator / Whatis4) mod 255));
                Pixels[I, K] := TempColor;
              end;
            1..400: {  Green 0,255,0}
              begin
                TempColor := RGB(0,
                  abs(round(Elevator / Whatis5) mod 255),
                  0);
                Pixels[I, K] := TempColor;
              end;
            401..1000: {Yellow 255,255,0}
              begin
                TempColor := RGB(abs(round(Elevator / Whatis6) mod
                  255),
                  abs(round(Elevator / Whatis6) mod 255),
                  0);
                Pixels[I, K] := TempColor;
              end;
            1001..2000: { Red 255,0,0}
              begin
                TempColor := RGB(abs(round(Elevator / Whatis2) mod
                  255),
                  0,
                  0);
                Pixels[I, K] := TempColor;
              end;
            2001..3000: { Purple 255,0,255}
              begin
                TempColor := RGB(abs(round(Elevator / Whatis2) mod
                  255),
                  0,
                  abs(round(Elevator / Whatis2) mod 255));
                Pixels[I, K] := TempColor;
              end;
            3001..50000: { Smog 255,255,255}
              begin
                TempColor := RGB(abs(round(Elevator / Whatis9) mod
                  255),
                  abs(round(Elevator / Whatis9) mod 255),
                  abs(round(Elevator / Whatis9) mod 255));
                Pixels[I, K] := TempColor;
              end;
          end; {Case}
        end;
      end;
    end else
      if (Fractalgorhythym = 1) then begin
        for K := 0 to maxrowy do begin
          Mainform.ProgressBar1.Position :=
           Round((K / (FYImageY - 1)) * 100);
          Application.ProcessMessages;
          for I := 0 to maxcolx do begin
            Elevator := ManMatrix[I, K];
{Blue 0,0,255 }
{Aqua 0,255,255 }
{Green 0,255,0}
{Yellow 255,255,0}
{Red 255,0,0}
{Purple 255,0,255}
{Smog 255,255,255}
       If (NullDemValue = Elevator)then Pixels[I, K] := clBlue else

            if ((Elevator >= MinimumElevation)
              and (Elevator <= (MinimumElevation + ContourInterval)))
                then begin
              TempColor := RGB(0, 0,
                ((Elevator - (MinimumElevation)) mod 255));
              Pixels[I, {FYImageY} K {*Placemat}] := TempColor;
                {Blue 0,0,255 }
            end else
              if ((Elevator >= (MinimumElevation + ContourInterval))
                and (Elevator <= (MinimumElevation + (ContourInterval
                  * 2)))) then begin
                TempColor := RGB(0,
                  ((Elevator - (MinimumElevation + (ContourInterval)))
                    mod 255),
                  ((Elevator - (MinimumElevation + (ContourInterval)))
                    mod 255));
                Pixels[I, {FYImageY} K {*Placemat}] := TempColor;
                  {Aqua 0,255,255 }
              end else
                if ((Elevator >= (MinimumElevation + (ContourInterval
                  * 2)))
                  and (Elevator <= (MinimumElevation +
                    (ContourInterval * 3)))) then begin
                  TempColor := RGB(0,
                    ((Elevator - (MinimumElevation + (ContourInterval
                      * 2))) mod 255),
                    0);
                  Pixels[I, {FYImageY} K {*Placemat}] := TempColor;
                    {Green 0,255,0}
                end else
                  if ((Elevator >= (MinimumElevation +
                    (ContourInterval * 3)))
                    and (Elevator <= (MinimumElevation +
                      (ContourInterval * 4)))) then begin
                    TempColor := RGB(((Elevator - (MinimumElevation +
                      (ContourInterval * 3))) mod 255),
                      ((Elevator - (MinimumElevation +
                        (ContourInterval * 3))) mod 255),
                      0);
                    Pixels[I, {FYImageY} K {*Placemat}] := TempColor;
                      {Yellow 255,255,0}
                  end else
                    if ((Elevator >= (MinimumElevation +
                      (ContourInterval * 4)))
                      and (Elevator <= (MinimumElevation +
                        (ContourInterval * 5)))) then begin
                      TempColor := RGB(((Elevator - (MinimumElevation
                        + (ContourInterval * 4))) mod 255),
                        0, 0);
                      Pixels[I, {FYImageY} K {*Placemat}] :=
                        TempColor; {Red 255,0,0}
                    end else
                      if ((Elevator >= (MinimumElevation +
                        (ContourInterval * 5)))
                        and (Elevator <= (MinimumElevation +
                          (ContourInterval * 6)))) then begin
                        TempColor := RGB(((Elevator -
                          (MinimumElevation + (ContourInterval * 5)))
                          mod 255),
                          0,
                          ((Elevator - (MinimumElevation +
                            (ContourInterval * 5))) mod 255));
                        Pixels[I, {FYImageY} K {*Placemat}] :=
                          TempColor; {Purple 255,0,255}
                      end else
                        if ((Elevator >= (MinimumElevation +
                          (ContourInterval * 6)))
                          and (Elevator <= (MinimumElevation +
                            (ContourInterval * 7)))) then begin
                          TempColor := RGB(((Elevator -
                            (MinimumElevation + (ContourInterval * 6)))
                            mod 255),
                            ((Elevator - (MinimumElevation +
                              (ContourInterval * 6))) mod 255),
                            ((Elevator - (MinimumElevation +
                              (ContourInterval * 6))) mod 255));
                          Pixels[I, {FYImageY} K {*Placemat}] :=
                            TempColor; {Smog 255,255,255}
                        end;
          end;
        end;
      end else
        if (Fractalgorhythym = 2) then begin
          for K := 0 to maxrowy do begin
            Mainform.ProgressBar1.Position := Round((K / (FYImageY -
              1)) * 100);
            Application.ProcessMessages;
            for I := 0 to maxcolx do begin
              Elevator := ManMatrix[I, K];
              TempColor := RGB(Colors[0, ((Elevator mod 255))],
                Colors[1, ((Elevator mod 255))],
                Colors[2, ((Elevator mod 255))]);
              Pixels[I, {FYImageY-} K] := TempColor;
            end;
          end;
        end else {Most only rotate 16 color set, ? to ? use 256 colors}
          if (Fractalgorhythym = 3) then begin
            for K := 0 to maxrowy do begin
              Mainform.ProgressBar1.Position :=
              Round((K / (FYImageY - 1)) * 100);
              Application.ProcessMessages;
              for I := 0 to maxcolx do begin
                Elevator := ManMatrix[I, K];
                if (Elevator = MaximumElevation) then
                  TempColor := RGB(RGBArray[0, 0],
                    RGBArray[1, 0],
                    RGBArray[2, 0])
                else
                  if (Elevator < 16) then
                    TempColor := RGB(RGBArray[0, Elevator],
                      RGBArray[1, Elevator],
                      RGBArray[2, Elevator])
                  else
                    TempColor := RGB(Colors[0, ((Elevator mod 255))],
                      Colors[1, ((Elevator mod 255))],
                      Colors[2, ((Elevator mod 255))]);
                Pixels[I, {FYImageY-} K] := TempColor;
              end;
            end;
          end else
            if (Fractalgorhythym = 4) then begin
{Determine 9 Terrain RANGES having 255 units}
              val(ECI1Edit.Text, Whatis1i, CodeVx);
                Codefx(ECI1Edit.Text, CodeVx);
              val(ECI2Edit.Text, Whatis2i, CodeVx);
                Codefx(ECI2Edit.Text, CodeVx);
              val(ECI3Edit.Text, Whatis3i, CodeVx);
                Codefx(ECI3Edit.Text, CodeVx);
              val(ECI4Edit.Text, Whatis4i, CodeVx);
                Codefx(ECI4Edit.Text, CodeVx);
              val(ECI5Edit.Text, Whatis5i, CodeVx);
                Codefx(ECI5Edit.Text, CodeVx);
              val(ECI6Edit.Text, Whatis6i, CodeVx);
                Codefx(ECI6Edit.Text, CodeVx);
              val(ECI7Edit.Text, Whatis7i, CodeVx);
                Codefx(ECI7Edit.Text, CodeVx);
              val(ECI8Edit.Text, Whatis8i, CodeVx);
                Codefx(ECI8Edit.Text, CodeVx);

              Whatis1 := ((MinimumElevation + Whatis1i) / 255);
              Whatis2 := ((Whatis1i + Whatis2i) / 255);
              Whatis3 := ((Whatis2i + Whatis3i) / 255);
              Whatis4 := ((Whatis4i + Whatis3i) / 255);
              Whatis5 := ((Whatis5i - Whatis4i) / 255);
              Whatis6 := ((Whatis6i - Whatis5i) / 255);
              Whatis7 := ((Whatis7i - Whatis6i) / 255);
              Whatis8 := ((Whatis8i - Whatis7i) / 255);
              Whatis9 := ((MaximumElevation - Whatis8i) / 255);
              for K := 0 to maxrowy do begin
                Mainform.ProgressBar1.Position := Round((K / (FYImageY
                  - 1)) * 100);
                Application.ProcessMessages;
                for I := 0 to maxcolx do begin
                  Elevator := ManMatrix[I, K];
{Blue 0,0,255 }
{Aqua 0,255,255 }
{Green 0,255,0}
{Yellow 255,255,0}
{Red 255,0,0}
{Purple 255,0,255}
{Smog 255,255,255}
          If (NullDemValue = Elevator)then Pixels[I, K] := clBlue else

                  if ((Elevator >= MinimumElevation)
                    and (Elevator <= (Whatis1i))) then begin
                    TempColor := RGB(255 - abs(round(Elevator /
                      Whatis1) mod 255),
                      255 - abs(round(Elevator / Whatis1) mod 255),
                      255 - abs(round(Elevator / Whatis1) mod 255));
                    Pixels[I, K] := TempColor; {Smog 255,255,255}
                  end else
                    if ((Elevator >= Whatis1i)
                      and (Elevator <= Whatis2i)) then begin
                      TempColor := RGB(255 - abs(round(Elevator /
                        Whatis2) mod 255),
                        0,
                        255 - abs(round(Elevator / Whatis2) mod
                          255));
                      Pixels[I, K] := TempColor; {Purple 255,0,255}
                    end else
                      if ((Elevator >= Whatis2i)
                        and (Elevator <= Whatis3i)) then begin
                        TempColor := RGB(0,
                          0,
                          255 - abs(round(Elevator / Whatis3) mod
                            255));
                        Pixels[I, K] := TempColor; {Blue 0,0,255 }
                      end else
                        if ((Elevator >= Whatis3i)
                          and (Elevator <= Whatis4i)) then begin
                          TempColor := RGB(0,
                            255 - abs(round(Elevator / Whatis4) mod
                              255),
                            255 - abs(round(Elevator / Whatis4) mod
                              255));
                          Pixels[I, K] := TempColor;
                        end else {Aqua 0,255,255 }
                          if ((Elevator >= Whatis4i)
                            and (Elevator <= Whatis5i)) then begin
                            TempColor := RGB(0,
                              255 - abs(round(Elevator / Whatis5) mod
                                255),
                              0);
                            Pixels[I, K] := TempColor;
                          end else {Green 0,255,0}
                            if ((Elevator >= Whatis5i)
                              and (Elevator <= Whatis6i)) then begin
                              TempColor := RGB(255 -
                                abs(round(Elevator / Whatis6) mod 255),
                                255 - abs(round(Elevator / Whatis6)
                                  mod 255),
                                0);
                              Pixels[I, K] := TempColor;
                            end else {Yellow 255,255,0}
                              if ((Elevator >= Whatis6i)
                                and (Elevator <= Whatis7i)) then begin
                                TempColor := RGB(255 -
                                  abs(round(Elevator / Whatis7) mod
                                  255),
                                  0,
                                  0);
                                Pixels[I, K] := TempColor;
                              end else {Red 255,0,0}
                                if ((Elevator >= Whatis7i)
                                  and (Elevator <= Whatis8i)) then
                                    begin
                                  TempColor := RGB(255 -
                                    abs(round(Elevator / Whatis8) mod
                                    255),
                                    0,
                                    255 - abs(round(Elevator /
                                      Whatis8) mod 255));
                                  Pixels[I, K] := TempColor;
                                end else {Purple 255,0,255}
                                  if ((Elevator >= Whatis8i)
                                    and (Elevator <=
                                      MaximumElevation)) then begin
                                    TempColor := RGB(255 -
                                      abs(round(Elevator / Whatis9) mod
                                      255),
                                      255 - abs(round(Elevator /
                                        Whatis9) mod 255),
                                      255 - abs(round(Elevator /
                                        Whatis9) mod 255));
                                    Pixels[I, K] := TempColor;
                                  end; {Smog 255,255,255}
                end;
              end;
            end;
  end;
  Mainform.ProgressBar1.Position := 0;
  MainForm.ProgressBar1.Visible := False;
  Application.ProcessMessages;
end;


procedure TXYZ3DForm.ContourDisplay;
var {Placemat,}  C1, C2, C3, C4, C5, C6, C7,
  sX, sY, Elevator, Elevated, maxcolx, maxrowy, I, K: Integer;
  TempColor: TColor; MtnString: string;
begin
  MainForm.ProgressBar1.Visible := True;
  Application.ProcessMessages;
{Range:=(MaximumElevation-MinimumElevation);}
{ContourInterval:=(Range div 7);}
{Step:=(Range * ContourInterval);}
  if (ContourInterval = 0) then begin
    ContourInterval := ((MaximumElevation - MinimumElevation) div 7);
    str(ContourInterval, MtnString);
    DemCIE.Caption := 'Contour Interval:  ' + MtnString;
  end;
{val( EMaxEdit.Text,MaximumElevation,CodeVx);Codefx(EMaxEdit.Text,CodeVx);
val( EMinEdit.Text,MinimumElevation,CodeVx);Codefx(EMinEdit.Text,CodeVx);
val( ECIEdit.Text,ContourInterval,CodeVx);Codefx(ECIEdit.Text,CodeVx);}
  Fractalgorhythym := EColorsRG.ItemIndex;
  maxcolx := (FileSizeX - 2);
  maxrowy := (FileSizeY - 2);
  C1 := (MinimumElevation + ContourInterval);
  C2 := (MinimumElevation + (ContourInterval * 2));
  C3 := (MinimumElevation + (ContourInterval * 3));
  C4 := (MinimumElevation + (ContourInterval * 4));
  C5 := (MinimumElevation + (ContourInterval * 5));
  C6 := (MinimumElevation + (ContourInterval * 6));
  C7 := (MinimumElevation + (ContourInterval * 7));

{        Placemat:=trunc(FYImageY / FileSizeY);
        If (Placemat = 0) then Placemat := 1;}
  with MainForm.Image2.Canvas do begin
    for K := 1 to maxrowy do begin
      Mainform.ProgressBar1.Position := Round((K / (FYImageX - 1)) *
        100);
      Application.ProcessMessages;

      for I := 1 to maxcolx do begin
        Elevator := ManMatrix[I, K];
{Blue 0,0,255 }
{Aqua 0,255,255 }
{Green 0,255,0}
{Yellow 255,255,0}
{Red 255,0,0}
{Purple 255,0,255}
{Smog 255,255,255}
        if (Elevator = MaximumElevation) then
          Pixels[I, {FYImageY} K {*Placemat}] := clWhite
        else
          if ((Elevator >= MinimumElevation)
            and (Elevator <= C1)) then begin
            Pixels[I, {FYImageY} K {*Placemat}] := clWhite;
            for sY := -1 to 1 do begin
              for sX := -1 to 1 do begin
                Elevated := ManMatrix[I + sX, K + sY];
                if ((Elevator = C1) or
                  ((Elevator < C1) and (Elevated > C1))
                  or ((Elevator > C1) and (Elevated < C1)))
                  then begin
                  TempColor := RGB(0, 0,
                    ({(Elevator-(MinimumElevation)) mod } 255));
                  Pixels[I, {FYImageY} K {*Placemat}] := TempColor;
                    {Blue 0,0,255 }
                end;
              end;
            end;
          end else
            if ((Elevator >= C1) and (Elevator <= C2)) then begin
              Pixels[I, {FYImageY} K {*Placemat}] := clWhite;
              for sY := -1 to 1 do begin
                for sX := -1 to 1 do begin
                  Elevated := ManMatrix[I + sX, K + sY];
                  if ((Elevator = C2) or
                    ((Elevator < C2) and (Elevated > C2))
                    or ((Elevator > C2) and (Elevated < C2)))
                    then begin
                    TempColor := RGB(0,
                      ({(Elevator-(MinimumElevation+(ContourInterval))) mod}
                        255),
                      ({(Elevator-(MinimumElevation+(ContourInterval))) mod }
                        255));
                    Pixels[I, {FYImageY} K {*Placemat}] := TempColor;
                      {Aqua 0,255,255 }
                  end;
                end;
              end;
            end else
              if ((Elevator >= C2) and (Elevator <= C3)) then begin
                Pixels[I, K] := clWhite;
                for sY := -1 to 1 do begin
                  for sX := -1 to 1 do begin
                    Elevated := ManMatrix[I + sX, K + sY];
                    if ((Elevator = C3) or
                      ((Elevator < C3) and (Elevated > C3))
                      or ((Elevator > C3) and (Elevated < C3)))
                      then begin
                      TempColor := RGB(0,
                        ({(Elevator-(MinimumElevation+(ContourInterval*2))) mod }
                          255),
                        0);
                      Pixels[I, {FYImageY} K {*Placemat}] :=
                        TempColor; {Green 0,255,0}
                    end;
                  end;
                end;
              end else
                if ((Elevator >= C3) and (Elevator <= C4)) then begin
                  Pixels[I, K] := clWhite;
                  for sY := -1 to 1 do begin
                    for sX := -1 to 1 do begin
                      Elevated := ManMatrix[I + sX, K + sY];
                      if ((Elevator = C4) or
                        ((Elevator < C4) and (Elevated > C4))
                        or ((Elevator > C4) and (Elevated < C4)))
                        then begin
                        TempColor :=
                          RGB(({(Elevator-(MinimumElevation+(ContourInterval*3))) mod} 255),
                          ({(Elevator-(MinimumElevation+(ContourInterval*3))) mod }
                            255),
                          0);
                        Pixels[I, K] := TempColor; {Yellow 255,255,0}
                      end;
                    end;
                  end;
                end else
                  if ((Elevator >= C4) and (Elevator <= C5)) then
                    begin
                    Pixels[I, K] := clWhite;
                    for sY := -1 to 1 do begin
                      for sX := -1 to 1 do begin
                        Elevated := ManMatrix[I + sX, K + sY];
                        if ((Elevator = C5) or
                          ((Elevator < C5) and (Elevated > C5))
                          or ((Elevator > C5) and (Elevated < C5)))
                          then begin
                          TempColor :=
                            RGB(({(Elevator-(MinimumElevation+(ContourInterval*4))) mod} 255),
                            0, 0);
                          Pixels[I, {FYImageY} K {*Placemat}] :=
                            TempColor; {Red 255,0,0}
                        end;
                      end;
                    end;
                  end else
                    if ((Elevator >= C5) and (Elevator <= C6)) then
                      begin
                      Pixels[I, K] := clWhite;
                      for sY := -1 to 1 do begin
                        for sX := -1 to 1 do begin
                          Elevated := ManMatrix[I + sX, K + sY];
                          if ((Elevator = C6) or
                            ((Elevator < C6) and (Elevated > C6))
                            or ((Elevator > C6) and (Elevated < C6)))
                            then begin
                            TempColor :=
                              RGB(({(Elevator-(MinimumElevation+(ContourInterval*5))) mod} 255),
                              0,
                              ({(Elevator-(MinimumElevation+(ContourInterval*5))) mod}
                                255));
                            Pixels[I, {FYImageY} K {*Placemat}] :=
                              TempColor; {Purple 255,0,255}
                          end;
                        end;
                      end;
                    end else
                      if ((Elevator >= C6) and (Elevator <= C7)) then
                        begin
                        Pixels[I, K] := clWhite;
                        for sY := -1 to 1 do begin
                          for sX := -1 to 1 do begin
                            Elevated := ManMatrix[I + sX, K + sY];
                            if ((Elevator = C7) or
                              ((Elevator < C7) and (Elevated > C7))
                              or ((Elevator > C7) and (Elevated <
                                C7)))
                              then begin
                              TempColor := RGB(((Elevator -
                                (MinimumElevation + (ContourInterval *
                                6))) mod 255),
                                ((Elevator - (MinimumElevation +
                                  (ContourInterval * 6))) mod 255),
                                ((Elevator - (MinimumElevation +
                                  (ContourInterval * 6))) mod 255));
                              Pixels[I, {FYImageY} K {*Placemat}] :=
                                TempColor; {Smog 255,255,255}
                            end;
                          end;
                        end;
                      end;
      end;
    end;
  end;
  Mainform.ProgressBar1.Position := 0;
  MainForm.ProgressBar1.Visible := False;
  Application.ProcessMessages;
end;



procedure TXYZ3DForm.WireDisplay;
var C, MtnRowY, Y_Height, maxcolx, maxrowy, I, K {,CodeVx}: Integer;
  TempColor: TColor;
  HM: array[0..4096] of Extended;
{Color: array[0..4096] of Integer;  }
  View_Angle, Illum_Angle, MtnZ, Z, {X, R,} Y, H, HS: Extended;
{MtnString:String;}
begin
  MainForm.ProgressBar1.Visible := True;
  Application.ProcessMessages;
{Range:=(MaximumElevation-MinimumElevation);}
{ContourInterval:=(Range div 7);}
{Step:=(Range * ContourInterval);}
{val( EMaxEdit.Text,MaximumElevation,CodeVx);Codefx(EMaxEdit.Text,CodeVx);
val( EMinEdit.Text,MinimumElevation,CodeVx);Codefx(EMinEdit.Text,CodeVx);
val( ECIEdit.Text,ContourInterval,CodeVx);Codefx(ECIEdit.Text,CodeVx);
Fractalgorhythym:=EColorsRG.ItemIndex;
}
  maxcolx := (FileSizeX - 1);
  maxrowy := (FYImageY {FileSizeY} - 1);
  MtnRowY := 47;
  MtnZ := 10.0;
  case iMadeMountains of
    0: begin
        MtnRowY := 47;
        MtnZ := 10.0;
      end;
    1: begin
        MtnRowY := 119;
        MtnZ := 5.0;
      end;
    2: begin
        MtnRowY := 479;
        MtnZ := 1.0;
      end;
    50: begin
        MtnRowY := 479;
        MtnZ := 1.0;
      end;
    60..90:
      begin
        MtnRowY := FileSizeY - 1;
        MtnZ := 1.0;
      end;
  end; {Case}
(*
   If ( (MtnRowY > (FYImageY-1))or
        (FileSizeX > (FYImageY))or
        (FileSizeY > (FYImageY)) ) then begin
        ShowMessage
          ('State of Confusion'+#13#10+
          'File size larger than Screen'+#13#10+
           'Display Top Left portion');
        If (MtnRowY > (FYImageY-1)) then MtnRowY:= (FYImageY-1);
        If (FileSizeX > (FYImageY)) then FileSizeX := (FYImageY);
        If (FileSizeY > (FYImageY))  then FileSizeY := (FYImageY);
 maxcolx :=(FileSizeX-1);
 maxrowy := (FYImageY{FileSizeY}-1);
        end;
*)
  Z := 0.0;
  HS := 0.0;
  View_Angle := 2.0; {3.0; steep} {2.0  Makes an angle of 30 degrees}
  Illum_Angle := 0.5; {0.3;} {0.5  Makes an angle of 30 degrees}
  for I := 0 to maxcolx do HM[I] := 0.0;
  with MainForm.Image2.Canvas do begin
    for K := 0 to MtnRowY do begin
      Mainform.ProgressBar1.Position := Round((K / (FYImageX - 1)) *
        100);
      Application.ProcessMessages;

      for I := 0 to maxcolx do begin
        Y := ManMatrix[I, K];
        H := Y + (Z / View_Angle);
        HS := HS - Illum_Angle;
        if H < HS then C := 5 {green}
        else begin
          C := 10; {purple}
          HS := H;
        end;
        if Y < -20 then begin
          c := 1; {blue water}
          H := (z / View_Angle) - 20;
            {20;  goes with View 2.0, Illum 0.5}
        end;
        if (H > HM[I]) then
        begin {Place on screen }
          HM[I] := H;
          Y_Height := ((maxrowy - round(H)));
          TempColor := RGB(RGBArray[0, C], RGBArray[1, C],
            RGBArray[2, C]);
          Pixels[I, Y_Height] := TempColor;
        end;
      end;
      HS := 0;
      Z := Z + MtnZ; {15}
    end;
  end;
  Mainform.ProgressBar1.Position := 0;
  MainForm.ProgressBar1.Visible := False;
  Application.ProcessMessages;
end;

(*************************************************************)
(*************************************************************)
procedure TXYZ3DForm.DemiHelpClick(Sender: TObject);
begin
  Application.HelpContext(4800); {DEM Import 4900}
end;

procedure TXYZ3DForm.CopyClipBtnClick(Sender: TObject);
begin
  DemiMemo.SelectAll;
  DemiMemo.CopyToClipboard;
end;

procedure TXYZ3DForm.DemiImportClick(Sender: TObject);
begin {}
  OpenDialog1.InitialDir:=DemsDir;
  if ((DemiRG.ItemIndex + 1) > 0) then begin
    case DemiRG.ItemIndex of
{      0: NASA: ; Read in IMG file}
      0: DoEtopo;
      1: DoNoaaGlobe;{NOAA GLOBE ... NO extensions}
      2: DoGtopo;{Gtopo HDR file}
{      4: DoUSGS;}
    end; {Case}
  end else
    DoMessages(30004);
  Mainform.ProgressBar1.Position := 0;
  MainForm.ProgressBar1.Visible := False;
  Application.ProcessMessages;
end;

(*************************************************************)
procedure TXYZ3DForm.DoEtopo;
var
   ElevationCount: Smallint;
  Xcount, Zcount, CodeVx: Integer;
  MtnString: string;
{  DTM_File: TextFile; }
  Bin_File: file of Smallint; { Integer;}
{  TStackX: array of Smallint;}
      begin {Etopo requires the data input}
          if ((DemiRows.Text <> '0') and (DemiColumns.Text <> '0'))
            then begin
            OpenDialog1.Filter := 'Etopo files|*.bin';
            OpenDialog1.Filename := 'Etopo files.bin';
              OpenDialog1.InitialDir:=DemsDir;
            if (OpenDialog1.Execute) then
            begin
              DemsDir:=ExtractFilePath(OpenDialog1.FileName);
              val(DemiRows.Text, FileSizeY, CodeVx);
                Codefx(DemiRows.Text, CodeVx);
              val(DemiColumns.Text, FileSizeX, CodeVx);
                Codefx(DemiColumns.Text, CodeVx);
                val(DemiLeftX1.Text, DemiLeftX1e, CodeVx);
                  Codefx(DemiLeftX1.Text, CodeVx);
                val(DemiTopY1.Text, DemiTopY1e, CodeVx);
                  Codefx(DemiTopY1.Text, CodeVx);
                val(DemiRightX2.Text, DemiRightX2e, CodeVx);
                  Codefx(DemiRightX2.Text, CodeVx);
                val(DemiBottomY2.Text, DemiBottomY2e, CodeVx);
                  Codefx(DemiBottomY2.Text, CodeVx);
                if ((DemiLeftX1e >= DemiRightX2e)
                    or (DemiBottomY2e >= DemiTopY1e))
                 then DoMessages(30010);
              FractalFileMatrix := OpenDialog1.Filename;
VDEM:=0; {'VR GIS NOAA GLOBE Header'; }
ILorUGridString:='LatLong'; {Image Latlong UTM}
ProjectionString:='Geographic'; {Geographic}
UtmZoneString:= 'None';  {UTM ZONE for UTM projection}
DatumString:='WGS84'; {Datum  Gtopo:WGS84}
SpheroidString:='WGS84';
ZunitsString:= 'Meters';   {Z Units Meters Feet Sensor}
GridUnitString:= 'DD';{Degrees Gtopo:DD DMS Grid Unit}
CellSizeX:= 0.083333333; {X Size 5 Minute}
CellSizeY:= 0.083333333; {Y Size 5 Minute}
GridOriginString:= 'NW'; {Grid Origin}
CellOriginString:= 'Center';

              MainForm.HintPanel.Caption :=
                'Reading Elevations from disk';
              Application.ProcessMessages;
              AssignFile(Bin_File, FractalFileMatrix);
              Reset(Bin_File);
              if (IoResult = 0) then
              begin
                MaximumElevation := -2147483647;
                MinimumElevation := 2147483646;
                MainForm.ProgressBar1.Visible := True;
                Application.ProcessMessages;
                for Zcount := 0 to FileSizeY - 1 do
                begin
                  Mainform.ProgressBar1.Position := Round((Zcount /
                    (FileSizeY - 1)) * 100);
                  Application.ProcessMessages;
                  for Xcount := 0 to FileSizeX - 1 do
                  begin
                    Read(Bin_File, ElevationCount);
                    if (MaximumElevation <= ElevationCount) then
                      MaximumElevation := ElevationCount;
                    if (MinimumElevation >= ElevationCount) then
                      MinimumElevation := ElevationCount;
                  end; end;
                CloseFile(Bin_File);
                if (IoResult <> 0) then DoMessages(30005)
                else begin
     {DO ALL the Stuff and SAVE a FLM file and a HDR File}
                  str(MaximumElevation, MtnString);
                  DemMaxE.Caption := 'Maximum Elevation:  ' +
                    MtnString;
                  str(MinimumElevation, MtnString);
                  DemMinE.Caption := 'Minimum Elevation:  ' +
                    MtnString;
                  ContourInterval := ((MaximumElevation -
                    MinimumElevation) div 7);
                  str(ContourInterval, MtnString);
                  DemCIE.Caption := 'Contour Interval:  ' +
                    MtnString;
                  iMadeMountains := 110; {ETopo}
                  NullDemValue:=-32767;
                  Vre := 0; Vde := 0; Vg := 0; Viki := 0;
                  Vzx := 0; Vzy := 0; Vx := 0; Vy := 0;
                  Vi := 0; Vh:= 0;
                  XYZDEMRG.ItemIndex := 0;
                  VDEM := 0;
                  EColorsRG.ItemIndex := 0;
                  Fractalgorhythym:= 0;
                  MtnString :=
                  ChangeFileExt(OpenDialog1.FileName,'.flm');
                  EFileNameEdit.Text := MtnString;
                  WriteFlmFile(MtnString);
                end;
              MtnString :=
              ChangeFileExt(OpenDialog1.FileName,'.deh');
              WriteDehFile(MtnString);
              end else DoMessages(30006);
            end;
          end else DoMessages(30008);
        end;

(*************************************************************)
procedure TXYZ3DForm.DoNoaaGlobe;
var
{  ElevationCount: Smallint;    }
{  Xcount, Zcount, CodeVx: Integer;  }
{  Tempe,}
  DemiLeftX1e, DemiTopY1e,
  DemiRightX2e, DemiBottomY2e: Double;
  FileString,  MtnString: string;
{  f:file;}
  DTM_File: TextFile;
{  Bin_File, Dem_File: file of Smallint; }{ Integer;}
{  TStackX: array of Smallint;}
begin
OpenDialog1.Filter := 'NOAA GLOBE files (e10.bin)|*.bin';
OpenDialog1.Filename := 'NOAA GLOBE files.bin';
OpenDialog1.InitialDir:=DemsDir;
if (OpenDialog1.Execute) then
begin
If (lowercase(extractFileExt(OpenDialog1.FileName)) <> '.bin') then
begin
DoMessages(30009);
exit;
{            AssignFile(f, OpenDialog1.FileName);
            rename(f,
            (extractfilepath(OpenDialog1.Filename)
             +extractfilename(OpenDialog1.Filename)
             +'.bin'));}
end;
DemsDir:=ExtractFilePath(OpenDialog1.FileName);
FractalFileMatrix := OpenDialog1.Filename;
MtnString := ChangeFileExt(OpenDialog1.FileName, '.deh');
FileString := lowercase(extractfilename(OpenDialog1.FileName))[1];
VDEM:=0; {'VR GIS NOAA GLOBE Header'; }
NullDemValue:=-500;
ILorUGridString:='LatLong'; {Image Latlong UTM}
ProjectionString:='Geographic'; {Geographic}
UtmZoneString:= 'None';  {UTM ZONE for UTM projection}
DatumString:='WGS84'; {Datum  Gtopo:WGS84}
SpheroidString:='WGS84';
ZunitsString:= 'Meters';   {Z Units Meters Feet Sensor}
GridUnitString:= 'DD';{Degrees Gtopo:DD DMS Grid Unit}
CellSizeX:= 0.0083333333; {X Size .5 Minute}
CellSizeY:= 0.0083333333; {Y Size .5 Minute}
GridOriginString:= 'NW'; {Grid Origin}
CellOriginString:= 'Center';
                  {Cell Origin usually center of pixel...}
{    DemiLeftX1e:=-180;    DemiTopY1e:=90;
    DemiRightX2e:=-90;    DemiBottomY2e:=50;  }
if FileString= 'a' then
  begin
    MinimumElevation:=1;
    MaximumElevation:=6098;
    FileSizeX:=10800;
    FileSizeY:=4800;
    DemiLeftX1e:=-180;
    DemiTopY1e:=90;
    DemiRightX2e:=-90;
    DemiBottomY2e:=50;
  end
else if FileString= 'b' then
  begin
    MinimumElevation:=1;
    MaximumElevation:=3940;
    FileSizeX:=10800;
    FileSizeY:=4800;
    DemiLeftX1e:=-90;
    DemiTopY1e:=90;
    DemiRightX2e:=0;
    DemiBottomY2e:=50;
  end
else if FileString= 'c' then
  begin
    MinimumElevation:=-30;
    MaximumElevation:=4010;
    FileSizeX:=10800;
    FileSizeY:=4800;
    DemiLeftX1e:=0;
    DemiTopY1e:=90;
    DemiRightX2e:=90;
    DemiBottomY2e:=50;
  end
else if FileString= 'd' then
  begin
    MinimumElevation:=1;
    MaximumElevation:=4588;
    FileSizeX:=10800;
    FileSizeY:=4800;
    DemiLeftX1e:=90;
    DemiRightX2e:=180;
    DemiTopY1e:=90;
    DemiBottomY2e:=50;
  end
else if FileString= 'e' then
  begin
    MinimumElevation:=-84;
    MaximumElevation:=5443;
    FileSizeX:=10800;
    FileSizeY:=6000;
    DemiLeftX1e:=-180;
    DemiTopY1e:=50;
    DemiRightX2e:=-90;
    DemiBottomY2e:=0;
  end
else if FileString= 'f' then
  begin
    MinimumElevation:=-40;
    MaximumElevation:=6085;
    FileSizeX:=10800;
    FileSizeY:=6000;
    DemiLeftX1e:=-90;
    DemiRightX2e:=0;
    DemiTopY1e:=50;
    DemiBottomY2e:=0;
  end
else if FileString= 'g' then
  begin
    MinimumElevation:=-407;
    MaximumElevation:=8752;
    FileSizeX:=10800;
    FileSizeY:=6000;
    DemiLeftX1e:=0;
    DemiRightX2e:=90;
    DemiTopY1e:=50;
    DemiBottomY2e:=0;
  end
else if FileString= 'h' then
  begin
    MinimumElevation:=-63;
    MaximumElevation:=7491;
    FileSizeX:=10800;
    FileSizeY:=6000;
    DemiLeftX1e:=90;
    DemiRightX2e:=180;
    DemiTopY1e:=50;
    DemiBottomY2e:=0;
  end

else if FileString= 'i' then
  begin
    MinimumElevation:=1;
    MaximumElevation:=2732;
    FileSizeX:=10800;
    FileSizeY:=6000;
    DemiLeftX1e:=-180;
    DemiRightX2e:=-90;
    DemiTopY1e:=0;
    DemiBottomY2e:=-50;
  end
else if FileString= 'j' then
  begin
    MinimumElevation:=-127;
    MaximumElevation:=6798;
    FileSizeX:=10800;
    FileSizeY:=6000;
    DemiLeftX1e:=-90;
    DemiRightX2e:=0;
    DemiTopY1e:=0;
    DemiBottomY2e:=-50;
  end
else if FileString= 'k' then
  begin
    MinimumElevation:=1;
    MaximumElevation:=5825;
    FileSizeX:=10800;
    FileSizeY:=6000;
    DemiLeftX1e:=0;
    DemiRightX2e:=90;
    DemiTopY1e:=0;
    DemiBottomY2e:=-50;
  end
else if (lowercase(extractfilename(OpenDialog1.FileName))='l10g') then
  begin
    MinimumElevation:=1;
    MaximumElevation:=5179;
    FileSizeX:=10800;
    FileSizeY:=6000;
    DemiLeftX1e:=90;
    DemiRightX2e:=180;
    DemiTopY1e:=0;
    DemiBottomY2e:=-50;
  end
else if (lowercase(extractfilename(OpenDialog1.FileName))='l10b') then
  begin
    MinimumElevation:=-34;
    MaximumElevation:=5179;
    FileSizeX:=10800;
    FileSizeY:=6000;
    DemiLeftX1e:=90;
    DemiRightX2e:=180;
    DemiTopY1e:=0;
    DemiBottomY2e:=-50;
  end
else if FileString= 'm' then
  begin
    MinimumElevation:=1;
    MaximumElevation:=4009;
    FileSizeX:=10800;
    FileSizeY:=4800;
    DemiLeftX1e:=-180;
    DemiRightX2e:=-90;
    DemiTopY1e:=-50;
    DemiBottomY2e:=-90;
  end
else if FileString= 'n' then
  begin
    MinimumElevation:=1;
    MaximumElevation:=4743;
    FileSizeX:=10800;
    FileSizeY:=4800;
    DemiLeftX1e:=-90;
    DemiRightX2e:=0;
    DemiTopY1e:=-50;
    DemiBottomY2e:=-90;
  end
else if FileString= 'o' then
  begin
    MinimumElevation:=1;
    MaximumElevation:=4039;
    FileSizeX:=10800;
    FileSizeY:=4800;
    DemiLeftX1e:=0;
    DemiRightX2e:=90;
    DemiTopY1e:=-50;
    DemiBottomY2e:=-90;
  end
else if FileString= 'p' then
  begin
    MinimumElevation:=1;
    MaximumElevation:=4363;
    FileSizeX:=10800;
    FileSizeY:=4800;
    DemiLeftX1e:=90;
    DemiRightX2e:=180;
    DemiTopY1e:=-50;
    DemiBottomY2e:=-90;
  end
else begin
CloseFile(DTM_File);
DoMessages(30011);
exit;
end;
    if ((DemiLeftX1e >= DemiRightX2e)
        or (DemiBottomY2e>= DemiTopY1e))
    then DoMessages(30010);
 ContourInterval := 0;
 iMadeMountains:=  105; {Globe}
 NullDemValue:=-500;
 Vre := 0; Vde := 0; Vg := 0; Viki := 0;
 Vzx := 0; Vzy := 0; Vx := 0; Vy := 0; Vi := 0; Vh:= 0;
 XYZDEMRG.ItemIndex := 0;
 Fractalgorhythym:= 0;
 EColorsRG.ItemIndex := 0;
 WriteDehFile(MtnString);
 MtnString :=
 ChangeFileExt(OpenDialog1.FileName,'.flm');
 EFileNameEdit.Text := MtnString;
 WriteFlmFile(MtnString);
 end else DoMessages(30007);
end;

(*************************************************************)
procedure TXYZ3DForm.DoGtopo;
var
  MtnChar:Char;
{  Nodata,} ElevationCount: Smallint;
  Xcount, Zcount, CodeVx: Integer;
  Tempe, DemiLeftX1e, DemiTopY1e, DemiRightX2e, DemiBottomY2e:
    Double;
  MtnString: string;
{  f:file;  }
  DTM_File: TextFile;
  Bin_File, Dem_File: file of Smallint; { Integer;}
  TStackX: array of Smallint;
begin
  {NoData:=-9999;}
  {DemiLeftX1e:=0;DemiTopY1e:=0;DemiRightX2e:=0;DemiBottomY2e:=0;}
          OpenDialog1.Filter := 'Gtopo files|*.dem';
          OpenDialog1.Filename := 'Gtopo files.dem';
            OpenDialog1.InitialDir:=DemsDir;
          if (OpenDialog1.Execute) then
  begin
            DemsDir:=ExtractFilePath(OpenDialog1.FileName);
            FractalFileMatrix := OpenDialog1.Filename;
            MtnString := ChangeFileExt(OpenDialog1.FileName, '.hdr');
            AssignFile(DTM_File, MtnString);
            Reset(DTM_File);
            Readln(DTM_File, MtnString);
            If ((pos('NROWS',MtnString)>0)
              and (pos(#13,MtnString)=0)) then
            begin
            showmessage('rewriting it');
            Reset(DTM_File);
            MtnString:='';
            DemiMemo.Clear;
            CodeVx:=0;
            While (not eof(DTM_File)) do
            begin
            Read(DTM_File, MtnChar);
            If (MtnChar = #10) then
               begin      {CR }
               MtnString:=MtnString+MtnChar;
               MtnString:=MtnString+#13;{Linefeed}
               {write to memo}
               DemiMemo.Lines[CodeVx]:=MtnString;
               MtnString:='';
               inc(CodeVx);
               end
               else MtnString:=MtnString+MtnChar;
            end;
            Rewrite(DTM_File);
            {Write file from memo}
            For Xcount:=0 to CodeVx-1 do
            writeln(DTM_File,DemiMemo.Lines[Xcount]);
            end;
            DemiMemo.Clear;
            Reset(DTM_File);
            if (IoResult = 0) then
    begin {BYTEORDER      M ad as a hatter}
              DemiMemo.Lines.Clear;
              Readln(DTM_File, MtnString);
              DemiMemo.Lines.Append(MtnString);
              Readln(DTM_File, MtnString); {LAYOUT       BIL}
              DemiMemo.Lines.Append(MtnString);
              Readln(DTM_File, MtnString); {NCOLS   X      4800}
              MtnString := copy(MtnString, 6, 66);
              val(MtnString, FileSizeX, CodeVx);
              Codefx(MtnString,CodeVx);

              Readln(DTM_File, MtnString); {NROWS    Y     6000}
              MtnString := copy(MtnString, 6, 66);
              val(MtnString, FileSizeY, CodeVx); Codefx(MtnString,
                CodeVx);
              DemiMemo.Lines.Append(IntToStr(FileSizeX));
       DemiColumns.Text:=IntToStr(FileSizeX);
              DemiMemo.Lines.Append(IntToStr(FileSizeY));
       DemiRows.Text:=IntToStr(FileSizeY);
              Readln(DTM_File, MtnString); {NBANDS        1}
              DemiMemo.Lines.Append(MtnString);
              Readln(DTM_File, MtnString); {NBITS         16}
              DemiMemo.Lines.Append(MtnString);
              Readln(DTM_File, MtnString); {BANDROWBYTES         9600}
              DemiMemo.Lines.Append(MtnString);
              Readln(DTM_File, MtnString); {TOTALROWBYTES        9600}
              DemiMemo.Lines.Append(MtnString);
              Readln(DTM_File, MtnString); {BANDGAPBYTES         0}
              DemiMemo.Lines.Append(MtnString);
              Readln(DTM_File, MtnString); {NODATA        -9999}
              MtnString := copy(MtnString, 7, 66);
              val(MtnString, NullDemValue, CodeVx);
              Codefx(MtnString,CodeVx);
              DemiMemo.Lines.Append(MtnString);
      {DemiLeftX1e,DemiTopY1e,DemiRightX2e,DemiBottomY2e}
              Readln(DTM_File, MtnString);
                {ULXMAP        -59.99583333333333}
              MtnString := copy(MtnString, 7, 66);
              val(MtnString, DemiLeftX1e, CodeVx);
              Codefx(MtnString,CodeVx);
              DemiMemo.Lines.Append(MtnString);
              Readln(DTM_File, MtnString);
                {ULYMAP        39.99583333333333}
              MtnString := copy(MtnString, 7, 66);
              val(MtnString, DemiTopY1e, CodeVx);
              Codefx(MtnString, CodeVx);
              DemiMemo.Lines.Append(MtnString);
              Readln(DTM_File, MtnString); {XDIM  0.00833333333333}
              MtnString := copy(MtnString, 5, 66);
              val(MtnString, DemiRightX2e, CodeVx);
              Codefx(MtnString,CodeVx);
              DemiMemo.Lines.Append(Floattostr(DemiRightX2e));
              Readln(DTM_File, MtnString); {YDIM   0.00833333333333}
              MtnString := copy(MtnString, 5, 66);
              val(MtnString, DemiBottomY2e, CodeVx);
                Codefx(MtnString, CodeVx);
              DemiMemo.Lines.Append(Floattostr(DemiBottomY2e));
              CloseFile(DTM_File);
              if (IoResult <> 0) then DoMessages(30012);
              MainForm.HintPanel.Caption :=
                'Reading Elevations from disk';
              Application.ProcessMessages;
              AssignFile(DEM_File, FractalFileMatrix);
              Reset(DEM_File);
      if (IoResult = 0) then
      begin
                MtnString :=
                  ChangeFileExt(OpenDialog1.FileName,'.bin');
                AssignFile(Bin_File, MtnString);
                Rewrite(Bin_File);
                MaximumElevation := -2147483647;
                MinimumElevation := 2147483646;
                MainForm.ProgressBar1.Visible := True;
                Application.ProcessMessages;
        {Make an array the size of Xs}
                SetLength(TStackX, FileSizeX);
                for Zcount := 0 to FileSizeY - 1 do begin
                  Mainform.ProgressBar1.Position := Round((Zcount /
                    (FileSizeY - 1)) * 100);
                  Application.ProcessMessages;
                  for Xcount := 0 to FileSizeX - 1 do begin
                    Read(DEM_File, ElevationCount);
                    ElevationCount := Swap(ElevationCount);
                      {Gtopo is M ordered... NOT Intel ordered}
         {Fill an array the size of Xs}
                    TStackX[Xcount] := ElevationCount;
                    if (ElevationCount > NullDemValue) then
                    {  TStackX[Xcount] := (-32767) was going to change em...
                    else}
                    begin
                      if (MaximumElevation <= ElevationCount) then
                        MaximumElevation := ElevationCount;
                      if (MinimumElevation >= ElevationCount) then
                        MinimumElevation := ElevationCount;
                    end;
                  end;
                  for Xcount := 0 to FileSizeX - 1 do begin
                    Write(Bin_File, TStackX[Xcount]);
                  end;
                end;
                CloseFile(DEM_File);
                CloseFile(Bin_File);
                SetLength(TStackX, 0);
      if (IoResult <> 0) then DoMessages(30013) else
      begin
        {DO ALL the Stuff and SAVE a FLM file and a HDR File}
                  str(MaximumElevation, MtnString);
                  DemMaxE.Caption := 'Maximum Elevation:  ' +
                    MtnString;
                  str(MinimumElevation, MtnString);
                  DemMinE.Caption := 'Minimum Elevation:  ' +
                    MtnString;
                  ContourInterval := ((MaximumElevation -
                    MinimumElevation) div 7);
                  str(ContourInterval, MtnString);
                  DemCIE.Caption := 'Contour Interval:  ' +
                    MtnString;
                  iMadeMountains := 106; {GTopo}
                  {NullDemValue:=-9999;}
                  Vre := 0; Vde := 0;
                  Vzx := 0; Vzy := 0; Vx := 0; Vy := 0;
                  Vi := 0; Vh := 0;Vg := 0; Viki := 0;
                  XYZDEMRG.ItemIndex := 0; VDEM:=0;
                  EColorsRG.ItemIndex := 0;
                  MtnString :=
                  ChangeFileExt(OpenDialog1.FileName,'.flm');
                  EFileNameEdit.Text := MtnString;
                  WriteFlmFile(MtnString);
      end; {write a FLM file}
      if (IoResult = 0) then
      begin
      {Write a DTM header  'VR GIS GTOPO Header'}
                  ILorUGridString:= 'LatLong'; {Image Latlong UTM}
                  ProjectionString:='Geographic'; {Geographic}
                  UtmZoneString:= 'None'; {UTM ZONE for UTM projection}
                  DatumString:= 'WGS84'; {Datum  Gtopo:WGS84}
                  SpheroidString:= 'None';
                  ZunitsString:= 'Meters';{Z Units Meters Feet Sensor}
                   {Z Min}{Z Max}
                  GridUnitString:='DD';{Degrees Gtopo:DD DMS Grid Unit}
                  {ColumnsX}
                   {RowsY}
        {DemiLeftX1e,DemiTopY1e,DemiRightX2e,DemiBottomY2e}

CellSizeX:=DemiRightX2e; {X Size 5 Minute}
CellSizeY:=DemiBottomY2e; {Y Size 5 Minute}
        {DemiLeftX1e,DemiTopY1e,DemiRightX2e,DemiBottomY2e}
                  Tempe := (DemiLeftX1e + (CellSizeX / 2));
                  DemiLeftX1e:= Tempe;
       str(Tempe {DemiLeftX1e}, MtnString);
       DemiLeftX1.Text:=MtnString;
                  Tempe := (DemiTopY1e - (CellSizeY / 2));
                  DemiTopY1e:= Tempe;
       str(Tempe {DemiTopY1e}, MtnString);
       DemiTopY1.Text:=MtnString;
                  Tempe := (DemiLeftX1e +
                    (CellSizeX * (FileSizeX - 1)));
                  DemiRightX2e:=Tempe;
       str(DemiRightX2e, MtnString);
       DemiRightX2.Text:=MtnString;
                  Tempe := (DemiTopY1e +
                    (CellSizeY * (FileSizeY - 1)));
       DemiBottomY2e:=Tempe;
       str(DemiBottomY2e, MtnString);
       DemiBottomY2.Text:=MtnString;
                  GridOriginString:= 'NW'; {Grid Origin}
                  CellOriginString:= 'Center';
                    {Cell Origin usually center of pixel...}
{Write deh DTM HDR}
                MtnString :=
                  ChangeFileExt(OpenDialog1.FileName,'.deh');
                  WriteDehFile(MtnString);
        end else DoMessages(30011);
      end else DoMessages(30005);
    end else DoMessages(30012);
  end; {Opened file}
end; {1}


(*************************************************************)
procedure TXYZ3DForm.DoUSGS;
(*
var
  Nodata, ElevationCount: Smallint;
  Xcount, Zcount, CodeVx: Integer;
  Tempe, DemiLeftX1e, DemiTopY1e, DemiRightX2e, DemiBottomY2e:
    Double;
  MtnString: string;
  f:file;
  DTM_File: TextFile;
  Bin_File, Dem_File: file of Smallint; { Integer;}
  TStackX: array of Smallint;
*)
begin {USGS
  MainForm.ProgressBar1.Visible:=True;
  Application.ProcessMessages;
are in ASCII combo Hdr-data-proj

and written in Fortran exponents...
   if (((ic == '-') or (ic == '+')) and (ic1 = 'D'))
   then ic1:='E';

are tilted
        for Xcount:=0 to FileSizeX-1 do begin
         Read(DEM_File,ElevationCountin);
        if ElevationCountin =-32767 then do NOT use for Min Max

      Mainform.ProgressBar1.Position:=Round((Zcount / (FileSizeY-1))*100);
     Application.ProcessMessages;
}
        end;
(*************************************************************)
(*************************************************************)

(*************************************************************)
(*************************************************************)
procedure TXYZ3DForm.MtnsHelpClick(Sender: TObject);
begin
  Application.HelpContext(4500); {Mtn}
end;

procedure TXYZ3DForm.MtnsFLSBtnClick(Sender: TObject);
var
  MtnSize, MtnDo,
  CodeVzx, CodeVzy, CodeVk, CodeVg,
  CodeVx, CodeVy, CodeVi, CodeVh, CodeV1e, CodeV2e: Integer;
begin
  if (MtnsRG.ItemIndex > -1) then
    begin {U_Math_F}
    MtnSize:=MtnsRLSCB.ItemIndex;
    val(MtnsXEdit.Text, Vx, CodeVx);
    Codefx(MtnsXEdit.Text, CodeVx);
    val(MtnsYEdit.Text, Vy, CodeVy);
    Codefx(MtnsYEdit.Text, CodeVy);
    val(MtnsZxEdit.Text, Vzx, CodeVzx);
    Codefx(MtnsZxEdit.Text, CodeVzx);
    val(MtnsZyEdit.Text, Vzy, CodeVzy);
    Codefx(MtnsZyEdit.Text,   CodeVzy);
    val(MtnsrEdit.Text, Vre, CodeV1e);
    Codefx(MtnsrEdit.Text,  CodeV1e);
    val(MtnsdEdit.Text, Vde, CodeV2e);
    Codefx(MtnsdEdit.Text,   CodeV2e);
    val(MtnsGEdit.Text, Vg, CodeVg);
    Codefx(MtnsGEdit.Text,  CodeVg);
    val(MtnsiEdit.Text, Vi, CodeVi);
    Codefx(MtnsiEdit.Text, CodeVi);
    val(MtnshEdit.Text, Vh, CodeVh);
    Codefx(MtnshEdit.Text, CodeVh);
    val(MtnsKEdit.Text, Viki, CodeVk);
    Codefx(MtnsKEdit.Text,   CodeVk);
    if ((CodeVx = 0) and (CodeVy = 0)
    and (CodeVzx = 0) and (CodeVzy = 0)
    and (CodeVi = 0) and (CodeVh = 0)
    and (CodeVg = 0) and (CodeVk = 0)
    and (CodeV1e = 0) and (CodeV2e = 0)) then
      begin
      VDEM:= XYZDEMRG.ItemIndex;
      MtnDo := MtnsRG.ItemIndex;
      iMadeMountains := MtnDo;
      {Make it happen image maker}
      MainForm.DoImageStart;
      MtnsDisplayDo(Vre, Vde,
             MtnDo, Vx, Vy, Vzx, Vzy, Vg, Vh, Vi,Viki, MtnSize);
      Mainform.DoImageDone;
      end;
    end;
end;

(*************************************************************)
procedure TXYZ3DForm.MtnsRLSBtnClick(Sender: TObject);
var
  MtnSize, MtnDo,
  CodeVzx, CodeVzy, CodeVk, CodeVg,
  CodeVx, CodeVy, CodeVi, CodeVh, CodeV1e, CodeV2e: Integer;
begin
  if (MtnsRLSRG.ItemIndex > -1) then
    begin {U_Math_F}
    MtnSize:=MtnsRLSCB.ItemIndex;
    val(MtnsXEdit.Text, Vx, CodeVx);
    Codefx(MtnsXEdit.Text, CodeVx);
    val(MtnsYEdit.Text, Vy, CodeVy);
    Codefx(MtnsYEdit.Text, CodeVy);
    val(MtnsZxEdit.Text, Vzx, CodeVzx);
    Codefx(MtnsZxEdit.Text, CodeVzx);
    val(MtnsZyEdit.Text, Vzy, CodeVzy);
    Codefx(MtnsZyEdit.Text,   CodeVzy);
    val(MtnsiEdit.Text, Vi, CodeVi);
    Codefx(MtnsiEdit.Text, CodeVi);
    val(MtnshEdit.Text, Vh, CodeVh);
    Codefx(MtnshEdit.Text, CodeVh);
    val(MtnsrEdit.Text, Vre, CodeV1e);
    Codefx(MtnsrEdit.Text,  CodeV1e);
    val(MtnsdEdit.Text, Vde, CodeV2e);
    Codefx(MtnsdEdit.Text,   CodeV2e);
    val(MtnsGEdit.Text, Vg, CodeVg);
    Codefx(MtnsGEdit.Text,  CodeVg);
    val(MtnsKEdit.Text, Viki, CodeVk);
    Codefx(MtnsKEdit.Text,   CodeVk);
    if ((CodeVx = 0) and (CodeVy = 0)
    and (CodeVzx = 0) and (CodeVzy = 0)
    and (CodeVi = 0) and (CodeVh = 0)
    and (CodeVg = 0) and (CodeVk = 0)
    and (CodeV1e = 0) and (CodeV2e = 0)) then
      begin
      VDEM:= XYZDEMRG.ItemIndex;
      If MtnsRLSRG.ItemIndex =0 then
      MtnDo := MtnsRG.ItemIndex else
      MtnDo := MtnsRLSRG.ItemIndex+3;{+3 to keep in order}
      iMadeMountains := MtnDo;
      {Make it happen image maker}
      MtnsDisplayDo(Vre, Vde,
            MtnDo, Vx, Vy, Vzx, Vzy,Vg, Vh, Vi,Viki, MtnSize);
      {XYZGL}dtmGlForm.FormShowDown;
      {XYZGL}dtmGlForm.show;
      end;
    end;
end;

procedure TXYZ3DForm.MtnsRLSCBChange(Sender: TObject);
begin
case  MtnsRLSCB.Itemindex of
{320x 240y
640x 480y
800x 600y
360x 180y
720x 360y
256x 256y
512x 512y
1024x 1024y
Input Zx Zy}
0: {320x 240y   76800}
   Begin
   MtnsZxEdit.Text:='320';
   MtnsZyEdit.Text:='240';
   end;
1: {640 480     307200}
   Begin
   MtnsZxEdit.Text:='640';
   MtnsZyEdit.Text:='480';
   end;
2: {800 600    480000}
   Begin
   MtnsZxEdit.Text:='800';
   MtnsZyEdit.Text:='600';
   end;
3: Begin {360x 180y  64800}
   MtnsZxEdit.Text:='360';
   MtnsZyEdit.Text:='180';
   end;
4: Begin  {360x 180y 129600}
   MtnsZxEdit.Text:='720';
   MtnsZyEdit.Text:='360';
   end;
5: {256x 256y    65536}
   Begin
   MtnsZxEdit.Text:='256';
   MtnsZyEdit.Text:='256';
   end;
6: {512 512y      262144}
   Begin
   MtnsZxEdit.Text:='512';
   MtnsZyEdit.Text:='512';
   end;
7: {1024x 1024y  1048576}
   Begin
   MtnsZxEdit.Text:='1024';
   MtnsZyEdit.Text:='1024';
   end;
8: {Input Zx Zy}
   Begin
   MtnsZxEdit.Text:='124';
   MtnsZyEdit.Text:='124';
   end;
end;{case}
end;


{these are in fuMathF}
procedure TXYZ3DForm.MtnsDisplayDo(Vre, Vde: Extended;
             MtnDo, Vx, Vy, Vzx, Vzy, Vg, Vh, Vi,Viki, MtnSize: Integer);

begin
  case MtnDo of
  0..2:begin
    MainForm.FalseSize;
    MainForm.FV_IS640x480.Checked := (not MainForm.FV_IS640x480.Checked);
    FYImageX := 640;
    FYImageY := 480;
   MtnsZxEdit.Text:='640';
   MtnsZyEdit.Text:='480';
   Vzx:= 640;
   Vzy:= 480;
   If Vzy= Vzx then {Shut up compiler this resets data};
    MainForm.OverBit;
    end;
    End;{case{
{MtnSize is used to select SIZE of the image made
0: 320x 240y
1: 360x 180y
2: 256x 256y
3: Input Zx Zy}
{Required data for GL .. even if 'blank'..
other stuff for flm and deh in file saver}
{grd.pts:array of array of single;         Manmatrix
          height,width:integer;          FileSizeY    FileSizeX
          maxz:single;                   MaximumElevation
          null:single;                   NullDemValue   MinimumElevation
          cellsize:single;               CellSizeX, CellSizeY,
          originX,OriginY:double;         DemiLeftX1e, DemiTopY1e,
                                      DemiRightX2e, DemiBottomY2e}
    FileSizeX := Vzx;
    FileSizeY := Vzy;
NullDemValue:=-32767;
CellSizeX:=1; {Vzx div 10;  }
CellSizeY:=1;{ Vzx div 10;}
DemiLeftX1e:=0; DemiTopY1e:=0;
DemiRightX2e:=FileSizeX*CellSizeX;
DemiBottomY2e:=FileSizeY*CellSizeY;
  case MtnDo of
    0: Fra_Mtns(Vre, Vde, 0 );
    1: Fra_Mtns(Vre, Vde, 1 );
    2: Fra_Mtns(Vre, Vde, 2 );{the 012 select the y size }
      {Fractal Mountains [r] [d] [i]}
       {Fra_Mtns(Fun, D : Extended);1.5, 1.5 B_B_Mtns }
        { Procedure Fra_Mtns(Fune, D : Extended;Intensity:Integer);}
    3:  FractalGrid(Vre, Vde);
        {Fractal TIN Procedure FractalGrid(
        [X] [Y] [r] [d] [i]
        InX,InY,Intensity:Integer;Freq,F_Dim: Extended);}
     4: FoldedGlobe(Viki); {Sine wave fractures}
     {FoldedGlobe [X] [Y] [r] [d] [i];}
     {FoldedGlobe(InX,InY,Freq,F_Dim: Extended;Intensity:Integer);}
     {Code on how to WRAP image onto a
       Ball-> Resize to 360x180->Globe
       Copy MARS image}
     5: DupliGlobe(Viki);
{procedure DupliGlobe(Krypton : Integer);}
    6:  Craters(Vre, Vg, Vi);
        {Random size and variable 'filters' from Grip
        added to data at random locations}
    {Craters [r] [d] [h] [i]}
        {Procedure Craters(Freq,F_Dim: Extended;
        Choice,Intensity:Integer);}
    7: Waves( Vh, Vi);
    {Waves [r] [d] [h] [i]}
        {Procedure Waves(Freq,F_Dim: Extended;
        Choice,Intensity:Integer);
        Open GL wave machine...}
    8: Rippled(Vg, Vh, Vi);
     {Circular Waves [X] [Y] [r] [d] [i]}
     (* begin
         {Procedure WaveCenter(Freq,F_Dim: Extended;
        InX,InY,Intensity:Integer);}
        WaveCenter(V1e, V2e, Vx, Vy, Vi);
      end;*)
    9: Splattered(Vg, Vh, Vi);
     {Splattered(Freq, F_Dim: Extended; Choice, Intensity: Integer);}
    {Splattered.. Random balls of data added to image data}
  end; {of case}
end; {of inputs}

(*************************************************************)
procedure TXYZ3DForm.MtnsSaveClick(Sender: TObject);
var
  MyFilesS: string;
  CodeVx, CodeVy, CodeVzx, CodeVzy,
  CodeVi, CodeVh, CodeV1e, CodeV2e: Integer;
begin
if (iMadeMountains <> -1) then
  begin
    {Case iMadeMountains of}
    val(MtnsXEdit.Text, Vx, CodeVx);
    Codefx(MtnsXEdit.Text, CodeVx);
    val(MtnsYEdit.Text, Vy, CodeVy);
    Codefx(MtnsYEdit.Text, CodeVy);
    val(MtnsZxEdit.Text, Vzx, CodeVzx);
    Codefx(MtnsZxEdit.Text,CodeVzx);
    val(MtnsZyEdit.Text, Vzy, CodeVzy);
    Codefx(MtnsZyEdit.Text,      CodeVzy);
    val(MtnsiEdit.Text, Vi, CodeVi);
    Codefx(MtnsiEdit.Text, CodeVi);
    val(MtnshEdit.Text, Vh, CodeVh);
    Codefx(MtnshEdit.Text, CodeVh);
    val(MtnsrEdit.Text, Vre, CodeV1e);
    Codefx(MtnsrEdit.Text,      CodeV1e);
    val(MtnsdEdit.Text, Vde, CodeV2e);
    Codefx(MtnsdEdit.Text,      CodeV2e);
    if ((CodeVx = 0) and (CodeVy = 0) and (CodeVi = 0) and
      (CodeVh = 0) and (CodeV1e = 0) and (CodeV2e = 0)) then
      begin { Display Open dialog box }
      SaveDialog1.Filter := 'Mountain (*.flm)|*.flm';
      SaveDialog1.Filename := MtnsFileEdit.Text;
      SaveDialog1.InitialDir:=DemsDir;
      if SaveDialog1.Execute then
        begin
        MyFilesS := lowercase(ExtractFileExt(SaveDialog1.FileName));
        if MyFilesS = '.flm' then
          begin
          FractalFileName := SaveDialog1.FileName;
          MyFilesS := ExtractFileName(SaveDialog1.FileName);
          MtnsFileEdit.Text := MyFilesS;
    bSaveTheMountains := True; {Flag to save the last mountains Array}
          {Make it happen image maker}
{          MainForm.DoImageStart;}
          Vg:=0;
          Viki:=0;
          MtnsDisplayDo(Vre, Vde,
          iMadeMountains,
          Vx, Vy, Vzx, Vzy,
          Vg, Vh, Vi,Viki,
          MtnsRLSCB.ItemIndex);
{      MtnsDisplayDo(Vre, Vde,
             MtnDo, Vx, Vy, Vzx, Vzy, Vg, Vh, Vi,Vk, MtnSize);}
{          Mainform.DoImageDone;}
          bSaveTheMountains := False;
{      VDEM:= XYZDEMRG.ItemIndex;
      MtnDo := MtnsRLSRG.ItemIndex+3;
      iMadeMountains := MtnDo;}
ContourInterval := 0;
Fractalgorhythym := 0;
GridOriginString:=    'NW';
CellOriginString:=  'NW';
ILorUGridString:= 'Mountains';
ProjectionString:= 'None';
UtmZoneString:= 'None';
DatumString:= 'None';
SpheroidString:= 'None';
ZunitsString:=   'Meters';
GridUnitString:=  'Grid Unit';
{FractalFileMatrix got from inside procedures}
           WriteFlmFile(SaveDialog1.FileName);
           WriteDehFile(ChangeFileExt(SaveDialog1.FileName,'.deh'));
        end;
      end;
    end;
  end else DoMessages(30015);
end;


procedure TXYZ3DForm.MtnsClearClick(Sender: TObject);
begin
  MtnsrEdit.Text := '1.4';
  MtnsdEdit.Text := '1.6';
  MtnsgEdit.Text := '9';
  MtnshEdit.Text := '22';
  MtnsiEdit.Text := '36';
  MtnskEdit.Text := '20000';
  MtnsXEdit.Text:= '0';
  MtnsYEdit.Text := '0';
  MtnsZxEdit.Text := IntToStr(FYImageX);
  MtnsZyEdit.Text := IntToStr(FYImageY);
end;
(*************************************************************)
(*************************************************************)


(*************************************************************)
{Todo testing}
(*************************************************************)
procedure TXYZ3DForm.RgbToDemBtnClick(Sender: TObject);
var
MyFilesS, MyFilesExtension: string;
  F_File: file of Smallint;
Red,Green,Blue:Byte;
Tempman,Pixelxs,
maxcolx, maxrowy,  XCount, YCount,y,Pixelx,height:Integer;
  BitMap : TBitMap;
  PixelLine : PByteArray;
begin
            OpenDialog1.Filter := 'dem bmp|*.bmp';
            OpenDialog1.Filename := RgbToDemEdit.text;
            OpenDialog1.InitialDir:=DemsDir;
if OpenDialog1.Execute then
begin
  MyFilesExtension :=
      Uppercase(ExtractFileExt(OpenDialog1.FileName));
  if MyFilesExtension = '.BMP' then
  begin
  FractalFilename := OpenDialog1.FileName;
  BitMap := TBitMap.create;
  try
  BitMap.LoadFromFile(FractalFilename);
{  PixelScanSize := 3;   pf24bit  PixelScanSize := 4;   pf32bit  }
  if (
  (BitMap.PixelFormat = MyPixelFormat)or
  (BitMap.PixelFormat = pf24bit)or
  (BitMap.PixelFormat = pf32bit))  then
  begin
  {change so scanline works PixelScanSize is already set}
    If(BitMap.PixelFormat <> MyPixelFormat)then
       BitMap.PixelFormat := MyPixelFormat;
    FileSizeX:=Bitmap.Width;
    FileSizeY:=Bitmap.Height;
    // Dynamically allocate the ManMatrix array
    SetLength(ManMatrix, FileSizeX, FileSizeY);
    for y := 0 to BitMap.height -1 do
    begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((y / FileSizeY) * 100);
            Application.ProcessMessages;
      PixelLine := BitMap.ScanLine[y];
      for Pixelx := 0 to BitMap.width -1 do
      begin
        Pixelxs := (Pixelx * PixelScanSize);
        Red:=GetRValue(PixelLine[Pixelxs]);
        Green:=GetGValue(PixelLine[(Pixelxs + 1)]);
        Blue:=GetBValue(PixelLine[(Pixelxs + 2)]);
        // Set the height to a percentage of the maximum
        // value (255+255+255 = 765) this sets the height in a range
        // of 0.0 (0+0+0 = black pixel) to 1.0 (255+255+255 = white pixel)
        height := Round(((Red + Green + Blue) / 765)*1000);
        // Set the Z value of the current point in the mesh to the
        // value calculated above
        ManMatrix[Pixelx, y] := height;
      end;
    end;
{Gather results}
          MaximumElevation := -2147483647;
          MinimumElevation := 2147483646;
          maxcolx:=  FileSizeX- 1;
          maxrowy := FileSizeY- 1;
          for Ycount := 0 to maxrowy do
          begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((Ycount / maxrowy) * 100);
            Application.ProcessMessages;
            for Xcount := 0 to maxcolx do
            begin
              TempMan:=ManMatrix[Xcount, Ycount];
              if (MaximumElevation <= TempMan) then
                MaximumElevation := TempMan;
              if (MinimumElevation >= TempMan) then
                MinimumElevation := TempMan;
            end;
          end;

    XYZ3DForm.MtnsProgressBar.Position :=0;
    Application.ProcessMessages;
{Save the data}
          MyFilesS := ExtractFileName(FractalFileName);
          RgbToDemEdit.Text := MyFilesS;
          Vre:=0;
          Vde:=0;
          iMadeMountains:=200;
          Vx:=0; Vy:=0;
          Vzx:=FileSizeX;
          Vzy:=FileSizeY;
          Vg:=0; Vh:=0; Vi:=0;Viki:=0;
VDEM:=5;
NullDemValue:=-32767;
CellSizeX:=1; {Vzx div 10;  }
CellSizeY:=1;{ Vzx div 10;}
DemiLeftX1e:=0;
DemiTopY1e:=0;
DemiRightX2e:=FileSizeX*CellSizeX;
DemiBottomY2e:=FileSizeY*CellSizeY;
ContourInterval := 0;
Fractalgorhythym := 0;
GridOriginString:=    'NW';
CellOriginString:=  'NW';
ILorUGridString:= 'RGB Import';
ProjectionString:= 'None';
UtmZoneString:= 'None';
DatumString:= 'None';
SpheroidString:= 'None';
ZunitsString:=   'Meters';
GridUnitString:=  'Grid Unit';
  begin
      begin
        MainForm.HintPanel.Caption := 'Saving Mountains to disk';
        Application.ProcessMessages;
        FractalFileMatrix := ChangeFileExt(FractalFilename, '.bin');
        AssignFile(F_File, FractalFileMatrix);
        ReWrite(F_File);
        if IoResult = 0 then
        begin
          maxcolx:=  FileSizeX- 1;
          maxrowy := FileSizeY- 1;
          for Ycount := 0 to maxrowy do
          begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((Ycount / maxrowy) * 100);
            Application.ProcessMessages;
            for Xcount := 0 to maxcolx do
            begin
              Write(F_File, ManMatrix[Xcount, Ycount]);
            end;
          end;
          CloseFile(F_File);
          if (IoResult <> 0) then DoMessages(30064);
        end else DoMessages(30065);
      end;
  end;{this set}
           WriteFlmFile(ChangeFileExt(FractalFileName,'.flm'));
           WriteDehFile(ChangeFileExt(FractalFileName,'.deh'));
    {Display or whatever}
    XYZ3DForm.MtnsProgressBar.Position :=0;
    Application.ProcessMessages;
      {XYZGL}dtmGlForm.FormShowDown;
      {XYZGL}dtmGlForm.show;
  end else DoMessages(12);
  finally
    BitMap.free;
  end;
    end;
  end;
end;
(*************************************************************)
(*************************************************************)
procedure TXYZ3DForm.FileSeedDemBtnClick(Sender: TObject);
{var SeedSide:Integer;}
begin
{    SeedSide:=FileSeedRG.Itemindex;
Read the file
Flip it to copy side adjacent .. copy ALL data
Run Splatter to 'slide' changes
from seed side to increased intensity}
end;
(*************************************************************)
(*************************************************************)








end.
