unit frmBenchMark;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.UITypes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.CheckLst,
  Vcl.Dialogs, Vcl.ExtCtrls, VclTee.TeEngine, VclTee.Series, Vcl.StdCtrls,
  VclTee.TeeProcs, VclTee.TeeGDIPlus,
  VclTee.Chart, Vcl.Buttons,
   
  GLCadencer, GLAVIRecorder,
  //GLData
  geIntegerEdit;

type
  TformBenchMark = class(TForm)
    Series1: TLineSeries;
    Series2: TLineSeries;
    Series3: TLineSeries;
    Series4: TLineSeries;
    Series5: TLineSeries;
    Series6: TLineSeries;
    Series7: TLineSeries;
    Series8: TLineSeries;
    Series9: TLineSeries;
    Series10: TLineSeries;




    chartBench: TChart;
    splitterBench: TSplitter;
    pnlBenchMark: TPanel;
    BenchTimer: TTimer;
    GLCadencer: TGLCadencer;
    pnlSettings: TPanel;
    memBenchmark: TMemo;
    pnlTop: TPanel;
    lblTest: TLabel;
    cbBenchMark: TComboBox;
    lblN: TLabel;
    clbSeries: TCheckListBox;
    bTest: TBitBtn;
    bOK: TBitBtn;

    bCopyCurrent: TButton;
    ColorDialog: TColorDialog;
    bCopyAverage: TButton;
    bRename: TButton;
    bColour: TButton;
    procedure BenchTimerTimer(Sender: TObject);
    procedure bTestClick(Sender: TObject);
    procedure GLCadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormShow(Sender: TObject);
    procedure geieSecondsExit(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure clbSeriesClickCheck(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bCopyCurrentClick(Sender: TObject);
    procedure bRenameClick(Sender: TObject);
    procedure bCopyAverageClick(Sender: TObject);
    procedure bColourClick(Sender: TObject);
  private
    dMin:single;
    dmax:single;
    dSum:single;
    dAverage:single;
    iTime:integer;
    iCount:integer;
    procedure UpdateBenchMarkStats;
     
  public
     
  end;

var
  formBenchMark: TformBenchMark;

implementation

uses
  frmMain;

{$R *.dfm}
// ----- TformBenchMark.UpdateBenchMarkStats -----------------------------------
procedure TformBenchMark.UpdateBenchMarkStats;

begin
  with memBenchMark do
  begin
    Lines.Clear;
    Lines.Add(Format('Time: %d seconds',[iTime]));
    Lines.Add(Format('Current: %.2f FPS',
      [formMain.GLSceneViewer.FramesPerSecond]));
    Lines.Add('');
    Lines.Add(Format('Minimum: %.2f FPS',[dMin]));
    Lines.Add(Format('Average: %.2f FPS',[dAverage]));
    Lines.Add(Format('Maximum: %.2f FPS',[dMax]));
  end;
end;
// ----- TformBenchMark.BenchTimerTimer ----------------------------------------
procedure TformBenchMark.BenchTimerTimer(Sender: TObject);

begin
  iTime := iTime + Trunc(BenchTimer.Interval*0.001);
//  if iTime > geIESeconds.Value then   bTestClick(nil);
  if formMain.GLSceneViewer.FramesPerSecond < dMin then
    dMin := formMain.GLSceneViewer.FramesPerSecond;
  if formMain.GLSceneViewer.FramesPerSecond > dMax then
    dmax := formMain.GLSceneViewer.FramesPerSecond;
  Inc(iCount);
  dSum := dSum + formMain.GLSceneViewer.FramesPerSecond;
  dAverage := dSum/iCount;

  UpdateBenchMarkStats;
  Series1.AddXY(iTime,formMain.GLSceneViewer.FramesPerSecond);
  Series2.AddXY(iTime,dAverage);

	formmain.GLSceneViewer.ResetPerformanceMonitor;
end;
// ----- TformBenchMark.bTestClick ---------------------------------------------
procedure TformBenchMark.bTestClick(Sender: TObject);

begin
  if bTest.Caption = 'Start' then
  begin
    iTime := 0; // on show
    dmax := 0.0;
    dSum := 0.0;
    dmin := 1000;
    iCount := 0;
    Series1.Clear;
    Series2.Clear;
    bTest.Caption := 'Stop';
    bOk.Enabled := false;
  end else
  begin
    bTest.Caption := 'Start';
    bOK.Enabled := true;
  end;
  GLCadencer.Enabled := (bTest.Caption = 'Stop');
  BenchTimer.Enabled := GLCadencer.Enabled;
end;
// ----- TformBenchMark.GLCadencerProgress -------------------------------------
procedure TformBenchMark.GLCadencerProgress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  formMain.GLCamera.MoveAroundTarget(0,1); // turn at one degree intervals...
  Application.ProcessMessages;
end;
// ----- TformBenchMark.FormShow -----------------------------------------------
procedure TformBenchMark.FormShow(Sender: TObject);
begin
//   geieSeconds.Value := 30;
   geiesecondsExit(nil);
end;
// ----- TformBenchMark.geieSecondsExit ----------------------------------------
procedure TformBenchMark.geieSecondsExit(Sender: TObject);
begin
//  chartBench.BottomAxis.Maximum := geieSeconds.Value;
//  chartBench.BottomAxis.Increment := Trunc(0.1*geieSeconds.Value);
  clbSeries.Checked[0] := true; // preset current and average to be checked
  clbSeries.Checked[1] := true;
end;
// ----- TformBenchMark.FormClose ----------------------------------------------
procedure TformBenchMark.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  GLCadencer.Enabled := false;
  BenchTimer.Enabled := false;
end;
// ----- TformBenchMark.clbSeriesClickCheck ------------------------------------
procedure TformBenchMark.clbSeriesClickCheck(Sender: TObject);

var
  bCheck : boolean;

begin
  if clbSeries.ItemIndex = -1 then
    exit;
  bcheck := clbSeries.CHecked[clbSeries.ItemIndex];

  case clbSeries.ItemIndex of
    0: Series1.Active := bCheck;
    1: Series2.Active := bCheck;
    2: Series3.Active := bCheck;
    3: Series4.Active := bCheck;
    4: Series5.Active := bCheck;
    5: Series6.Active := bCheck;
    6: Series7.Active := bCheck;
    7: Series8.Active := bCheck;
    8: Series9.Active := bCheck;
    9: Series10.Active := bCheck;
  end;
end;
// ----- TformBenchMark.bOKClick -----------------------------------------------
procedure TformBenchMark.bOKClick(Sender: TObject);
begin
  Close;
end;
// ----- TformBenchMark.bCopyCurrentClick --------------------------------------
procedure TformBenchMark.bCopyCurrentClick(Sender: TObject);

var
  i:integer;
  aSeries : TLineSeries;

begin
  if clbSeries.ItemIndex = -1 then
    exit;

  case clbSeries.ItemIndex of
    0,1: begin
      MessageDlg('Cannot copy to the live series!',mtError,[mbOK],0);
      exit;
    end;
    2: aSeries := Series3;
    3: aSeries := Series4;
    4: aSeries := Series5;
    5: aSeries := Series6;
    6: aSeries := Series7;
    7: aSeries := Series8;
    8: aSeries := Series9;
    9: aSeries := Series10;
  end;

  bRenameClick(nil);
  aSeries.Clear;
  for i := 0 to Series1.Count-1 do
    aSeries.AddXY(Series1.XValue[i],Series1.YValue[i]);
end;
// ----- TformBenchMark.bRenameClick -------------------------------------------
procedure TformBenchMark.bRenameClick(Sender: TObject);

var
  aSeries:TLineSeries;

begin
  if clbSeries.ItemIndex = -1 then
    exit;
  case clbSeries.ItemIndex of
    0: aSeries := Series1;
    1: aSeries := Series2;
    2: aSeries := Series3;
    3: aSeries := Series4;
    4: aSeries := Series5;
    5: aSeries := Series6;
    6: aSeries := Series7;
    7: aSeries := Series8;
    8: aSeries := Series9;
    9: aSeries := Series10;
  end;
  aSeries.Title := InputBox('Name series',
    'Please enter a name for this series',aSeries.Title);
  clbSeries.Items.Strings[clbSeries.ItemIndex] := aSeries.Title;
end;
// ----- TformBenchMark.bCopyAverageClick --------------------------------------
procedure TformBenchMark.bCopyAverageClick(Sender: TObject);

var
  i:integer;
  aSeries : TLineSeries;

begin
  if clbSeries.ItemIndex = -1 then
    exit;

  case clbSeries.ItemIndex of
    0,1: begin
      MessageDlg('Cannot copy to the live series!',mtError,[mbOK],0);
      exit;
    end;
    2: aSeries := Series3;
    3: aSeries := Series4;
    4: aSeries := Series5;
    5: aSeries := Series6;
    6: aSeries := Series7;
    7: aSeries := Series8;
    8: aSeries := Series9;
    9: aSeries := Series10;
  end;

  bRenameClick(nil);
  aSeries.Clear;
  for i := 0 to Series2.Count-1 do
    aSeries.AddXY(Series2.XValue[i],Series2.YValue[i]);
end;
// ----- TformBenchMark.bColourClick -------------------------------------------
procedure TformBenchMark.bColourClick(Sender: TObject);

var
  aSeries:TLineSeries;

begin
  if clbSeries.ItemIndex = -1 then
    exit;
  case clbSeries.ItemIndex of
    0: aSeries := Series1;
    1: aSeries := Series2;
    2: aSeries := Series3;
    3: aSeries := Series4;
    4: aSeries := Series5;
    5: aSeries := Series6;
    6: aSeries := Series7;
    7: aSeries := Series8;
    8: aSeries := Series9;
    9: aSeries := Series10;
  end;
  ColorDialog.Color := aSeries.SeriesColor;
  if ColorDialog.Execute then
    aSeries.SeriesColor := ColorDialog.Color;
end;
// =============================================================================
end.
