unit fFitnessMonitor;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ActnList, Vcl.ActnMan, Vcl.Menus, Vcl.ToolWin, Vcl.ImgList,
  Vcl.StdActns, StrUtils, Vcl.XPStyleActnCtrls,
  VclTee.TeeGDIPlus, VclTee.TeEngine, VclTee.Series, VclTee.TeeProcs, VclTee.Chart,

  uNEATClasses;

type
  TfrmFitnessMonitor = class(TfrmFitnessMonitorBase)
    Chart_GenerationFitness: TChart;
    Series1: TFastLineSeries;
    Series2: TFastLineSeries;
    Splitter1: TSplitter;
    Series3: TFastLineSeries;
    Series4: TFastLineSeries;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Chart_FitnessLine: TChart;
    Chart_CumSuccess: TChart;
    FastLineSeries2: TFastLineSeries;
    Label1: TLabel;
    Label_Runs: TLabel;
    Label_Hits: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    Label_HitPercentage: TLabel;
    TabSheet3: TTabSheet;
    Memo_History: TMemo;
    TabSheet4: TTabSheet;
    Memo_Histogram: TMemo;
    Button4: TButton;
    ActionManager1: TActionManager;
    Action_Close: TAction;
    Action_CreateHistogram: TAction;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Close1: TMenuItem;
    Evolution1: TMenuItem;
    Action_StopRun: TAction;
    Action_StartRun: TAction;
    Startrun1: TMenuItem;
    Stoprun1: TMenuItem;
    Label_Generation: TLabel;
    Label5: TLabel;
    CoolBar1: TCoolBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton2: TToolButton;
    Action_SaveReport: TFileSaveAs;
    N1: TMenuItem;
    SaveAs1: TMenuItem;
    Action_AboutCambrianGP: TAction;
    Help1: TMenuItem;
    AboutCambrianGP1: TMenuItem;
    Label4: TLabel;
    Label_AvgWinGeneration: TLabel;
    Label6: TLabel;
    Label_AverageEvalsPerWin: TLabel;
    Label7: TLabel;
    Label_SpeciesCount: TLabel;
    Label8: TLabel;
    Label_MinRelatedness: TLabel;
    Series5: TLineSeries;
    TabSheet5: TTabSheet;
    Image_WinnerView: TImage;
    CheckBox_ShowNodeNumbers: TCheckBox;
    CheckBox_ShowConnectNumbers: TCheckBox;
    TabSheet6: TTabSheet;
    Memo_ChampCode: TMemo;
    Label9: TLabel;
    Label_PopulationSize: TLabel;
    CheckBox_ShowValues: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Action_CloseExecute(Sender: TObject);
    procedure Action_CreateHistogramExecute(Sender: TObject);
    procedure Action_StopRunExecute(Sender: TObject);
    procedure Action_StartRunExecute(Sender: TObject);
    procedure Action_SaveReportAccept(Sender: TObject);
    procedure Action_SaveReportBeforeExecute(Sender: TObject);
    procedure Action_AboutCambrianGPExecute(Sender: TObject);
  protected
  public
    { Public declarations }

    procedure SetBestFitness(const Value: double);override;
    procedure SetRunState(Running : boolean); override;
    procedure ResetForNewRun;override;
    procedure UpdatePopulationView;override;

    procedure PublishGenotype(Genotype : TGenotype);override;

    procedure DrawFitnessSpread;
  end;

var
  frmFitnessMonitor: TfrmFitnessMonitor;
  FitnessLandscape : TLineSeries;
  GenerationFitness : TFastLineSeries;
  GenerationAvgFitness : TFastLineSeries;
  GenerationSize : TFastLineSeries;
  GenerationAvgSize : TFastLineSeries;

implementation

uses
  uNEATStrFunctions, uDrawGenotype;

{$R *.dfm}

procedure TfrmFitnessMonitor.FormCreate(Sender: TObject);
begin
  FitnessLandscape := TLineSeries(Chart_FitnessLine.SeriesList[0]);

  GenerationFitness := TFastLineSeries(Chart_GenerationFitness.SeriesList[0]);
  GenerationAvgFitness := TFastLineSeries(Chart_GenerationFitness.SeriesList[1]);

  GenerationSize := TFastLineSeries(Chart_GenerationFitness.SeriesList[2]);
  GenerationAvgSize := TFastLineSeries(Chart_GenerationFitness.SeriesList[3]);

  PageControl1.ActivePageIndex := 0;

  RunSuccess := TList.Create;
  TotalRuns := 0;
  TotalHits := 0;
end;

procedure TfrmFitnessMonitor.ResetForNewRun;
var
  i : integer;
  procedure AddLine(s : string);
  begin
    Memo_History.Lines.Add(s);
  end;
  procedure ReportPopulation;
  begin
    AddLine(Format('Started : %s',[DateTimeToStr(now)]));
    AddLine('');
    AddLine('Population');
    AddLine(Format('  Individuals : %d',[NEATPopulation.PopulationSize]));
    AddLine(Format('  Generations : %d',[NEATPopulation.GenerationsToRun]));
    AddLine(Format('  Inputs      : %d',[NEATPopulation.InputNodeCount]));
    AddLine(Format('  Outputs     : %d',[NEATPopulation.OutputNodeCount]));
    case NEATPopulation.SelectionType of
      stSpecies         : AddLine('  SelectionType : smSpecies');
      stFitProp         : AddLine('  SelectionType : stFitProp');
      stTournament      : AddLine(Format('  SelectionType : stTournament, TSize = %d',[NEATPopulation.TournamentSize]));
      stLocalTournament : AddLine(Format('  SelectionType : stLocalTournament, TSize = %d, TRange = %d',[NEATPopulation.TournamentSize, NEATPopulation.TournamentRange]));
    end;
    AddLine(Format('  ConnectionWeightMutationChance      : %f%%',[NEATPopulation.ConnectionWeightMutationChance*100]));
    AddLine(Format('  ConnectionMutationBigChangeChance   : %f%%',[NEATPopulation.ConnectionMutationBigChangeChance*100]));

    AddLine(Format('  WeightPeturbationFactor   : %f',[NEATPopulation.WeightPeturbationFactor]));
    AddLine(Format('  InitialWeightMagnitude    : %f',[NEATPopulation.InitialWeightMagnitude]));


    AddLine(Format('  ReproduceMutateRate   : %f%%',[NEATPopulation.ReproduceMutateRate*100]));
    AddLine(Format('  Survival Threshold    : %f%%',[NEATPopulation.SurvivalThreshold*100]));
    AddLine(Format('  MutateCrossed         : %s',[IfThen(NEATPopulation.MutateCrossed, 'TRUE', 'FALSE')]));
    AddLine(Format('  ActivationIterations  : %d',[NEATPopulation.ActivationIterations]));
    AddLine(Format('  MinSpeciesRelatedness : %f',[NEATPopulation.MinSpeciesRelatedness]));
    AddLine(Format('  SpeciesTargetCount    : %d',[NEATPopulation.SpeciesTargetCount]));
    AddLine(Format('  SpeciesFitness        : %s',[IfThen(NEATPopulation.SpeciesFitnessMethod = sfmAverageFitness, 'Average', 'Max')]));
    AddLine(Format('  Allow Recurencies     : %s',[IfThen(NEATPopulation.AllowRecurrentLinks, 'TRUE', 'FALSE')]));
  end;
begin
  Assert(Assigned(NEATPopulation),'No population assigned!');

  RunSuccess.Clear;
  TotalRuns := 0;
  TotalHits := 0;
  WinGeneration := 0;

  for i := 0 to NEATPopulation.GenerationsToRun do
    RunSuccess.Add(pointer(0));

  Memo_History.Lines.Clear;
  AddLine('');
  ReportPopulation;

  AddLine('');
  AddLine('End success rate:');
  AddLine('Average win generation:');
  AddLine('----------------------');
  AddLine(CL_Format('Result|Fin.Gen|Run|Size|Evals|Hidden',[]));

  Memo_Histogram.Lines.Clear;
end;

procedure TfrmFitnessMonitor.SetBestFitness(const Value: double);
begin
  Chart_GenerationFitness.LeftAxis.Maximum := Value;
  Chart_FitnessLine.LeftAxis.Maximum := Value;
  FBestFitness := Value;
end;

procedure TfrmFitnessMonitor.UpdatePopulationView;
var
  SuccessSoFar : integer;
  procedure UpdateCumlSuccess;
  var
    i : integer;
  begin
    Chart_CumSuccess.BottomAxis.Maximum := NEATPopulation.GenerationsToRun;
    Chart_CumSuccess.Series[0].Clear;
    SuccessSoFar := 0;

    for i := 0 to NEATPopulation.GenerationsToRun do
    begin
      SuccessSoFar := SuccessSoFar + integer(RunSuccess[i]);

      if TotalRuns > 0 then
        Chart_CumSuccess.Series[0].AddXY(i, SuccessSoFar/TotalRuns*100);
    end;
  end;
begin
  Assert(Assigned(NEATPopulation), 'No population has been assigned!');

  Label_Generation.Caption := IntToStr(NEATPopulation.CurrentGeneration);
  Label_SpeciesCount.Caption := Format('%d (%d)',[NEATPopulation.LivingSpeciesCount, NEATPopulation.SpeciesList.Count]);
  Label_MinRelatedness.Caption := Format('%2.1f',[NEATPopulation.MinSpeciesRelatedness]);
  Label_PopulationSize.Caption := IntToStr(NEATPopulation.PopulationSize);

  // Is this the last generation?
  if NEATPopulation.WinnerFound then
  begin
    Memo_History.Lines.Add(CL_Format('Won|%d|%d|%d|%d|%d',
      [NEATPopulation.CurrentGeneration, TotalRuns, NEATPopulation.BestGenotype.GetSize,
       NEATPopulation.IndividualsEvaluatedThisRun, NEATPopulation.BestGenotype.GetHiddenNodeCount]));
  end else
  if (NEATPopulation.CurrentGeneration>=NEATPopulation.GenerationsToRun) then
  begin
    Memo_History.Lines.Add(CL_Format('Failed|%d|%d|%d|0|%d',
      [NEATPopulation.CurrentGeneration+1, TotalRuns, NEATPopulation.BestGenotype.GetSize,
        NEATPopulation.BestGenotype.GetHiddenNodeCount]));
  end;

  if NEATPopulation.CurrentGeneration=0 then
  begin
    UpdateCumlSuccess;

    GenerationFitness.Clear;
    GenerationAvgFitness.Clear;
    GenerationSize.Clear;
    GenerationAvgSize.Clear;

    Chart_CumSuccess.BottomAxis.Maximum := NEATPopulation.GenerationsToRun;

    if TotalRuns > 0 then
      Label_HitPercentage.Caption := Format('%f%%',[TotalHits/TotalRuns*100])
    else
      Label_HitPercentage.Caption := '0%';

    Label_Runs.Caption := IntToStr(TotalRuns);
    Label_Hits.Caption := IntToStr(TotalHits);

    if TotalHits>0 then
    begin
      Label_AvgWinGeneration.Caption := Format('%2.2f',[WinGeneration/TotalHits]);
      Label_AverageEvalsPerWin.Caption := Format('%2.2f',[WinGeneration/TotalHits*NEATPopulation.PopulationSize]);
    end;
  end;

  if Chart_GenerationFitness.BottomAxis.Maximum <> NEATPopulation.GenerationsToRun then
    Chart_GenerationFitness.BottomAxis.Maximum := NEATPopulation.GenerationsToRun;

  GenerationFitness.AddXY(NEATPopulation.CurrentGeneration, NEATPopulation.BestGenotype.Fitness);
  GenerationAvgFitness.AddXY(NEATPopulation.CurrentGeneration, NEATPopulation.GetAverageFitness);

  GenerationSize.AddXY(NEATPopulation.CurrentGeneration, NEATPopulation.BestGenotype.GetSize);
  GenerationAvgSize.AddXY(NEATPopulation.CurrentGeneration, NEATPopulation.GetAverageSize);

  if NEATPopulation.BestFitnessOldGeneration<>NEATPopulation.BestGenotype.Fitness then
  begin
    PublishGenotype(NEATPopulation.BestGenotype);
  end;

  if Visible and (PageControl1.ActivePageIndex=1) then
    DrawFitnessSpread;

  if NEATPopulation.WinnerFound then
  begin
    inc(TotalRuns);
    inc(TotalHits);
    WinGeneration := WinGeneration+NEATPopulation.CurrentGeneration;
    RunSuccess[NEATPopulation.CurrentGeneration] :=
      pointer(integer(RunSuccess[NEATPopulation.CurrentGeneration])+1);
  end else
  begin
    if NEATPopulation.CurrentGeneration=NEATPopulation.GenerationsToRun then
      inc(TotalRuns);
  end;
end;

procedure TfrmFitnessMonitor.Action_CloseExecute(Sender: TObject);
begin
  Hide;
end;

procedure TfrmFitnessMonitor.Action_CreateHistogramExecute(Sender: TObject);
var
  i,j,SuccessSoFar : integer;
  s : string;
  Started : boolean;
  NoCopy : boolean;
begin
  Memo_Histogram.Lines.Clear;

  NoCopy := NEATPopulation.CurrentlyRunning;

  Memo_Histogram.Lines.Add(CL_Format('%s|%s|%s',['Generation','Successrate','Hits']));

  Started := false;

  for i := 0 to Memo_History.Lines.Count-1 do
  begin
    s := Memo_History.Lines[i];

    if Started then
    begin
      if trim(s)='' then
      begin
        if not NoCopy then
          for j := Memo_History.Lines.Count - 1 downto i do
            Memo_History.Lines.Delete(j);
        break;
      end;
    end else
    begin
      if trim(GetBefore(#9,s))='Result' then
        Started := true;
    end;
  end;

  SuccessSoFar := 0;
  if TotalRuns>0 then
    for i := 0 to NEATPopulation.GenerationsToRun do
    begin
      SuccessSoFar := SuccessSoFar + integer(RunSuccess[i]);

      Memo_Histogram.Lines.Add(CL_Format('%d|%f|%f',
        [i,SuccessSoFar/TotalRuns * 100, integer(RunSuccess[i])/TotalRuns * 100]));
    end;

  i := 0;
  if TotalRuns>0 then
    while i<Memo_History.Lines.Count-1 do
    begin
      s := Memo_History.Lines[i];
      if (System.Pos('End success rate:', s) > 0) then
      begin
        Memo_History.Lines[i] := Format('End success rate: %f%%',
          [TotalHits/TotalRuns * 100]);
      end;

      if (System.Pos('Average win generation:', s) > 0) then
      begin
        if TotalHits>0 then
          Memo_History.Lines[i] := Format('Average win generation: %2.2f, Evals=%f',
            [WinGeneration/TotalHits,
             (WinGeneration+1)/TotalHits*NEATPopulation.PopulationSize]);
        break;
      end;

      inc(i);
    end;

  if not NoCopy then
  begin
    Memo_History.Lines.Add('');
    Memo_History.Lines.Add(Format('Total runs : %d',[TotalRuns]));
    Memo_History.Lines.Add('');
    Memo_History.Lines.Add('Histogram');
    Memo_History.Lines.Add('---------------');
    Memo_History.Lines.Add(Memo_Histogram.Lines.Text);
  end;
end;

procedure TfrmFitnessMonitor.Action_StopRunExecute(Sender: TObject);
begin
  if NEATPopulation<>nil then
    NEATPopulation.StopRun;
end;

procedure TfrmFitnessMonitor.Action_StartRunExecute(Sender: TObject);
begin
  if NEATPopulation<>nil then
  begin
    //NEATPopulation.RestartRun;
    NEATPopulation.StartRun;
  end;
end;

procedure TfrmFitnessMonitor.SetRunState(Running: boolean);
begin
  inherited;
  Action_StopRun.Enabled := running;
  Action_StartRun.Enabled := not Running;

  if Running=false then
    Action_CreateHistogram.Execute;
end;

procedure TfrmFitnessMonitor.Action_SaveReportAccept(Sender: TObject);
begin
  Memo_History.Lines.SaveToFile(Action_SaveReport.Dialog.FileName);
end;

procedure TfrmFitnessMonitor.Action_SaveReportBeforeExecute(
  Sender: TObject);
begin
  Action_SaveReport.Dialog.FileName := '*.txt';
end;

procedure TfrmFitnessMonitor.Action_AboutCambrianGPExecute(
  Sender: TObject);
begin
  ShowMessage('Aboutbox goes here!')
end;

procedure TfrmFitnessMonitor.DrawFitnessSpread;
var
  i,j : integer;
  Genotype : TGenotype;
begin
  FitnessLandscape.Clear;

  if (NEATPopulation.SelectionType = stSpecies) and false then
  begin
    for i := 0 to NEATPopulation.SpeciesList.Count-1 do
      for j := 0 to NEATPopulation.SpeciesList[i].Genotypes.Count-1 do
      begin
        Genotype := NEATPopulation.SpeciesList[i].Genotypes[j];

        if Genotype.SpeciesHint<>nil then
          FitnessLandscape.AddY(Genotype.Fitness,'', TColor(Genotype.SpeciesHint.Color))
        else
          FitnessLandscape.AddY(Genotype.Fitness);//}
      end;
  end else
  begin
    for i := 0 to NEATPopulation.Genotypes.Count-1 do
    begin
      Genotype := NEATPopulation.Genotypes[i];

      if Genotype.SpeciesHint<>nil then
        FitnessLandscape.AddY(Genotype.Fitness,'', TColor(Genotype.SpeciesHint.Color))
      else
        FitnessLandscape.AddY(Genotype.Fitness);//}
    end;
  end;

  Chart_FitnessLine.Refresh;
  Application.ProcessMessages;
end;

procedure TfrmFitnessMonitor.PublishGenotype(Genotype : TGenotype);
begin
  DrawGenotype(Genotype, Image_WinnerView.Canvas, CheckBox_ShowNodeNumbers.Checked, CheckBox_ShowConnectNumbers.Checked, CheckBox_ShowValues.Checked);
  Memo_ChampCode.Lines.Text := Genotype.SaveToString;
end;

end.
