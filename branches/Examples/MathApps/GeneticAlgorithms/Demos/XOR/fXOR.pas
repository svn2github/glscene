unit fXOR;

// Increasing InitialWeightMAgn

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, math, Menus, mmSystem, ExtCtrls,
  uNeatClasses;

const
  cNumberOfBits = 2;
  cAddBias = true;
  cPrebuildStructure = false;
  cOverfit = false;
  cMinSpeciesRelatedness = 2.5;

type
  TfrmXOR = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    MainMenu1: TMainMenu;
    GO1: TMenuItem;
    BigGO1: TMenuItem;
    Panel1: TPanel;
    CheckBox_SpeciesDetails: TCheckBox;
    Button_LoadIndividual: TButton;
    Button_ShowCode: TButton;
    Edit_Tests: TEdit;
    Label1: TLabel;
    Edit_InitialWeightRange: TEdit;
    Label2: TLabel;
    NEATPopulation: TNEATPopulation;
    function NEATPopulationCalculateFitness(NEATPopulation: TNEATPopulation;
      Genotype: TGenotype): double;
    procedure FormCreate(Sender: TObject);
    procedure GO1Click(Sender: TObject);
    procedure BigGO1Click(Sender: TObject);
    function NEATPopulationShowBestIndividual(NEATPopulation: TNEATPopulation;
      Genotype: TGenotype; DifferentFromPrevious: Boolean): double;
    procedure NEATPopulationPrepareInitialGenotype(
      NEATPopulation: TNEATPopulation; Genotype: TGenotype);
    procedure NEATPopulationGenotypeIteration(NEATPopulation: TNEATPopulation;
      Genotype: TGenotype; IterationState: TIterationState);
    procedure CheckBox_ViewInteractiveIterationsClick(Sender: TObject);
    procedure Button_LoadIndividualClick(Sender: TObject);
    procedure Button_ShowCodeClick(Sender: TObject);
  private
    { Private declarations }
    function MyModify(x : double) : double;
  public
    { Public declarations }
  end;

var
  frmXOR: TfrmXOR;

implementation

uses uTransferFunctionClasses, uNEATStrFunctions, uDrawGenotype;

{$R *.dfm}

var
  DrawResult : boolean=false;
  VisibleRun : boolean;
function TfrmXOR.NEATPopulationCalculateFitness(
  NEATPopulation: TNEATPopulation; Genotype: TGenotype): double;

  function FToB(x : double) : boolean;
  begin
    result := (x>0.5);
  end;

  function BToF(b : boolean) : double;
  begin
    if b then
      result := 1
    else
      result := 0;
  end;
var
  i,j,res,cnt,hits,tests,RealTests : integer;
  Bits : integer;
  op, OneActive : double;
  ErrorSum : double;
  dError : double;
  s : string;
begin
  //if Genotype = NEATPopulation.Genotypes[0] then
  if CheckBox_SpeciesDetails.Checked and (Genotype = NEATPopulation.Genotypes[0]) then
  begin
    Memo1.Lines.Add(' ** ');
    for i := 0 to NEATPopulation.SpeciesList.Count-1 do
    begin
      if not NEATPopulation.SpeciesList[i].DeadSpecies then
        Memo1.Lines.Add(Format('%d : CFit=%f, AdjFitSum=%f, Size=%d',[i, NEATPopulation.SpeciesList[i].Champion.Fitness, NEATPopulation.SpeciesList[i].AdjustedFitnessSum, NEATPopulation.SpeciesList[i].MemberCount]))
        //Memo1.Lines.Add(Format('%d : CFit=%f, Size=%d',[i, NEATPopulation.SpeciesList[i].Champion.Fitness, NEATPopulation.SpeciesList[i].MemberCount]))
      else
        //Memo2.Lines.Add(Format('%d : dead',[i]))
    end;
  end;//}

  // BIAS
  //Genotype.InputNodes[0].Value := 1;

  Bits := cNumberOfBits;
  hits := 0;
  RealTests := 0;
  tests := trunc(power(2,bits));

  if DrawResult then
    s := '';

  ErrorSum := 0;

  //for i := 0 to tests-1 do
  i := 0;
  while i<tests do
  begin
    // Clear out all old values from the nodes
    Genotype.Flush;

    // Add the value to the bias node
    if cAddBias then
    begin
      Genotype.InputNodes[0].Value := 1;
      Genotype.InputNodes[0].BiasNode := true;
    end;

    cnt := 0;
    inc(RealTests);

    for j := 0 to Bits-1 do
    begin
      res := i and (1 shl j);

      if res<>0 then
      begin
        if cAddBias then
          Genotype.InputNodes[j+1].Value := 1
        else
          Genotype.InputNodes[j].Value := 1;

        inc(cnt);
      end
      else
      begin
        if cAddBias then
          Genotype.InputNodes[j+1].Value := 0
        else
          Genotype.InputNodes[j].Value := 0;
      end;
    end;

    Genotype.Iterate;

    op := Genotype.OutputNodes[0].Value;

{   // These settings make more sense, but they break the system somewhat,
    // solving takes longer (~36 generations)

    if cnt=1 then
      OneActive := 1
    else
      OneActive := BoolToFloat(false);

    dError := abs(OneActive-op)/2;//}

    if cnt=1 then
      OneActive := 1
    else
      OneActive := 0;

    dError := abs(OneActive-op);

    // Controversial line?
    if (FTob(OneActive)=FToB(op)) and (not cOverfit) then
      dError := 0;//}

    {if FTob(OneActive)=FToB(op) then
      dError := 0
    else
      dError := 1;//}

    ErrorSum := ErrorSum + dError;

    if DrawResult then
    begin
      for j := 0 to Bits-1 do
        s := s + CL_IfThen(Genotype.Inputs[j+1]=1,'1 ','0 ');

      s := s + ' xor = ' + CL_IfThen(FToB(OneActive),'1','0');
      s := s + ', AI = ' + CL_IfThen(FToB(op),'(1) ','(0) ')+Format('%-1.02f',[OP]);
      s := s + Format(' Error=%f',[dError]);
    end;

    if FTob(OneActive)=FToB(op) then
    begin
      inc(hits);
    end else
      if DrawResult then
        s := s + ' MISS';

    if DrawResult then
    begin
      Memo2.Lines.Add(s);
      s := '';
    end;
    inc(i);
  end;

  Assert(RealTests=tests,'Bad math!');

  if (hits<tests) then
    result := MyModify(tests - ErrorSum)
  else
    result := MyModify(tests);

  if DrawResult then
  begin
    Memo2.Lines.Add('');
    Memo2.Lines.Add(Format('Hits : %d, ErrorSum : %f, Fitness : %f',[Hits, ErrorSum, Result]));
  end;
end;

procedure TfrmXOR.FormCreate(Sender: TObject);
begin
  //Randomize;
  Show;

  with NEATPopulation do
  begin
    if cPrebuildStructure then
    begin
      ConnectionAddChance := 0;
      ConnectionSplitChance := 0;
    end;

    if cNumberOfBits = 2 then
    begin
      SurvivalThreshold := 0.2;
      PopulationSize := 150;
      GenerationsToRun := 300;

    end else if cNumberOfBits = 3 then
    begin
      SurvivalThreshold := 0.2;
      SpeciesFitnessMethod := sfmHighestFitness;
      SpeciesTargetCount := 45;
      GenerationsToRun := 400;
      PopulationSize := 500;
    end else
    begin
      SurvivalThreshold := 0.2;
      SpeciesFitnessMethod := sfmHighestFitness;
      SpeciesTargetCount := 60;
      GenerationsToRun := 400;
      PopulationSize := 1000;
    end;

    MinSpeciesRelatedness := cMinSpeciesRelatedness;

    if cAddBias then
      InputNodeCount := cNumberOfBits+1
    else
      InputNodeCount := cNumberOfBits;

    TargetFitness := MyModify(power(2,cNumberOfBits));

    CreateFitnessMonitor;


    FitnessMonitor.ResetForNewRun;
    FitnessMonitor.SetBestFitness(TargetFitness);
  end;

  Caption := Format('NEAT XOR, %d bits',[cNumberOfBits]);
end;

procedure TfrmXOR.GO1Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  VisibleRun := true;
  NEATPopulation.FitnessMonitor.ResetForNewRun;
  NEATPopulation.InitialWeightMagnitude := StrToFloat(Edit_InitialWeightRange.Text);
  NEATPopulation.StartRun(1);
  //NEATPopulation.StartRun;
end;

procedure TfrmXOR.BigGO1Click(Sender: TObject);
var
  i : integer;
  win : integer;
  sizesum : integer;
  wingensum : integer;
  starttime : cardinal;
  TestRunCount : integer;
begin
  NEATPopulation.InitialWeightMagnitude := StrToFloat(Edit_InitialWeightRange.Text);

  VisibleRun := false;
  Memo2.Lines.Clear;

  startTime := timeGetTime;

  win := 0;
  wingensum := 0;
  sizesum := 0;

  // Prepare the fitnessmonitor
  NEATPopulation.CreateFitnessMonitor;
  NEATPopulation.FitnessMonitor.ResetForNewRun;

  TestRunCount := StrToInt(Edit_Tests.Text);

  for i := 0 to TestRunCount-1 do
  begin
    Memo1.Lines.Clear;
    NEATPopulation.StartRun(1, false);

    if Application.Terminated then
      exit;

    if NEATPopulation.WinnerFound then
    begin
      inc(win);
      sizesum := sizesum + NEATPopulation.BestGenotype.GetSize;
      wingensum := wingensum + NEATPopulation.CurrentGeneration;
    end;

    Memo2.Lines.Add(Format('%d'#9'%d'#9'%d'#9'%f%%'#9'%d',
      [i,NEATPopulation.CurrentGeneration,win,IfThen(win>0,100*win/(i+1),0), NEATPopulation.BestGenotype.GetSize]));
  end;
  Memo2.Lines.Add(Format('win generation :%f',[wingensum/win]));
  Memo2.Lines.Add(Format('(AvgSize :%f)',[sizesum/win]));
  Memo2.Lines.Add('');
  Memo2.Lines.Add(Format('Time : %s',[TimeToStr((timeGetTime-starttime)/1000/60/60/24)]));
end;

function TfrmXOR.NEATPopulationShowBestIndividual(
  NEATPopulation: TNEATPopulation; Genotype: TGenotype;
  DifferentFromPrevious: Boolean): double;
begin
  result := 0;
  if DifferentFromPrevious then
  begin
    Memo1.Lines.Add(Format('Gen:%d, Best fitness : %f,spcs:%d (%f)',[
      NEATPopulation.CurrentGeneration, Genotype.Fitness, NEATPopulation.SpeciesList.Count, NEATPopulation.MinSpeciesRelatedness]));//}

    if VisibleRun then
    begin
      Memo2.Lines.Text := Genotype.SaveToString;
      Memo2.Lines.Add('');
      DrawResult := true;

      try
        NEATPopulation.GenotypeIterationEvent := true;
        NEATPopulationCalculateFitness(NEATPopulation, Genotype);
      finally
        NEATPopulation.GenotypeIterationEvent := false;
      end;

      DrawResult := false;
      Memo2.Lines.Add('');
      Memo2.Lines.Add(Format('Depth=%d',[Genotype.MaxNodeDepth]));
    end;//}
  end;
end;

function TfrmXOR.MyModify(x: double): double;
begin
  result := sqr(x);
end;

procedure TfrmXOR.NEATPopulationPrepareInitialGenotype(
  NEATPopulation: TNEATPopulation; Genotype: TGenotype);
begin
  if cPrebuildStructure then
  begin
    try
      Genotype.ConnectList[0].Enabled := true;
      Genotype.ConnectList[0].MutateSplit;

      if cAddBias then
      begin
        Genotype.AddConnection(1, 4, NEATPopulation.GetRandomWeight);
        Genotype.AddConnection(2, 4, NEATPopulation.GetRandomWeight);
      end else
        Genotype.AddConnection(1, 3, NEATPopulation.GetRandomWeight);
    except
      on exception do
        ShowMessage('Failed to build initial construct.');
    end;
  end;
end;

procedure TfrmXOR.NEATPopulationGenotypeIteration(
  NEATPopulation: TNEATPopulation; Genotype: TGenotype;
  IterationState: TIterationState);
begin
{  case IterationState of
    isPreIterations : Label1.Caption := 'isPreIterations';
    isMidIterations : Label1.Caption := 'isMidIterations';
    isPostIterations : Label1.Caption := 'isPostIterations';
  end;

  DrawGenotype(Genotype, Image1.Canvas, true, true, true);

  while Button_Cont.Enabled do
  begin
    Sleep(150);
    Application.ProcessMessages;
  end;//}
end;

procedure TfrmXOR.CheckBox_ViewInteractiveIterationsClick(Sender: TObject);
begin
{  if CheckBox_ViewInteractiveIterations.Checked then
    NEATPopulation.OnGenotypeIteration := NEATPopulationGenotypeIteration
  else
    NEATPopulation.OnGenotypeIteration := nil;//}
end;

procedure TfrmXOR.Button_LoadIndividualClick(Sender: TObject);
var
  Genotype : TGenotype;
begin
  VisibleRun := true;
  NEATPopulation.CreatePopulation;
  Genotype := NEATPopulation.Genotypes[0];
  Genotype.LoadFromFile('XORWinner1.KNEAT');

{  Genotype.ConnectList[0].MutateSplit;
  Genotype.PreparePhenotype;//}


  NEATPopulationShowBestIndividual(NEATPopulation, Genotype, true);
  NEATPopulation.FitnessMonitor.PublishGenotype(Genotype);
  VisibleRun := false;
end;

procedure TfrmXOR.Button_ShowCodeClick(Sender: TObject);
begin
  Memo2.Lines.Text := NEATPopulation.Genotypes[0].AsCode;
end;

function fSigmoid(x : double) : double;
begin
  result := 1/(1+exp(-4.924273*x));
end;

{procedure GenotypeAsCode(const Input1 : double; const Input2 : double; const Input3 : double; var Output1 : double);
var
  i : integer;
  Node1Value, Node1OldValue : double;
  Node2Value, Node2OldValue : double;
  Node3Value, Node3OldValue : double;
  Node4Value, Node4OldValue, Activate4Sum : double;
  Node12Value, Node12OldValue, Activate12Sum : double;
begin
  // Set inputs
  Node1Value := Input1; Node1OldValue := Input1;
  Node2Value := Input2; Node2OldValue := Input2;
  Node3Value := Input3; Node3OldValue := Input3;
  Node4Value := 0; Node4OldValue := 0;
  Node12Value := 0; Node12OldValue := 0;

  // Activate the network
  for i := 0 to 2 do
  begin
    Activate4Sum := 
      Node1OldValue * -4.2249 + 
      Node2OldValue * -0.7118 + 
      Node3OldValue * -2.4810 + 
      Node12OldValue * 7.1382;
    Node4Value := fSigmoid(Activate4Sum);

    Activate12Sum := 
      Node2OldValue * 2.2435 + 
      Node3OldValue * 3.8710;
    Node12Value := fSigmoid(Activate12Sum);

    Node4OldValue := Node4Value;
    Node12OldValue := Node12Value;
  end;

  // Set outputs
  Output1 := Node4Value;
end;//}

end.
