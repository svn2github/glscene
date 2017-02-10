unit fPoleCart;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, uPoleClasses, StdCtrls,  ComCtrls, ActnList, ActnMan,
  Menus, CheckLst, TeEngine, Series, TeeProcs, Chart, math, StrUtils,
  uNEATClasses, XPStyleActnCtrls, uTransferFunctionClasses, System.Actions;

const
  // HOW MANY POLES?
  cTwoPoles = true;

  // SUPPLY cVelocities?
  cVelocities = true;

  cForceMag = 10;
  cOutputNodes = 1; // 2 is bad, 1 is good

type
  TGPPoleCart = class(TPoleCart)
    Genotype : TGenotype;
    cVelocities : boolean;

    procedure SetGamePosition(i : integer);
    function GetForceActivator : single; override;
  end;

  TfrmPoleBalancer = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    Label1: TLabel;
    Label_Description: TLabel;
    Label2: TLabel;
    LabelTime: TLabel;
    ActionManager1: TActionManager;
    Action_OnlyDrawWinners: TAction;
    CheckBox5: TCheckBox;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Draw1: TMenuItem;
    Onlydrawwinners1: TMenuItem;
    Action_SkipAnimation: TAction;
    Stopanimation1: TMenuItem;
    Button1: TButton;
    Help1: TMenuItem;
    About1: TMenuItem;
    Label3: TLabel;
    Label_Generation: TLabel;
    Label4: TLabel;
    Label_Fitness: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Memo_Individual: TMemo;
    CheckBox1: TCheckBox;
    Action_NoDraw: TAction;
    Nodraw1: TMenuItem;
    Action_FitnessLine: TAction;
    Fitnessline1: TMenuItem;
    TabSheet2: TTabSheet;
    Memo_Info: TMemo;
    NEATPopulation: TNEATPopulation;
    CheckBox_AllowGaussian: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Action_InvertChecked(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Action_StopRunExecute(Sender: TObject);
    procedure Action_SkipAnimationExecute(Sender: TObject);
    function NEATPopulationCalculateFitness(NEATPopulation: TNEATPopulation;
      Genotype: TGenotype): double;
    function NEATPopulationShowBestIndividual(NEATPopulation: TNEATPopulation;
      Genotype: TGenotype; DifferentFromPrevious: Boolean): double;
    procedure CheckBox_AllowGaussianClick(Sender: TObject);
  private
    { Private declarations }
    function DoFitnessTest(Genotype: TGenotype; MaxSteps : integer; Visible : boolean): single;
  public
    { Public declarations }
    PoleCart : TGPPoleCart;

    RunCount : integer;
  end;

var
  frmPoleBalancer: TfrmPoleBalancer;

implementation

uses uNEATStrFunctions;

{$R *.dfm}

function HasParam(Param : string) : boolean;
var
  i : integer;
begin
  Result := false;
  for i := 1 to ParamCount do
    if UpperCase(Param) = UpperCase(ParamStr(i)) then
    begin
      Result := true;
      break;
    end;
end;

procedure TfrmPoleBalancer.FormCreate(Sender: TObject);
var
  BiasCount : integer;
begin
  if cTwoPoles then
    Caption := Caption + ' : Double Pole '
  else
    Caption := Caption + ' : Single Pole ';

  if cVelocities then
    Caption := Caption + 'With Velocities '
  else
    Caption := Caption + 'Without Velocities ';

  PoleCart := TGPPoleCart.Create(cTwoPoles);
  PoleCart.cVelocities := cVelocities;
  PoleCart.TimeLabel := LabelTime;

  NEATPopulation.OutputNodeCount := cOutputNodes;

  if cTwoPoles and not cVelocities then
  begin
    NEATPopulation.PopulationSize := 1000;
    NEATPopulation.SpeciesFitnessMethod := sfmHighestFitness;
    NEATPopulation.ConnectionAddChance := 0.3;
    NEATPopulation.MinSpeciesRelatedness := 4;
    NEATPopulation.WeightUnrelatednessFactor := 3;
  end
  else if not cTwoPoles and not cVelocities then
  begin
    NEATPopulation.PopulationSize := 500;
    //NEATPopulation.SpeciesFitnessMethod := sfmHighestFitness;
    NEATPopulation.SpeciesFitnessMethod := sfmAverageFitness;
  end
  else
  begin
    NEATPopulation.PopulationSize := 150;
    NEATPopulation.SpeciesFitnessMethod := sfmAverageFitness;
  end;

  BiasCount := 1;

  PageControl1.ActivePageIndex := 0;
  Panel1.DoubleBuffered := true;

  PoleCart.Canvas := Image1.Canvas;

  if PoleCart.TwoPoles then
  begin
    if PoleCart.cVelocities then
      NEATPopulation.InputNodeCount := 6+BiasCount
    else
      NEATPopulation.InputNodeCount := 3+BiasCount;
  end else
  begin
    if PoleCart.cVelocities then
      NEATPopulation.InputNodeCount := 4+BiasCount
    else
      NEATPopulation.InputNodeCount := 2+BiasCount;
  end;

  Show;
  Randomize;
  NEATPopulation.CreateFitnessMonitor;
  NEATPopulation.FitnessMonitor.ResetForNewRun;
  NEATPopulation.FitnessMonitor.SetBestFitness(1);

  if HasParam('nodraw') then
    Action_NoDraw.Checked := true;
end;

{ TGPPoleCart }

function TGPPoleCart.GetForceActivator: single;
begin
  if TwoPoles then
  begin
    CopyStateToVariables;
    
    if cVelocities then
      Genotype.LoadValues([
        Angle/0.52,
        Position/2.4,
        ShortAngle/0.52,
        ShortAngleSpeed/2,
        AngleSpeed/2,
        Speed/2])
    else
      Genotype.LoadValues([
        state[0] / 4.8,
        state[2] / 0.52,
        state[4] / 0.52]);
  end else
  begin
    if cVelocities then
      Genotype.LoadValues([
        Angle/0.52,
        Position/2.4,
        AngleSpeed/2,
        Speed/2])
    else
      Genotype.LoadValues([
        Position/2.4,
        Angle/0.52]);
  end;

  // Bias
  Genotype.InputNodes[Genotype.InputNodes.Count-1].Value := 1;
  Genotype.InputNodes[Genotype.InputNodes.Count-1].BiasNode := true;

  Genotype.Iterate;

  if Genotype.OutputNodes.Count>1 then
  begin
    if (Genotype.Outputs[0] > Genotype.Outputs[1]) then
      result := 1
    else
      result := -1;
  end else
  begin
    // force = (action>0)? cForceMag : -cForceMag;
    if FloatToBool(Genotype.Outputs[0]) then
      result := -1
    else
      result := 1;
  end;
end;

function TfrmPoleBalancer.DoFitnessTest(Genotype : TGenotype; MaxSteps : integer; Visible : boolean) : single;
var
  Fitness, F1, F2 : single;
  TimeStepsBalanced : integer;
begin
  PoleCart.Visible := Visible;
  PoleCart.Genotype := Genotype;

  try
    Genotype.Flush;
    TimeStepsBalanced := PoleCart.BalancePole(MaxSteps);
    Genotype.Tag := trunc(TimeStepsBalanced);

    F1 := (TimeStepsBalanced/MaxSteps);
    if (TimeStepsBalanced>100) and (PoleCart.WiggleCount>0) then
        F2 := min(1,0.75/(PoleCart.WiggleCount))
      else
        F2 := 0;

    Fitness := 0.1 * F1 + 0.9 * F2;
    result := Fitness;
  except
    result := 0;
  end;
end;

var
  //double statevals[5]={0.05, 0.25, 0.5, 0.75, 0.95};
  statevals : array[0..4] of single = (0.05, 0.25, 0.5, 0.75, 0.95);

procedure TGPPoleCart.SetGamePosition(i: integer);
begin
  // set up zero start state
  if TwoPoles then
  begin
    state[0] := 0; // Pos
    state[1] := 0; // Speed
    state[2] := one_degree * 4.5;// angle
    state[3] := 0; // angle speed
    state[4] := 0; // angle 2
    state[5] := 0; // angle 2 speed
  end else
  begin
{    Position := RAIL_LENGTH*0.35;
    Speed := 0.35;

    Angle := 0.2;
    AngleSpeed := 0;//}
    Position := 0;
    Speed := 0;

    Angle := one_degree * 18;
    AngleSpeed := 0;
  end;
end;

procedure TfrmPoleBalancer.Action_InvertChecked(Sender: TObject);
begin
  TAction(Sender).Checked := not TAction(Sender).Checked; 
end;

procedure TfrmPoleBalancer.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmPoleBalancer.Action_StopRunExecute(Sender: TObject);
begin
  PoleCart.Visible := false;
end;

procedure TfrmPoleBalancer.Action_SkipAnimationExecute(Sender: TObject);
begin
  Action_SkipAnimation.Enabled := false;
  PoleCart.Visible := false;
end;

function TfrmPoleBalancer.NEATPopulationCalculateFitness(
  NEATPopulation: TNEATPopulation; Genotype: TGenotype): double;
begin
  PoleCart.SetGamePosition(0);
  result := DoFitnessTest(Genotype, MAX_STEPS, false);
end;

function TfrmPoleBalancer.NEATPopulationShowBestIndividual(
  NEATPopulation: TNEATPopulation; Genotype: TGenotype;
  DifferentFromPrevious: Boolean): double;
var
  xs : string;
  t : integer;
  SpeciesID : integer;

  function GeneralizationTest: boolean;
  var
    score : integer;
    s0c, s1c, s2c, s3c : integer;
  begin
    // Try the longer run!!
    PoleCart.SetGamePosition(0);
    DoFitnessTest(Genotype,100000, false);

    // If it fails on 100.000, then there's no point in going on!
    if PoleCart.Step<100000 then
    begin
      // We failed
      result := false;

      Memo_Info.Lines.Add(Format('100k failed (%d)',[PoleCart.Step]));

      // Report how it faired
      //xs := Format('fail %d steps of %d',[PoleCart.Step, 100000]);
      exit;
    end;

    if cVelocities then
    begin
      // No further testing for "with vel"
      xs := Format('A WINNER %d!', [PoleCart.Step]);
      result := true;
      exit;
    end;

    // Reset the score
    Score := 0;

    for s0c := 0 to 4 do
      for s1c := 0 to 4 do
      begin
        for s2c := 0 to 4 do
          for s3c := 0 to 4 do
            with PoleCart do
            begin
              state[0] := statevals[s0c] * 4.32 - 2.16;
              state[1] := statevals[s1c] * 2.70 - 1.35;
              state[2] := statevals[s2c] * 0.12566304 - 0.06283152;
              // 0.06283152 =  3.6 degrees */
              state[3] := statevals[s3c] * 0.30019504 - 0.15009752;
              // 00.15009752 =  8.6 degrees */
              state[4] :=0.0;
              state[5] :=0.0;

              // If it's a single pole!
              CopyStateToVariables;

              DoFitnessTest(Genotype,999, false);

              if PoleCart.Step>=999 then
                inc(Score);
            end;
        Application.ProcessMessages;
      end;

    Memo_Info.Lines.Add(Format('100k done, Score : %d',[Score]));

    // If we scored 200 or more, then it's a winner
    if Score>=200 then
    begin
      xs := Format('A WINNER %d/625!', [Score]);
      result := true;
    end else
    begin
      xs := Format('Got %d trials of 625!', [Score]);
      result := false;
    end;
  end;
begin
  result := 0;
  t := 0;
  try
    t := Genotype.Tag;

    if NEATPopulation.GetSpeciesForGenotype(NEATPopulation.BestGenotype) <> nil then
      SpeciesID := NEATPopulation.GetSpeciesForGenotype(NEATPopulation.BestGenotype).SpeciesID
    else
      SpeciesID := -1;

    Memo_Individual.Lines.Text :=
      Format('Species : %d (%d, %f)',[SpeciesID, NEATPopulation.SpeciesList.Count, NEATPopulation.MinSpeciesRelatedness])+#13#10+
      Genotype.SaveToString;

    Label_Generation.Caption := Format('%d',[NEATPopulation.CurrentGeneration]);
    Label_Fitness.Caption := Format('%0.5f', [Genotype.Fitness]);

    // If this one's no better than the previous one, just keep going
    if not DifferentFromPrevious then exit;

    // Has it succeeded for 10 seconds?
    if (Genotype.Tag >= MAX_STEPS) then
    begin
      if GeneralizationTest then
      begin
        // If it succeeds with the gen. test, it's considered a winner!
        Genotype.Fitness := NEATPopulation.TargetFitness;

        //ShowMessage(NEATCreatedAndDestroyed);
      end;
    end else
      xs := '';

    if Genotype.Fitness = 0 then
      Label_Description.Caption := Format('%s',[xs])
    else
      if Genotype.Tag<MAX_STEPS then
        Label_Description.Caption := Format('Early fail (%f s) %s',[Genotype.Tag*PoleCart.TAU, xs])
      else
        Label_Description.Caption := Format('A contestant (%f s) %s',[Genotype.Tag*PoleCart.TAU, xs]);

    if DifferentFromPrevious then
    begin
      if Action_OnlyDrawWinners.Checked and (Genotype.Fitness<>0) then
        exit;

      if Action_NoDraw.Checked then
        exit;

      Action_SkipAnimation.Enabled := true;
      PoleCart.SetGamePosition(0);
      DoFitnessTest(Genotype, MAX_STEPS*4, true);
      Action_SkipAnimation.Enabled := false;
    end;
  finally
    Genotype.Tag := t;
  end;
end;

procedure TfrmPoleBalancer.CheckBox_AllowGaussianClick(Sender: TObject);
begin
  if CheckBox_AllowGaussian.Checked then
    RegisterNewTransferFuntion('Main','Gaussian',TTransferFunctionGaussian.Create)
  else
  begin
    TransferFunctionList.Delete(1); // Not very clean
  end;
end;

end.
