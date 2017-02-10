unit fSymbolicRegression;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,  StdCtrls, TeEngine, Series, ExtCtrls, TeeProcs,
  Chart, Math, uNEATClasses, uTransferFunctionClasses, ActnList,
  XPStyleActnCtrls, ActnMan, VclTee.TeeGDIPlus, System.Actions;

type
  TfrmMainForm = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Chart1: TChart;
    Series2: TFastLineSeries;
    Series1: TFastLineSeries;
    Population: TNEATPopulation;
    Timer1: TTimer;
    RadioGroup_Function: TRadioGroup;
    CheckBox_AllowRecurrencies: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    ActionManager1: TActionManager;
    Action_Start: TAction;
    Action_Stop: TAction;
    procedure FormCreate(Sender: TObject);
    function PopulationCalculateFitness(TNEATPopulation: TNEATPopulation;
      Genotype: TGenotype): double;
    function PopulationShowBestIndividual(TNEATPopulation: TNEATPopulation;
      Genotype: TGenotype; DifferentFromPrevious: Boolean): double;
    procedure Timer1Timer(Sender: TObject);
    procedure CheckBox_AllowRecurrenciesClick(Sender: TObject);
    procedure Action_StartExecute(Sender: TObject);
    procedure PopulationAfterRunStopped(Sender: TObject);
    procedure Action_StopExecute(Sender: TObject);
    procedure PopulationBeforeStartRun(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RadioGroup_FunctionClick(Sender: TObject);
  private
    { Private declarations }
    FValueList : TList;
  public
    { Public declarations }
    function CalculateFitness(Genotype : TGenotype) : double;
  end;

var
  frmMainForm: TfrmMainForm;
  Population : TNEATPopulation;

implementation

{$R *.dfm}

var
  DrawResult : boolean=false;
function TfrmMainForm.CalculateFitness(Genotype: TGenotype): double;
var
  nf, f,x,err : single;
  T : single;
  Steps : integer;
  FunctionID : integer;
begin
  if DrawResult then
    T := 1
  else
    T := 1;
  x := -T;

  err := 0;

  if DrawResult then
  begin
    Chart1.Series[0].Clear;
    Chart1.Series[1].Clear;
  end;

  FunctionID := RadioGroup_Function.ItemIndex;


  Steps := 0;
  repeat
    if not CheckBox_AllowRecurrencies.Checked then
      Genotype.Flush;

    Genotype.Inputs[0] := x;
    Genotype.Inputs[1] := 1;
    Genotype.InputNodes[1].BiasNode := true;
    Genotype.Iterate;

    if ZERO_BASED then
      nf := Genotype.OutputNodes[0].Value*2-1
    else
      nf := Genotype.OutputNodes[0].Value;

    f := 0;
    if Steps>=FValueList.Count then
    begin
      case FunctionID of
        0 : f := power(x,3)*2 + sqr(x)+x/3;
        1 : f := x*x*x*3+sqr(x);
        2 : f := round(x);
        3 : f := AdjustedGaussian(x);
        4 : f := 2*exp(-sqr(x*2.5))-1;
        5 : if abs(x)>0.5 then f := -abs(x) else f := abs(x);//}
        6 : f := abs(x);
        7 : f := tanh(x*5);
        8 : f := IfThen(x>0, -sqr(x), round(-x));
        9 : f := sin(x*2*pi);
      end;

      FValueList.Add(pointer(f));
    end else
      f := single(FValueList[Steps]);

    if DrawResult then
    begin
      Chart1.Series[0].AddXY(x,f);
      Chart1.Series[1].AddXY(x,nf);
    end;

    err := err + sqr(nf-f);

    x:=x+0.025*2;

    inc(Steps);
  until x>T;

  //result := (err/Steps)*100;
  result := power(1/(1+err/Steps), 4);
end;

procedure TfrmMainForm.FormCreate(Sender: TObject);
begin
  Show;
  Randomize;

  RegisterNewTransferFuntion('Main','Gaussian',TTransferFunctionGaussian.Create);
  RegisterNewTransferFuntion('Main','Mul',TTransferFunctionMUL.Create);
  RegisterNewTransferFuntion('Main','Add',TTransferFunctionADD.Create);

  FValueList := TList.Create;
  RadioGroup_Function.ItemIndex := 0;
end;

function TfrmMainForm.PopulationCalculateFitness(
  TNEATPopulation: TNEATPopulation; Genotype: TGenotype): double;
begin
  try
    result := CalculateFitness(Genotype)
  except
    result := 0;
  end;
end;

function TfrmMainForm.PopulationShowBestIndividual(
  TNEATPopulation: TNEATPopulation; Genotype: TGenotype;
  DifferentFromPrevious: Boolean): double;
begin
  Memo1.Lines.Add(Format('Gen:%d, Best fitness : %f,spcs:%d',
    [Population.CurrentGeneration,
     Population.BestGenotype.Fitness,
     Population.SpeciesList.Count]));

  Memo2.Lines.Text := Population.BestGenotype.SaveToString;

  if DifferentFromPrevious then
  begin
    DrawResult := true;
    CalculateFitness(Population.BestGenotype);
    DrawResult := false;
  end;//}

  result := 0;
end;

procedure TfrmMainForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  Population.StartRun;
end;

procedure TfrmMainForm.CheckBox_AllowRecurrenciesClick(Sender: TObject);
begin
  Population.AllowRecurrentLinks := CheckBox_AllowRecurrencies.Checked;
end;

procedure TfrmMainForm.Action_StartExecute(Sender: TObject);
begin
  Population.StartRun;
end;

procedure TfrmMainForm.PopulationAfterRunStopped(Sender: TObject);
begin
  Action_Start.Enabled := true;
  Action_Stop.Enabled := false;
end;

procedure TfrmMainForm.Action_StopExecute(Sender: TObject);
begin
  Population.StopRun;
end;

procedure TfrmMainForm.PopulationBeforeStartRun(Sender: TObject);
begin
  Action_Start.Enabled := false;
  Action_Stop.Enabled := true;
end;

procedure TfrmMainForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Population.StopRun;
end;

procedure TfrmMainForm.RadioGroup_FunctionClick(Sender: TObject);
begin
  FValueList.Clear;
end;

end.

