unit fMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,  StdCtrls, TeEngine, Series, ExtCtrls, TeeProcs,
  Chart, Math, uCambrianNNClasses, uTransferFunctionClasses;

type
  TfrmMainForm = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Chart1: TChart;
    Series2: TFastLineSeries;
    Series1: TFastLineSeries;
    Population: TNNPopulation;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    function PopulationCalculateFitness(TNNPopulation: TNNPopulation;
      Genotype: TGenotype): Single;
  private
     
  public
     
    function CalculateFitness(Genotype : TGenotype) : single;
  end;

var
  frmMainForm: TfrmMainForm;
  Population : TNNPopulation;

implementation

{$R *.dfm}

var
  DrawResult : boolean=false;
function TfrmMainForm.CalculateFitness(Genotype: TGenotype): single;
var
  nf, f,x,err : single;
  T : single;
  Steps : integeR;
begin
  if DrawResult then
    T := 1
  else
    T := 1;
  x := -T;

  Genotype.ResetValues;
  err := 0;

  if DrawResult then
  begin
    Chart1.Series[0].Clear;
    Chart1.Series[1].Clear;
  end;

  Genotype.InputNodes[1].Value := 1;

  Steps := 0;
  repeat
    inc(Steps);
    Genotype.InputNodes[0].Value := x;
    Genotype.Iterate(1);

    nf := Genotype.OutputNodes[0].Value;

    //f := power(x,3)*2 + sqr(x)+x/3;
    //f := x*x*x*3+sqr(x);
    //f := round(x);
    //f := AdjustedGaussian(x);
    //f := 2*exp(-sqr(x*2.5))-1;
    //f := AdjustedSigmoid(x);
    //f := 1/(1+exp(-4.9*x));

{    if x>0 then
      f := power(x,3)*2 + sqr(x)+x/3+0.68
    else
      f := 0.68;//}

    //f := round(x);
{    if abs(x)>0.5 then
      f := -abs(x)
    else
      f := abs(x);//}
    //f := abs(x);

//    f := tanh(x*5);
    if x>0 then
      f := -sqr(x)
    else
      f := round(-x);//}


{    if x>0.15 then
      f := 0.5
    else
      f := -0.25;//}

    if DrawResult then
    begin
      Chart1.Series[0].AddXY(x,f);
      Chart1.Series[1].AddXY(x,nf);
    end;

    err := err + sqr(nf-f);

    x:=x+0.025;
  until x>T;

  result := (err/Steps)*100;
end;

procedure TfrmMainForm.FormCreate(Sender: TObject);
var
  Genotype : TGenotype;
  gen : integer;
begin
  Show;
  Randomize;

  Population.CreatePopulation;
  Population.CalculateFitnesses;

  for gen := 0 to 5000 do
  begin
    if Application.Terminated then exit;

    Population.CreateNewGeneration;
    Population.CalculateFitnesses;

    Memo1.Lines.Add(Format('Gen:%d, Best fitness : %f,spcs:%d',[Gen, Population.BestGenotype.Fitness, Population.SpeciesList.Count]));
    Memo2.Lines.Text := Population.BestGenotype.SaveToString;

    DrawResult := true;
    CalculateFitness(Population.BestGenotype);
    DrawResult := false;//}

    Sleep(1);
    Application.ProcessMessages;
  end;//}
end;

procedure TfrmMainForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Application.TErminate;
end;

function TfrmMainForm.PopulationCalculateFitness(
  TNNPopulation: TNNPopulation; Genotype: TGenotype): Single;
begin
  try
    result := CalculateFitness(Genotype)
  except
    result := 100000;
  end;
end;

end.
