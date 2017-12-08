unit fMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  uObstacleWorld, uAgent, ExtCtrls, StdCtrls, uNEATAgent, MMSystem,
  ActnList, ActnMan, ComCtrls, uTransferFunctionClasses, uNeatClasses,
  XPStyleActnCtrls;

const
  cBASE_FITNESS = 10;

{$DEFINE SINGLE_NODE_STEERING}  

type
  TfrmMainForm = class(TForm)
    ContinuousDraw: TCheckBox;
    Panel1: TPanel;
    Image1: TImage;
    StatusBar1: TStatusBar;
    Memo1: TMemo;
    NEATPopulation: TNEATPopulation;
    CheckBox_NoDraw: TCheckBox;
    CheckBox_FastDraw: TCheckBox;
    Memo2: TMemo;
    RadioGroup_Steering: TRadioGroup;
    Button1: TButton;
    Button2: TButton;
    ActionManager1: TActionManager;
    Action_Start: TAction;
    Action_Stop: TAction;
    CheckBox_AllowGaussian: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Button1Click(Sender: TObject);
    function NEATPopulationShowBestIndividual(NEATPopulation: TNEATPopulation;
      Genotype: TGenotype; DifferentFromPrevious: Boolean): double;
    function NEATPopulationCalculateFitness(NEATPopulation: TNEATPopulation;
      Genotype: TGenotype): double;
    procedure NEATPopulationAfterRunStopped(Sender: TObject);
    procedure NEATPopulationBeforeStartRun(Sender: TObject);
    procedure RadioGroup_SteeringClick(Sender: TObject);
    procedure Action_StartExecute(Sender: TObject);
    procedure Action_StopExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBox_AllowGaussianClick(Sender: TObject);

  private
     
  public
     
    function RunIndividualOnWorlds(Genotype : TGenotype; Draw : boolean) : single;
    function RunIndividual(Genotype : TGenotype; Draw : boolean) : single;
    function Shake(a: single) : single;
  end;

var
  frmMainForm: TfrmMainForm;
  Agent         : TAgent;
  NEATAgent       : TNEATAgent;
  SensorUsed    : integer;

implementation

uses uWorlds;

{$R *.DFM}

procedure TfrmMainForm.FormCreate(Sender: TObject);
begin
  Randomize;
//  Agent         := TAgentScanner2.Create;
//  Agent         := TAgentRandomScanner2.Create;
//  Agent         := TAgentRandomScanner.Create;
//  Agent         := TAgentScanner.Create;
  Agent         := TAgent.Create;
  Agent.ObstacleWorld := ObstacleWorld;

  Randomize;

  ObstacleWorld.Draw(Image1.Canvas);

  NEATAgent := TNEATAgent.Create;
  NEATAgent.Canvas := Image1.Canvas;
  NEATAgent.ObstacleWorld := ObstacleWorld;

  Panel1.DoubleBuffered := true;

  NEATPopulation.TargetFitness := sqr(cBASE_FITNESS);

  Show;
  NEATPopulation.CreateFitnessMonitor;
end;

procedure TfrmMainForm.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  myx, myy  : single;
  angle     : single;
begin
  with ObstacleWorld do
  begin
    Draw(Image1.Canvas);

    myx := X/DrawScale;
    myy := Y/DrawScale;

    angle := 0;

    while angle <  pi*2 do
    begin
      SenseDistance(myx, myy, angle, Image1.Canvas);
      angle := angle + 2*pi/25;
    end;

    CheckObstacleImpact(myx,myy, Image1.Canvas);
  end;
end;

procedure TfrmMainForm.Button1Click(Sender: TObject);
begin
{  Agent.X := 15;
  Agent.Y := 90;
  //Agent.Direction := -pi/2+0.5;
  Agent.Direction := random*2*pi;
  Agent.Canvas := Image1.Canvas;

  while not Application.Terminated do
  begin
    ObstacleWorld.Draw(Image1.Canvas);
    Agent.Draw(Agent.Canvas);
    Agent.PrepareToMove;
    Agent.Move;

    if ObstacleWorld.CheckObstacleImpact(Agent.X, Agent.Y, Agent.Canvas) < SIZE then
    begin
      Agent.X := 15;
      Agent.Y := 90;
      Agent.Direction := -pi/2+0.5;
//      Agent.Direction := random*2*pi;
    end;

    Application.ProcessMessages;
  end;//}
end;

function TfrmMainForm.RunIndividual(Genotype: TGenotype;
  Draw: boolean) : single;
var
  StepsLeft   : single;
  Energy  : single;
  time    : cardinal;
begin
  ObstacleWorld := NEATAgent.ObstacleWorld;

  Genotype.Flush;

  NEATAgent.Genotype := Genotype;

  NEATAgent.X := NEATAgent.ObstacleWorld.StartingX;
  NEATAgent.Y := NEATAgent.ObstacleWorld.StartingY;
  NEATAgent.Direction := NEATAgent.ObstacleWorld.StartingDir;
  StepsLeft := NEATAgent.ObstacleWorld.StepsToFinish;

  Energy := StepsLeft;

  if Draw then
    NEATAgent.Canvas := Image1.Canvas
  else
    NEATAgent.Canvas := nil;

  SensorUsed := 0;

  while Energy>0 do
  begin
    time := timeGetTime;

    if Draw then
      ObstacleWorld.Draw(Image1.Canvas);

    NEATAgent.PrepareSensors(NEATAgent.Canvas);

    if Draw then
    begin
      NEATAgent.Draw(NEATAgent.Canvas);

      if not CheckBox_FastDraw.Checked then
        while (timeGetTime-time) < 1 do
          Application.ProcessMessages;//}
    end;

    NEATAgent.PrepareToMove;
    NEATAgent.Move;

    if ObstacleWorld.CheckObstacleImpact(NEATAgent.X, NEATAgent.Y, nil {NEATAgent.Canvas}) < SIZE then
    begin
      result := cBASE_FITNESS-StepsLeft/NEATAgent.ObstacleWorld.StepsToFinish*cBASE_FITNESS;
      exit;
    end;

    StepsLeft := StepsLeft - 1;
    Energy := Energy - 1;// - SensorUsed/10;

    Application.ProcessMessages;
  end;

  result := cBASE_FITNESS-StepsLeft/NEATAgent.ObstacleWorld.StepsToFinish*cBASE_FITNESS;
end;

function TfrmMainForm.Shake(a: single): single;
begin
  result := 1+random*a-a/2;
end;

function TfrmMainForm.RunIndividualOnWorlds(Genotype : TGenotype;
  Draw: boolean) : single;
var
  i : integer;
  df, f : single;
begin
  try
    if Draw then
      Memo2.Lines.Clear;

    f := 0;
    for i := 0 to Worlds.Count-1 do
    begin
      NEATAgent.ObstacleWorld := TObstacleWorld(Worlds[i]);
      df := RunIndividual(Genotype, Draw);
      f := f + df;

      if Draw then
        Memo2.Lines.Add(Format('%d => %f (%f)',[i, df, f ]));
    end;

    result := f / Worlds.Count;
  except
    result := 0;
  end;
end;

function TfrmMainForm.NEATPopulationShowBestIndividual(
  NEATPopulation: TNEATPopulation; Genotype: TGenotype;
  DifferentFromPrevious: Boolean): double;
begin
  StatusBar1.Panels[0].Text := IntToStr(NEATPopulation.CurrentGeneration);

  if DifferentFromPrevious then
  begin
    Memo1.Lines.Clear;
    Memo1.Lines.Add(Genotype.SaveToString);
    RunIndividualOnWorlds(Genotype, not CheckBox_NoDraw.Checked);
  end;

  result := 0;
end;

function TfrmMainForm.NEATPopulationCalculateFitness(
  NEATPopulation: TNEATPopulation; Genotype: TGenotype): double;
begin
  result := sqr(RunIndividualOnWorlds(Genotype, ContinuousDraw.Checked and not CheckBox_NoDraw.Checked));
end;

procedure TfrmMainForm.NEATPopulationAfterRunStopped(Sender: TObject);
begin
  if RadioGroup_Steering<>nil then
    RadioGroup_Steering.Enabled := true;

  Action_Start.Enabled := true;
  Action_Stop.Enabled := false;
end;

procedure TfrmMainForm.NEATPopulationBeforeStartRun(Sender: TObject);
begin
  RadioGroup_Steering.Enabled := false;

  Action_Start.Enabled := false;
  Action_Stop.Enabled := true;
end;

procedure TfrmMainForm.RadioGroup_SteeringClick(Sender: TObject);
begin
  if RadioGroup_Steering.ItemIndex = 0 then
    NEATPopulation.OutputNodeCount := 1
  else
    NEATPopulation.OutputNodeCount := 2;
end;

procedure TfrmMainForm.Action_StartExecute(Sender: TObject);
begin
  NEATPopulation.StartRun;
end;

procedure TfrmMainForm.Action_StopExecute(Sender: TObject);
begin
  NEATPopulation.StopRun;
end;

procedure TfrmMainForm.FormDestroy(Sender: TObject);
begin
  NEATPopulation.StopRun;
end;

procedure TfrmMainForm.CheckBox_AllowGaussianClick(Sender: TObject);
begin
  if CheckBox_AllowGaussian.Checked then
    RegisterNewTransferFuntion('Main','Gaussian',TTransferFunctionGaussian.Create)
  else
  begin
    TransferFunctionList.Delete(1); // Not very clean
  end;
end;

end.


