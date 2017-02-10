unit uNEATAgent;

interface

uses
  uAgent, uNEATClasses, Graphics;

type
  TNEATAgent = class(TAgent)
    Genotype : TGenotype;

    FrontSensor,
    Left1Sensor, Left2Sensor,
    Right1Sensor, Right2Sensor  : single;

    procedure PrepareSensors(Canvas : TCanvas);
    procedure PrepareToMove;override;
  end;

implementation

{ TNEATAgent }

procedure TNEATAgent.PrepareSensors(Canvas : TCanvas);
var
  w1,w2 : single;
begin
  w1 := 20/180*pi;
  w2 := 65/180*pi;

  FrontSensor := SenseDistance(0,Canvas);
  Left1Sensor := SenseDistance(-w1,Canvas);
  Left2Sensor := SenseDistance(-w2,Canvas);

  Right1Sensor := SenseDistance(w1,Canvas);
  Right2Sensor := SenseDistance(w2,Canvas);

  Genotype.Inputs[0] := 1;
  Genotype.InputNodes[0].BiasNode := true;

  Genotype.Inputs[1] := FrontSensor;
  Genotype.Inputs[2] := Left1Sensor;
  Genotype.Inputs[3] := Left2Sensor;
  Genotype.Inputs[4] := Right1Sensor;
  Genotype.Inputs[5] := Right2Sensor;
end;

procedure TNEATAgent.PrepareToMove;
const
  cTRHESHOLD = 0.00;
  
begin
  if not Genotype.NEATPopulation.AllowRecurrentLinks then
    Genotype.Flush;

  // Run the network
  Genotype.Iterate;

  // One output node method
  // Turn(Genotype.Outputs[0]);

  if Genotype.OutputNodes.Count=1 then
  begin
    if FloatToBool(Genotype.Outputs[0]) then
      Turn(1)
    else
      Turn(-1);
  end else
  begin
    // Turn the agent
    if abs(Genotype.Outputs[0]-Genotype.Outputs[1])<=cTRHESHOLD then
      Turn(0)
    else if (Genotype.Outputs[0]<Genotype.Outputs[1]) then
      Turn(1)
    else
      Turn(-1);
  end;
end;

end.
