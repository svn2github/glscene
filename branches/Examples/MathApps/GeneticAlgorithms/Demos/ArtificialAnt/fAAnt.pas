unit fAAnt;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ImgList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.Menus, Vcl.ActnList, Vcl.StdActns,
  Vcl.ActnMan, Vcl.CheckLst, Vcl.XPStyleActnCtrls, Vcl.Grids, Vcl.ValEdit,

  uNeatClasses, uTransferFunctionClasses, sdXmlDocuments;

const
  cMapSize      = 31;

  cEmptyGround  = 0;
  cVisited      = 1;
  cFood         = 2;
  cAteHere      = 4;
  cBoxSize      = 409 div cMapSize;

type
  TfrmAAnt = class(TForm)
    Panel2: TPanel;
    imgAntPath: TImage;
    Memo1: TMemo;
    ActionManager1: TActionManager;
    FileExit1: TFileExit;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    View1: TMenuItem;
    Action_ViewFitnessProgress: TAction;
    Viewfitnessprogress1: TMenuItem;
    Population: TNEATPopulation;
    Button1: TButton;
    Button2: TButton;
    Action_Start: TAction;
    Action_Stop: TAction;
    ValueListEditor: TValueListEditor;
    Button_SaveBest: TButton;
    Button_Load: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Action_ViewFitnessProgressExecute(Sender: TObject);
    function PopulationCalculateFitness(
      NEATPopulation: TNEATPopulation; Genotype: TGenotype): Double;
    function PopulationShowBestIndividual(
      NEATPopulation: TNEATPopulation; Genotype: TGenotype;
      DifferentFromPrevious: Boolean): Double;
    procedure Action_StartExecute(Sender: TObject);
    procedure Action_StopExecute(Sender: TObject);
    procedure PopulationAfterRunStopped(Sender: TObject);
    procedure PopulationBeforeStartRun(Sender: TObject);
    procedure Button_SaveBestClick(Sender: TObject);
    procedure Button_LoadClick(Sender: TObject);
  private
    function TestGenotype(Genotype: TGenotype; bDraw: boolean) : single;
    procedure SaveXML(AGenotype: TGenotype);
{    function CalculateFitness: TIndividual;
    procedure InitFunctionAndTerminalSet;//}
    { Private declarations }
  public
    { Public declarations }
    procedure DrawMap;
    procedure ReadMapFromText;
    procedure InitializeMap;
  end;

  TAnt = class
  	X :	integer;
    Y : integer;
    Direction : integer;
    FoodConsumed  : integer;
    TimeCount     : integer;
    Alive : boolean;

    function SenseFoodAhead : boolean;
    procedure NewPosition(x,y : integer;var newX,newY : integer);
  	procedure MoveAhead;
    procedure TurnLeft;
    procedure TurnRight;
    procedure EatUnderneath;
    procedure Reset;
    constructor Create;
  end;

var
  frmAAnt: TfrmAAnt;
  MyAnt : TAnt;
  AntMap  : array[0..cMapSize,0..cMapSize] of byte;
  OrgMap  : array[0..cMapSize,0..cMapSize] of byte;
  bOrgMapMade : boolean;
  iTurnRate : integer;
  START_X : integer;
  START_Y : integer;
  TotalFoodOnMap : integer;
  CyclesPerAnt   : integer;

implementation

uses uNEATToXML;

{$R *.DFM}

//******************************************************************************
function TfrmAAnt.TestGenotype(Genotype: TGenotype; bDraw : boolean) : single;
var
  x : integer;
  i, MaxNode : integer;
  MaxValue : double;
begin
  MyAnt.Reset;
  frmAAnt.InitializeMap;

  Genotype.Flush;

  repeat
    Genotype.Inputs[0] := 1;
    Genotype.InputNodes[0].BiasNode := true;

    if MyAnt.SenseFoodAhead then
      Genotype.Inputs[1] := 1
    else
      Genotype.Inputs[1] := 0;

    Genotype.Iterate;

    // Figure out which output node fired strongest
    MaxValue := Genotype.Outputs[0];
    MaxNode := 0;

    for i := 1 to Genotype.OutputNodes.Count-1 do
    begin
      if Genotype.Outputs[i]>MaxValue then
      begin
        MaxValue := Genotype.Outputs[i];
        MaxNode := i;
      end;
    end;

    case MaxNode of
      0 : MyAnt.MoveAhead;
      1 : MyAnt.TurnLeft;
      2 : MyAnt.TurnRight;
      3 : MyAnt.EatUnderneath;
      4 : inc(MyAnt.TimeCount); // NOP - this is usually disabled
    end;

  until (MyAnt.TimeCount > CyclesPerAnt) or (not MyAnt.Alive);  // The ant is allowed 500 time units!

  // Normalized fitness
  //result := TotalFoodOnMap-MyAnt.FoodConsumed;
  result := MyAnt.FoodConsumed;

  if bDraw then frmAAnt.DrawMap;
end;

//******************************************************************************
procedure TfrmAAnt.DrawMap;
var
  x,y : integer;
  c,c2,c3   : byte;
  Color     : TColor;
begin
  for x := 0 to cMapSize do
    for y := 0 to cMapSize do
    begin
      c := AntMap[x,y];
      if (c = cVisited) then
        Color := clBlack
      else if (c = cFood) then
        Color := clGreen
      else if (c and cAteHere<>0) then
        Color := clRed
      else if (c = cFood or cVisited) then
        Color := clBlue
      else
        Color := clWhite;

      imgAntPath.Canvas.Brush.Color := color;
      imgAntPath.Canvas.Rectangle(x*cBoxSize,y*cBoxSize,(x+1)*cBoxSize+1,(y+1)*cBoxSize+1);
    end;
end;

//******************************************************************************
var
  bMapRead : boolean;
procedure TfrmAAnt.ReadMapFromText;
var
  x,y : integer;
  bFirst  : boolean;
  s   : string;
  StringList : TStringList;
begin
  if bMapRead = true then exit;

  bMapRead := true;

  for x := 0 to cMapSize do
    for y := 0 to cMapSize do
    begin
      OrgMap[x,y] := 0;
    end;

  StringList := TStringList.Create;
  StringList.LoadFromFile('santafe.txt');


  bFirst := true;

  TotalFoodOnMap := 0;
  for y := 1 to StringList.Count - 1 do
  begin
    s := StringList[y];
    for x := 1 to Length(s) do
      if s[x]='#' then
      begin
        OrgMap[x,y] := cFood;
        if bFirst then
        begin
          Start_X := x;
          Start_Y := y;
        end;
        bFirst := false;
        inc(TotalFoodOnMap);
      end;
  end;

  StringList.Free;
  InitializeMap;
  DrawMap;
end;

//******************************************************************************
procedure TfrmAAnt.InitializeMap;
begin
  Move(OrgMap,AntMap,(cMapSize+1)*(cMapSize+1));
end;

//******************************************************************************
procedure TAnt.EatUnderneath;
begin
  if not Alive then exit;

  if (AntMap[x,y] and cFood) <> 0 then
  begin
    inc(FoodConsumed);
    AntMap[x,y] := cVisited;
    AntMap[x,y] := AntMap[x,y] or cAteHere;
  end;

  inc(TimeCount);
end;

//******************************************************************************
function TAnt.SenseFoodAhead : boolean;
var
  AheadX,AheadY : integer;
begin
  if not Alive then exit;

  NewPosition(x,y,AheadX,AheadY);
  if ((AntMap[AheadX,AheadY] and cFood) <> 0) then
    SenseFoodAhead := true
  else
    SenseFoodAhead := false;
end;

//******************************************************************************
procedure TAnt.NewPosition(x,y : integer;var newX,newY : integer);
begin
  if not Alive then exit;

 case Direction of
    1 : begin inc(y); end;
    2 : begin inc(x);inc(Y); end;
    3 : begin inc(X); end;
    4 : begin dec(Y);inc(x); end;
    5 : begin dec(y); end;
    6 : begin dec(x);dec(y); end;
    7 : begin dec(X); end;
    8 : begin dec(X);inc(y) end;
  end;

  // Toroidal!
  if X > cMapSize then X := 0;
  if X < 0 then X := cMapSize;

  if Y > cMapSize then Y := 0;
  if Y < 0 then Y := cMapSize;//}
  {if (X > cMapSize) or (X < 0) or
     (Y > cMapSize) or (Y < 0) then
  begin
    Alive := false;
    newX := 5;
    newY := 5;
    exit;
  end;//}

{  if X > cMapSize then X := cMapSize;
  if X < 0 then X := 0;

  if Y > cMapSize then Y := cMapSize;
  if Y < 0 then Y := 0;//}

  newX := x;
  newY := y;
end;

//******************************************************************************
procedure TAnt.MoveAhead;
begin
  if not Alive then exit;

  NewPosition(x,y,x,y);

  // Register that the ant has been on the spot
  AntMap[x,y] := AntMap[x,y] or cVisited;

  // Try to eat if there's any food!
  EatUnderneath;

  inc(TimeCount);
end;

//******************************************************************************
procedure TAnt.TurnLeft;
begin
  if not Alive then exit;

	Direction := Direction + iTurnRate;
  if ((iTurnRate = 1) and (Direction > 8)) or
     ((iTurnRate = 2) and (Direction > 7)) then Direction := 1;
  inc(TimeCount);
end;

//******************************************************************************
procedure TAnt.TurnRight;
begin
  if not Alive then exit;

	Direction := Direction - iTurnRate;
  if Direction < 1 then
  begin
    if iTurnRate = 1 then
      Direction := 8
    else
      Direction := 7;
  end;
  inc(TimeCount);
end;

//******************************************************************************
procedure TAnt.Reset;
begin
  TimeCount := 0;
	Direction := 3;
  Alive := true;
  X := 1;
  Y := 1;
  iTurnRate := 2;
  FoodConsumed := 0;
end;

//******************************************************************************
constructor TAnt.Create;
begin
  Reset;
end;

//******************************************************************************
procedure TfrmAAnt.FormCreate(Sender: TObject);
begin
  CyclesPerAnt := 600;

  MyAnt := TAnt.Create;
  ReadMapFromText;

  Randomize;
  Population.TargetFitness := TotalFoodOnMap;
  Population.CreateFitnessMonitor;

  Population.InputNodeCount := Population.InputNodeCount;
  Population.OutputNodeCount := Population.OutputNodeCount;

  RegisterNewTransferFuntion('Main','Gaussian',TTransferFunctionGaussian.Create);
  //RegisterNewTransferFuntion('Main','1DSigmoid',TTransferFunction1DelaySigmoid.Create);
  //RegisterNewTransferFuntion('Main','1DGaussian',TTransferFunction1DelayGaussian.Create);

  //Population.StartRun;
end;

procedure TfrmAAnt.Action_ViewFitnessProgressExecute(Sender: TObject);
begin
  Population.FitnessMonitor.Show;
end;

function TfrmAAnt.PopulationCalculateFitness(
  NEATPopulation: TNEATPopulation; Genotype: TGenotype): Double;
begin
  result := TestGenotype(Genotype,False);
end;

function TfrmAAnt.PopulationShowBestIndividual(
  NEATPopulation: TNEATPopulation; Genotype: TGenotype;
  DifferentFromPrevious: Boolean): Double;
begin
  if DifferentFromPrevious then
  begin
    TestGenotype(Genotype, DifferentFromPrevious);
    Caption := Format('The Artificial Ant %d, (Ate %d)',[NEATPopulation.CurrentGeneration, MyAnt.FoodConsumed]) ;
    //Memo1.Lines.Text := Genotype.AsCode;
  end;

  if Genotype.Fitness = NEATPopulation.TargetFitness then
    Memo1.Lines.Text := Genotype.SaveToString;
end;

procedure TfrmAAnt.Action_StartExecute(Sender: TObject);
begin
  Population.StartRun;
end;

procedure TfrmAAnt.Action_StopExecute(Sender: TObject);
begin
  Population.StopRun;
end;

procedure TfrmAAnt.PopulationAfterRunStopped(Sender: TObject);
begin
  Action_Start.Enabled := true;
  Action_Stop.Enabled := false;
  ValueListEditor.Enabled := true;
end;

procedure TfrmAAnt.PopulationBeforeStartRun(Sender: TObject);
begin
  Population.ClearPopulation;

  Population.ActivationIterations := StrToInt(ValueListEditor.Values['ActivationIterations']);
  Population.PopulationSize := StrToInt(ValueListEditor.Values['PopulationSize']);
  FormatSettings.DecimalSeparator := '.';
  Population.MinSpeciesRelatedness := StrToFloat(ValueListEditor.Values['MinSpeciesRelatedness']);
  FormatSettings.DecimalSeparator := ',';

  Population.CreatePopulation;

  Action_Start.Enabled := false;
  Action_Stop.Enabled := true;
  ValueListEditor.Enabled := false;
end;

procedure TfrmAAnt.Button_SaveBestClick(Sender: TObject);
begin
  SaveXML(Population.BestGenotype);
end;

procedure TfrmAAnt.SaveXML(AGenotype: TGenotype);
var
  Doc : TsdXmlDocument;
begin
  Doc := TsdXmlDocument.Create;
  try
    Doc.CreateName('Genotypes');

    SaveGenotypeToXMLNode(AGenotype, Doc.Root.NodeNew('Genotype'));

    Doc.XmlFormat := xfReadable;
    Doc.SaveToFile('Genotypes.xml');
  finally
    FreeAndNil(Doc);
  end;
end;

procedure TfrmAAnt.Button_LoadClick(Sender: TObject);
var
  Doc : TsdXmlDocument;
  Genotype : TGenotype;
begin
  Genotype := nil;
  Doc := TsdXmlDocument.Create;
  try
    Doc.LoadFromFile('Genotypes.xml');

    Genotype := TGenotype.Create(Population);

    LoadGenotypeFromXMLNode(Genotype, Doc.Root.FindNode('Genotype'));
    Population.FitnessMonitor.PublishGenotype(Genotype);

    TestGenotype(Genotype,True);
  finally
    FreeAndNil(Genotype);
    FreeAndNil(Doc);
  end;
end;

end.

