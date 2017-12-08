unit fOCR;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, uNeatClasses, StrUtils;

const
  cCharCount = 26;

  cLeft = 4;
  cTop = 4;
  cWidth = 7;
  cSpace = 3;
  cHeight = 14;

  cInputCount = cWidth * cHeight;

  cInitialConnections = 4;

type
  TByteSet = set of byte;
  TCharArray = array[0..cCharCount-1] of TByteSet;
  TfrmOCR = class(TForm)
    Image1: TImage;
    Population: TNEATPopulation;
    Memo_Report: TMemo;
    Memo_Hits: TMemo;
    procedure FormCreate(Sender: TObject);
    function PopulationCalculateFitness(NEATPopulation: TNEATPopulation;
      Genotype: TGenotype): Double;
    function PopulationShowBestIndividual(NEATPopulation: TNEATPopulation;
      Genotype: TGenotype; DifferentFromPrevious: Boolean): Double;
    procedure PopulationPrepareInitialGenotype(
      NEATPopulation: TNEATPopulation; Genotype: TGenotype);
  private
    FCharArray: TCharArray;
     
  public
     
    procedure GenerateImages;
    procedure AddLine(Msg: string);
    function TestGenotype(Genotype : TGenotype; Draw : boolean) : single;

    property CharArray : TCharArray  read FCharArray write FCharArray;
  end;

var
  frmOCR: TfrmOCR;

implementation

uses Types, uTransferFunctionClasses;

{$R *.dfm}

procedure TfrmOCR.AddLine(Msg: string);
begin
  Memo_Report.Lines.Add(Msg);
end;

procedure TfrmOCR.FormCreate(Sender: TObject);
begin
  Randomize;
  GenerateImages;
  Population.TargetFitness := cCharCount;
  Population.CreateFitnessMonitor;
  Population.FitnessMonitor.Show;
  RegisterNewTransferFuntion('Main','Gaussian',TTransferFunctionGaussian.Create);
end;

procedure TfrmOCR.GenerateImages;
var
  c : char;
  x : integer;
  px, py : integer;
  pc : byte;
  Rect : TRect;
  RealTop, RealBottom : integer;
begin
  Image1.Canvas.Font.Name := 'Courier New';
  Image1.Canvas.Font.Size := 8;

  RealTop := cHeight;
  RealBottom := 0;
  x := 0;
  for c := 'a' to 'z' do
  begin
    Assert(x<cCharCount,'Too many characters!');

    Rect := Classes.Rect(cLeft+x*(cWidth+cSpace),cTop,cLeft+x*(cWidth+cSpace)+cWidth, cTop+cHeight);
    Image1.Canvas.Rectangle(Rect.Left-1, Rect.Top-1, Rect.Right+1, Rect.Bottom+1);
    Image1.Canvas.TextRect(Rect, Rect.Left, Rect.Top, c);
    inc(x);

    for px := 0 to cWidth-1 do
      for py := 0 to cHeight-1 do
      begin
        if ((Image1.Canvas.Pixels[Rect.Left+px, Rect.Top+py]) = 0) then
        begin
          if py<RealTop then
            RealTop := py;

          if py>RealBottom then
            RealBottom := py;
        end;
      end;
  end;

  x := 0;
  for c := 'a' to 'z' do
  begin
    Rect := Classes.Rect(cLeft+x*(cWidth+cSpace),cTop,cLeft+x*(cWidth+cSpace)+cWidth, cTop+cHeight);
    {Image1.Canvas.Rectangle(Rect.Left-1, Rect.Top-1, Rect.Right+1, Rect.Bottom+1);
    Image1.Canvas.TextRect(Rect, Rect.Left, Rect.Top, c);//}

    Assert(x<cCharCount,'Too many characters!');
    // Store the char i the char array
    pc := 0;
    for px := 0 to cWidth-1 do
      for py := RealTop to RealBottom do
      begin
        if (Image1.Canvas.Pixels[Rect.Left+px, Rect.Top+py]) = 0 then
          FCharArray[x] := FCharArray[x] + [pc];

        inc(pc);
      end;

    inc(x);
  end;

  Population.InputNodeCount := (RealBottom-RealTop) * cWidth + 1;
  Population.OutputNodeCount := cCharCount;

{  Caption := Format('%d x %d => %d inputs',
    [Image1.Canvas.TextWidth('a'), Image1.Canvas.TextHeight('a'), Image1.Canvas.TextWidth('a') * Image1.Canvas.TextHeight('a')]);//}
  Caption := Format('Top=%d, Bottom=%d, %d x %d => %d inputs',
    [RealTop, RealBottom, (RealBottom-RealTop), cWidth, (RealBottom-RealTop) * cWidth]);//}
end;

function TfrmOCR.PopulationShowBestIndividual(
  NEATPopulation: TNEATPopulation; Genotype: TGenotype;
  DifferentFromPrevious: Boolean): Double;
var
  Hits : single;
begin
  result := 0;
  
  if DifferentFromPrevious then
  begin
    Hits := TestGenotype(Genotype, true);

    AddLine(Format('Gen[%d]: %d hits',[NEATPopulation.CurrentGeneration, trunc(Hits)]));
  end;
end;

function TfrmOCR.PopulationCalculateFitness(
  NEATPopulation: TNEATPopulation; Genotype: TGenotype): Double;
begin
  result := TestGenotype(Genotype, false);
end;

function TfrmOCR.TestGenotype(Genotype: TGenotype; Draw: boolean) : single;
var
  c : char;
  CharID : integer;
  Input : byte;
  Output, StrongestOutput : integer;
  StrongestOutputValue : single;
  Hits : integer;
begin
  if Draw then Memo_Hits.Lines.Clear;
  
  CharID := 0;
  Hits := 0;
  for c := 'a' to 'z' do
  begin
    Genotype.Flush;
    Genotype.InputNodes[Genotype.InputNodes.Count-1].Value := 1;
    Genotype.InputNodes[Genotype.InputNodes.Count-1].BiasNode := true;

    for Input := 0 to Genotype.InputNodes.Count-2 do
      if Input in FCharArray[CharID] then
        Genotype.Inputs[Input] := 1
      else
        Genotype.Inputs[Input] := 0;

    Genotype.Iterate;

    StrongestOutput := 0;
    StrongestOutputValue := Genotype.Outputs[0];

    for Output := 1 to Genotype.OutputNodes.Count - 1 do
    begin
      if Genotype.Outputs[Output] > StrongestOutputValue then
      begin
        StrongestOutput := Output;
        StrongestOutputValue := Genotype.Outputs[Output];
      end;
    end;

    if StrongestOutput = CharID then
      inc(Hits);

    if Draw and (StrongestOutput = CharID) then
      Memo_Hits.Lines.Add(Format('%s => %s: HIT',[c, chr(StrongestOutput+ord('a'))]));

    inc(CharID);
  end;

  result := Hits;
end;

procedure TfrmOCR.PopulationPrepareInitialGenotype(
  NEATPopulation: TNEATPopulation; Genotype: TGenotype);
var
  i : integer;
begin
  if not Population.ConnectNodesOfInitialPop then
  begin
    for i := 0 to cInitialConnections-1 do
    begin
      Genotype.AddRandomConnection;
    end;
  end;
end;

end.
