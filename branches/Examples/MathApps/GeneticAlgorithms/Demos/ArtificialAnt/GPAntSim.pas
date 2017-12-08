unit GPAntSim;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,MMSystem, ExtCtrls, Menus,

  MainParser,uParser,uEvolver,Arithmetic, Misc, Flow, BooleanOp, Compare,
  uAntParticulars, ComCtrls;

type
  TfrmGPAntWindow = class(TForm)
    MainMenu1: TMainMenu;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    Show1: TMenuItem;
    Classcounters1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Set1: TMenuItem;
    Mutatablefunctions1: TMenuItem;
    Mutationfunctions1: TMenuItem;
    Mutationterminals1: TMenuItem;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel1: TPanel;
    lstProgramList: TListBox;
    Panel2: TPanel;
    txtMonitor: TMemo;
    txtMutated: TMemo;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Panel4: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure cmdStartClick(Sender: TObject);
    procedure lstProgramListClick(Sender: TObject);
    procedure Classcounters1Click(Sender: TObject);
    procedure cmdSetPopSizeClick(Sender: TObject);
    procedure txtPopSizeChange(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Mutatablefunctions1Click(Sender: TObject);
    procedure Mutationfunctions1Click(Sender: TObject);
    procedure Mutationterminals1Click(Sender: TObject);
    procedure cmdInitGenomeClick(Sender: TObject);
    procedure CreatemapClick(Sender: TObject);
    procedure cmdLoadMapClick(Sender: TObject);
    procedure cmdUseMapClick(Sender: TObject);
    procedure rgrpAntSteeringClick(Sender: TObject);
    procedure cmdGarbageClick(Sender: TObject);
  private
     
  public
     
    procedure CreatePopulation;
    procedure Compile(sExpression : string);
    procedure RunTest(Parser :TParser);
    procedure RunInit;
    procedure CreateEvolver;
    procedure FillParserArray(Parser:TParser; bForce : boolean);
    procedure DrawMap;
  end;


  procedure AddText(sText : string);

var
  frmGPAntWindow: TfrmGPAntWindow;



implementation

uses ufrmFunctions;

var
 	Parser      : TParser;

  Evolver  : TAntEvolver;//}
{  Evolver     : TEvolver;//}
  G_bChangingtxtEquationToFind : boolean;


{$R *.DFM}

procedure TfrmGPAntWindow.DrawMap;
var
  x,y : integer;
  c,c2,c3   : byte;
  Color     : TColor;
begin
  for x := 0 to MAPSIZE do
    for y := 0 to MAPSIZE do
    begin
      c := AntMap[x,y];
      if (c = VISITED) then
        Color := clBlack
      else if (c = FOOD) then
        Color := clGreen
      else if (c and ATE_HERE<>0) then
        Color := clRed
      else if (c = FOOD or VISITED) then
        Color := clBlue
      else
        Color := clWhite;

      imgAntPath.Canvas.Brush.Color := color;
      imgAntPath.Canvas.Rectangle(x*5,y*5,(x+1)*5+1,(y+1)*5+1);
    end;
end;

//******************************************************************************
procedure AddText(sText : string);
begin
	frmGPAntWindow.txtMonitor.Lines.Add(sText);
end;

//******************************************************************************
procedure TfrmGPAntWindow.Compile(sExpression : string);
begin
	if Parser.Main <> nil then
	  Parser.Main.Destroy;

  Parser.Main:= TStructureNode.Create;

  // Read the actual code!
  Parser.Expression := sExpression;
  Parser.Parse;
end;

//******************************************************************************
procedure TfrmGPAntWindow.RunTest(Parser :TParser);
var
	//MainNode : TStructureNode;
  fResult : TOutputType;
  sExtra	: string;
  bCont   : boolean;
begin
  repeat
    try
      bCont := true;
    	fResult := Parser.Execute;
    except
      on ELoopReached do bCont := true;
      on EParserOutOfCycles do bCont := false;
      on EAntStarve do bCont := false;
    end;
  until not bCont;

  AddText('Result (numeric) :' + FloatToStr(fResult) + sExtra);

  txtMutated.Text := Parser.Code;
end;

procedure TfrmGPAntWindow.CreateEvolver;
begin
  if Evolver <> nil then
  begin
    Evolver.Clear;

    FreeMem(ParserArray);
    ParserArray := nil;

    Evolver.Destroy;
  end;

  Evolver := TAntEvolver.Create(StrToInt(txtPopSize.Text),15);
end;


//******************************************************************************
procedure TfrmGPAntWindow.RunInit;
begin
	Parser := TParser.Create(true);
  Parser.AddLibraryFunction('+',TAdd.Create);
  Parser.AddLibraryFunction('-',TSub.Create);
  Parser.AddLibraryFunction('neg', TNegation.Create);
  Parser.AddLibraryFunction('*',TMult.Create);
  Parser.AddLibraryFunction('/',TDiv.Create);

  Parser.AddLibraryFunction('max',TMin.Create);
  Parser.AddLibraryFunction('min',TMax.Create);

  Parser.AddLibraryFunction('progn',TProgN.Create);
  Parser.AddLibraryFunction('sqr',TSqr.Create);
  Parser.AddLibraryFunction('sqrt',TSqrT.Create);

  Parser.AddLibraryFunction('setq',TSetQ.Create);

  Parser.AddLibraryFunction('if',TIf.Create);
  Parser.AddLibraryFunction('if-with-else',TIfWithElse.Create);
  Parser.AddLibraryFunction('while',TWhile.Create);
  Parser.AddLibraryFunction('unless',TUnless.Create);
  Parser.AddLibraryFunction('when',TWhen.Create);

  Parser.AddLibraryFunction('>',TGreaterThan.Create);
  Parser.AddLibraryFunction('>=',TGreaterThanEqual.Create);

  Parser.AddLibraryFunction('<',TSmallerThan.Create);
  Parser.AddLibraryFunction('<=',TSmallerThanEqual.Create);

  Parser.AddLibraryFunction('=',TEqual.Create);

  Parser.AddLibraryFunction('and',TAnd.Create);
  Parser.AddLibraryFunction('or',TOr.Create);
  Parser.AddLibraryFunction('not',TNot.Create);

  Parser.AddLibraryFunction('moveahead',TMoveAhead.Create);
  Parser.AddLibraryFunction('turnleft',TTurnLeft.Create);
  Parser.AddLibraryFunction('turnright',TTurnRight.Create);
  Parser.AddLibraryFunction('eat-underneath',TEatUnderneath.Create);
  Parser.AddLibraryFunction('food-ahead',TFoodAhead.Create);
  Parser.AddLibraryFunction('loop',TLoop.Create);//}

  // Read the default settings!
  //Parser.Expression := txtDefault.Text;
  Parser.Expression := '(+ 1 2)';
  Parser.Parse;
  Parser.Execute;
end;

procedure TfrmGPAntWindow.CreatePopulation;
var
	x,y,i : integer;
begin
  CreateEvolver;
//  Randomize;

  if Parser <> nil then
    Parser.Destroy;

  RunInit;

  // For the antdemo!

  {Parser.TerminalMutationList.Add(TMoveAhead.Create('moveahead'));
  Parser.TerminalMutationList.Add(TTurnLeft.Create('turnleft'));
  Parser.TerminalMutationList.Add(TTurnRight.Create('turnright'));
  Parser.TerminalMutationList.Add(TEatUnderneath.Create('eat-underneath'));
  Parser.TerminalMutationList.Add(TLoop.Create('loop'));}

  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('progn'));

  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('loop'));
  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('moveahead'));
  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('turnleft'));
  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('turnright'));
  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('eat-underneath'));
  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('food-ahead'));//}

	// Terminals
 { Parser.TerminalMutationList.Add(TConstant.Create(1));
  Parser.TerminalMutationList.Add(TConstant.Create(2));
  Parser.TerminalMutationList.Add(TConstant.Create(3));
  Parser.TerminalMutationList.Add(TConstant.Create(4));
  Parser.TerminalMutationList.Add(TConstant.Create(5));
  Parser.TerminalMutationList.Add(TConstant.Create(6));
  Parser.TerminalMutationList.Add(TConstant.Create(7));
  Parser.TerminalMutationList.Add(TConstant.Create(8));
  Parser.TerminalMutationList.Add(TConstant.Create(9));

  Parser.TerminalMutationList.Add(Parser.Context.GetSymbol('x'));
  Parser.TerminalMutationList.Add(Parser.Context.GetSymbol('y'));
	Parser.Context.GetSymbol('x').SetValue(1,Parser.Context);
  Parser.Context.GetSymbol('y').SetValue(1,Parser.Context);

	// Functions
{  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('+'));
  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('*'));
  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('-'));
  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('neg'));
  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('/'));//}

 { Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('max'));
  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('min')); //}

 { Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('sqr'));
  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('sqrt'));//}

{  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('if'));
  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('<'));
{  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('when'));
  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('unless'));

  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('and'));
  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('not'));
  Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('or'));

  //Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('quad'));
  //Parser.FunctionMutationList.Add(Parser.Context.GetSymbol('add'));}

  LimitCycles(50);
  Compile('(+ 1 1)');

	RunTest(Parser);
end;

procedure TfrmGPAntWindow.FormCreate(Sender: TObject);
begin
  MyAnt := TAnt.Create;
  CreatePopulation;
  InitializeMap;
  DrawMap;
  SetPriorityClass(GetCurrentProcess, IDLE_PRIORITY_CLASS);
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_IDLE);
end;

procedure LogLine(sL : string;iGen : integer; MyTime : integer);
var
	sTimeString : string;
begin
  if MyTime/1000.0 < 60 then
     sTimeString := floattostrf(MyTime/1000.0,ffFixed,5,3)+' s'
   else if MyTime/60000.0 < 60 then
     sTimeString := floattostrf(MyTime/60000.0,ffFixed,5,3)+' m'
   else
     sTimeString := floattostrf(MyTime/3600000.0,ffFixed,5,3)+' h';

  AddText(
     inttostr(iGen) + sL  +
     floattostrf(Evolver.BestIndividual.Quality,ffFixed,10,5) + ' (' +
     IntToStr(Evolver.BestIndividual.Main.CountNodes) + ') ' + sTimeString+
     ' AvgQ = ' + floattostrf(Evolver.AverageQuality,ffFixed,10,5));
   frmGPAntWindow.txtMutated.Text := Evolver.BestIndividual.Code;
end;

var
	bRunning,bHalt : bytebool;

procedure TfrmGPAntWindow.cmdStartClick(Sender: TObject);
var
	i : integer;
  Q : single;
  myTime : integer;
begin
	if bRunning then
  begin
  	bHalt := true;
    exit;
  end;

  bRunning := true;
  i := 0;
  cmdStart.Caption := '&Stop';
  myTime := timeGetTime;

  AddText(Format('Evolution starts with %d individuals.',[Evolver.NumberOfIndividuals]));

	// Fill the array!
  FillParserArray(Parser,false);

  Evolver.DoBeforeEvolve;

  repeat
  	inc(i);

    //Evolver.RunMutGeneration(Evolver.BestIndividual);
    //Evolver.RunTourMutGeneration;
    //Evolver.RunTourXGeneration;
    Evolver.RunTourX_SIX_PARTICIPANTS_Generation;
    //Evolver.RunTourX_SIX_PARTICIPANTS_ALL_CHANGE_Generation;

    if Evolver.BestIndividual.Quality <> Q then
    begin
    	if Evolver.BestIndividual.Quality < Q then
      	LogLine(' diff!   ; ',i,timeGetTime-MyTime)
      else
        LogLine(' quality ; ',i,timeGetTime-MyTime);

      Q := Evolver.BestIndividual.Quality;

      if chkShowbestMap.Checked then
      begin
        Evolver.CalculateIndividualFitness(Evolver.BestIndividual);
        DrawMap;

      end;//}
    end;

		lblGeneration.Caption := IntToStr(i);
    Application.ProcessMessages;
	until bHalt;

	bRunning := false;
  bHalt := false;
  cmdStart.Caption := '&Start';

  LogLine(' stopped at ; ',i,timeGetTime-MyTime);
end;

//******************************************************************************
procedure TfrmGPAntWindow.FillParserArray(Parser:TParser; bForce : boolean);
var
  i : integer;
begin

  if (ParserArray[0] = nil) or bForce then
	begin
    Evolver.PremSoupRepetitions := StrToInt(txtInitCount.Text);

    AddText(Format('Generating premordial soup [max depth %d].',[Evolver.PremSoupRepetitions]));
  	Evolver.InitPopulation(Parser);
		Parser := Parser.CopyParser;

    lstProgramList.Clear;
    for i := 0 to Evolver.NumberOfIndividuals-1 do
      lstProgramList.Items.Add(IntToStr(i));

    AddText('Premordial soup generated');
  end;
end;

//******************************************************************************
procedure TfrmGPAntWindow.lstProgramListClick(Sender: TObject);
begin
  txtMutated.Text := ParserArray[lstProgramList.ItemIndex].Code;
  txtMutated.Lines.Add('');
  txtMutated.Lines.Add('Fitness:'+
    FloatToStr(ParserArray[lstProgramList.ItemIndex].Quality));

  Evolver.CalculateIndividualFitness(ParserArray[lstProgramList.ItemIndex]);
  DrawMap;
end;

procedure TfrmGPAntWindow.Classcounters1Click(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to G_NumberOfAlocatedList.Count - 1 do
    AddText(format('%-16s = %04d copies.',
      [G_NumberOfAlocatedList[i],Integer(G_NumberOfAlocatedList.Objects[i])]));
end;

procedure TfrmGPAntWindow.cmdSetPopSizeClick(Sender: TObject);
begin
  AddText('Population set to '+txtPopSize.Text);
//  CreatePopulation;
  CreateEvolver;
  cmdSetPopSize.Enabled := false;
end;

//******************************************************************************
procedure TfrmGPAntWindow.txtPopSizeChange(Sender: TObject);
begin
  cmdSetPopSize.Enabled := true;
end;

//******************************************************************************
procedure TfrmGPAntWindow.About1Click(Sender: TObject);
begin
  ShowMessage('Gjort av mig. Jajjamensan.' +#10#13+ 'Mattias, alltså.' +#10#13+ 'Jajjamensan!');
end;

//******************************************************************************
procedure TfrmGPAntWindow.Mutatablefunctions1Click(Sender: TObject);
begin
  frmFunctions.SetMutatableFunctions(Parser);
end;

//******************************************************************************
procedure TfrmGPAntWindow.Mutationfunctions1Click(Sender: TObject);
var
  i : integer;
begin
  AddText('Functions that can be used in mutations:');
  with Parser.FunctionMutationList do
    for i := 0 to Count-1 do
    begin
      AddText(IntToStr(i)+ ' : ' + TNode(Items[i]).AsText(''));
    end;
end;

//******************************************************************************
procedure TfrmGPAntWindow.Mutationterminals1Click(Sender: TObject);
var
  i : integer;
begin
  AddText('Terminals that can be used in mutations:');
  with Parser.TerminalMutationList do
    for i := 0 to Count-1 do
    begin
      AddText(IntToStr(i)+ ' : ' + TNode(Items[i]).AsText(''));
    end;
end;

//******************************************************************************
procedure TfrmGPAntWindow.cmdInitGenomeClick(Sender: TObject);
begin
  FillParserArray(Parser,true);
end;

//******************************************************************************
procedure TfrmGPAntWindow.CreatemapClick(Sender: TObject);
begin
  CreateOrgMap(StrToInt(txtSeed.Text),StrToInt(cmdFillPercent.Text));
  InitializeMap;
  DrawMap;
end;

//******************************************************************************
procedure TfrmGPAntWindow.cmdLoadMapClick(Sender: TObject);
begin
  mmoTrail.Lines.LoadFromFile(txtTrailName.Text);
end;

//******************************************************************************
procedure TfrmGPAntWindow.cmdUseMapClick(Sender: TObject);
var
  x,y : integer;
  bFirst  : boolean;
  s   : string;
begin
  for x := 0 to MAPSIZE do
    for y := 0 to MAPSIZE do
    begin
      OrgMap[x,y] := 0;
    end;

  bFirst := true;

  for y := 1 to mmoTrail.Lines.Count - 1 do
  begin
    s := mmoTrail.Lines[y];
    for x := 1 to Length(s) do
      if s[x]='#' then
      begin
        OrgMap[x,y] := FOOD;
        if bFirst then
        begin
          Start_X := x;
          Start_Y := y;
        end;
        bFirst := false;
      end;
  end;

  InitializeMap;
  DrawMap;
end;

procedure TfrmGPAntWindow.rgrpAntSteeringClick(Sender: TObject);
begin
  if rgrpAntSteering.ItemIndex = 0 then
    iTurnRate := 2
  else
    iTurnRate := 1;
end;

//******************************************************************************
procedure TfrmGPAntWindow.cmdGarbageClick(Sender: TObject);
begin
  CreateGarbageMap(StrToInt(txtSeed.Text),StrToInt(cmdFillPercent.Text));
  InitializeMap;
  DrawMap;
end;

end.
