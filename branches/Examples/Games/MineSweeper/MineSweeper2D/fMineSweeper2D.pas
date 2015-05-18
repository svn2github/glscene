unit fMineSweeper2D;

interface

uses
  Windows, Messages, SysUtils, Variants, Actions, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls, ActnList, Menus, StrUtils,

  uMinesClasses, uSimpleRenderer;

type
  TfrmMineSweeper2D = class(TForm)
    MinesPanel: TPanel;
    Timer_GameTimer: TTimer;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    ActionList1: TActionList;
    Action_Exit: TAction;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Action_Restart: TAction;
    Action_BeginnerGame: TAction;
    Action_AdvancedGame: TAction;
    Action_Intermediate: TAction;
    Beginner1: TMenuItem;
    Restart1: TMenuItem;
    Advanced1: TMenuItem;
    N2: TMenuItem;
    Intermediate1: TMenuItem;
    Action_ShowMoves: TAction;
    Showmoves1: TMenuItem;
    N3: TMenuItem;
    PanelStatus: TPanel;
    GroupBox1: TGroupBox;
    Label_GameState: TLabel;
    Label_GameTime: TLabel;
    Label_MinesLeft: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Timer_GameTimerTimer(Sender: TObject);
    procedure Action_ExitExecute(Sender: TObject);
    procedure Action_RestartExecute(Sender: TObject);
    procedure Action_BeginnerGameExecute(Sender: TObject);
    procedure Action_IntermediateExecute(Sender: TObject);
    procedure Action_AdvancedGameExecute(Sender: TObject);
    procedure Label_GameStateClick(Sender: TObject);
    procedure Label_GameStateMouseEnter(Sender: TObject);
    procedure Label_GameStateMouseLeave(Sender: TObject);
    procedure Action_ShowMovesExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure GameStarted(Sender: TObject);
    procedure GameWon(Sender: TObject);
    procedure GameLost(Sender: TObject);
    procedure GameReset(Sender: TObject);

    procedure ResizeToFitPanel;

    procedure RestartGame;
  end;

var
  frmMineSweeper2D: TfrmMineSweeper2D;
  Mines : TMines;
  Renderer : TMinesRenderer;

implementation

uses fGameHistory;

{$R *.dfm}

procedure TfrmMineSweeper2D.FormCreate(Sender: TObject);
begin
  Randomize;
  Show;

  Mines := TMines.Create;
  Mines.OnGameWon := GameWon;
  Mines.OnGameLost := GameLost;
  Mines.OnGameStarted := GameStarted;
  Mines.OnGameReset := GameReset;

  frmGameHistory := TfrmGameHistory.Create(self);
  frmGameHistory.SetMines(Mines);

  Renderer := TMinesRenderer.Create(Mines, MinesPanel);

  Action_BeginnerGame.Execute;
end;

procedure TfrmMineSweeper2D.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Panel : TPanel;
begin
  Panel := TPanel(Sender);
  Panel.BevelOuter := bvNone;
end;

procedure TfrmMineSweeper2D.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Panel : TPanel;
begin
  Panel := TPanel(Sender);
  Panel.BevelOuter := bvRaised;
end;

procedure TfrmMineSweeper2D.ResizeToFitPanel;
begin
{  ClientHeight := MinesPanel.Top+MinesPanel.Height;
  ClientWidth := MinesPanel.Left+MinesPanel.Width;//}

  Height := MinesPanel.Top+MinesPanel.Height+50;
  Width := MinesPanel.Left+MinesPanel.Width+12;//}
end;

procedure TfrmMineSweeper2D.Timer_GameTimerTimer(Sender: TObject);
var
  s : string;
  MinesLeft : integer;
begin
  s := Format('000%0.0f',[Mines.GameTimePassed]);
  Label_GameTime.Caption := RightStr(s,3);

  MinesLeft := Mines.MineCount - Mines.FlagsPlaced;
  if MinesLeft>= 0 then
  begin
    s := Format('000%d',[MinesLeft]);
    Label_MinesLeft.Caption := RightStr(s,3);
  end else
  begin
    s := Format('00%d',[abs(MinesLeft)]);
    Label_MinesLeft.Caption := '-'+RightStr(s,2);
  end;

  if MinesLeft= 0 then
    Label_MinesLeft.Font.Color := clGreen
  else if MinesLeft < 0 then
    Label_MinesLeft.Font.Color := clRed
  else
    Label_MinesLeft.Font.Color := clBlack;
end;

procedure TfrmMineSweeper2D.Action_ExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMineSweeper2D.Action_RestartExecute(Sender: TObject);
begin
  RestartGame;
end;

procedure TfrmMineSweeper2D.RestartGame;
begin
  Mines.BuildRandomMap(Mines.MineCount,Mines.CountX,Mines.CountY);
end;

procedure TfrmMineSweeper2D.Action_BeginnerGameExecute(Sender: TObject);
begin
  Mines.BuildRandomMapSpecified(msBeginner);
end;

procedure TfrmMineSweeper2D.Action_IntermediateExecute(Sender: TObject);
begin
  Mines.BuildRandomMapSpecified(msIntermediate);
end;

procedure TfrmMineSweeper2D.Action_AdvancedGameExecute(Sender: TObject);
begin
  Mines.BuildRandomMapSpecified(msAdvanced);
end;

procedure TfrmMineSweeper2D.Label_GameStateClick(Sender: TObject);
begin
  RestartGame;
end;

procedure TfrmMineSweeper2D.Label_GameStateMouseEnter(Sender: TObject);
begin
  Label_GameState.Font.Style := Label_GameState.Font.Style + [fsUnderLine];
end;

procedure TfrmMineSweeper2D.Label_GameStateMouseLeave(Sender: TObject);
begin
  Label_GameState.Font.Style := Label_GameState.Font.Style - [fsUnderLine];
end;

procedure TfrmMineSweeper2D.GameStarted(Sender: TObject);
begin
  Label_GameState.Caption := 'Running!';
  Label_GameState.Font.Color := clBlack;
end;

procedure TfrmMineSweeper2D.GameLost(Sender: TObject);
begin
  Label_GameState.Caption := 'You lost!';
  Label_GameState.Font.Color := clRed;
end;

procedure TfrmMineSweeper2D.GameReset(Sender: TObject);
begin
  Label_GameState.Caption := 'Waiting';
  Label_GameState.Font.Color := clBlack;

  Renderer.Prepare;
  Renderer.Render(false);
  ResizeToFitPanel;
end;

procedure TfrmMineSweeper2D.GameWon(Sender: TObject);
begin
  Label_GameState.Caption := 'You WON!';
  Label_GameState.Font.Color := clGreen;
end;

procedure TfrmMineSweeper2D.Action_ShowMovesExecute(Sender: TObject);
begin
  frmGameHistory.Show;
end;
end.
