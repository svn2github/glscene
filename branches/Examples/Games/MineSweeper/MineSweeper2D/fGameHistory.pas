unit fGameHistory;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uMinesClasses;

type
  TfrmGameHistory = class(TForm)
    ListBox_Actions: TListBox;
    Label1: TLabel;
  private
    { Private declarations }
    FMines : TMines;
  public
    { Public declarations }

    procedure AfterUpdate(Sender: TObject);

    procedure SetMines(Mines : TMines);
  end;

var
  frmGameHistory: TfrmGameHistory;

implementation

{$R *.dfm}

{ TfrmGameActions }

procedure TfrmGameHistory.AfterUpdate(Sender: TObject);
begin
  Assert(Assigned(FMines),'Mines not set!');

  ListBox_Actions.Items.Assign(FMines.MoveHistory);
  FMines.MoveHistory.Assign(ListBox_Actions.Items);
end;

procedure TfrmGameHistory.SetMines(Mines: TMines);
begin
  FMines := Mines;
  Mines.OnAfterUpdate := AfterUpdate;
end;

end.
