program MineSweeper2D;

uses
  Forms,
  fMineSweeper2D in 'fMineSweeper2D.pas' {frmMineSweeper2D},
  uSimpleRenderer in 'uSimpleRenderer.pas',
  uMinesClasses in 'uMinesClasses.pas',
  fGameHistory in 'fGameHistory.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMineSweeper2D, frmMineSweeper2D);
  Application.Run;
end.
