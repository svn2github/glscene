program TheBall;

uses
  Forms,
  FMain in 'FMain.pas' {Main},
  UTheBallStructures in 'UTheBallStructures.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'TheBall';
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
