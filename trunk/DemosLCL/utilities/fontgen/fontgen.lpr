program fontgen;

{$MODE Delphi}

uses
  Forms, Interfaces,
  main in 'main.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
