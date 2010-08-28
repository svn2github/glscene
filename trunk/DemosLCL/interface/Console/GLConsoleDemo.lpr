program GLConsoleDemo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  uMainForm in 'uMainForm.pas', glscene_runtime;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
