program GLConsoleDemo;

uses
  Forms,
  uMainForm in 'uMainForm.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
