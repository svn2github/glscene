program Submarine;

uses
  Forms,
  Unit1 in 'Unit1.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Sub';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
