program TerrainPack;

uses
  Forms,
  FMainForm in 'FMainForm.pas' {MainForm},
  FNavForm in 'FNavForm.pas' {NavForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TNavForm, NavForm);
  Application.Run;
end.
