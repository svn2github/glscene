program DemoProject;

uses
  Forms,
  uMain in 'uMain.pas' {MainForm},
  uLauncher in 'uLauncher.pas' {LauncherFrm},
  GLSLWater in 'GLSLWater.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TLauncherFrm, LauncherFrm);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
