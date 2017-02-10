program ObstacleNavigator;

uses
  Forms,
  fMainForm in 'fMainForm.pas' {frmMainForm},
  uObstacleWorld in 'uObstacleWorld.pas',
  uAgent in 'uAgent.pas',
  uNEATAgent in 'uNEATAgent.pas',
  uWorlds in 'uWorlds.pas',
  uNEATClasses in '..\..\uNEATClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
