program PostShaderDemo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  Forms, Interfaces, umainform in 'umainform.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPostShaderDemoForm, PostShaderDemoForm);
  Application.Run;
end.
