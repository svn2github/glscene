program PostShaderDemo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  Forms, Interfaces, UMainForm in 'UMainForm.pas', glscene_designtime,
  glscene_runtime {PostShaderDemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPostShaderDemoForm, PostShaderDemoForm);
  Application.Run;
end.
