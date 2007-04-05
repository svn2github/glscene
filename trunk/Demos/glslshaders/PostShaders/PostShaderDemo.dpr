program PostShaderDemo;

uses
  Forms,
  UMainForm in 'UMainForm.pas' {PostShaderDemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPostShaderDemoForm, PostShaderDemoForm);
  Application.Run;
end.
