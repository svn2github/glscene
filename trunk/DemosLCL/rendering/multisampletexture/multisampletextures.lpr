program MultiSampleTextures;

{$MODE Delphi}

uses
  Forms, Interfaces,
  uMain in 'uMain.pas', glscene_designtime, glscene_runtime;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGLDemoForm, GLDemoForm);
  Application.Run;
end.
