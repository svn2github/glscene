program GlsBoid;

uses
  Forms,
  GlsFrenzy in 'GlsFrenzy.pas' {FrenzyForm},
  GlsBirdFrm in 'GlsBirdFrm.pas' {AAABirdForm},
  GLSDemo in 'GLSDemo.pas' {AAADemoForm},
  GlsAbout in 'GlsAbout.pas' {AboutBoids},
  aclass5 in 'aclass5.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Gls Boid';
  Application.CreateForm(TAAABirdForm, AAABirdForm);
  Application.CreateForm(TFrenzyForm, FrenzyForm);
  Application.CreateForm(TAAADemoForm, AAADemoForm);
  Application.CreateForm(TAboutBoids, AboutBoids);
  Application.Run;
end.
