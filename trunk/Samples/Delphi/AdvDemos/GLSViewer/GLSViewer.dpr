program GLSViewer;

uses
  Forms,
  DGLSViewer in 'Source\DGLSViewer.pas' {dmGLSViewer: TDataModule},
  FGLForm in 'Source\FGLForm.pas' {GLForm},
  FGLDialog in 'Source\FGLDialog.pas' {GLDialog},
  FGLOptions in 'Source\FGLOptions.pas' {fmOptions},
  FMain in 'Source\FMain.pas' {Main},
  FGLAbout in 'Source\FGLAbout.pas' {GLAbout},
  GNUgettext in 'Source\GNUgettext.pas',
  UGlobals in 'Source\UGlobals.pas',
  USettings in 'Source\USettings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'GLSViewer';
  Application.CreateForm(TdmGLSViewer, dmGLSViewer);
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
