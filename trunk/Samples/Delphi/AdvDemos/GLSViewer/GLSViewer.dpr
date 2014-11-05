program GLSViewer;

uses
  Forms,
  FMain in 'FMain.pas' {Main},
  GNUgettext in 'GNUgettext.pas',
  DGLSViewer in 'DGLSViewer.pas' {dmGLSViewer: TDataModule},
  FGLForm in 'FGLForm.pas' {GLForm},
  FGLDialog in 'FGLDialog.pas' {GLDialog},
  FGLAbout in 'FGLAbout.pas' {GLAbout},
  FGLOptions in 'FGLOptions.pas' {fmOptions},
  GLFileDXF in 'GLFileDXF.pas',
  GLFileGRD in 'GLFileGRD.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'GLSViewer';
  Application.CreateForm(TdmGLSViewer, dmGLSViewer);
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
