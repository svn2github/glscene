program GLSViewer;

{: GLScene Viewer based on GLSViewer by Eric Grange
   http://www.sourceforge.net/projects/glscene
}

uses
  Forms,
  DGLSViewer in 'Source\DGLSViewer.pas' {dmGLSViewer: TDataModule},
  FGLForm in 'Source\FGLForm.pas' {GLForm},
  FGLDialog in 'Source\FGLDialog.pas' {GLDialog},
  FGLOptions in 'Source\FGLOptions.pas' {GLOptions},
  FMain in 'Source\FMain.pas' {MainForm},
  FGLAbout in 'Source\FGLAbout.pas' {GLAbout},
  UGlobals in 'Source\UGlobals.pas',
  USettings in 'Source\USettings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'GLSViewer';
  Application.CreateForm(TdmGLSViewer, dmGLSViewer);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
