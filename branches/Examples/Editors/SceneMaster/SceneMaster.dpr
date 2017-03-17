program SceneMaster;

{: GLScene Viewer based on GLSViewer by Eric Grange
   http://www.sourceforge.net/projects/glscene
}

uses
  Forms,
  fSForm in 'Source\fSForm.pas' {SForm},
  fSDialog in 'Source\fSDialog.pas' {SDialog},
  fSMain in 'Source\fSMain.pas' {SMaster},
  uNavCube in 'Source\uNavCube.pas',
  uGlobals in 'Source\uGlobals.pas',
  uSettings in 'Source\uSettings.pas',
  dSMaster in 'Source\dSMaster.pas' {dmSMaster: TDataModule},
  fSAbout in 'Source\fSAbout.pas' {SAbout},
  fSOptions in 'Source\fSOptions.pas' {GLOptions},
  fSContentBrowser in 'Source\fSContentBrowser.pas' {SContentBrowser};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'GLSViewer';
  Application.CreateForm(TSMaster, SMaster);
  Application.CreateForm(TdmSMaster, dmSMaster);
  Application.CreateForm(TSContentBrowser, SContentBrowser);
  Application.Run;
end.
