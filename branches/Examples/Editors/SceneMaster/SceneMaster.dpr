program SceneMaster;

uses
  Forms,
  fInitial in 'Source\fInitial.pas' {InitialForm},
  fDialog in 'Source\fDialog.pas' {FormDialog},
  fMain in 'Source\fMain.pas' {fmSceneMaster},
  uNavCube in 'Source\uNavCube.pas',
  uGlobals in 'Source\uGlobals.pas',
  uSettings in 'Source\uSettings.pas',
  dDialogs in 'Source\dDialogs.pas' {DMDialogs: TDataModule},
  fAbout in 'Source\fAbout.pas' {AboutForm},
  fOptions in 'Source\fOptions.pas' {OptionsForm},
  fContent in 'Source\fContent.pas' {ContentForm},
  fKitpack in 'Source\fKitpack.pas' {fmKitpack},
  dImages in 'Source\dImages.pas' {DMImages: TDataModule},
  GnuGettext in 'Source\GnuGettext.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'GLSViewer';
  Application.CreateForm(TfmSceneMaster, fmSceneMaster);
  Application.CreateForm(TContentForm, ContentForm);
  Application.CreateForm(TfmKitpack, fmKitpack);
  Application.CreateForm(TDMDialogs, DMDialogs);
  Application.CreateForm(TDMImages, DMImages);
  Application.Run;
end.
