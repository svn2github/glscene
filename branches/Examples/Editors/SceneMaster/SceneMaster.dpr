program SceneMaster;

uses
  Forms,
  fInitial in 'Source\fInitial.pas' {InitialForm},
  fDialog in 'Source\fDialog.pas' {FormDialog},
  fMain in 'Source\fMain.pas' {FormMaster},
  uNavCube in 'Source\uNavCube.pas',
  uGlobals in 'Source\uGlobals.pas',
  uSettings in 'Source\uSettings.pas',
  dDialogs in 'Source\dDialogs.pas' {DMDialogs: TDataModule},
  fAbout in 'Source\fAbout.pas' {AboutForm},
  fOptions in 'Source\fOptions.pas' {OptionsForm},
  fContent in 'Source\fContent.pas' {ContentForm},
  fKitpack in 'Source\fKitpack.pas' {KitpackForm},
  dImages in 'Source\dImages.pas' {DMImages: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'GLSViewer';
  Application.CreateForm(TFormMaster, FormMaster);
  Application.CreateForm(TDMDialogs, DMDialogs);
  Application.CreateForm(TContentForm, ContentForm);
  Application.CreateForm(TKitpackForm, KitpackForm);
  Application.CreateForm(TDMImages, DMImages);
  Application.Run;
end.
