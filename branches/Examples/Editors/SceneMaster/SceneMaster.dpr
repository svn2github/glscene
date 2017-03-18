program SceneMaster;

uses
  Forms,
  fInitial in 'Source\Interface\fInitial.pas' {FormInitial},
  fDialog in 'Source\Interface\fDialog.pas' {FormDialog},
  fMain in 'Source\Interface\fMain.pas' {FormMaster},
  uNavCube in 'Source\Module\uNavCube.pas',
  uGlobals in 'Source\Module\uGlobals.pas',
  uSettings in 'Source\Module\uSettings.pas',
  dDialogs in 'Source\Interface\dDialogs.pas' {DMDialogs: TDataModule},
  fAbout in 'Source\Interface\fAbout.pas' {FormAbout},
  fOptions in 'Source\Interface\fOptions.pas' {GLOptions},
  fContent in 'Source\Interface\fContent.pas' {FormContent},
  fKitbag in 'Source\Interface\fKitbag.pas' {FormKitbag},
  dImages in 'Source\Interface\dImages.pas' {DMImages: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'GLSViewer';
  Application.CreateForm(TFormMaster, FormMaster);
  Application.CreateForm(TDMDialogs, DMDialogs);
  Application.CreateForm(TFormContent, FormContent);
  Application.CreateForm(TFormKitbag, FormKitbag);
  Application.CreateForm(TDMImages, DMImages);
  Application.Run;
end.
