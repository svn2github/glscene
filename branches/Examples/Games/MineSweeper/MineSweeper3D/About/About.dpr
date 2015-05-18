program About;

uses
  Forms,
  fAboutCambrianLabs in 'fAboutCambrianLabs.pas' {frmAboutCambrianLabs};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmAboutCambrianLabs, frmAboutCambrianLabs);
  Application.Run;
end.
