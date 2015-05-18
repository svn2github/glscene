program MineSweeper3D;

uses
  Forms,
  fMineSweeper3D in 'fMineSweeper3D.pas' {frmMineSweeper3D},
  fAboutCambrianLabs in 'About\fAboutCambrianLabs.pas' {frmAboutCambrianLabs},
  uMinesClasses in 'uMinesClasses.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMineSweeper3D, frmMineSweeper3D);
  Application.CreateForm(TfrmAboutCambrianLabs, frmAboutCambrianLabs);
  Application.Run;
end.
