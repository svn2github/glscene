// Initial version of Phill Scadden
// Extended by Pavel Vassiliev

program HeightFieldGrid2D;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  grdfuncs in 'grdfuncs.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
