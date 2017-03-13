program Buggy;

uses
  Forms,
  fBuggy in 'fBuggy.pas' {frmBuggy};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmBuggy, frmBuggy);
  Application.Run;
end.
