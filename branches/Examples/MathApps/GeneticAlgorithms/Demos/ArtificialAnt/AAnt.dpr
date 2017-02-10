
program AAnt;

uses
  Forms,
  fAAnt in 'fAAnt.pas' {frmAAnt},
  uNEATToXML in '..\..\uNEATToXML.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmAAnt, frmAAnt);
  Application.Run;
end.
