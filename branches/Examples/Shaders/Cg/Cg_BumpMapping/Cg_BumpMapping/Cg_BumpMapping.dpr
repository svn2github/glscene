program Cg_BumpMapping;

uses
  Forms,
  Unit1 in 'Unit1.pas' {BumpDemo_frm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBumpDemo_frm, BumpDemo_frm);
  Application.Run;
end.
