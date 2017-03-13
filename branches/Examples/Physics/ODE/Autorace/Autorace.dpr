program Autorace;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Frm},
  Frm2 in 'Frm2.pas' {FrmMenu};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrm, Frm);
  Application.CreateForm(TFrmMenu, FrmMenu);
  Application.Run;
end.
