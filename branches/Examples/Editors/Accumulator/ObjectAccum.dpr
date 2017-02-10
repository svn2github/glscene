program ObjectAccum;

uses
  Forms,
  Unit1 in 'Unit1.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Демо - Аккумулятор Объектов';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
