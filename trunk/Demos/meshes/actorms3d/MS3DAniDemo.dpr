program MS3DAniDemo;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain};

{$I GLScene.inc}
{$R *.res}

begin
  Application.Initialize;
{$IFDEF GLS_DELPHI_2009_UP}
  Application.MainFormOnTaskbar := True;
{$ENDIF}
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
