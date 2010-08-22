program TTB;

uses
  Forms,
  FTTBMain in 'FTTBMain.pas' {TTBMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTTBMain, TTBMain);
  Application.Run;
end.
