program GLSViewer;

uses
  Forms,
  FMain in 'FMain.pas' {Main};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'GLSViewer';
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
