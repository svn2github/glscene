program Shadows;

uses
  Forms,
  Main in 'Main.pas' {MainFm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainFm, MainFm);
  Application.Run;
end.
