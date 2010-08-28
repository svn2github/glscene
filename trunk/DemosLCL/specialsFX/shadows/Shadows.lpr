program Shadows;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Main in 'Main.pas' {MainFm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainFm, MainFm);
  Application.Run;
end.
