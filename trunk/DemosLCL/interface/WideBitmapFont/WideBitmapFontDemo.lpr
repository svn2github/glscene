program WideBitmapFontDemo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  MainFormUnit in 'MainFormUnit.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
