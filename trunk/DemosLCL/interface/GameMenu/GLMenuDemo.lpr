program GLMenuDemo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitM in 'UnitM.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
