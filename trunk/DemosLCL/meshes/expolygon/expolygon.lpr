program ExPolygon;

{$MODE Delphi}

uses
  Forms, Interfaces,
  ExPolygon1 in 'ExPolygon1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
