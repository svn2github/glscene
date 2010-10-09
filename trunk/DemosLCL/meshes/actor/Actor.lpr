program Actor;

{$MODE Delphi}

uses
  Forms, Interfaces,
  demo;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
