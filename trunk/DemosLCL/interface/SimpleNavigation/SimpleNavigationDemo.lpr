program SimpleNavigationDemo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Unit1 in 'Unit1.pas', glscene_runtime {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
