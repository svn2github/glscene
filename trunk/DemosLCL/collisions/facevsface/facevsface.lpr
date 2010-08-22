program facevsface;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Unit1 in 'Unit1.pas', GLS_WinOnly{Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
