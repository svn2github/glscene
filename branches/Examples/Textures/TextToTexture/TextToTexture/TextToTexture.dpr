///////////////////////////////////////////////////////////////////////////////////
//  By Danjel Grosar 06.03.2002
///////////////////////////////////////////////////////////////////////////////////

program TextToTexture;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.