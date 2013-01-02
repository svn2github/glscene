program raycast;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Polynomials in '..\..\..\Source\Base\Polynomials.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
