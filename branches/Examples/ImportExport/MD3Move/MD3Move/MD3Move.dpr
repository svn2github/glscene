program MD3Move;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  MD3Helper in 'MD3Helper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
