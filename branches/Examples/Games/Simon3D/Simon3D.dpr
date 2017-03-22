program Simon;

uses
  Forms,
  ucodigo in 'ucodigo.pas' {Form1};

//{$R *.res}
{$R Simon3D.res}

begin
  Application.Initialize;
  Application.Title := 'Simon 3D';
  Application.HelpFile := '';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
