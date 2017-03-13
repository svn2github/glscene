program NewRagdoll;

uses
  Forms,
  demo_main in 'demo_main.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
