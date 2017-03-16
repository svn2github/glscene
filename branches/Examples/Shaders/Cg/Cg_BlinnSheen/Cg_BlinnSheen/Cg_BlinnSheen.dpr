{ : Advanced Normal/Bump shading with CgShaders.<p>}
program Cg_BlinnSheen;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
