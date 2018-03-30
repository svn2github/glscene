program Texture3D;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Texture_3D_Unit in 'Texture_3D_Unit.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
