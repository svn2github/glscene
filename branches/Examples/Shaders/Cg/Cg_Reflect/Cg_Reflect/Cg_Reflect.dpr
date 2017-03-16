{ Simple Cg Shader Cubemap Demo (incomplete)}
program Cg_Reflect;
{%File 'reflect_vp.cg'}
{%File 'reflect_fp.cg'}

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
