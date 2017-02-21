program MegaCube2;

uses
  Forms,
  umain in 'umain.pas' {Form1},
  ucpuinst in 'ucpuinst.pas',
  ugpuinst in 'ugpuinst.pas',
  uglfreeform in 'uglfreeform.pas',
  ugpuminst in 'ugpuminst.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
