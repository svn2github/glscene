program LogoEdit;

uses
  Forms,
  pngzlib in 'png1_535\pngzlib.pas',
  pnglang in 'png1_535\pnglang.pas',
  pngextra in 'png1_535\pngextra.pas',
  uMain in 'uMain.pas' {Form1},
  uNew in 'uNew.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
