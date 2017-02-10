program Mathdemo;


uses
  Forms,
  WorldDrawing in 'WorldDrawing.pas',
  Mdemo1 in 'Mdemo1.pas' {DemoForm},
  OverlayImage in 'OverlayImage.pas',
  MathImage in 'MathImage.pas',
  Contour in 'Contour.pas' {ContourForm};

begin
  Application.Initialize;
  Application.CreateForm(TDemoForm, DemoForm);
  Application.CreateForm(TContourForm, ContourForm);
  Application.Run;
end.

