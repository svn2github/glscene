program fractl3d;

uses
  Forms,
  fmain in 'fmain.PAS' {MainForm},
  fUGlobal in 'fUGlobal.pas',
  fabout in 'fabout.pas' {AboutBox},
  FMath in 'FMath.pas' {FractalForm},
  fGMath in 'fGMath.pas' {MathForm},
  fGStyle in 'fGStyle.pas' {DIYStyleForm},
  fUMathA in 'fUMathA.pas',
  fuMathC in 'fuMathC.pas',
  fuMathF in 'fuMathF.pas',
  fumathif in 'fumathif.pas',
  fumathp in 'fumathp.pas',
  futurtlep in 'futurtlep.pas',
  fMJPEG in 'fMJPEG.pas' {JPEGForm},
  fMResize in 'fMResize.pas' {ResizeForm},
  fanno in 'fanno.pas' {AnnotationForm},
  fGeometry in 'fGeometry.pas',
  fSysInfo in 'fSysInfo.pas' {SystemInfoForm},
  fXYZ3D in 'fXYZ3D.pas' {XYZ3DForm},
  fjul in 'fjul.pas' {FMJForm},
  FastDIB in 'FastDIB.pas',
  fGlfrm in 'fGlfrm.pas' {dtmGlForm},
  aclass5 in 'aclass5.pas',
  fPhoenix in 'fPhoenix.pas',
  Fractal in 'Fractal.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Fractals 3D';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFractalForm, FractalForm);
  Application.CreateForm(TMathForm, MathForm);
  Application.CreateForm(TAnnotationForm, AnnotationForm);
  Application.CreateForm(TDIYStyleForm, DIYStyleForm);
  Application.CreateForm(TJPEGForm, JPEGForm);
  Application.CreateForm(TResizeForm, ResizeForm);
  Application.CreateForm(TSystemInfoForm, SystemInfoForm);
  Application.CreateForm(TXYZ3DForm, XYZ3DForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TFMJForm, FMJForm);
  Application.CreateForm(TdtmGlForm, dtmGlForm);
  Application.Run;
end.
