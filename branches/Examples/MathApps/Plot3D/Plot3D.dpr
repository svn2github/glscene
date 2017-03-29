
program Plot3D;

uses
  Forms,
  fMain in 'Source\fMain.pas' {ViewForm},
  fFunctions in 'Source\fFunctions.pas' {FunctionsForm},
  fGridOptions in 'Source\fGridOptions.pas' {GridOptionsForm},
  fEvaluate in 'Source\fEvaluate.pas' {EvaluateForm},
  fCoordOptions in 'Source\fCoordOptions.pas' {CoordsForm},
  fDerivativeOptions in 'Source\fDerivativeOptions.pas' {DerivativesForm},
  fGridColors in 'Source\fGridColors.pas' {GridColorsForm},
  fPlotColors in 'Source\fPlotColors.pas' {PlotColorsForm},
  fAddPlotColors in 'Source\fAddPlotColors.pas' {AddPlotColorsForm},
  uParser in 'Source\uParser.pas',
  uGlobal in 'Source\uGlobal.pas',
  fAbout in 'Source\fAbout.pas' {AboutForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TViewForm, ViewForm);
  Application.CreateForm(TFunctionsForm, FunctionsForm);
  Application.CreateForm(TGridOptionsForm, GridOptionsForm);
  Application.CreateForm(TEvaluateForm, EvaluateForm);
  Application.CreateForm(TCoordsForm, CoordsForm);
  Application.CreateForm(TDerivativesForm, DerivativesForm);
  Application.CreateForm(TGridColorsForm, GridColorsForm);
  Application.CreateForm(TPlotColorsForm, PlotColorsForm);
  Application.CreateForm(TAddPlotColorsForm, AddPlotColorsForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.Run;
end.
