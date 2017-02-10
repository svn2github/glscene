program SymbolicRegression;

uses
  Forms,
  fSymbolicRegression in 'fSymbolicRegression.pas' {frmMainForm},
  uNEATClasses in '..\..\uNEATClasses.pas',
  uNEATStrFunctions in '..\..\uNEATStrFunctions.pas',
  uTransferFunctionClasses in '..\..\uTransferFunctionClasses.pas',
  fFitnessMonitor in '..\..\fFitnessMonitor.pas' {frmFitnessMonitor},
  uNEATMisc in '..\..\uNEATMisc.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.CreateForm(TfrmFitnessMonitor, frmFitnessMonitor);
  Application.Run;
end.
