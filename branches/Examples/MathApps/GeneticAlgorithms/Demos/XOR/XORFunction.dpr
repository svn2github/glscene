program XORFunction;

uses
  Forms,
  fXOR in 'fXOR.pas' {frmXOR},
  uNEATStrFunctions in '..\..\uNEATStrFunctions.pas',
  uTransferFunctionClasses in '..\..\uTransferFunctionClasses.pas',
  uNEATClasses in '..\..\uNEATClasses.pas',
  fFitnessMonitor in '..\..\fFitnessMonitor.pas' {frmFitnessMonitor},
  uDrawGenotype in '..\..\uDrawGenotype.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmXOR, frmXOR);
  Application.Run;
end.
