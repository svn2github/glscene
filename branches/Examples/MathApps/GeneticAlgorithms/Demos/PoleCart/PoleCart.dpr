program PoleCart;



uses
  Forms,
  fPoleCart in 'fPoleCart.pas' {frmPoleBalancer},
  uPoleClasses in 'uPoleClasses.pas',
  uNEATClasses in '..\..\uNEATClasses.pas',
  uNEATStrFunctions in '..\..\uNEATStrFunctions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmPoleBalancer, frmPoleBalancer);
  Application.Run;
end.
