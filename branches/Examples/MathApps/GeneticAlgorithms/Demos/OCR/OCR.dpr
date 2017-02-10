program OCR;

uses
  Forms,
  fOCR in 'fOCR.pas' {frmOCR},
  uNeatClasses in '..\..\uNeatClasses.pas',
  uTransferFunctionClasses in '..\..\uTransferFunctionClasses.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmOCR, frmOCR);
  Application.Run;
end.
