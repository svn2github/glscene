program Plot_2D_v3;

uses
  Forms,
  fMain in 'Source\fMain.pas' {MainForm},
  fFuncts in 'Source\fFuncts.pas' {FunctionsForm},
  fGridOpts in 'Source\fGridOpts.pas' {GridOptionsForm},
  fNumeric in 'Source\fNumeric.pas' {NumericForm},
  fTextBlocks in 'Source\fTextBlocks.pas' {TextBlocksForm},
  fDerivative in 'Source\fDerivative.pas' {DerivativeForm},
  fIntegrateX in 'Source\fIntegrateX.pas' {IntegrateXForm},
  fIntegrateY in 'Source\fIntegrateY.pas' {IntegrateYForm},
  fBetween in 'Source\fBetween.pas' {BetweenForm},
  fVolumeX in 'Source\fVolumeX.pas' {VolumeXForm},
  fVolumeY in 'Source\fVolumeY.pas' {VolumeYForm},
  fBitmap in 'Source\fBitmap.pas' {BitmapForm},
  fPrint in 'Source\fPrint.pas' {PrintForm},
  fStyle in 'Source\fStyle.pas' {StyleNameForm},
  fxValue in 'Source\fxValue.pas' {fxValueForm},
  fx1Value in 'Source\fx1Value.pas' {fx1ValueForm},
  fx2Value in 'Source\fx2Value.pas' {fx2ValueForm},
  uCanvas in 'Source\uCanvas.pas',
  uGlobal in 'Source\uGlobal.pas',
  uParser in 'Source\uParser.pas',
  fAbout in 'Source\fAbout.pas' {AboutForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Plot 2D';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFunctionsForm, FunctionsForm);
  Application.CreateForm(TGridOptionsForm, GridOptionsForm);
  Application.CreateForm(TNumericForm, NumericForm);
  Application.CreateForm(TTextBlocksForm, TextBlocksForm);
  Application.CreateForm(TDerivativeForm, DerivativeForm);
  Application.CreateForm(TIntegrateXForm, IntegrateXForm);
  Application.CreateForm(TIntegrateYForm, IntegrateYForm);
  Application.CreateForm(TBetweenForm, BetweenForm);
  Application.CreateForm(TVolumeXForm, VolumeXForm);
  Application.CreateForm(TVolumeYForm, VolumeYForm);
  Application.CreateForm(TfxValueForm, fxValueForm);
  Application.CreateForm(Tfx1ValueForm, fx1ValueForm);
  Application.CreateForm(Tfx2ValueForm, fx2ValueForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.Run;
end.
