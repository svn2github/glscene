program Plot_2D_v3;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  Functs in 'Functs.pas' {FunctionsForm},
  GridOpts in 'GridOpts.pas' {GridOptionsForm},
  Numeric in 'Numeric.pas' {NumericForm},
  TextBlocks in 'TextBlocks.pas' {TextBlocksForm},
  Derivative in 'Derivative.pas' {DerivativeForm},
  IntegrateX in 'IntegrateX.pas' {IntegrateXForm},
  IntegrateY in 'IntegrateY.pas' {IntegrateYForm},
  Between in 'Between.pas' {BetweenForm},
  VolumeX in 'VolumeX.pas' {VolumeXForm},
  VolumeY in 'VolumeY.pas' {VolumeYForm},
  Bitmap in 'Bitmap.pas' {BitmapForm},
  Print in 'Print.pas' {PrintForm},
  Style in 'Style.pas' {StyleNameForm},
  fxValue in 'fxValue.pas' {fxValueForm},
  fx1Value in 'fx1Value.pas' {fx1ValueForm},
  fx2Value in 'fx2Value.pas' {fx2ValueForm},
  uCanvas in 'uCanvas.pas',
  uGlobal in 'uGlobal.pas',
  uParser in 'uParser.pas';

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
  Application.Run;
end.
