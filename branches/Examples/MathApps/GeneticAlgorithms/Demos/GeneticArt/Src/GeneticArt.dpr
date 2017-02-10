program GeneticArt;

uses
  Forms,
  fGeneticArt in 'fGeneticArt.pas' {frmGeneticArt},
  fZoomImage in 'fZoomImage.pas' {frmZoomImage},
  fGodMode in 'fGodMode.pas' {frmGodMode},
  fFavorites in 'fFavorites.pas' {frmFavorites},
  Noise in 'Noise.pas',
  fAddToFavorites in 'fAddToFavorites.pas' {frmAddToFavorites};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmGeneticArt, frmGeneticArt);
  Application.Run;
end.
