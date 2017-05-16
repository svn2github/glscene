program AsteroidField;

uses
  Forms,
  AsteroidMain in 'AsteroidMain.pas' {fAsteroidField};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfAsteroidField, fAsteroidField);
  Application.Run;
end.
