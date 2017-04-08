program AStar;

uses
  RecyclerMM,
  Forms,
  AStarFrm in 'AStarFrm.pas' {AStarForm},
  AStarGlobals in 'AStarGlobals.pas',
  AStarAboutFrm in 'AStarAboutFrm.pas' {AStarAboutForm},
  AStarCode in 'AStarCode.pas',
  AStarCodeH in 'AStarCodeH.pas',
  AStarBlitzCodeH in 'AStarBlitzCodeH.pas',
  AStarBlitzCode in 'AStarBlitzCode.pas',
  AStarBlitzUnit in 'AStarBlitzUnit.pas',
  UnusedStuff in 'UnusedStuff.pas',
  AProjectOptionsFrm in 'AProjectOptionsFrm.pas' {AProjectOptionsForm},
  AGr32ViewerFrm in 'AGr32ViewerFrm.pas' {AGr32ViewerForm},
  ATerrainFrm in 'ATerrainFrm.pas' {ATerrainForm},
  fLandscape in 'fLandscape.pas' {frmLandscape},
  StrFunctions in 'StrFunctions.pas',
  uGrid in 'uGrid.pas',
  uHeightmapClasses in 'uHeightmapClasses.pas',
  AProjectMapMakerFrm in 'AProjectMapMakerFrm.pas' {AProjectMapMakerForm},
  AProgramOptionsFrm in 'AProgramOptionsFrm.pas' {AProgramOptionsForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'AStar';
  Application.CreateForm(TAStarForm, AStarForm);
  Application.CreateForm(TAProjectOptionsForm, AProjectOptionsForm);
  Application.CreateForm(TAProgramOptionsForm, AProgramOptionsForm);
  Application.Run;
end.
