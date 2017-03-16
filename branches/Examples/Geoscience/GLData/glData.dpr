program glData;

uses
  Forms,
  cColourSpectrum in 'Source\Code\cColourSpectrum.pas',
  cDelaunay in 'Source\Code\cDelaunay.pas',
  cContourTypes in 'Source\Code\cContourTypes.pas',
  cGridImportFn in 'Source\Code\cGridImportFn.pas',
  cStructured3DDataSet in 'Source\Code\cStructured3DDataSet.pas',
  cUsersSettings in 'Source\Code\cUsersSettings.pas',
  frmAbout in 'Source\Interface\frmAbout.pas' {formAbout},
  frmMain in 'Source\Interface\frmMain.pas' {formMain},
  frmSurferImport in 'Source\Interface\frmSurferImport.pas' {formSurferImport},
  cGLGridImportFn in 'source\code\cGLGridImportFn.pas',
  frmGeosimGrid in 'source\interface\frmGeosimGrid.pas' {formGeoGrid},
  cSimulationGrids in 'source\code\cSimulationGrids.pas',
  cGLSimulationGrids in 'source\code\cGLSimulationGrids.pas',
  frmProcess in 'source\interface\frmProcess.pas' {formProcess},
  frmColourTool in 'source\interface\frmColourTool.pas' {formColourEdit},
  frmBenchMark in 'source\interface\frmBenchMark.pas' {formBenchMark},
  frmOpenGL in 'source\interface\frmOpenGL.pas' {formOpenGL},
  cGLCoordinateAxes in 'source\code\cGLCoordinateAxes.pas',
  frmAxes in 'source\interface\frmAxes.pas' {formAxes},
  frmWorld in 'source\interface\frmWorld.pas' {formWorld},
  cUtilities in 'source\code\cUtilities.pas',
  frmBlock in 'source\interface\frmBlock.pas' {formBlock},
  cIsoSurfaceMC in 'source\code\cIsoSurfaceMC.pas',
  frmSort in 'comps\frmSort.pas' {formSort},
  frmPrefs in 'Source\Interface\frmPrefs.pas' {formPreferences},
  geImportFile in 'comps\geImportFile.pas',
  frmImport in 'comps\frmImport.pas',
  geExportFile in 'comps\geExportFile.pas',
  frmExport in 'comps\frmExport.pas' {formexporter},
  geFloatEdit in 'comps\geFloatEdit.pas',
  geIntegerEdit in 'comps\geIntegerEdit.pas',
  geTipofDay in 'comps\geTipofDay.pas',
  frmTip in 'comps\frmTip.pas' {formGETip};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'glData';
  Application.CreateForm(TformMain, formMain);
  Application.CreateForm(TformBenchMark, formBenchMark);
  Application.CreateForm(TformAxes, formAxes);
  Application.CreateForm(TformWorld, formWorld);
  Application.Run;
end.
