
program nlife;

uses
  Forms,
  nLifeMain in 'nLifeMain.pas' {LifeMainForm},
  nUGlobal in 'nUGlobal.pas',
  nclife in 'nclife.pas' {LifeForm},
  nColors in 'nColors.pas' {ColorsForm},
  nEditor in 'nEditor.pas' {EditorForm},
  nEP in 'nEP.pas' {EditorPlacer},
  nGRF in 'nGRF.pas' {GRFForm},
  nGSRShow in 'nGSRShow.pas' {GSRShowForm},
  nTermite in 'nTermite.pas' {TermitesForm},
  nAntsFarm in 'nAntsFarm.pas' {AntsForm},
  nTorso in 'nTorso.pas' {DXTorsoForm},
  nULifeAlien in 'nULifeAlien.pas',
  nSysInfo in 'nSysInfo.pas' {SystemInfoForm},
  GlsAbout in 'GlsAbout.pas' {AboutBoids},
  GlsBirdFrm in 'GlsBirdFrm.pas' {AAABirdForm},
  GLSDemo in 'GLSDemo.pas' {AAADemoForm},
  GlsFrenzy in 'GlsFrenzy.pas' {FrenzyForm},
  GlsTreeFrm in 'GlsTreeFrm.pas' {ATreeForm},
  nUBirdDX in 'nUBirdDX.pas' {BirdDXForm},
  nlife3d in 'nlife3d.pas' {alife3dForm},
  nLifeFiles in 'nLifeFiles.pas' {LifeFilesForm},
  nlife3du in 'nlife3du.pas' {aUniversesForm};

{$R *.RES}
begin
  Application.Title := 'life';
  Application.CreateForm(TLifeMainForm, LifeMainForm);
  Application.CreateForm(TLifeForm, LifeForm);
  Application.CreateForm(TColorsForm, ColorsForm);
  Application.CreateForm(TEditorForm, EditorForm);
  Application.CreateForm(TEditorPlacer, EditorPlacer);
  Application.CreateForm(TGRFForm, GRFForm);
  Application.CreateForm(TGSRShowForm, GSRShowForm);
  Application.CreateForm(TTermitesForm, TermitesForm);
  Application.CreateForm(TAntsForm, AntsForm);
  Application.CreateForm(TDXTorsoForm, DXTorsoForm);
  Application.CreateForm(TATreeForm, ATreeForm);
  Application.CreateForm(TSystemInfoForm, SystemInfoForm);
  Application.CreateForm(TAboutBoids, AboutBoids);
  Application.CreateForm(TAAABirdForm, AAABirdForm);
  Application.CreateForm(TAAADemoForm, AAADemoForm);
  Application.CreateForm(TFrenzyForm, FrenzyForm);
  Application.CreateForm(TBirdDXForm, BirdDXForm);
  Application.CreateForm(Talife3dForm, alife3dForm);
  Application.CreateForm(TaUniversesForm, aUniversesForm);
  Application.CreateForm(TLifeFilesForm, LifeFilesForm);
  Application.Run;
{Array8450 = Array[0..84,0..50]  of Byte;}
{TV:Array3924;  TV7848:Array7848;  TV15696:Array15696;
Array3924 = Array[0..38,0..23]  of Byte;
Array7848 = Array[0..77,0..47]  of Byte;
Array15696 = Array[0..155,0..95]  of Byte;}  
end.


