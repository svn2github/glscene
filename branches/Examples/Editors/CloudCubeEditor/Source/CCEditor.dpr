// Cloud Editor by Ivan Lee Herring, Dec2005 .. Jan2006
// 713W  558H to make 512x512 Viewer  ...Means a minimum 800x600 screen Rez
// Conglomeration of many GLScene demos...

program CCEditor;

uses
  Forms,
  GLGizmoArv in 'GLGizmoArv.pas',
  CCEditorFrm in 'CCEditorFrm.pas' {ACloudDemoForm},
  CCTextureFrm in 'CCTextureFrm.pas' {ATextureCombinerForm},
  CCProceduralCloudsFrm in 'CCProceduralCloudsFrm.pas' {ProceduralCloudsForm},
  CCATextureEditorFrm in 'CCATextureEditorFrm.pas' {ATextureEditorForm},
  CCMaskfillunit in 'CCMaskfillunit.pas',
  CCNanMakerFrm in 'CCNanMakerFrm.pas' {NanMakerForm},
  CCMFMakerFrm in 'CCMFMakerFrm.pas' {MFMakerForm},
  uHeightmapClasses in 'uHeightmapClasses.pas',
  StrFunctions in 'StrFunctions.pas',
  uGrid in 'uGrid.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'GLS Cloud Cube Editor';
  Application.CreateForm(TACloudDemoForm, ACloudDemoForm);
  Application.CreateForm(TATextureCombinerForm, ATextureCombinerForm);
  Application.CreateForm(TProceduralCloudsForm, ProceduralCloudsForm);
  Application.CreateForm(TATextureEditorForm, ATextureEditorForm);
  Application.CreateForm(TNanMakerForm, NanMakerForm);
  Application.CreateForm(TMFMakerForm, MFMakerForm);
  Application.Run;
end.
