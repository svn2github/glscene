program OctreeDemo;

uses
  Forms,
  fOctreeDemo in 'fOctreeDemo.pas' {frmOctreeDemo},
  SpatialPartitioning in 'SpatialPartitioning.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmOctreeDemo, frmOctreeDemo);
  Application.Run;
end.
