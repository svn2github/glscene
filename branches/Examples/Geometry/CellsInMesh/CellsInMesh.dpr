program CellsInMesh;

uses
  Forms,
  fPointInMesh in 'fPointInMesh.pas' {frmCellsInMesh};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmCellsInMesh, frmCellsInMesh);
  Application.Run;
end.
