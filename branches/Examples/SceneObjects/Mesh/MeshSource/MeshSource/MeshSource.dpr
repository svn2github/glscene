{
   CapeRaven mesh demo.
   Changing mesh vertex data, normals and striping redundent data.
   Custom cube class declared for vertex point identification.
   On moving these vertex modifiers, the apointed vertex follows.
}
program MeshSource;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  MeshData in 'MeshData.pas' {frmMeshData};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
