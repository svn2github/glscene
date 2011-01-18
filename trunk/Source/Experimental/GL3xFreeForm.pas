unit GL3xFreeForm;

interface

{$I GLScene.inc}

uses
  Classes, BaseClasses, GLScene, GL3xObjects, GLRenderContextInfo;

type
  TGL3xFreeForm = class(TGL3xCustomObject)
  protected
    { Protected Declarations }
    procedure BuildMesh; override;
    function GetMeshName: string; virtual;
    procedure SetMeshName(const Value: string); virtual;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;

    procedure DoRender(var ARci: TRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;

    property MeshRefName: IGLName read FMesh;
  published
    property Mesh: string read GetMeshName write SetMeshName;
  end;

implementation

uses
  GLShaderManager, GL3xMaterial, GL3xMesh, GLDrawTechnique,
  GLStrings;

constructor TGL3xFreeForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Mesh := glsDEFAULTMESHNAME;
  StructureChanged;
end;

procedure TGL3xFreeForm.BuildMesh;
begin
//  with MeshManager.GetMeshBuilder(FMesh) do
//  begin
//    BeginMeshAssembly;
//    ComputeNormals(True);
//    EndMeshAssembly;
//  end;
  ClearStructureChanged;
end;

procedure TGL3xFreeForm.DoRender(var ARci: TRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
begin
    if ARenderSelf then
    begin

      try
        if ocStructure in Changes then
          BuildMesh;

        BeforeRender;
        DrawManager.Draw(ARci, FMesh);
        AfterRender;
      except
        Visible := False;
      end;
    end;

  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

function TGL3xFreeForm.GetMeshName: string;
begin
  Result := FMesh.GetValue;
end;

procedure TGL3xFreeForm.SetMeshName(const Value: string);
begin
  with MeshManager do
  try
    BeginWork;
    FMesh := GetMeshName(Value);
  finally
    EndWork;
  end;
  NotifyChange(Self);
end;

initialization

  RegisterClass(TGL3xFreeForm);

end.
