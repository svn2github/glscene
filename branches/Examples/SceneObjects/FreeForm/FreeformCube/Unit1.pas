unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.Imaging.JPEG,
  //GLS
  GLTexture,
  GLWin32Viewer, GLMaterial, GLScene, GLVectorTypes,
  GLObjects,
  GLVectorFileObjects,
  GLCoordinates, GLCrossPlatform,
  GLVectorLists,

  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Button1: TButton;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLFreeForm: TGLFreeForm;
    GLDummyCube1: TGLDummyCube;
    procedure Button1Click(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private

    FOldX, FOldY: Integer;

    procedure CreateCube;
  public
  end;

var
  Form1: TForm1;

type
  TFaceProperties = Record
    Normal3f   : TVector3f;
    Vertices   : Array[0..3] of TVector3f;
    TexCoords  : Array[0..3] of TVector2f;
    FaceIndex  : Array[0..5] of Integer;
    Material   : String;
  end;

const
  CCubeConstruct : Array[0..5] of TFaceProperties =
  (
    ( // Front Face
      Vertices:((X:-1; Y:-1; Z:1),(X:1;Y:-1;Z:1),(X:1;Y:1;Z:1),(X:-1;Y:1;Z:1));
      TexCoords:((X:0;Y:0),(X:1;Y:0),(X:1;Y:1),(X:0;Y:1));
      FaceIndex:(0,1,2,2,3,0);
      Material:'LibMaterial';
    ),
    ( // Back Face
      Vertices:((X:-1;Y:-1;Z:-1),(X:-1;Y:1;Z:-1),(X:1;Y:1;Z:-1),(X:1;Y:-1;Z:-1));
      TexCoords:((X:1;Y:0),(X:1;Y:1),(X:0;Y:1),(X:0;Y:0));
      FaceIndex:(4,5,6,6,7,4);
      Material:'LibMaterial1';
    ),
    ( // Top Face
      Vertices:((X:-1;Y:1;Z:-1),(X:-1;Y:1;Z:1),(X:1;Y:1;Z:1),(X:1;Y:1;Z:-1));
      TexCoords:((X:0;Y:1),(X:0;Y:0),(X:1;Y:0),(X:1;Y:1));
      FaceIndex:(8,9,10,10,11,8);
      Material:'LibMaterial2';
    ),
    ( // Bottom Face
      Vertices:((X:-1;Y:-1;Z:-1),(X:1;Y:-1;Z:-1),(X:1;Y:-1;Z:1),(X:-1;Y:-1;Z:1));
      TexCoords:((X:1;Y:1),(X:0;Y:1),(X:0;Y:0),(X:1;Y:0));
      FaceIndex:(12,13,14,14,15,12);
      Material:'LibMaterial3';
    ),
    ( // Left Face
      Vertices:((X:1;Y:-1;Z:-1),(X:1;Y:1;Z:-1),(X:1;Y:1;Z:1),(X:1;Y:-1;Z:1));
      TexCoords:((X:1;Y:0),(X:1;Y:1),(X:0;Y:1),(X:0;Y:0));
      FaceIndex:(16,17,18,18,19,16);
      Material:'LibMaterial4';
    ),
    ( // Right Face
      Vertices:((X:-1;Y:-1;Z:-1),(X:-1;Y:-1;Z:1),(X:-1;Y:1;Z:1),(X:-1;Y:1;Z:-1));
      TexCoords:((X:0;Y:0),(X:1;Y:0),(X:1;Y:1),(X:0;Y:1));
      FaceIndex:(20,21,22,22,23,20);
      Material:'LibMaterial5';
    )
  );

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  CreateCube;
end;

procedure TForm1.CreateCube;
var
  lMeshObj   : TMeshObject;
  lFaceGroup : TFGVertexIndexList;
  i, j       : Integer;
begin
  {Create Mesh Object}
  lMeshObj      := TMeshObject.CreateOwned(GLFreeForm.MeshObjects);
  lMeshObj.Mode := momFaceGroups;

  for i := Low(CCubeConstruct) to High(CCubeConstruct) do
  begin
    {Add the vertices}
    for j := Low(CCubeConstruct[i].Vertices) to High(CCubeConstruct[i].Vertices) do
      lMeshObj.Vertices.Add(CCubeConstruct[i].Vertices[j]);
    {Add texture coordinates}
    for j := Low(CCubeConstruct[i].TexCoords) to High(CCubeConstruct[i].TexCoords) do
      lMeshObj.TexCoords.Add(CCubeConstruct[i].TexCoords[j]);

    lFaceGroup := TFGVertexIndexList.CreateOwned(lMeshObj.FaceGroups);
    for j := Low(CCubeConstruct[i].FaceIndex) to High(CCubeConstruct[i].FaceIndex) do
      lFaceGroup.Add(CCubeConstruct[i].FaceIndex[j]);

    lFaceGroup.MaterialName := CCubeConstruct[i].Material;
  end;

  GLFreeForm.StructureChanged;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FOldX := X;
  FOldY := Y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    GLCamera1.MoveAroundTarget(FOldY - Y, FOldX - X);
    FOldX := X;
    FOldY := Y;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLSceneViewer1.Free;
end;

end.
