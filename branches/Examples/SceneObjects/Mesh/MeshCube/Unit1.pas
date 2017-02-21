unit Unit1;

interface

uses
  Winapi.OpenGL,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.Jpeg,
  GLTexture,
  GLWin32Viewer,
  GLMaterial,
  GLScene,
  GLVectorTypes,
  GLRenderContextInfo,
  GLVectorFileObjects, GLFile3ds,
  GLObjects,
  GLCadencer,
  GLVectorLists,
  GLKeyboard,
  GLSimpleNavigation,
  GLNavigator,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    room_materiallibrary: TGLMaterialLibrary;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    room: TGLFreeForm;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    scale_x: TEdit;
    scale_y: TEdit;
    scale_z: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Button7: TButton;
    OpenDialog1: TOpenDialog;
    GLCadencer1: TGLCadencer;
    GLNavigator1: TGLNavigator;
    GLUserInterface1: TGLUserInterface;
    cbMouseLook: TCheckBox;
    procedure GLDirectOpenGL1Render(var rci: TGLRenderContextInfo);
    procedure FormCreate(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure cbMouseLookClick(Sender: TObject);
  private
    FOldX, FOldY: Integer;
    procedure CreateMeshCube;
  public
     
  end;

var
  Form1: TForm1;

type
  TFaceProperties = record
    Normal3f: TVector3f;
    Vertices: array [0 .. 3] of TVector3f;
    TexCoords: array [0 .. 3] of TVector2f;
    FaceIndex: array [0 .. 5] of Integer;
    Material: string;
  end;

const
  CCubeConstruct: array [0 .. 5] of TFaceProperties =
  (( // Front Face
    Vertices: ((X: - 1; Y: - 1; Z: 1), (X: 1; Y: - 1; Z: 1), (X: 1; Y: 1; Z: 1), (X: - 1; Y: 1; Z: 1));
    TexCoords: ((X: 0; Y: 0), (X: 1; Y: 0), (X: 1; Y: 1), (X: 0; Y: 1));
    FaceIndex: (0, 1, 2, 2, 3, 0);
    Material: 'front';), (
    // Back Face
    Vertices: ((X: - 1; Y: - 1; Z: - 1), (X: - 1; Y: 1; Z: - 1),
               (X: 1; Y: 1; Z: - 1), (X: 1; Y: - 1; Z: - 1));
    TexCoords: ((X: 1; Y: 0), (X: 1; Y: 1), (X: 0; Y: 1), (X: 0; Y: 0));
    FaceIndex: (4, 5, 6, 6, 7, 4);
    Material: 'back';), ( // Top Face
    Vertices: ((X: - 1; Y: 1; Z: - 1), (X: - 1; Y: 1; Z: 1),
               (X: 1; Y: 1; Z: 1), (X: 1; Y: 1; Z: - 1));
    TexCoords: ((X: 0; Y: 1), (X: 0; Y: 0), (X: 1; Y: 0), (X: 1; Y: 1));
    FaceIndex: (8, 9, 10, 10, 11, 8);
    Material: 'top';),(
    // Bottom Face
    Vertices: ((X: - 1; Y: - 1; Z: - 1), (X: 1; Y: - 1; Z: - 1),
               (X: 1; Y: - 1; Z: 1), (X: - 1; Y: - 1; Z: 1));
    TexCoords: ((X: 1; Y: 1), (X: 0; Y: 1), (X: 0; Y: 0), (X: 1; Y: 0));
    FaceIndex: (12, 13, 14, 14, 15, 12);
    Material: 'bottom';), ( // Right Face
    Vertices: ((X: 1; Y: - 1; Z: - 1), (X: 1; Y: 1; Z: - 1),
               (X: 1; Y: 1; Z: 1), (X: 1; Y: - 1; Z: 1));
    TexCoords: ((X: 1; Y: 0), (X: 1; Y: 1), (X: 0; Y: 1), (X: 0; Y: 0));
    FaceIndex: (16, 17, 18, 18, 19, 16);
    Material: 'right';), ( // Left Face
    Vertices: ((X: - 1; Y: - 1; Z: - 1), (X: - 1; Y: - 1; Z: 1),
               (X: - 1; Y: 1; Z: 1), (X: - 1; Y: 1; Z: - 1));
    TexCoords: ((X: 0; Y: 0), (X: 1; Y: 0), (X: 1; Y: 1), (X: 0; Y: 1));
    FaceIndex: (20, 21, 22, 22, 23, 20);
    Material: 'left';));

implementation

{$R *.dfm}

procedure TForm1.CreateMeshCube;
var
  lMeshObj: TMeshObject;
  lFaceGroup: TFGVertexIndexList;
  i, j: Integer;
begin
  lMeshObj := TMeshObject.CreateOwned(room.MeshObjects);
  lMeshObj.Mode := momFaceGroups;

  for i := Low(CCubeConstruct) to High(CCubeConstruct) do
  begin
    for j := Low(CCubeConstruct[i].Vertices)
      to High(CCubeConstruct[i].Vertices) do
      lMeshObj.Vertices.Add(CCubeConstruct[i].Vertices[j]);

    for j := Low(CCubeConstruct[i].TexCoords)
      to High(CCubeConstruct[i].TexCoords) do
      lMeshObj.TexCoords.Add(CCubeConstruct[i].TexCoords[j]);

    lFaceGroup := TFGVertexIndexList.CreateOwned(lMeshObj.FaceGroups);
    for j := Low(CCubeConstruct[i].FaceIndex)
      to High(CCubeConstruct[i].FaceIndex) do
      lFaceGroup.Add(CCubeConstruct[i].FaceIndex[j]);

    lFaceGroup.MaterialName := CCubeConstruct[i].Material;
  end;
  room.StructureChanged;
end;

procedure TForm1.GLDirectOpenGL1Render(var rci: TGLRenderContextInfo);
begin
  glBegin(GL_QUADS);
  glVertex3f(-1.0, -1.0, 1.0);
  glVertex3f(1.0, -1.0, 1.0);
  glVertex3f(1.0, 1.0, 1.0);
  glVertex3f(-1.0, 1.0, 1.0);
  glEnd();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CreateMeshCube;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  if IsKeyDown('w') or IsKeyDown(VK_UP) then
    GLCamera1.Move(deltaTime);
  if IsKeyDown('a') or IsKeyDown(VK_RIGHT) then
    GLCamera1.Slide(-deltaTime);
  if IsKeyDown('s') or IsKeyDown(VK_DOWN) then
    GLCamera1.Move(-deltaTime);
  if IsKeyDown('d') or IsKeyDown(VK_LEFT) then
    GLCamera1.Slide(deltaTime);
  if IsKeyDown('z') then
    GLCamera1.Lift(deltaTime);
  if IsKeyDown('x') then
    GLCamera1.Lift(-deltaTime);
  if IsKeyDown(VK_ESCAPE) then
    Close;
  GLUserInterface1.MouseLook;
  GLUserInterface1.MouseUpdate;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  room.Scale.SetVector(StrToFloat(scale_x.Text), StrToFloat(scale_y.Text),
    StrToFloat(scale_z.Text));
end;

procedure TForm1.cbMouseLookClick(Sender: TObject);
begin
 if cbMouseLook.Checked then
   GLUserInterface1.MouseLookActivate
else
   GLUserInterface1.MouseLookDeactivate;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    with room_materiallibrary.Materials[TButton(Sender).Tag].Material.Texture do
    begin
      Disabled := true;
      Image.LoadFromFile(OpenDialog1.FileName);
      Disabled := false;
      room.StructureChanged;
    end;
end;

end.
