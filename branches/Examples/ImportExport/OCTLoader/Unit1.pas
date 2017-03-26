{ OCT lighting via FSRad.<p>

  The default model should work decently with FSRad default settings, maybe
  a little dark. You may want to turn on clamping in FSRad if you get
  ugly darker areas in the lightmap, and experiment/tweak FSRad parameters
  before rendering high-quality lightmaps.
}
unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Imaging.Jpeg,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  GLScene,
  GLVectorFileObjects,
  GLObjects,
  GLVectorGeometry,
  GLVectorLists,
  GLWin32Viewer,
  GLMaterial,
  GLTexture,
  GLCoordinates,
  GLCrossPlatform,
  GLFile3DS,
  GLFileOCT,
  FileOCT,
  GLFileTGA,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource: TGLLightSource;
    Panel1: TPanel;
    BULoad3DS: TButton;
    Mesh: TGLActor;
    GLSphere: TGLSphere;
    BUExportOCT: TButton;
    BULoadOCT: TButton;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    LightmapsLibrary: TGLMaterialLibrary;
    EDIntensity: TEdit;
    procedure BULoad3DSClick(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure BUExportOCTClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BULoadOCTClick(Sender: TObject);
  public
    mx, my: Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ForceCurrentDirectory := True;
end;

procedure TForm1.BULoad3DSClick(Sender: TObject);
begin
  Mesh.LoadFromFile('lighting-test.3ds');
  BUExportOCT.Enabled := True;
  GLSceneViewer1.Buffer.Lighting := True;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  ip: TVector;
begin
  if Shift = [ssRight] then
  begin
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
    mx := X;
    my := Y;
  end
  else if Shift = [ssLeft] then
  begin
    GLSceneViewer1.Buffer.ScreenVectorIntersectWithPlaneXZ
      (VectorMake(X, GLSceneViewer1.Height - Y, 0, 0), GLSphere.Position.Y, ip);
    GLSphere.Position.AsVector := ip;
  end
  else if Shift = [ssLeft, ssShift] then
  begin
    GLSceneViewer1.Buffer.ScreenVectorIntersectWithPlaneXY
      (VectorMake(X, GLSceneViewer1.Height - Y, 0, 0), GLSphere.Position.Z, ip);
    GLSphere.Position.AsVector := ip;
  end;
end;

procedure TForm1.BUExportOCTClick(Sender: TObject);
var
  oct: TOCTFile;
  fs: TFileStream;
  vertices, texCoords: TAffineVectorList;
begin
  if not SaveDialog.Execute then
    Exit;
  oct := TOCTFile.Create;

  texCoords := TAffineVectorList.Create;
  vertices := Mesh.MeshObjects.ExtractTriangles(texCoords);
  vertices.Scale(4);
  oct.AddTriangles(vertices, texCoords, 'dummy');

  oct.AddLight(Mesh.AbsoluteToLocal(GLSphere.Position.AsAffineVector),
    GLLightSource.Diffuse.Color, StrToIntDef(EDIntensity.Text, 100000));

  vertices.Free;
  texCoords.Free;

  fs := TFileStream.Create(SaveDialog.FileName, fmCreate);
  oct.SaveToStream(fs);
  fs.Free;

  oct.Free;

  ShowMessage('Saved!');
end;

procedure TForm1.BULoadOCTClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    LightmapsLibrary.Materials.Clear;
    Mesh.LoadFromFile(OpenDialog.FileName);
    Mesh.MeshObjects[0].vertices.Scale(1 / 4);
    GLSceneViewer1.Buffer.Lighting := False;
  end;
end;

end.
