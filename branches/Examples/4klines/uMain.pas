unit uMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Dialogs,
  Vcl.Controls,
  // GLS
  GLCrossPlatform,
  GLScene,
  GLObjects,
  GLCoordinates,
  GLWin32Viewer,
  GLKeyboard,
  GLBaseClasses,
  GLCadencer,
  GLAsyncTimer,
  GLVectorGeometry,
  GLRenderContextInfo,
  GLFileOBJ,
  GLVectorFileObjects,
  GLSelection;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    cam: TGLCamera;
    at: TGLAsyncTimer;
    dc_cam: TGLDummyCube;
    cad: TGLCadencer;
    dc_sample1: TGLDummyCube;
    line_sample2: TGLLines;
    dogl_sample2: TGLDirectOpenGL;
    dogl_sample3: TGLDirectOpenGL;
    dc_sample2: TGLDummyCube;
    Panel1: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    ff_sample2: TGLFreeForm;
    ff_sample3: TGLFreeForm;
    GLLightSource1: TGLLightSource;
    RadioButton4: TRadioButton;
    dogl_sample4: TGLDirectOpenGL;
    ff_sample4: TGLFreeForm;
    StaticText1: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure vpMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cadProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure atTimer(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure dogl_sample3Render(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure dogl_sample2Render(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure dogl_sample4Render(Sender: TObject; var rci: TGLRenderContextInfo);
  public

    procedure initSamples;
    procedure addMeshObj(srcFF, dstFF: TGLFreeForm; mR, mSTR: TMatrix);
    procedure mergeMeshObj(srcFF, dstFF: TGLFreeForm; mR, mSTR: TMatrix;
      index: Integer);

  end;

const
  icnt = 4000;

var
  Form1: TForm1;
  m_turn: boolean;

implementation

{$R *.dfm}

// setup
//
procedure TForm1.FormCreate(Sender: TObject);
begin
  initSamples;
  RadioButton1Click(nil);
end;

// init samples
//
procedure TForm1.initSamples;
var
  i: Integer;
  ff: TGLFreeForm;
  ln: TGLLines;
  pos: TVector;
  mST, mR, mSTR: TMatrix;
begin
  ff_sample2.LoadFromFile('1.obj');
  ff_sample2.scale.scale(0.03);

  for i := 0 to icnt - 1 do
  begin
    SetVector(pos, Random(80), Random(80), Random(20));
    ff := TGLFreeForm.Create(self);
    ff.LoadFromFile('1.obj');
    ff.Scale.Scale(0.03);

    // sample 1
    ln := TGLLines.CreateAsChild(dc_sample1);
    ln.AddNode(0, 0, 0);
    ln.AddNode(0, 0, 3);
    ln.NodesAspect := lnaInvisible;
    ln.Position.SetPoint(pos);
    ln.AddChild(ff);

    // samples 2..4
    line_sample2.AddNode(pos);
    line_sample2.AddNode(pos.X, pos.Y, pos.Z + 3);

    // samples 3 & 4
    mST := CreateScaleAndTranslationMatrix(vectormake(0.03, 0.03, 0.03), pos);
    mR := CreateRotationMatrix(vectormake(0, 0, 0), 0);
    mSTR := MatrixMultiply(mST, mR);
    addMeshObj(ff, ff_sample3, mR, mSTR);
    mergeMeshObj(ff, ff_sample4, mR, mSTR, i);
  end;
  // ff_sample3.SaveToFile('3.obj');
  // ff_sample4.SaveToFile('4.obj');
end;

// add mesh
//
procedure TForm1.addMeshObj(srcFF, dstFF: TGLFreeForm; mR, mSTR: TMatrix);
var
  i: Integer;
  MObj, srcMObj: TMeshObject;
  srcFG: TFGVertexNormalTexIndexList;
  FG: TFGVertexIndexList;
begin
  srcMObj := srcFF.MeshObjects[0];
  srcFG := TFGVertexNormalTexIndexList(srcMObj.FaceGroups[0]);

  MObj := TMeshObject.CreateOwned(dstFF.MeshObjects);
  MObj.Mode := momFaceGroups;

  MObj.Vertices.AddNulls(srcMObj.Vertices.Count);
  for i := 0 to srcMObj.Vertices.Count - 1 do
    MObj.Vertices[i] := vectorTransform(srcMObj.Vertices[i], mSTR);

  MObj.Normals.AddNulls(srcMObj.Vertices.Count);
  for i := 0 to srcFG.VertexIndices.Count - 1 do
    MObj.Normals[srcFG.VertexIndices[i]] :=
      vectorTransform(srcMObj.Normals[srcFG.NormalIndices[i]], mR);

  MObj.TexCoords.AddNulls(srcMObj.Vertices.Count);
  for i := 0 to srcFG.VertexIndices.Count - 1 do
    MObj.TexCoords[srcFG.VertexIndices[i]] :=
      vectorTransform(srcMObj.TexCoords[srcFG.TexCoordIndices[i]], mR);

  FG := TFGVertexIndexList.CreateOwned(MObj.FaceGroups);
  FG.Mode := srcFG.Mode;
  FG.MaterialName := srcFG.MaterialName;
  FG.VertexIndices.Add(srcFG.VertexIndices);
end;

// merge mesh
//
procedure TForm1.mergeMeshObj(srcFF, dstFF: TGLFreeForm; mR, mSTR: TMatrix;
  index: Integer);
var
  i, j: Integer;
  MObj, srcMObj: TMeshObject;
  srcFG: TFGVertexNormalTexIndexList;
  FG: TFGVertexIndexList;
begin
  srcMObj := srcFF.MeshObjects[0];
  if dstFF.MeshObjects.Count = 0 then
  begin
    MObj := TMeshObject.CreateOwned(dstFF.MeshObjects);
    MObj.Mode := momFaceGroups;
    MObj.Vertices.AddNulls(srcMObj.Vertices.Count * icnt);
    MObj.Normals.AddNulls(srcMObj.Normals.Count * icnt);
    MObj.TexCoords.AddNulls(srcMObj.TexCoords.Count * icnt);
  end
  else
    MObj := dstFF.MeshObjects[0];

  j := index * srcMObj.Vertices.Count;
  for i := 0 to srcMObj.Vertices.Count - 1 do
    MObj.Vertices[i + j] := vectorTransform(srcMObj.Vertices[i], mSTR);

  j := index * srcMObj.Normals.Count;
  for i := 0 to srcMObj.Normals.Count - 1 do
    MObj.Normals[i + j] := vectorTransform(srcMObj.Normals[i], mR);

  j := index * srcMObj.TexCoords.Count;
  for i := 0 to srcMObj.TexCoords.Count - 1 do
    MObj.TexCoords[i + j] := srcMObj.TexCoords[i];

  srcFG := TFGVertexNormalTexIndexList(srcFF.MeshObjects[0].FaceGroups[0]);
  if MObj.FaceGroups.Count = 0 then
  begin
    FG := TFGVertexIndexList.CreateOwned(MObj.FaceGroups);
    FG.Mode := srcFG.Mode;
    FG.MaterialName := srcFG.MaterialName;
    FG.VertexIndices.AddNulls(srcFG.VertexIndices.Count * icnt);
  end
  else
    FG := TFGVertexIndexList(MObj.FaceGroups[0]);

  j := index * srcFG.VertexIndices.Count;
  for i := 0 to srcFG.VertexIndices.Count - 1 do
    FG.VertexIndices[i + j] := srcFG.VertexIndices[i] + j;
end;

// start rotation
//
procedure TForm1.vpMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
// var
// pl: TGLPickList;
begin
  if Button = TMouseButton.mbLeft then
  begin
    m_turn := true;
    mouse.CursorPos := Point(screen.Width div 2, screen.Height div 2);
  end;
  // pl := vp.Buffer.GetPickedObjects(rect(x-5,y-5,x+5,y+5));
  // caption := inttostr(pl.Count);
end;

// cad progress
//
procedure TForm1.cadProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  if m_turn then
  begin
    cam.MoveAroundTarget((screen.Height div 2 - mouse.CursorPos.Y) * 0.2,
      (screen.Width div 2 - mouse.CursorPos.X) * 0.2);
    mouse.CursorPos := Point(screen.Width div 2, screen.Height div 2);
    if not iskeydown(VK_LBUTTON) then
      m_turn := false;
  end;
  vp.Invalidate;
end;

// timer
//
procedure TForm1.atTimer(Sender: TObject);
begin
  Caption := IntToStr(icnt) + ' lines: ' + vp.FramesPerSecondText(2);
  vp.ResetPerformanceMonitor;
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
var
  s: string;
begin
  dc_sample1.Visible := RadioButton1.Checked;
  dc_sample2.Visible := RadioButton2.Checked;
  dogl_sample3.Visible := RadioButton3.Checked;
  dogl_sample4.Visible := RadioButton4.Checked;

  s := IntToStr(icnt);
  if RadioButton1.Checked then
    StaticText1.caption := s + ' lines, ' + s + ' freeForms'
  else if RadioButton2.Checked then
    StaticText1.caption := 'single line, ' + s + ' freeForms'
  else if RadioButton3.Checked then
    StaticText1.caption := 'single line, single freeForm (' + s + ' faceGroups)'
  else if RadioButton4.Checked then
    StaticText1.caption := 'single line, single freeForm (single faceGroup)';

end;

// sample 2 render
//
procedure TForm1.dogl_sample2Render(Sender: TObject;
  var rci: TGLRenderContextInfo);
var
  i: Integer;
begin

  for i := 0 to icnt - 1 do
  begin
    ff_sample2.Position.SetPoint(line_sample2.Nodes[i * 2].AsVector);
    ff_sample2.Render(rci);
  end;

end;

// sample 3 render
//
procedure TForm1.dogl_sample3Render(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin

  line_sample2.Render(rci);

  ff_sample3.Render(rci);

end;

// sample 4 render
//
procedure TForm1.dogl_sample4Render(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  line_sample2.Render(rci);
  ff_sample4.Render(rci);
end;

end.
