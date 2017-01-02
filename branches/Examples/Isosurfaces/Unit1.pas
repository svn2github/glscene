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
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Buttons,
  Vcl.Forms,
  Vcl.Dialogs,
  //GLS
  GLWin32Viewer,
  GLCrossPlatform,
  GLBaseClasses,
  GLScene,
  GLObjects,
  GLCoordinates,
  GLColor,
  GLVectorGeometry,
  GLMesh,
  GLVectorFileObjects,
  GLState,
  GLGeomObjects,
  GLExtrusion,
  GLTypes,
  GLIsosurface,
  GLSimpleNavigation,
  GLMaterial;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    BitBtn1: TBitBtn;
    GLMesh1: TGLMesh;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLFreeForm1: TGLFreeForm;
    ComboBox2: TComboBox;
    procedure FormDestroy(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
  private
    mo: TMeshObject;
    MC: TGLMarchingCube;
  public
    //Public declarations
  end;


var
  Form1: TForm1;

implementation

{$R *.dfm}

// comment next line to use GLMesh instead GLFreeForm
{$DEFINE UseGLFreeForm}

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
{$IFDEF UseGLFreeForm}
  if CheckBox1.Checked then
    GLFreeForm1.Material.PolygonMode := pmFill
  else
    GLFreeForm1.Material.PolygonMode := pmLines
{$ELSE}
  if CheckBox1.Checked then
    GLMesh1.Material.PolygonMode := pmFill
  else
    GLMesh1.Material.PolygonMode := pmLines
{$ENDIF}
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  with GLLightSource1 do
  begin
    Position.SetPoint(100, 200, 300);
    Ambient.SetColor(0.6, 0.6, 0.0, 0.1);
    Diffuse.SetColor(0.2, 0.4, 1.0, 0.1);
    Specular.SetColor(1.0, 1.0, 1.0, 0.1)
  end;

{$IFDEF UseGLFreeForm}
  GLMesh1.Visible := False;
  mo := TMeshObject.CreateOwned(GLFreeForm1.MeshObjects);
  mo.Mode := momFaceGroups;
{$ELSE}
  GLFreeForm1.Visible := False;
{$ENDIF}
  MC := TGLMarchingCube.Create(100, 100, 100);
  BitBtn1Click(nil)
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  MC.Free
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  Edit1.Text := floattostr(DemoScalarField[ComboBox1.ItemIndex].IsoValue);
  BitBtn1Click(nil)
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
{$IFDEF UseGLFreeForm}
  GLFreeForm1.Material.FaceCulling := TFaceCulling(ComboBox2.ItemIndex)
{$ELSE}
  GLMesh1.Material.FaceCulling := TFaceCulling(ComboBox2.ItemIndex)
{$ENDIF}
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  IsoValue: TGLScalarValue;
begin
  // try to accept user value, but if uncorrect assign a correct demo value
  IsoValue := StrToFloatDef(Edit1.Text, DemoScalarField[ComboBox1.ItemIndex]
    .IsoValue);
  Edit1.Text := FormatFloat('0.0000', IsoValue);
  MC.FillVoxelData(IsoValue, DemoScalarField[ComboBox1.ItemIndex].ScalarField);
  MC.Run;

{$IFDEF UseGLFreeForm}
  mo.Free;
  mo := TMeshObject.CreateOwned(GLFreeForm1.MeshObjects);
  mo.Mode := momFaceGroups;
  MC.CalcMeshObject(mo, 0.6);
{$ELSE}
  MC.CalcVertices(GLMesh1.Vertices);
{$ENDIF}
  GLSceneViewer1.Invalidate;
end;

end.
