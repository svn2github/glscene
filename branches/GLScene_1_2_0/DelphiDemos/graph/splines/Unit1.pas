{ : This is a quick demo for the TGLLines object and spline functionality.<p>

  TGLLines can handle normal lines and cubic splines, each node can have a
  different color, and the line can be color-interpolated.<p>

  Note that the camera in this sample is in <i>orthogonal</i> mode, this makes
  for a quick and easy way to work in 2D with OpenGL (try switching the camera
  to perpective mode if you don't see the point).
}
unit Unit1;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  ExtCtrls,

  GLScene_Material,
  GLScene_MaterialEx,
  GLScene_Core,
  GLScene_Base_Coordinates,
  GLScene_Objects,
  GLScene_Platform,
  GLScene_Viewer_VCL;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    Lines1: TGLLines;
    GLMaterialLibraryEx1: TGLMaterialLibraryEx;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    ComboBox1: TComboBox;
    CheckBox2: TCheckBox;
    UpDown1: TUpDown;
    Edit1: TEdit;
    Label2: TLabel;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure UpDown1Changing(Sender: TObject; var AllowChange: Boolean);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure MoveCenterNodeTo(X, Y: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  GLScene_Base_Vector_Geometry;

procedure TForm1.MoveCenterNodeTo(X, Y: Integer);
begin
  Lines1.Nodes[1].AsAffineVector := GLSceneViewer1.Buffer.ScreenToWorld(X, Y);
end;

procedure TForm1.UpDown1Changing(Sender: TObject; var AllowChange: Boolean);
begin
  Lines1.Division := UpDown1.Position;
  Edit1.Text := IntToStr(UpDown1.Position);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  GLMaterialLibraryEx1.Materials[0].FixedFunction.Texture.Enabled :=
    CheckBox1.Checked;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then
    Lines1.Options := [loUseNodeColorForLines, loTextureCoord]
  else
    Lines1.Options := [loTextureCoord];
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
    0:
      Lines1.SplineMode := lsmLines;
    1:
      Lines1.SplineMode := lsmSegments;
    2:
      Lines1.SplineMode := lsmLoop;
    3:
      Lines1.SplineMode := lsmBezierSpline;
    4:
      Lines1.SplineMode := lsmCubicSpline;
    5:
      Lines1.SplineMode := lsmNURBSCurve;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Lines1.ObjectStyle := Lines1.ObjectStyle + [osStreamDraw];
  Lines1.Options := [loUseNodeColorForLines, loTextureCoord];
  Lines1.MaterialLibrary := GLMaterialLibraryEx1;
  Lines1.LibMaterialName := GLMaterialLibraryEx1.Materials[0].Name;
  GLSceneViewer1.Refresh;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MoveCenterNodeTo(X, Y);
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift <> [] then
    MoveCenterNodeTo(X, Y);
end;

end.
