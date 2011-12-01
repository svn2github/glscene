{: GLGizmo component demo.

  Version History:

  29/09/2007 - DaStr - Initial version.

}
unit DemoGizmoForm;

interface

uses
  // VCL
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Dialogs, ExtCtrls,

  // GLScene
  GLScene_Gizmo, GLScene_SimpleNavigation,
  GLScene_BitmapFont, GLScene_BitmapFont_System, GLScene_Cadencer, GLScene_Core,
  GLScene_ObjectsEx, GLScene_Objects, GLScene_Base_Coordinates,
  GLScene_Platform, GLScene_Viewer_VCL;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    Viewer: TGLSceneViewer;
    Camera: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    GLLightSource1: TGLLightSource;
    GLLightSource2: TGLLightSource;
    RootGizmo: TGLDummyCube;
    Panel1: TPanel;
    Bevel1: TBevel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CBXAxis: TComboBox;
    CheckBox3: TCheckBox;
    CBXOperation: TComboBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    ColorBox1: TColorBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ColorBox2: TColorBox;
    ColorBox3: TColorBox;
    Label4: TLabel;
    edAutoZoomFactor: TEdit;
    Label5: TLabel;
    edzoomfactor: TEdit;
    CheckBox12: TCheckBox;
    Label6: TLabel;
    Label7: TLabel;
    edMoveCoef: TEdit;
    edRotateCoef: TEdit;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    Label8: TLabel;
    GLDodecahedron3: TGLDodecahedron;
    GLArrowLine3: TGLArrowLine;
    GLArrowLine4: TGLArrowLine;
    Label9: TLabel;
    edtScaleCoef: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    edtGizmoThickness: TEdit;
    GLSphere1: TGLSphere;
    GLCube1: TGLCube;
    OptPickMode: TRadioGroup;
    GLSystemBitmapFont1: TGLSystemBitmapFont;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLDisk1: TGLDisk;
    GLGizmo1: TGLGizmo;
    procedure GLCadencer1Progress(Sender: TObject; const DeltaTime, newTime: Double);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure edAutoZoomFactorKeyPress(Sender: TObject; var Key: Char);
    procedure CheckBox12Click(Sender: TObject);
    procedure CBXAxisChange(Sender: TObject);
    procedure CBXOperationChange(Sender: TObject);
    procedure edMoveCoefChange(Sender: TObject);
    procedure edRotateCoefChange(Sender: TObject);
    procedure edAutoZoomFactorChange(Sender: TObject);
    procedure edzoomfactorChange(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
    procedure edtScaleCoefChange(Sender: TObject);
    procedure edtGizmoThicknessChange(Sender: TObject);
    procedure OptPickModeClick(Sender: TObject);
    procedure GLSimpleNavigation1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSimpleNavigation1MouseWheel(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    procedure FillPickableObjectsList(root: TGLBaseSceneObject; doClearList: Boolean);
    { Private declarations }
  public
    { Public declarations }
    mx, my: Integer;
    //    gizmo: TGLGizmoEx;
    noMouseMotion: Boolean;

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const DeltaTime, newTime: Double);
begin
  viewer.invalidate;
end;

procedure TForm1.GLSimpleNavigation1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (shift <> [ssRight]) and (GLGizmo1.SelectedObj <> nil) then
    GLGizmo1.ViewerMouseMove(X, Y);

  mx := X;
  my := Y;
end;

procedure TForm1.GLSimpleNavigation1MouseWheel(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  GLGizmo1.UpdateGizmo;
end;

procedure TForm1.OptPickModeClick(Sender: TObject);
begin
  GLGizmo1.PickMode := TGLGizmoPickMode(OptPickMode.ItemIndex);
end;

procedure TForm1.ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
  GLGizmo1.ViewerMouseDown(X, Y);
end;

procedure TForm1.ViewerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GLGizmo1.ViewerMouseUp(X, Y);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLSimpleNavigation1.OnMouseWheel := GLSimpleNavigation1MouseWheel;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Viewer.SetFocus;
  GLGizmo1.RootGizmo := rootGizmo;
  // Fill list of pickable objects when using PickMode=pmRaycast
  FillPickableObjectsList(GLDummyCube1, True);
end;


// Recurse root object to fill list of pickable objects when using PickMode=pmRaycast
procedure TForm1.FillPickableObjectsList(root: TGLBaseSceneObject; doClearList: Boolean);
var
  t: Integer;
begin
  if doClearList then
    GLGizmo1.PickableObjectsWithRayCast.Clear;
  for t := 0 to root.Count - 1 do
  begin
    GLGizmo1.PickableObjectsWithRayCast.Add(root[t]);
    FillPickableObjectsList(root[t], False);
  end;
end;


procedure TForm1.edAutoZoomFactorKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(key, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', ',']) then
    key := #0;
end;

procedure TForm1.CheckBox12Click(Sender: TObject);
var
  check: Boolean;
begin
  // (Sender as TCheckBox).Checked:=Not((Sender as TCheckBox).Checked);
  check := (Sender as TCheckBox).Checked;
  case (Sender as TCheckBox).Tag of
    0: GLGizmo1.Enabled := Check;
    1: GLGizmo1.ExcludeObjects := Check;
    2:
    begin
      GLGizmo1.ForceAxis := Check;
      CBXAxis.Enabled := Check;
    end;
    3:
    begin
      GLGizmo1.ForceOperation := Check;
      CBXOperation.Enabled := Check;
    end;
    4: GLGizmo1.ForceUniformScale := Check;
    5: if Check then
        GLGizmo1.GizmoElements := GLGizmo1.GizmoElements + [geAxisLabel]
      else
        GLGizmo1.GizmoElements := GLGizmo1.GizmoElements - [geAxisLabel];
    6:
    begin
      if Check then
      begin
        GLGizmo1.GizmoElements := GLGizmo1.GizmoElements + [geObjectInfos];
        CheckBox7.Enabled := Check;
        CheckBox8.Enabled := Check;
        CheckBox9.Enabled := Check;
      end
      else
      begin
        GLGizmo1.GizmoElements := GLGizmo1.GizmoElements - [geObjectInfos];
        CheckBox7.Enabled := Check;
        CheckBox8.Enabled := Check;
        CheckBox9.Enabled := Check;
      end;
    end;
    7: GLGizmo1.NoZWrite := Check;
    8:
    begin
      GLGizmo1.AutoZoom := Check;
      if Check then
      begin
        edAutoZoomFactor.Enabled := True;
        edZoomFactor.Enabled := False;
      end
      else
      begin
        edAutoZoomFactor.Enabled := False;
        edZoomFactor.Enabled := True;
      end;
    end;
    9: if Check then
        GLGizmo1.VisibleInfoLabels := GLGizmo1.VisibleInfoLabels + [vliName]
      else
        GLGizmo1.VisibleInfoLabels := GLGizmo1.VisibleInfoLabels - [vliName];
    10: if Check then
        GLGizmo1.VisibleInfoLabels := GLGizmo1.VisibleInfoLabels + [vliOperation]
      else
        GLGizmo1.VisibleInfoLabels := GLGizmo1.VisibleInfoLabels - [vliOperation];
    11: if Check then
        GLGizmo1.VisibleInfoLabels := GLGizmo1.VisibleInfoLabels + [vliCoords]
      else
        GLGizmo1.VisibleInfoLabels := GLGizmo1.VisibleInfoLabels - [vliCoords];
    12: if Check then
        GLGizmo1.GizmoElements := GLGizmo1.GizmoElements + [geMove]
      else
        GLGizmo1.GizmoElements := GLGizmo1.GizmoElements - [geMove];
    13: if Check then
        GLGizmo1.GizmoElements := GLGizmo1.GizmoElements + [geRotate]
      else
        GLGizmo1.GizmoElements := GLGizmo1.GizmoElements - [geRotate];
    14: if Check then
        GLGizmo1.GizmoElements := GLGizmo1.GizmoElements + [geScale]
      else
        GLGizmo1.GizmoElements := GLGizmo1.GizmoElements - [geScale];
  end;
end;

procedure TForm1.CBXAxisChange(Sender: TObject);
begin
  case CBXAxis.ItemIndex of
    0: GLGizmo1.SelAxis := gaNone;
    1: GLGizmo1.SelAxis := gaX;
    2: GLGizmo1.SelAxis := gaXY;
    3: GLGizmo1.SelAxis := gaXZ;
    4: GLGizmo1.SelAxis := gaY;
    5: GLGizmo1.SelAxis := gaYZ;
    6: GLGizmo1.SelAxis := gaZ;
  end;
end;

procedure TForm1.CBXOperationChange(Sender: TObject);
begin
  case CBXOperation.ItemIndex of
    0: GLGizmo1.Operation := gopNone;
    1: GLGizmo1.Operation := gopMove;
    2: GLGizmo1.Operation := gopRotate;
    3: GLGizmo1.Operation := gopScale;
  end;
end;

procedure TForm1.edMoveCoefChange(Sender: TObject);
begin
  if edMoveCoef.Text <> '' then
    GLGizmo1.MoveCoef := StrToFloat(edMoveCoef.Text);
end;

procedure TForm1.edRotateCoefChange(Sender: TObject);
begin
  if edRotateCoef.Text <> '' then
    GLGizmo1.RotationCoef := StrToFloat(edRotateCoef.Text);
end;

procedure TForm1.edtGizmoThicknessChange(Sender: TObject);
begin
  GLGizmo1.GizmoThickness := StrToFloat(edtGizmoThickness.Text);
end;

procedure TForm1.edtScaleCoefChange(Sender: TObject);
begin
  if edtScaleCoef.Text <> '' then
    GLGizmo1.ScaleCoef := StrToFloat(edtScaleCoef.Text);
end;

procedure TForm1.edAutoZoomFactorChange(Sender: TObject);
begin
  if edAutoZoomFactor.Text <> '' then
    GLGizmo1.AutoZoomFactor := StrToFloat(edAutoZoomFactor.Text);
end;

procedure TForm1.edzoomfactorChange(Sender: TObject);
begin
  if edZoomFactor.Text <> '' then
    GLGizmo1.ZoomFactor := StrToFloat(edZoomFactor.Text);
end;

procedure TForm1.ColorBox1Change(Sender: TObject);
begin
  case (Sender as TColorBox).Tag of
    0: GLGizmo1.BoundingBoxColor.AsWinColor := ColorBox1.Selected;
    1: GLGizmo1.VisibleInfoLabelsColor.AsWinColor := ColorBox2.Selected;
    2: GLGizmo1.SelectedColor.AsWinColor := ColorBox3.Selected;
  end;
end;

end.
