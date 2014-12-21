unit FGUILayoutEditor;

interface

{$I GLScene.inc}

uses
{$IFDEF MSWINDOWS}
  Windows, ShellApi,
{$ENDIF}
{$IFDEF FPC}
  LCLType,
{$ENDIF}

{$IFDEF GLS_DELPHI_XE2_UP}
  System.SysUtils, System.Variants, System.Classes,
  VCL.Graphics,  VCL.Controls,  VCL.Forms,  VCL.Dialogs,  VCL.Buttons,
  VCL.ExtDlgs,  VCL.StdCtrls,  VCL.ExtCtrls,  Vcl.Samples.Spin,  VCL.Grids,
{$ELSE}
  SysUtils, Variants, Classes,
  Graphics,  Controls,  Forms,  Dialogs,  Buttons,  ExtDlgs,  StdCtrls,
  ExtCtrls,  Spin,  Grids,
{$ENDIF}

  GLCrossPlatform, GLBaseClasses, GLGui, GLUtils

{$IFDEF LINUX}, Process{$ENDIF};

type
  TLayouts_Form = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Items_List: TListBox;
    X_Label: TLabel;
    Y_Label: TLabel;
    Open_Image_Button: TBitBtn;
    Open_Button: TBitBtn;
    Save_Button: TBitBtn;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Delete_Button: TBitBtn;
    Add_Button: TBitBtn;
    LabelX: TLabel;
    LabelY: TLabel;
    Left_Edit: TSpinEdit;
    Top_Edit: TSpinEdit;
    LabelHeight: TLabel;
    LabelWidth: TLabel;
    Height_Edit: TSpinEdit;
    Width_Edit: TSpinEdit;
    Label5: TLabel;
    Name_Edit: TEdit;
    Elements_Grid: TStringGrid;
    Panel3: TPanel;
    BitBtnZoomIn: TBitBtn;
    BitBtnZoomOut: TBitBtn;
    ScrollBox1: TScrollBox;
    Image2: TImage;
    PaintBox1: TPaintBox;
    Image1: TImage;
    BitBtnShow: TBitBtn;
    BitBtn1: TBitBtn;
    GLGuiLayout1: TGLGuiLayout;
    procedure Open_Image_ButtonClick(Sender: TObject);
    procedure Open_ButtonClick(Sender: TObject);
    procedure Save_ButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Add_ButtonClick(Sender: TObject);
    procedure Delete_ButtonClick(Sender: TObject);
    procedure Items_ListClick(Sender: TObject);
    procedure Name_EditExit(Sender: TObject);
    procedure Name_EditKeyPress(Sender: TObject; var Key: Char);
    procedure Elements_GridClick(Sender: TObject);
    procedure Left_EditChange(Sender: TObject);
    procedure Top_EditChange(Sender: TObject);
    procedure Width_EditChange(Sender: TObject);
    procedure Height_EditChange(Sender: TObject);
    procedure BitBtnZoomInClick(Sender: TObject);
    procedure BitBtnShowClick(Sender: TObject);
    procedure Elements_GridDblClick(Sender: TObject);
  private
    { Private declarations }
    procedure SyncImages;
    procedure DrawCurrentElement;
    procedure RefreshComponentBox;
    function GetEnabledSpins: Boolean;
    procedure SetEnabledSpins(Value: Boolean);
  public
    { Public declarations }
    procedure Execute(AGUILayout: TGLGuiLayout);
    property EnabledSpins: Boolean read GetEnabledSpins write SetEnabledSpins;
  end;

function GUILayoutEditorForm: TLayouts_Form;
procedure ReleaseGUILayoutEditor;

implementation

{$R *.dfm}

var
  vGUILayoutEditor: Tlayouts_form;
  rect_point1, rect_point2: TPoint;
  zoom: integer = 1;
  sorted_elements: array[0..9] of TGLGuiElement;

function GUILayoutEditorForm: Tlayouts_form;
begin
  if not Assigned(vGUILayoutEditor) then
    vGUILayoutEditor := Tlayouts_form.Create(nil);
  Result := vGUILayoutEditor;
end;

procedure ReleaseGUILayoutEditor;
begin
  if Assigned(vGUILayoutEditor) then
  begin
    vGUILayoutEditor.Free;
    vGUILayoutEditor := nil;
  end;
end;

function SnapPoint(X, Y: integer): TPoint;
begin
  Result.X := (X div zoom) * zoom;
  Result.Y := (Y div zoom) * zoom;
end;

procedure TLayouts_Form.SetEnabledSpins(Value: Boolean);
begin
  left_edit.Enabled := Value;
  top_edit.Enabled := Value;
  height_edit.Enabled := Value;
  width_edit.Enabled := Value;
end;

procedure TLayouts_Form.SyncImages;
begin
  Image2.Width := Image1.Width;
  Image2.Height := Image1.Height;
  Image2.Picture.Bitmap.Width := Image1.Width;
  Image2.Picture.Bitmap.Height := Image1.Height;
  PaintBox1.Width := Image1.Width;
  PaintBox1.Height := Image1.Height;
  ScrollBox1.HorzScrollBar.Range := Image1.Width;
  ScrollBox1.VertScrollBar.Range := Image1.Height;
  PaintBox1.Canvas.CopyRect(PaintBox1.Canvas.ClipRect,
    Image1.Canvas, Image1.Canvas.ClipRect);
end;

procedure TLayouts_Form.DrawCurrentElement;
begin
  with elements_grid do
    if (items_list.ItemIndex > -1) and (sorted_elements[Col + 3 * Row] <> nil)
      then
      with sorted_elements[Col + 3 * Row], Image2.Canvas do
      begin
        FillRect(ClipRect);
        Rectangle(Rect(zoom * Round(TopLeft.X), zoom * Round(TopLeft.Y),
          zoom * Round(BottomRight.X), zoom * Round(BottomRight.Y)));
      end;
end;

procedure TLayouts_Form.Open_Image_ButtonClick(Sender: TObject);
var
  LFileName: string;
begin
  LFileName := '';
  if OpenPictureDialog(LFileName) then
    try
      Image1.Stretch := false;
      Image1.AutoSize := true;
      Image1.Picture.LoadFromFile(LFileName);
      Image1.AutoSize := false;
      Image1.Stretch := true;
      Image2.Canvas.Pen.Width := 1;
      SyncImages;
      zoom := 1;
    except
      Application.MessageBox('Unable to load picture!', 'Error', MB_ICONERROR);
    end;
end;

procedure TLayouts_Form.RefreshComponentBox;
var
  i: integer;
begin
  items_list.Clear;
  for i := 0 to GLGuiLayout1.GuiComponents.Count - 1 do
    items_list.Items.Add(GLGuiLayout1.GuiComponents[i].Name);
  items_list.ItemIndex := 0;
  items_listClick(nil);
end;

procedure TLayouts_Form.Open_ButtonClick(Sender: TObject);

begin
  case Application.MessageBox('Save layout?',
    'Question', MB_ICONQUESTION + MB_YESNOCANCEL) of
    mrYes: save_buttonClick(nil);
    mrCancel: Exit;
  end;

  if OpenDialog1.Execute then
    try
      GLGuiLayout1.Clear;
      GLGuiLayout1.LoadFromFile(OpenDialog1.FileName);
      RefreshComponentBox;
    except
      Application.MessageBox('Unable to load layout!', 'Error', MB_ICONERROR);
    end;
end;

procedure TLayouts_Form.Save_ButtonClick(Sender: TObject);
begin
  if SaveDialog1.FileName = '' then
    if SaveDialog1.Execute then
      GLGuiLayout1.SaveToFile(SaveDialog1.FileName)
    else
  else
    GLGuiLayout1.SaveToFile(SaveDialog1.FileName);
end;

procedure TLayouts_Form.FormCreate(Sender: TObject);
begin
  rect_point1.X := -1;
  Image2.Canvas.FillRect(Image2.Canvas.ClipRect);
  Image2.Canvas.Pen.Color := clAqua;
end;

function TLayouts_Form.GetEnabledSpins: Boolean;
begin
  Result := left_edit.Enabled;
end;

procedure TLayouts_Form.Image1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  x_label.Caption := 'X: ' + IntToStr(X div zoom);
  y_label.Caption := 'Y: ' + IntToStr(Y div zoom);

  if not (ssRight in Shift) then
    Exit;
  if (X < 0) or (Y < 0) or (X > Image2.Width) or (Y > Image2.Height) then
    Exit;

  if rect_point1.X < 0 then
    rect_point1 := SnapPoint(X, Y)
  else
    with Image2.Canvas do
    begin
      FillRect(ClipRect);
      rect_point2 := SnapPoint(X, Y);
      Rectangle(rect_point1.x, rect_point1.y, X, Y);
    end;

  if items_list.ItemIndex = -1 then
    Exit;
  if rect_point1.X < rect_point2.X then
    left_edit.Value := rect_point1.X div zoom
  else
    left_edit.Value := rect_point2.X div zoom;
  if rect_point1.Y > rect_point2.Y then
    top_edit.Value := rect_point2.Y div zoom
  else
    top_edit.Value := rect_point1.Y div zoom;

  width_edit.Value := Abs(rect_point2.X - rect_point1.X) div zoom;
  height_edit.Value := Abs(rect_point2.Y - rect_point1.Y) div zoom;
end;

procedure TLayouts_Form.Image1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssRight in Shift) then
    Exit;
  with Image2.Canvas do
  begin
    FillRect(ClipRect);
    rect_point1 := SnapPoint(X, Y);
    rect_point2 := rect_point1;
  end;
end;

procedure TLayouts_Form.Image1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssRight in Shift) then
    Exit;
  rect_point1.X := -1;
end;

procedure TLayouts_Form.Add_ButtonClick(Sender: TObject);
var
  i: integer;
begin
  with TGLGuiComponent(GLGuiLayout1.GuiComponents.Add) do
  begin
    Name := InputBox('Question', 'Name of new region:', '');
    if Name = '' then
    begin
      Free;
      Exit;
    end;
    items_list.Items.Add(Name);
    items_list.ItemIndex := items_list.Count - 1;
    for i := 0 to 9 do
    begin
      sorted_elements[i] := TGLGuiElement(Elements.Add);
      sorted_elements[i].Align := TGUIAlignments(i);
    end;
    name_edit.Text := Name;
  end;
end;

procedure TLayouts_Form.Delete_ButtonClick(Sender: TObject);
begin
  if items_list.ItemIndex = -1 then
    Exit;
  GLGuiLayout1.GuiComponents.Delete(items_list.ItemIndex);
{$IFDEF FPC}
  items_list.ClearSelection;
{$ELSE}
  items_list.DeleteSelected;
{$ENDIF}
  if items_list.ItemIndex > -1 then
    name_edit.Text := GLGuiLayout1.GuiComponents[items_list.ItemIndex].Name
  else
    name_edit.Text := '';
end;

procedure TLayouts_Form.Items_ListClick(Sender: TObject);
var
  i, p: integer;
begin
  if items_list.ItemIndex = -1 then
    Exit;

  name_edit.Text := GLGuiLayout1.GuiComponents[
    items_list.ItemIndex].Name;
  elements_grid.Row := 0;
  elements_grid.Col := 0;
  for i := 0 to Length(sorted_elements) - 1 do
  begin
    sorted_elements[i] := nil;
    if I < 9 then
      elements_grid.Cells[i mod 3, i div 3] := #32;
  end;

  with GLGuiLayout1.GuiComponents[items_list.ItemIndex] do
    for i := 0 to Elements.Count - 1 do
    begin
      p := Integer(Elements[i].Align);
      sorted_elements[p] := Elements[i];
      elements_grid.Cells[p mod 3, p div 3] := '+';
    end;
  elements_gridClick(nil);
end;

procedure TLayouts_Form.Name_EditExit(Sender: TObject);
begin
  if items_list.ItemIndex > -1 then
  begin
    GLGuiLayout1.GuiComponents[items_list.
      ItemIndex].Name := name_edit.Text;
    items_list.Items[items_list.ItemIndex] := name_edit.Text;
    items_listClick(nil);
  end;
end;

procedure TLayouts_Form.Name_EditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    name_editExit(nil);
end;

procedure TLayouts_Form.Elements_GridClick(Sender: TObject);
begin
  with elements_grid do
    if (items_list.ItemIndex > -1) and (sorted_elements[Col + 3 * Row] <> nil)
      then
      with sorted_elements[Col + 3 * Row] do
      begin
        EnabledSpins := True;
        left_edit.Value := Round(TopLeft.X);
        top_edit.Value := Round(TopLeft.Y);
        width_edit.Value := Round(BottomRight.X - TopLeft.X);
        height_edit.Value := Round(BottomRight.Y - TopLeft.Y);
        DrawCurrentElement;
      end
    else
    begin
      EnabledSpins := False;
      Image2.Canvas.FillRect(Image2.Canvas.ClipRect);
    end;
end;

procedure TLayouts_Form.Elements_GridDblClick(Sender: TObject);
var
  I: Integer;
  E: TGLGuiElement;
begin
  if items_list.ItemIndex > -1 then
    with elements_grid do
    begin
      if Assigned(sorted_elements[Col + 3 * Row]) then
      begin
        I := GLGuiLayout1.GuiComponents[items_list.ItemIndex].Elements.IndexOf(sorted_elements[Col + 3 * Row]);
        GLGuiLayout1.GuiComponents[items_list.ItemIndex].Elements.Delete(I);
        sorted_elements[Col + 3 * Row] := nil;
        Cells[Col, Row] := #32;
        elements_gridClick(nil);
      end
      else begin
        E := TGLGuiElement(GLGuiLayout1.GuiComponents[items_list.ItemIndex].Elements.Add);
        E.Align := TGUIAlignments(Col + 3 * Row);
        sorted_elements[Col + 3 * Row] := E;
        Cells[Col, Row] := '+';
        elements_gridClick(nil);
      end;
    end;
end;

procedure TLayouts_Form.Left_EditChange(Sender: TObject);
begin
  if (items_list.ItemIndex = -1) or not EnabledSpins then
    Exit;
  if left_edit.Value + width_edit.Value > Image2.Width div zoom then
    left_edit.Value := (Image2.Width div zoom) - width_edit.Value;
  if left_edit.Value < 0 then
    left_edit.Value := 0;
  with elements_grid do
    sorted_elements[Col + 3 * Row].TopLeft.X := left_edit.Value;
  DrawCurrentElement;
end;

procedure TLayouts_Form.Top_EditChange(Sender: TObject);
begin
  if (items_list.ItemIndex = -1) or not EnabledSpins then
    Exit;
  if top_edit.Value + height_edit.Value > Image2.Height div zoom then
    top_edit.Value := (Image2.Height div zoom) - height_edit.Value;
  if top_edit.Value < 0 then
    top_edit.Value := 0;
  with elements_grid do
    sorted_elements[Col + 3 * Row].TopLeft.Y := top_edit.Value;
  DrawCurrentElement;
end;

procedure TLayouts_Form.Width_EditChange(Sender: TObject);
begin
  if (items_list.ItemIndex = -1) or not EnabledSpins then
    Exit;
  with elements_grid do
    sorted_elements[Col + 3 * Row].BottomRight.X := left_edit.Value +
      width_edit.Value;
  if left_edit.Value + width_edit.Value > Image2.Width div zoom then
    width_edit.Value := (Image2.Width div zoom) - left_edit.Value;
  if width_edit.Value < 0 then
    width_edit.Value := 0;
  DrawCurrentElement;
end;

procedure TLayouts_Form.Height_EditChange(Sender: TObject);
begin
  if (items_list.ItemIndex = -1) or not EnabledSpins  then
    Exit;
  with elements_grid do
    sorted_elements[Col + 3 * Row].BottomRight.Y := top_edit.Value +
      height_edit.Value;
  if top_edit.Value + height_edit.Value > Image2.Height div zoom then
    height_edit.Value := (Image2.Height div zoom) - top_edit.Value;
  if height_edit.Value < 0 then
    height_edit.Value := 0;
  DrawCurrentElement;
end;

procedure TLayouts_Form.BitBtnZoomInClick(Sender: TObject);
begin
  if zoom + TBitBtn(Sender).Tag < 1 then
    Exit;
  Image1.Width := (Image1.Width div zoom) * (zoom + TBitBtn(Sender).Tag);
  Image1.Height := (Image1.Height div zoom) * (zoom + TBitBtn(Sender).Tag);
  SyncImages;
  zoom := zoom + TBitBtn(Sender).Tag;
  Image2.Canvas.Pen.Width := zoom;
  elements_gridClick(nil);
end;

procedure TLayouts_Form.BitBtnShowClick(Sender: TObject);
{$IFDEF LINUX}
var
  lProcess: TProcess;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  ShellExecuteW(0, 'open', 'mspaint', '', '', SW_SHOW)
{$ENDIF}
{$IFDEF LINUX}
  lProcess := TProcess.Create(nil);
  lProcess.CommandLine := 'gimp';
  try
    lProcess.Execute;
  finally
    lProcess.Destroy;
  end;
{$ENDIF}
end;

procedure TLayouts_Form.Execute(AGUILayout: TGLGuiLayout);
begin
  GLGuiLayout1.Assign(AGUILayout);
  Image1.Stretch := false;
  Image1.AutoSize := true;
  Image1.Picture.Assign(AGUILayout.Material.GetActualPrimaryTexture.Image.GetBitmap32.Create32BitsBitmap);
  Image1.AutoSize := false;
  Image1.Stretch := true;
  Image2.Canvas.Pen.Width := 1;
  SyncImages;
  zoom := 1;

  RefreshComponentBox;
  if ShowModal = mrOk then
    AGUILayout.Assign(GLGuiLayout1);
end;

initialization

finalization

  ReleaseGUILayoutEditor;

end.

