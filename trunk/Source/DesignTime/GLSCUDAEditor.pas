//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSCUDAEditor<p>

   Editor of TGLSCUDA.<p>

	<b>History : </b><font size=-1><ul>
      <li>19/03/10 - Yar - Creation
	</ul></font>
}
unit GLSCUDAEditor;

interface

uses
{$IFDEF MSWINDOWS}Registry, {$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, StdCtrls, ComCtrls, ToolWin,

  DesignIntf, VCLEditors,
  GLSCUDA, GLSCUDAFFTPlan, GLSCUDAGraphics;

type
  TGLSCUDAEditorForm = class(TForm)
    ToolBar1: TToolBar;
    AddModuleButton: TToolButton;
    DeleteButton: TToolButton;
    ListBox1: TListBox;
    ImageList1: TImageList;
    AddMemDataButton: TToolButton;
    AddFFTPlanButton: TToolButton;
    AddGeometryResButton: TToolButton;
    AddImageResButton: TToolButton;
    procedure AddItemButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FCUDA: TGLSCUDA;
    FCurrentDesigner: IDesigner;
  protected
    { Protected declaration }
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    { Public declarations }
    procedure SetCUDAEditorClient(Client: TGLSCUDA; Designer: IDesigner);

  end;

function GLSCUDAEditorForm: TGLSCUDAEditorForm;
procedure ReleaseGLSCUDAEditorForm;

implementation

{$R *.dfm}

resourcestring
  cCUDAEditor = 'GLScene CUDA Items Editor';

const
  cRegistryKey = 'Software\GLScene.org\GLSCUDAEditor';

var
  vGLSCUDAEditorForm: TGLSCUDAEditorForm;

function GLSCUDAEditorForm: TGLSCUDAEditorForm;
begin
  if not Assigned(vGLSCUDAEditorForm) then
    vGLSCUDAEditorForm := TGLSCUDAEditorForm.Create(nil);
  Result := vGLSCUDAEditorForm;
end;

procedure ReleaseGLSCUDAEditorForm;
begin
  if Assigned(vGLSCUDAEditorForm) then
  begin
    vGLSCUDAEditorForm.Free;
    vGLSCUDAEditorForm := nil;
  end;
end;

{$IFDEF MSWINDOWS}

function ReadRegistryInteger(reg: TRegistry; const name: string;
  defaultValue: Integer): Integer;
begin
  if reg.ValueExists(name) then
    Result := reg.ReadInteger(name)
  else
    Result := defaultValue;
end;
{$ENDIF}

procedure TGLSCUDAEditorForm.AddItemButtonClick(Sender: TObject);
var
  obj: TCUDABaseItem;
begin
  if Assigned(FCurrentDesigner) then
  begin
    obj :=
      TCUDABaseItem(FCurrentDesigner.CreateComponent(TCUDABaseItemClass(TToolButton(Sender).Tag), FCUDA, 0,
      0, 0, 0));
    obj.Master := FCUDA;
    ListBox1.AddItem(obj.Name, obj);
    FCurrentDesigner.Modified;
  end;
end;

procedure TGLSCUDAEditorForm.DeleteButtonClick(Sender: TObject);
var
  obj: TCUDABaseItem;
  i: Integer;
begin
  if ListBox1.SelCount = 0 then
    exit;
  for i := 0 to ListBox1.Count - 1 do
  begin
    if ListBox1.Selected[i] then
    begin
      obj := TCUDABaseItem(ListBox1.Items.Objects[i]);
      obj.Destroy;
    end;
  end;
  ListBox1.DeleteSelected;
  FCurrentDesigner.Modified;
end;

procedure TGLSCUDAEditorForm.FormCreate(Sender: TObject);
var
{$IFDEF MSWINDOWS}
  reg: TRegistry;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  reg := TRegistry.Create;
  try
    if reg.OpenKey(cRegistryKey, True) then
    begin
      Left := ReadRegistryInteger(reg, 'Left', Left);
      Top := ReadRegistryInteger(reg, 'Top', Top);
      Width := ReadRegistryInteger(reg, 'Width', 250);
      Height := ReadRegistryInteger(reg, 'Height', Height);
    end;
  finally
    reg.Free;
  end;
{$ENDIF}
  AddModuleButton.Tag := Integer(TCUDAModule);
  AddMemDataButton.Tag := Integer(TCUDAMemData);
  AddFFTPlanButton.Tag := Integer(TCUDAFFTPlan);
  AddGeometryResButton.Tag := Integer(TCUDAGLGeometryResource);
  AddImageResButton.Tag := Integer(TCUDAGLImageResource);
end;

procedure TGLSCUDAEditorForm.FormDestroy(Sender: TObject);
{$IFDEF MSWINDOWS}
var
  reg: TRegistry;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  reg := TRegistry.Create;
  try
    if reg.OpenKey(cRegistryKey, True) then
    begin
      reg.WriteInteger('Left', Left);
      reg.WriteInteger('Top', Top);
      reg.WriteInteger('Width', Width);
      reg.WriteInteger('Height', Height);
    end;
  finally
    reg.Free;
  end;
{$ENDIF}
end;

procedure TGLSCUDAEditorForm.ListBox1Click(Sender: TObject);
var
  obj: TCUDABaseItem;
  i: Integer;
begin
  if not Assigned(FCurrentDesigner) then
    exit;
  obj := nil;
  if ListBox1.SelCount = 1 then
    for i := 0 to ListBox1.Count - 1 do
    begin
      if ListBox1.Selected[i] then
      begin
        obj := TCUDABaseItem(ListBox1.Items.Objects[i]);
        break;
      end;
    end;
  if Assigned(obj) then
    FCurrentDesigner.SelectComponent(obj);
end;

procedure TGLSCUDAEditorForm.SetCUDAEditorClient(Client: TGLSCUDA; Designer: IDesigner);
var
  i: Integer;
  child: TCUDABaseItem;
begin
  if Assigned(FCUDA) then
    FCUDA.RemoveFreeNotification(Self);
  FCUDA := Client;
  FCurrentDesigner := Designer;
  ListBox1.Clear;
  if Assigned(FCUDA) then
  begin
    FCUDA.FreeNotification(Self);
    Caption := cCUDAEditor + ' : ' + FCUDA.Name;
    for i := 0 to FCUDA.ItemsCount - 1 do
    begin
      child := FCUDA.Items[i];
      ListBox1.AddItem(child.Name, child);
    end;
  end
  else
    Caption := cCUDAEditor;
end;

procedure TGLSCUDAEditorForm.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if (FCUDA = AComponent) and (Operation = opRemove) then
  begin
    FCUDA := nil;
    SetCUDAEditorClient(nil, nil);
  end;
  inherited;
end;

initialization

finalization

  ReleaseGLSCUDAEditorForm;

end.

