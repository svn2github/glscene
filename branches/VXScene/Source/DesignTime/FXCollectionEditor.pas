//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  Edits a TVXXCollection
}
unit FXCollectionEditor;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Rtti,
  System.Variants,
  System.Actions,
  System.TypInfo,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.ListView.Types,
  FMX.ActnList,
  FMX.Menus,
  FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base,
  FMX.Controls.Presentation,
  FMX.ListView,
  VXS.Strings,
  VXS.Scene,
  VXS.CrossPlatform,
  VXS.XCollection,
  VXS.Behaviours,
  VXS.MaterialEx;

type
  IDesigner = interface //in designintf -> (IDesigner200)
    ['{93F3FCBC-968E-45A9-9641-609E8FB3AC60}']
    function CreateChild(ComponentClass: TComponentClass; Parent: TComponent): TComponent;
  end;

  TInstProp = record
    Instance: TPersistent;
    PropInfo: PPropInfo;
  end;

  PInstPropList = ^TInstPropList;
  TInstPropList = array[0..1023] of TInstProp;

type
  TVXXCollectionEditor = class(TForm)
    ToolBar1: TToolBar;
    TBAdd: TButton;
    ListView: TListView;
    PMListView: TPopupMenu;
    PMToolBar: TPopupMenu;
    ActionList: TActionList;
    ACRemove: TAction;
    ACMoveUp: TAction;
    ACMoveDown: TAction;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure ACRemoveExecute(Sender: TObject);
    procedure ACMoveUpExecute(Sender: TObject);
    procedure ACMoveDownExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure ListViewChange(Sender: TObject);
  private
    FXCollection: TVXXCollection;
    // ownerComponent : TComponent;
    FDesigner: IDesigner;
    UpdatingListView: Boolean;
    procedure PrepareListView;
    procedure PrepareXCollectionItemPopup(parent: TMenuItem);
    procedure OnAddXCollectionItemClick(Sender: TObject);
    procedure OnNameChanged(Sender: TObject);
    procedure OnXCollectionDestroyed(Sender: TObject);
  public
    procedure SetXCollection(aXCollection: TVXXCollection; designer: IDesigner);
  end;

function XCollectionEditor: TVXXCollectionEditor;
procedure ReleaseXCollectionEditor;

//=======================================================================
implementation
//=======================================================================

{$R *.fmx}

var
  vXCollectionEditor: TVXXCollectionEditor;

function XCollectionEditor: TVXXCollectionEditor;
begin
  if not Assigned(vXCollectionEditor) then
    vXCollectionEditor := TVXXCollectionEditor.Create(nil);
  Result := vXCollectionEditor;
end;

procedure ReleaseXCollectionEditor;
begin
  if Assigned(vXCollectionEditor) then
  begin
    vXCollectionEditor.Release;
    vXCollectionEditor := nil;
  end;
end;

procedure TVXXCollectionEditor.FormCreate(Sender: TObject);
begin
  RegisterGLBehaviourNameChangeEvent(OnNameChanged);
  RegisterGLMaterialExNameChangeEvent(OnNameChanged);
  RegisterXCollectionDestroyEvent(OnXCollectionDestroyed);
end;

procedure TVXXCollectionEditor.FormDestroy(Sender: TObject);
begin
  DeRegisterGLBehaviourNameChangeEvent(OnNameChanged);
  DeRegisterGLMaterialExNameChangeEvent(OnNameChanged);
  DeRegisterXCollectionDestroyEvent(OnXCollectionDestroyed);
end;

procedure TVXXCollectionEditor.FormHide(Sender: TObject);
begin
  SetXCollection(nil, nil);
  ReleaseXCollectionEditor;
end;

procedure TVXXCollectionEditor.SetXCollection(aXCollection: TVXXCollection;
  designer: IDesigner);
begin
  // if Assigned(ownerComponent) then
  // ownerComponent.RemoveFreeNotification(Self);
  FXCollection := aXCollection;
  FDesigner := designer;
  if Assigned(FXCollection) then
  begin
    // if Assigned(FXCollection.Owner) and (FXCollection.Owner is TComponent) then
    // ownerComponent:=TComponent(FXCollection.Owner);
    // if Assigned(ownerComponent) then
    // ownerComponent.FreeNotification(Self);
    Caption := FXCollection.GetNamePath;
  end
  else
  begin
    // ownerComponent:=nil;
    Caption := strXCollectionEditor;
  end;
  PrepareListView;
end;

procedure TVXXCollectionEditor.ListViewChange(Sender: TObject);
var
  sel: Boolean;
begin
  if (*(Change = ctState) and *)Assigned(FDesigner) and (not updatingListView) then
  begin
    // setup enablings
    sel := (ListView.Selected <> nil);
    TBAdd.Enabled := Assigned(FDesigner);
    ACRemove.Enabled := sel;
    ACMoveUp.Enabled := sel and (ListView.Selected.Index > 0);
    ACMoveDown.Enabled := sel and
      (ListView.Selected.Index < ListView.Items.Count - 1);
    if Assigned(FDesigner) then
    { TODO : E2003 Undeclared identifier: 'SelectComponent' }
    (*
      if sel then
        FDesigner.SelectComponent(TVXXCollectionItem(ListView.Selected.Data))
      else
        FDesigner.SelectComponent(nil);
     *)
  end;
end;

procedure TVXXCollectionEditor.ACMoveDownExecute(Sender: TObject);
begin
  if ListView.Selected <> nil then
  begin
    /// to do
  end;
end;

procedure TVXXCollectionEditor.ACMoveUpExecute(Sender: TObject);
begin
  if ListView.Selected <> nil then
  begin
    /// to do
  end;
end;

procedure TVXXCollectionEditor.ACRemoveExecute(Sender: TObject);
begin
  if ListView.Selected <> nil then
  begin
    /// to do
  end;
end;

procedure TVXXCollectionEditor.OnAddXCollectionItemClick(Sender: TObject);
begin
  /// to do
end;

procedure TVXXCollectionEditor.OnNameChanged(Sender: TObject);
begin
  /// to do
end;

procedure TVXXCollectionEditor.OnXCollectionDestroyed(Sender: TObject);
begin
  if TVXXCollection(Sender) = FXCollection then
    Close;
end;

procedure TVXXCollectionEditor.PrepareListView;
var
  i: Integer;
  prevSelData: TValue;   // prev Pointer;
  XCollectionItem: TVXXCollectionItem;
  DisplayedName: String;
begin
  Assert(Assigned(ListView));
  updatingListView := True;
  try
    if ListView.Selected <> nil then
///in VCL   prevSelData := ListView.Selected.Data[DisplayedName]
      prevSelData := ListView.Selected.FieldAddress(DisplayedName)
    else
      prevSelData := nil;
    with ListView.Items do
    begin
      BeginUpdate;
      Clear;
      if Assigned(FXCollection) then
      begin
        for i := 0 to FXCollection.Count - 1 do
          with Add do
          begin
            XCollectionItem := FXCollection[i];
            DisplayedName := XCollectionItem.Name;
            if DisplayedName = '' then
              DisplayedName := '(unnamed)';
            Caption := Format('%d - %s', [i, DisplayedName]);
            { TODO : E2003 Undeclared identifier: 'SubItems' }
            (*SubItems.Add(XCollectionItem.FriendlyName);*)
            Data[DisplayedName] := XCollectionItem;
          end;
        { TODO : E2015 Operator not applicable to this operand type }
        (*
        if prevSelData <> nil then
          ListView.Selected := ListView.FindData(0, prevSelData, True, False);
        *)
      end;
      EndUpdate;
    end;
  finally
    updatingListView := False;
  end;
  ListViewChange(Self);
end;

procedure TVXXCollectionEditor.PrepareXCollectionItemPopup(parent: TMenuItem);
var
  i: Integer;
  list: TList;
  XCollectionItemClass: TVXXCollectionItemClass;
  mi, categoryItem: TMenuItem;
begin
  list := GetXCollectionItemClassesList(FXCollection.ItemsClass);
  try
    parent.Clear;
    for i := 0 to list.Count - 1 do
    begin
      XCollectionItemClass := TVXXCollectionItemClass(list[i]);
      if XCollectionItemClass.ItemCategory <> '' then
      begin
        { TODO : E2003 Undeclared identifier: 'Find' }
        (*categoryItem := parent.Find(XCollectionItemClass.ItemCategory);*)
        if categoryItem = nil then
        begin
          categoryItem := TMenuItem.Create(owner);
          categoryItem.Text := XCollectionItemClass.ItemCategory;
          parent.AddObject(categoryItem);
        end;
      end
      else
        categoryItem := parent;
      mi := TMenuItem.Create(owner);
      mi.Text := XCollectionItemClass.FriendlyName;
      mi.OnClick := OnAddXCollectionItemClick;
      mi.Tag := Integer(XCollectionItemClass);
      mi.Enabled := Assigned(FXCollection) and
        FXCollection.CanAdd(XCollectionItemClass);
      categoryItem.AddObject(mi);
    end;
  finally
    list.Free;
  end;
end;

end.
