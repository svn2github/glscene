{: GLSceneEdit<p>

   Handles all the color and texture stuff.<p>

	<b>History : </b><font size=-1><ul>
      <li>31/06/03 - EG - Cosmetic changes, form position/state now saved to the
                          registry 
      <li>21/06/03 - DanB - Added behaviours/effects listviews
      <li>22/01/02 - EG - Fixed controls state after drag/drop (Anton Zhuchkov)
      <li>06/08/00 - EG - Added basic Clipboard support
      <li>14/05/00 - EG - Added workaround for VCL DesignInfo bug (thx Nelson Chu)
      <li>28/04/00 - EG - Fixed new objects not being immediately reco by IDE
      <li>26/04/00 - EG - Added support for objects categories
		<li>17/04/00 - EG - Added access to TInfoForm
		<li>16/04/00 - EG - Fixed occasionnal crash when rebuilding GLScene dpk
									while GLSceneEdit is visible
      <li>10/04/00 - EG - Minor Create/Release change
      <li>24/03/00 - EG - Fixed SetScene not updating enablings
		<li>13/03/00 - EG - Object names (ie. node text) is now properly adjusted
									when a GLScene object is renamed,
									Added Load/Save whole scene
      <li>07/02/00 - EG - Fixed notification logic
      <li>06/02/00 - EG - DragDrop now starts after moving the mouse a little,
                           Form is now auto-creating, fixed Notification,
                           Added actionlist and moveUp/moveDown
      <li>05/02/00 - EG - Fixed DragDrop, added root nodes auto-expansion
   </ul></font>
}
unit GLSceneEdit;

interface

{$i GLScene.inc}
{$IFDEF LINUX}{$Message Error 'Unit not supported'}{$ENDIF LINUX}

uses
   XCollection, Registry,
   {$IFDEF GLS_CLX}
   QDialogs, QImgList, QActnList, QForms, QMenus, QTypes, QComCtrls, QControls, Types,
   {$ELSE}
   Controls, Windows, Forms, ComCtrls, ImgList, Dialogs, Menus, ActnList, ToolWin,
   {$ENDIF}
   GLScene, Classes, sysutils, ExtCtrls, StdCtrls,
   {$ifdef GLS_DELPHI_6_UP} DesignIntf {$else} DsgnIntf {$endif};

const
  SCENE_SELECTED=0;
  BEHAVIOURS_SELECTED=1;
  EFFECTS_SELECTED=2;
type
  TSetSubItemsEvent = procedure(Sender:TObject) of object;

  TGLSceneEditorForm = class(TForm)
    Tree: TTreeView;
    PopupMenu: TPopupMenu;
    MIAddCamera: TMenuItem;
    MIAddObject: TMenuItem;
    N1: TMenuItem;
    MIDelObject: TMenuItem;
    ToolBar: TToolBar;
    ActionList: TActionList;
    ToolButton1: TToolButton;
    TBAddObjects: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    PMToolBar: TPopupMenu;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ACAddCamera: TAction;
    ACAddObject: TAction;
    ImageList: TImageList;
    ACDeleteObject: TAction;
    ACMoveUp: TAction;
    ACMoveDown: TAction;
    N2: TMenuItem;
    Moveobjectup1: TMenuItem;
    Moveobjectdown1: TMenuItem;
    ACSaveScene: TAction;
    ACLoadScene: TAction;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ToolButton2: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton11: TToolButton;
    ACInfo: TAction;
    ACCopy: TAction;
    ACCut: TAction;
    ACPaste: TAction;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Cut1: TMenuItem;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    PABehaviours: TPanel;
    BehavioursListView: TListView;
    Splitter3: TSplitter;
    EffectsListView: TListView;
    Splitter: TSplitter;
    PMBehavioursToolbar: TPopupMenu;
    ACAddBehaviour: TAction;
    MIAddBehaviour: TMenuItem;
    MIAddEffect: TMenuItem;
    N3: TMenuItem;
    ACDeleteBehaviour: TAction;
    BehavioursPopupMenu: TPopupMenu;
    Delete1: TMenuItem;
    MoveUp1: TMenuItem;
    MoveDown1: TMenuItem;
    N4: TMenuItem;
    Label1: TLabel;
    Label2: TLabel;
    PMEffectsToolbar: TPopupMenu;
    ACAddEffect: TAction;
    ToolBar1: TToolBar;
    TBAddBehaviours: TToolButton;
    TBAddEffects: TToolButton;
    TBEffectsPanel: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure TreeEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure TreeEdited(Sender: TObject; Node: TTreeNode; var S: String);
    procedure TreeDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure TreeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeEnter(Sender: TObject);
    procedure TreeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ACAddCameraExecute(Sender: TObject);
    procedure ACDeleteObjectExecute(Sender: TObject);
    procedure ACMoveUpExecute(Sender: TObject);
    procedure ACMoveDownExecute(Sender: TObject);
    procedure ACAddObjectExecute(Sender: TObject);
    procedure ACSaveSceneExecute(Sender: TObject);
    procedure ACLoadSceneExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ACInfoExecute(Sender: TObject);
    procedure ACCopyExecute(Sender: TObject);
    procedure ACCutExecute(Sender: TObject);
    procedure ACPasteExecute(Sender: TObject);
    procedure BehavioursListViewEnter(Sender: TObject);
    procedure EffectsListViewEnter(Sender: TObject);

    procedure ACAddBehaviourExecute(Sender: TObject);
    procedure DeleteBaseBehaviour(ListView:TListView);
    procedure PMBehavioursToolbarPopup(Sender: TObject);
    procedure PMEffectsToolbarPopup(Sender: TObject);
    procedure BehavioursListViewSelectItem(Sender: TObject;
      Item: TListItem; Selected: Boolean);
    procedure ACAddEffectExecute(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure TBEffectsPanelClick(Sender: TObject);

  private
    FSelectedItems:Integer; //

    FScene: TGLScene;
    FObjectNode, FCameraNode: TTreeNode;
    FCurrentDesigner: {$ifdef GLS_DELPHI_6_UP} IDesigner {$else} IFormDesigner {$endif};
    FLastMouseDownPos : TPoint;
    procedure ReadScene;
    procedure ResetTree;
    function AddNodes(ANode: TTreeNode; AObject: TGLBaseSceneObject): TTreeNode;
    procedure AddObjectClick(Sender: TObject);
    procedure AddBehaviourClick(Sender: TObject);
    procedure AddEffectClick(Sender: TObject);
    procedure SetObjectsSubItems(parent : TMenuItem);
    procedure SetXCollectionSubItems(parent : TMenuItem ; XCollection: TXCollection; Event:TSetSubItemsEvent);    
    procedure SetBehavioursSubItems(parent : TMenuItem; XCollection: TXCollection);
    procedure SetEffectsSubItems(parent : TMenuItem; XCollection: TXCollection);
    procedure OnBaseSceneObjectNameChanged(Sender : TObject);
    function IsValidClipBoardNode : Boolean;
    function IsPastePossible : Boolean;
    function CanPaste(obj, destination : TGLBaseSceneObject) : Boolean;
    function GetComponentFromClipBoard : TComponent;
    procedure RenameObjectToBeUnique(anObject:TGLBaseSceneObject);
    procedure ShowBehaviours(BaseSceneObject:TGLBaseSceneObject);
    procedure ShowEffects(BaseSceneObject:TGLBaseSceneObject);
    procedure ShowBehavioursAndEffects(BaseSceneObject:TGLBaseSceneObject);
    procedure EnableAndDisableActions();
  protected
	 procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    procedure SetScene(Scene: TGLScene; Designer: {$ifdef GLS_DELPHI_6_UP} IDesigner {$else} IFormDesigner {$endif});

  end;

function GLSceneEditorForm : TGLSceneEditorForm;
procedure ReleaseGLSceneEditorForm;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R *.DFM}

uses GLSceneRegister, GLStrings, Info, OpenGL12, ClipBrd, GLWin32Viewer;

resourcestring
   cGLSceneEditor = 'GLScene Editor';

const
   cRegistryKey = 'Software/GLScene.org/GLSceneEdit';

var
	vGLSceneEditorForm : TGLSceneEditorForm;

function GLSceneEditorForm : TGLSceneEditorForm;
begin
	if not Assigned(vGLSceneEditorForm) then
		vGLSceneEditorForm:=TGLSceneEditorForm.Create(nil);
	Result:=vGLSceneEditorForm;
end;

procedure ReleaseGLSceneEditorForm;
begin
	if Assigned(vGLSceneEditorForm) then begin
		vGLSceneEditorForm.Free;
      vGLSceneEditorForm:=nil;
   end;
end;

function ReadRegistryInteger(reg : TRegistry; const name : String;
                             defaultValue : Integer) : Integer;
begin
   if reg.ValueExists(name) then
      Result:=reg.ReadInteger(name)
   else Result:=defaultValue;
end;

// FindNodeByData
//
function FindNodeByData(treeNodes : TTreeNodes; data : Pointer;
								baseNode : TTreeNode = nil) : TTreeNode;
var
	n : TTreeNode;
begin
	Result:=nil;
	if Assigned(baseNode) then begin
		n:=baseNode.getFirstChild;
		while Assigned(n) do begin
			if n.Data=data then begin
				Result:=n; Break;
			end else	if n.HasChildren then begin
				Result:=FindNodeByData(treeNodes, data, n);
				if Assigned(Result) then Break;
			end;
			n:=baseNode.GetNextChild(n);
		end;
	end else begin
		n:=treeNodes.GetFirstNode;
		while Assigned(n) do begin
			if n.Data=data then begin
				Result:=n; Break;
			end else	if n.HasChildren then begin
				Result:=FindNodeByData(treeNodes, data, n);
				if Assigned(Result) then Break;
			end;
			n:=n.getNextSibling;
		end;
	end;
end;

//----------------- TGLSceneEditorForm ---------------------------------------------------------------------------------

// SetScene
//
procedure TGLSceneEditorForm.SetScene(Scene: TGLScene;
   Designer: {$ifdef GLS_DELPHI_6_UP} IDesigner {$else} IFormDesigner {$endif});
begin
   if Assigned(FScene) then
{$ifdef GLS_DELPHI_5_UP}
		FScene.RemoveFreeNotification(Self);
{$else}
		FScene.Notification(Self, opRemove);
{$endif}
	FScene:=Scene;
   FCurrentDesigner:=Designer;
   ResetTree;
   BehavioursListView.Items.Clear;
   EffectsListView.Items.Clear;

   if Assigned(FScene) then begin
      FScene.FreeNotification(Self);
      ReadScene;
      Caption:=cGLSceneEditor+' : '+FScene.Name;
   end else Caption:=cGLSceneEditor;
   TreeChange(Self, nil);
   if Assigned(FScene) then
   begin
         Tree.Enabled:=true;
         BehavioursListView.Enabled:=true;
         EffectsListView.Enabled:=true;
      FSelectedItems:=SCENE_SELECTED;
      EnableAndDisableActions();
   end
   else begin
         Tree.Enabled:=false;
         BehavioursListView.Enabled:=false;
         EffectsListView.Enabled:=false;
         ACLoadScene.Enabled:=False;
         ACSaveScene.Enabled:=False;
         ACAddCamera.Enabled:=False;
         ACAddObject.Enabled:=False;
         ACAddBehaviour.Enabled:=False;
         ACAddEffect.Enabled:=False;
         ACDeleteObject.Enabled:=False;
         ACMoveUp.Enabled:=False;
         ACMoveDown.Enabled:=False;
         ACCut.Enabled:=False;
         ACCopy.Enabled:=False;
         ACPaste.Enabled:=False;

   end;
   ShowBehavioursAndEffects(nil);
end;

// FormCreate
//
procedure TGLSceneEditorForm.FormCreate(Sender: TObject);
var
   CurrentNode: TTreeNode;
   reg : TRegistry;
begin
	RegisterGLBaseSceneObjectNameChangeEvent(OnBaseSceneObjectNameChanged);
   Tree.Images:=ObjectManager.ObjectIcons;
   Tree.Indent:=ObjectManager.ObjectIcons.Width;
   with Tree.Items do begin
      // first add the scene root
      CurrentNode:=Add(nil, glsSceneRoot);
      with CurrentNode do begin
         ImageIndex:=ObjectManager.SceneRootIndex;
         SelectedIndex:=ImageIndex;
      end;
      // next the root for all cameras
      FCameraNode:=AddChild(CurrentNode, glsCameraRoot);
      with FCameraNode do begin
         ImageIndex:=ObjectManager.CameraRootIndex;
         SelectedIndex:=ObjectManager.CameraRootIndex;
      end;
      // and the root for all objects
      FObjectNode:=AddChild(CurrentNode, glsObjectRoot);
      with FObjectNode do begin
         ImageIndex:=ObjectManager.ObjectRootIndex;
         SelectedIndex:=ObjectManager.ObjectRootIndex;
      end;
   end;
   // Build SubMenus
   SetObjectsSubItems(MIAddObject);
{$ifdef GLS_DELPHI_5_UP}
	MIAddObject.SubMenuImages:=ObjectManager.ObjectIcons;
{$endif}
   SetObjectsSubItems(PMToolBar.Items);
   PMToolBar.Images:=ObjectManager.ObjectIcons;

   SetBehavioursSubItems(MIAddBehaviour,nil);
   SetBehavioursSubItems(PMBehavioursToolBar.Items,nil);
   SetEffectsSubItems(MIAddEffect,nil);
   SetEffectsSubItems(PMEffectsToolBar.Items,nil);

   reg:=TRegistry.Create;
   try
      if reg.OpenKey(cRegistryKey, True) then begin
         if reg.ValueExists('EffectsPanel') then
            TBEffectsPanel.Down:=reg.ReadBool('EffectsPanel');
         TBEffectsPanelClick(Self);
         Left:=ReadRegistryInteger(reg, 'Left', Left);
         Top:=ReadRegistryInteger(reg, 'Top', Top);
         Width:=ReadRegistryInteger(reg, 'Width', Width);
         Height:=ReadRegistryInteger(reg, 'Height', Height);
      end;
   finally
      reg.Free;
   end;
end;

// FormDestroy
//
procedure TGLSceneEditorForm.FormDestroy(Sender: TObject);
var
   reg : TRegistry;
begin
	DeRegisterGLBaseSceneObjectNameChangeEvent(OnBaseSceneObjectNameChanged);

   reg:=TRegistry.Create;
   try
      if reg.OpenKey(cRegistryKey, True) then begin
         reg.WriteBool('EffectsPanel', TBEffectsPanel.Down);
         reg.WriteInteger('Left', Left);
         reg.WriteInteger('Top', Top);
         reg.WriteInteger('Width', Width);
         reg.WriteInteger('Height', Height);
      end;
   finally
      reg.Free;
   end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGLSceneEditorForm.ReadScene;

var
  I: Integer;

begin
  Tree.Items.BeginUpdate;
  with FScene do
  begin
    if Assigned(Cameras) then
    begin
      FCameraNode.Data:=Cameras;
      for I:=0 to Cameras.Count - 1 do AddNodes(FCameraNode, Cameras[I]);
      FCameraNode.Expand(False);
    end;

    if Assigned(Objects) then
	 begin
      FObjectNode.Data:=Objects;
      with Objects do
        for I:=0 to Count - 1 do AddNodes(FObjectNode, Children[I]);
      FObjectNode.Expand(False);
    end;
  end;
  Tree.Items.EndUpdate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGLSceneEditorForm.ResetTree;
begin
   // delete all subtrees (empty tree)
   Tree.Items.BeginUpdate;
   try
      FCameraNode.DeleteChildren;
      FCameraNode.Data:=nil;
      with FObjectNode do begin
         DeleteChildren;
			Data:=nil;
         Parent.Expand(True);
      end;
   finally
      Tree.Items.EndUpdate;
   end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGLSceneEditorForm.AddNodes(ANode: TTreeNode; AObject: TGLBaseSceneObject): TTreeNode;

// adds the given scene object as well as its children to the tree structure and returns
// the last add node (e.g. for selection)

var
  I: Integer;
  CurrentNode: TTreeNode;

begin
  Result:=Tree.Items.AddChildObject(ANode, AObject.Name, AObject);
  Result.ImageIndex:=ObjectManager.GetImageIndex(TGLSceneObjectClass(AObject.ClassType));
  Result.SelectedIndex:=Result.ImageIndex;
  CurrentNode:=Result;
  for I:=0 to AObject.Count - 1 do Result:=AddNodes(CurrentNode, AObject[I]);
end;

procedure TGLSceneEditorForm.SetObjectsSubItems(parent : TMenuItem);
var
   objectList : TStringList;
   i, j : Integer;
   item, currentParent : TMenuItem;
   currentCategory : String;
   soc : TGLSceneObjectClass;
begin
   objectList:=TStringList.Create;
   try
      ObjectManager.GetRegisteredSceneObjects(objectList);
      for i:=0 to objectList.Count-1 do if objectList[i]<>'' then begin
         with ObjectManager do
            currentCategory:=GetCategory(TGLSceneObjectClass(objectList.Objects[i]));
         if currentCategory='' then
            currentParent:=parent
         else begin
            currentParent:=NewItem(currentCategory, 0, False, True, nil, 0, '');
            parent.Add(currentParent);
         end;
         for j:=i to objectList.Count-1 do if objectList[j]<>'' then with ObjectManager do begin
            soc:=TGLSceneObjectClass(objectList.Objects[j]);
            if currentCategory=GetCategory(soc) then begin
               item:=NewItem(objectList[j], 0, False, True, AddObjectClick, 0, '');
               item.ImageIndex:=GetImageIndex(soc);
               item.Tag:=Integer(soc);
               currentParent.Add(item);
               objectList[j]:='';
               if currentCategory='' then Break; 
            end;
         end;
      end;
	finally
      objectList.Free;
   end;
end;

procedure TGLSceneEditorForm.SetXCollectionSubItems(parent : TMenuItem ; XCollection: TXCollection; Event:TSetSubItemsEvent);
var
	i : Integer;
	list : TList;
	XCollectionItemClass : TXCollectionItemClass;
	mi : TMenuItem;
begin
        if Assigned(XCollection) then
        begin
	list:=GetXCollectionItemClassesList(XCollection.ItemsClass);
	try
{$ifdef GLS_DELPHI_5_UP}
		parent.Clear;
{$else}
		for i:=parent.Count-1 downto 0 do parent.Delete(i);
{$endif}
		for i:=0 to list.Count-1 do begin
			XCollectionItemClass:=TXCollectionItemClass(list[i]);
			mi:=TMenuItem.Create(owner);
			mi.Caption:=XCollectionItemClass.FriendlyName;
			mi.OnClick:=Event;//AddBehaviourClick;
			mi.Tag:=Integer(XCollectionItemClass);
                        if Assigned(XCollection) then
                          mi.Enabled:=XCollection.CanAdd(XCollectionItemClass)
                        else
                          mi.Enabled:=TBAddBehaviours.enabled;
			parent.Add(mi);
		end;
	finally
		list.Free;
	end;
        end
        else
{$ifdef GLS_DELPHI_5_UP}
          parent.Clear;
{$else}
		for i:=parent.Count-1 downto 0 do parent.Delete(i);
{$endif}

end;


// SetBehavioursSubItems
//
procedure TGLSceneEditorForm.SetBehavioursSubItems(parent : TMenuItem ; XCollection: TXCollection);
begin
  SetXCollectionSubItems(parent, XCollection, AddBehaviourClick);
end;

// SetEffectsSubItems
//
procedure TGLSceneEditorForm.SetEffectsSubItems(parent : TMenuItem ; XCollection: TXCollection);
begin
  SetXCollectionSubItems(parent, XCollection, AddEffectClick);
end;


procedure TGLSceneEditorForm.AddObjectClick(Sender: TObject);
var
   AParent, AObject: TGLBaseSceneObject;
   Node: TTreeNode;
begin
   if Assigned(FCurrentDesigner) then with Tree do
      if Assigned(Selected) and (Selected.Level > 0) then begin
         AParent:=TGLBaseSceneObject(Selected.Data);
       //  FCurrentDesigner.cr
         AObject:=TGLBaseSceneObject(FCurrentDesigner.CreateComponent(TGLSceneObjectClass(TMenuItem(Sender).Tag), AParent, 0, 0, 0, 0));
         TComponent(AObject).DesignInfo:=0;
         AParent.AddChild(AObject);
         Node:=AddNodes(Selected, AObject);
         Node.Selected:=True;
         FCurrentDesigner.Modified;
      end;
end;

procedure TGLSceneEditorForm.AddBehaviourClick(Sender: TObject);
var
	XCollectionItemClass : TXCollectionItemClass;
   AParent: TGLBaseSceneObject;
begin
        if Assigned(Tree.Selected) then
        begin
        AParent:=TGLBaseSceneObject(Tree.Selected.Data);
	XCollectionItemClass:=TXCollectionItemClass((Sender as TMenuItem).Tag);
	XCollectionItemClass.Create(AParent.Behaviours);
       	//PrepareListView;
        ShowBehaviours(AParent);
	//ListView.Selected:=ListView.FindData(0, XCollectionItem, True, False);
   FCurrentDesigner.Modified;
   end;
end;


procedure TGLSceneEditorForm.AddEffectClick(Sender: TObject);
var
	XCollectionItemClass : TXCollectionItemClass;
        AParent: TGLBaseSceneObject;
begin
        if Assigned(Tree.Selected) then
        begin

        AParent:=TGLBaseSceneObject(Tree.Selected.Data);
	XCollectionItemClass:=TXCollectionItemClass((Sender as TMenuItem).Tag);
	XCollectionItemClass.Create(AParent.Effects);
       	//PrepareListView;
        ShowEffects(AParent);
	//ListView.Selected:=ListView.FindData(0, XCollectionItem, True, False);
   FCurrentDesigner.Modified;
   end;
end;

procedure TGLSceneEditorForm.TreeDragOver(Sender, Source: TObject; X, Y: Integer;
                                          State: TDragState; var Accept: Boolean);
var
   Target : TTreeNode;
begin
   Accept:=False;
   if Source=Tree then with Tree do begin
      Target:=DropTarget;
      Accept:=Assigned(Target) and (Selected <> Target)
                and Assigned(Target.Data) and (not Target.HasAsParent(Selected));
   end;
end;

procedure TGLSceneEditorForm.TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
var
   SourceNode, DestinationNode: TTreeNode;
   SourceObject, DestinationObject: TGLBaseSceneObject;
begin
   if Assigned(FCurrentDesigner) then begin
      DestinationNode:=Tree.DropTarget;
      if Assigned(DestinationNode) and (Source = Tree) then begin
			SourceNode:=TTreeView(Source).Selected;
         SourceObject:=SourceNode.Data;
         DestinationObject:=DestinationNode.Data;
         DestinationObject.Insert(0, SourceObject);
         SourceNode.MoveTo(DestinationNode, naAddChildFirst);
         TreeChange(Self, nil);
         FCurrentDesigner.Modified;
      end;
   end;
end;

// Notification
//
procedure TGLSceneEditorForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (FScene=AComponent) and (Operation=opRemove) then begin
      FScene:=nil;
      SetScene(nil, nil);
	end;
	inherited;
end;

// OnBaseSceneObjectNameChanged
//
procedure TGLSceneEditorForm.OnBaseSceneObjectNameChanged(Sender : TObject);
var
	n : TTreeNode;
begin
	n:=FindNodeByData(Tree.Items, Sender);
	if Assigned(n) then
		n.Text:=(Sender as TGLBaseSceneObject).Name;
end;

// TreeChange
//
procedure TGLSceneEditorForm.TreeChange(Sender: TObject; Node: TTreeNode);
var
//   selNode : TTreeNode;
   BaseSceneObject1:TGLBaseSceneObject;
begin
   if Assigned(FCurrentDesigner) then begin

   if Node<>nil then
   begin
      BaseSceneObject1:=TGLBaseSceneObject(Node.Data);
      if BaseSceneObject1<>nil then
      begin
        ShowBehavioursAndEffects(BaseSceneObject1);
      end;
   end;

   EnableAndDisableActions();
   end;
end;

// TreeEditing
//
procedure TGLSceneEditorForm.TreeEditing(Sender: TObject; Node: TTreeNode;
                                         var AllowEdit: Boolean);
begin
   AllowEdit:=(Node.Level>1);
end;

procedure TGLSceneEditorForm.ShowBehaviours(BaseSceneObject:TGLBaseSceneObject);
var
  i:integer;
begin
      BehavioursListView.Items.Clear;
      BehavioursListView.Items.BeginUpdate;
      if Assigned(BaseSceneObject) then
      begin
      for i:=0 to BaseSceneObject.Behaviours.Count-1 do
      begin
        with BehavioursListView.Items.Add do
        begin
          Caption:=IntToStr(i)+' - '+BaseSceneObject.Behaviours[i].Name;
          Data:=BaseSceneObject.Behaviours[i];
        end;
      end;
      end;
      BehavioursListView.Items.EndUpdate;
end;

procedure TGLSceneEditorForm.ShowEffects(BaseSceneObject:TGLBaseSceneObject);
var
  i:integer;
begin
      EffectsListView.Items.Clear;
      EffectsListView.Items.BeginUpdate;
      if Assigned(BaseSceneObject) then
      begin
      for i:=0 to BaseSceneObject.Effects.Count-1 do
      begin
        with EffectsListView.Items.Add do
        begin
          caption:=IntToStr(i)+' - '+BaseSceneObject.Effects[i].Name;
          Data:=BaseSceneObject.Effects[i];
        end;
      end;
      end;
      EffectsListView.Items.EndUpdate;
end;


procedure TGLSceneEditorForm.ShowBehavioursAndEffects(BaseSceneObject:TGLBaseSceneObject);
begin
  ShowBehaviours(BaseSceneObject);
  ShowEffects(BaseSceneObject);
end;

// TreeEdited
//
procedure TGLSceneEditorForm.TreeEdited(Sender: TObject; Node: TTreeNode; var S: String);
var
  BaseSceneObject1:TGLBaseSceneObject;
begin
   if Assigned(FCurrentDesigner) then begin
      // renaming a node means renaming a scene object
      BaseSceneObject1:=TGLBaseSceneObject(Node.Data);
      if FScene.FindSceneObject(S)=nil then
        BaseSceneObject1.Name:=S
      else
      begin
        Messagedlg('A component named '+S+' already exists',mtWarning,[mbok],0);
        Node.Text:=BaseSceneObject1.Name;
      end;
      ShowBehavioursAndEffects(BaseSceneObject1);

      FCurrentDesigner.Modified;
   end;
end;

// TreeMouseDown
//
procedure TGLSceneEditorForm.TreeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   FLastMouseDownPos:=Point(X, Y);
end;

// TreeMouseMove
//
procedure TGLSceneEditorForm.TreeMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
   node: TTreeNode;
begin
   if Shift=[ssLeft] then begin
      node:=Tree.Selected;
      if Assigned(node) and (node.Level>1) then
         if (Abs(FLastMouseDownPos.x-x)>4) or (Abs(FLastMouseDownPos.y-y)>4) then
            Tree.BeginDrag(False);
   end;
end;

// TreeEnter
//
procedure TGLSceneEditorForm.TreeEnter(Sender: TObject);
begin
   if Assigned(FCurrentDesigner) and Assigned(Tree.Selected) then
      FCurrentDesigner.SelectComponent(Tree.Selected.Data);
   FSelectedItems:=SCENE_SELECTED;
   EnableAndDisableActions();
end;

// ACAddCameraExecute
//
procedure TGLSceneEditorForm.ACAddCameraExecute(Sender: TObject);
var
   AObject: TGLBaseSceneObject;
   Node: TTreeNode;
begin
   if Assigned(FCurrentDesigner) then begin
      AObject:=TGLBaseSceneObject(FCurrentDesigner.CreateComponent(TGLCamera, FScene.Cameras, 0, 0, 0, 0));
      FScene.Cameras.AddChild(AObject);
      Node:=AddNodes(FCameraNode, AObject);
      Node.Selected:=True;
      FCurrentDesigner.Modified;
   end;
end;

// ACDeleteObjectExecute
//
procedure TGLSceneEditorForm.ACDeleteObjectExecute(Sender: TObject);
var
	anObject : TGLBaseSceneObject;
   allowed, keepChildren : Boolean;
   confirmMsg : String;
   buttons : TMsgDlgButtons;
begin
  if FSelectedItems=BEHAVIOURS_SELECTED then
  begin
    DeleteBaseBehaviour(BehavioursListView);
    FCurrentDesigner.SelectComponent(Tree.Selected.data);
    ShowBehaviours(TGLBaseSceneObject(Tree.Selected.data));
  end
  else if FSelectedItems=EFFECTS_SELECTED then
  begin
    DeleteBaseBehaviour(EffectsListView);
    FCurrentDesigner.SelectComponent(Tree.Selected.data);
    ShowEffects(TGLBaseSceneObject(Tree.Selected.data));
  end
  else if FSelectedItems=SCENE_SELECTED then
  begin
	if Assigned(Tree.Selected) and (Tree.Selected.Level > 1) then begin
      anObject:=TGLBaseSceneObject(Tree.Selected.Data);
      // ask for confirmation
      if anObject.Name<>'' then
         ConfirmMsg:='Delete '+anObject.Name
      else ConfirmMsg:='Delete the marked object';
      buttons:=[mbOK, mbCancel];
      // are there children to care for?
      if anObject.Count>0 then begin
         confirmMsg:=ConfirmMsg+' only or with ALL its children?';
         buttons:=[mbAll]+Buttons;
      end else confirmMsg:=confirmMsg+'?';
      case MessageDlg(confirmMsg, mtConfirmation, buttons, 0) of
         mrAll : begin
            keepChildren:=False;
            allowed:=True;
			end;
         mrOK : begin
            keepChildren:=True;
            allowed:=True;
         end;
         mrCancel : begin
            allowed:=False;
            keepChildren:=True;
         end;
      else
         allowed:=False;
         keepChildren:=True;
      end;
      // deletion allowed?
      if allowed then begin
         Tree.Selected.Free;
         FCurrentDesigner.SelectComponent(nil);
         anObject.Parent.Remove(anObject, keepChildren);
         anObject.Free;
      end
   end;
   end;
end;

// ACMoveUpExecute
//
procedure TGLSceneEditorForm.ACMoveUpExecute(Sender: TObject);
var
   node : TTreeNode;
   prevData:Pointer;
begin
  if FSelectedItems=BEHAVIOURS_SELECTED then
  begin
        PrevData:=BehavioursListView.Selected.Data;
        TGLBaseBehaviour(PrevData).MoveUp;
        ShowBehaviours(TGLBaseSceneObject(Tree.Selected.Data));
        BehavioursListView.Selected:=BehavioursListView.FindData(0,PrevData,True,False);
        FCurrentDesigner.Modified;
  end
  else if FSelectedItems=EFFECTS_SELECTED then
  begin
        PrevData:=EffectsListView.Selected.Data;
        TGLBaseBehaviour(PrevData).MoveUp;
        ShowEffects(TGLBaseSceneObject(Tree.Selected.Data));
        EffectsListView.Selected:=EffectsListView.FindData(0,PrevData,True,False);
        FCurrentDesigner.Modified;
  end
  else if FSelectedItems=SCENE_SELECTED then
  begin
   if ACMoveUp.Enabled then begin
      node:=Tree.Selected;
      if Assigned(node) then begin
         node.MoveTo(node.GetPrevSibling, naInsert);
         with TGLBaseSceneObject(node.Data) do begin
            MoveUp;
            Update;
         end;
         TreeChange(Self, node);
         FCurrentDesigner.Modified;
      end;
   end;
  end; 
end;

// ACMoveDownExecute
//
procedure TGLSceneEditorForm.ACMoveDownExecute(Sender: TObject);
var
   node : TTreeNode;
   prevData:Pointer;
begin
  if FSelectedItems=BEHAVIOURS_SELECTED then
  begin
        PrevData:=BehavioursListView.Selected.Data;
        TGLBaseBehaviour(PrevData).MoveDown;
        ShowBehaviours(TGLBaseSceneObject(Tree.Selected.Data));
        BehavioursListView.Selected:=BehavioursListView.FindData(0,PrevData,True,False);
        FCurrentDesigner.Modified;
  end
  else if FSelectedItems=EFFECTS_SELECTED then
  begin
        PrevData:=EffectsListView.Selected.Data;
        TGLBaseBehaviour(PrevData).MoveDown;
        ShowEffects(TGLBaseSceneObject(Tree.Selected.Data));
        EffectsListView.Selected:=EffectsListView.FindData(0,PrevData,True,False);
        FCurrentDesigner.Modified;
  end
  else if FSelectedItems=SCENE_SELECTED then
  begin
   if ACMoveDown.Enabled then begin
      node:=Tree.Selected;
      if Assigned(node) then begin
         node.GetNextSibling.MoveTo(node, naInsert);
         with TGLBaseSceneObject(node.Data) do begin
				MoveDown;
            Update;
         end;
			TreeChange(Self, node);
         FCurrentDesigner.Modified;
		end;
	end;
      end;
end;

// ACAddObjectExecute
//
procedure TGLSceneEditorForm.ACAddObjectExecute(Sender: TObject);
begin
	TBAddObjects.CheckMenuDropdown;
end;

// ACSaveSceneExecute
//
procedure TGLSceneEditorForm.ACSaveSceneExecute(Sender: TObject);
begin
	if SaveDialog.Execute then
		FScene.SaveToFile(SaveDialog.FileName);
end;

// ACLoadSceneExecute
//
procedure TGLSceneEditorForm.ACLoadSceneExecute(Sender: TObject);
begin
	if OpenDialog.Execute then begin
		FScene.LoadFromFile(OpenDialog.FileName);
		ResetTree;
		ReadScene;
                ShowBehavioursAndEffects(nil);
	end;
end;

// ACInfoExecute
//
procedure TGLSceneEditorForm.ACInfoExecute(Sender: TObject);
var
	AScene: TGLSceneViewer;
begin
	AScene:=TGLSceneViewer.Create(Self);
	AScene.Name:='GLSceneEditor';
	AScene.Width:=0;
	AScene.Height:=0;
	AScene.Parent:=Self;
	try
		AScene.Buffer.ShowInfo;
	finally
		AScene.Free;
	end;
end;

// IsValidClipBoardNode
//
function TGLSceneEditorForm.IsValidClipBoardNode : Boolean;
var
   selNode : TTreeNode;
begin
   selNode:=Tree.Selected;
   Result:=((selNode<>nil) and (selNode.Parent<>nil)
            and (selNode.Parent.Parent<>nil));
end;

// IsPastePossible
//
function TGLSceneEditorForm.IsPastePossible : Boolean;
var
   selNode : TTreeNode;
	anObject, destination : TGLBaseSceneObject;
begin
   selNode:=Tree.Selected;
   if (selNode<>nil) and (selNode.Parent<>nil) and ClipBoard.HasFormat(CF_COMPONENT) then begin
      anObject:=TGLBaseSceneObject(GetComponentFromClipBoard);
      destination:=TGLBaseSceneObject(selNode.Data);
      Result:=CanPaste(anObject, destination);
      anObject.Free;
   end else Result:=False;
end;

// CanPaste
//
function TGLSceneEditorForm.CanPaste(obj, destination : TGLBaseSceneObject) : Boolean;
begin
   Result:=((obj is TGLCamera) or (destination<>FScene.Cameras))
           and (obj is TGLBaseSceneObject);
end;

// GetComponentFromClipBoard
//
function TGLSceneEditorForm.GetComponentFromClipBoard : TComponent;
var
  Data: THandle;
  DataPtr: Pointer;
  MemStream: TMemoryStream;
  Reader: TReader;
begin
   // a bug in the VCL prevents use of the standard function...
   // here is a simplified fixed one
   Result := nil;
   with ClipBoard do begin
      Open;
      try
         Data := GetClipboardData(CF_COMPONENT);
         if Data = 0 then Exit;
         DataPtr := GlobalLock(Data);
         if DataPtr = nil then Exit;
         try
            MemStream:=TMemoryStream.Create;
            try
               MemStream.WriteBuffer(DataPtr^, GlobalSize(Data));
               MemStream.Position:=0;
               Reader:=TReader.Create(MemStream, 256);
               try
                  Result:=Reader.ReadRootComponent(nil);
               finally
                  Reader.Free;
               end;
            finally
               MemStream.Free;
            end;
         finally
            GlobalUnlock(Data);
         end;
      finally
        Close;
      end;
   end;
end;

// ACCopyExecute
//
procedure TGLSceneEditorForm.ACCopyExecute(Sender: TObject);
begin
   if IsValidClipBoardNode then
      ClipBoard.SetComponent(TGLBaseSceneObject(Tree.Selected.Data));
   ACPaste.Enabled:=IsPastePossible;
end;

// ACCutExecute
//
procedure TGLSceneEditorForm.ACCutExecute(Sender: TObject);
var
	AObject : TGLBaseSceneObject;
   selNode : TTreeNode;
begin
   selNode:=Tree.Selected;
   if IsValidClipBoardNode then begin
      AObject:=TGLBaseSceneObject(selNode.Data);
      ClipBoard.SetComponent(AObject);
      AObject.Parent.Remove(AObject, False);
      AObject.Free;
      Tree.Selected.Free;
   end;
end;

procedure TGLSceneEditorForm.RenameObjectToBeUnique(anObject:TGLBaseSceneObject);
var
  i:integer;
  OriginalName,NewName:String;
begin
   OriginalName:=anObject.Name;
   NewName:=OriginalName;
   i:=1;
   while FScene.FindSceneObject(NewName)<>nil do
   begin
     NewName:=OriginalName+IntToStr(i);
     i:=i+1;
   end;

   anObject.Name:=NewName;

   FCurrentDesigner.Modified;
   for i:=0 to anObject.Count-1 do
     RenameObjectToBeUnique(anObject.Children[i]);

//         if Assigned(FCurrentDesigner) then
//           anObject.Name:=FCurrentDesigner.UniqueName(anObject.Name);
end;

// ACPasteExecute
//
procedure TGLSceneEditorForm.ACPasteExecute(Sender: TObject);
var
   selNode : TTreeNode;
	anObject, destination : TGLBaseSceneObject;
begin
   selNode:=Tree.Selected;
   if (selNode<>nil) and (selNode.Parent<>nil) then begin
      anObject:=TGLBaseSceneObject(GetComponentFromClipBoard);
      destination:=TGLBaseSceneObject(selNode.Data);
      if CanPaste(anObject, destination) then begin
         RenameObjectToBeUnique(anObject);
         destination.AddChild(anObject);
         AddNodes(selNode, anObject);
         selNode.Expand(False);
      end else anObject.Free;
      FCurrentDesigner.Modified;
   end;
end;


procedure TGLSceneEditorForm.BehavioursListViewEnter(Sender: TObject);
begin
   if Assigned(FCurrentDesigner) and Assigned(BehavioursListView.Selected) then
      FCurrentDesigner.SelectComponent(BehavioursListView.Selected.Data);
   FSelectedItems:=BEHAVIOURS_SELECTED;
   EnableAndDisableActions();
end;

procedure TGLSceneEditorForm.EffectsListViewEnter(Sender: TObject);
begin
   if Assigned(FCurrentDesigner) and Assigned(EffectsListView.Selected) then
      FCurrentDesigner.SelectComponent(EffectsListView.Selected.Data);
   FSelectedItems:=EFFECTS_SELECTED;
   EnableAndDisableActions();   
end;

procedure TGLSceneEditorForm.ACAddBehaviourExecute(Sender: TObject);
begin
  TBAddBehaviours.CheckMenuDropdown
end;

procedure TGLSceneEditorForm.DeleteBaseBehaviour(ListView:TListView);
begin
  if ListView.Selected<>nil then
  begin

      FCurrentDesigner.Modified;
{$ifndef GLS_DELPHI_4}
      FCurrentDesigner.NoSelection;
{$else}
      FCurrentDesigner.SelectComponent(nil);
{$endif}
      TXCollectionItem(ListView.Selected.Data).Free;
      ListView.Selected.Free;
     // ListViewChange(Self, nil, ctState);
      ShowBehavioursAndEffects(TGLBaseSceneObject(Tree.Selected.Data));
  end;
end;

procedure TGLSceneEditorForm.PMBehavioursToolbarPopup(Sender: TObject);
var
  object1:TGLBaseSceneObject;
begin
   if (Tree.Selected)<>nil then
   begin
     object1:=TGLBaseSceneObject(Tree.Selected.Data);
     SetBehavioursSubItems(PMBehavioursToolbar.Items, object1.Behaviours);
   end;
end;

procedure TGLSceneEditorForm.PMEffectsToolbarPopup(Sender: TObject);
var
  object1:TGLBaseSceneObject;
begin
   if (Tree.Selected)<>nil then
   begin
     object1:=TGLBaseSceneObject(Tree.Selected.Data);
     SetEffectsSubItems(PMEffectsToolbar.Items, object1.Effects);
   end;
end;


procedure TGLSceneEditorForm.BehavioursListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  EnableAndDisableActions();
end;

procedure TGLSceneEditorForm.ACAddEffectExecute(Sender: TObject);
begin
   TBAddEffects.CheckMenuDropdown;
end;

procedure TGLSceneEditorForm.EnableAndDisableActions();
var
  SelNode:TTreeNode;
begin
  if FSelectedItems=SCENE_SELECTED then
  begin
      selNode:=Tree.Selected;
      // select in Delphi IDE
      if Assigned(selNode) then
      begin
         if Assigned(selNode.Data) then
            FCurrentDesigner.SelectComponent(selNode.Data)
         else FCurrentDesigner.SelectComponent(FScene);
         // enablings
         ACAddCamera.Enabled:=(selNode=FCameraNode);
         ACAddObject.Enabled:=((selNode=FObjectNode) or selNode.HasAsParent(FObjectNode));
         ACAddBehaviour.Enabled:=(selNode.HasAsParent(FObjectNode));
         ACAddEffect.Enabled:=(selNode.HasAsParent(FObjectNode));
         ACDeleteObject.Enabled:=(selNode.Level>1);
         ACMoveUp.Enabled:=((selNode.Index>0)and(selNode.Level>1));
         ACMoveDown.Enabled:=((selNode.GetNextSibling<>nil)and(selNode.Level>1));
         ACCut.Enabled:=IsValidClipBoardNode;
         ACPaste.Enabled:=IsPastePossible;

      end
      else
      begin
         ACAddCamera.Enabled:=False;
         ACAddObject.Enabled:=False;
         ACAddBehaviour.Enabled:=False;
         ACAddEffect.Enabled:=False;
         ACDeleteObject.Enabled:=False;
         ACMoveUp.Enabled:=False;
         ACMoveDown.Enabled:=False;
         ACCut.Enabled:=False;
         ACPaste.Enabled:=False;
      end;
//   end;
   ACCopy.Enabled:=ACCut.Enabled;

  end
  else if FSelectedItems=BEHAVIOURS_SELECTED then
  begin
    if (BehavioursListView.Selected<>nil) then
    begin
         FCurrentDesigner.SelectComponent(BehavioursListView.Selected.Data);
         ACDeleteObject.Enabled:=True;
         ACMoveUp.Enabled:=(BehavioursListView.Selected.Index>0);
         ACMoveDown.Enabled:=(BehavioursListView.Selected.Index<BehavioursListView.Selected.Owner.Count-1);
         ACCut.Enabled:=False;
         ACCopy.Enabled:=false;
         ACPaste.Enabled:=False;
    end
      else
      begin
         ACDeleteObject.Enabled:=false;
         ACMoveUp.Enabled:=false;
         ACMoveDown.Enabled:=false;
         ACCut.Enabled:=false;
         ACCopy.Enabled:=false;
         ACPaste.Enabled:=false;
      end;
  end
  else if FSelectedItems=EFFECTS_SELECTED then
  begin
    if (EffectsListView.Selected<>nil)then
    begin
         FCurrentDesigner.SelectComponent(EffectsListView.Selected.Data);
         ACDeleteObject.Enabled:=True;
         ACMoveUp.Enabled:=(EffectsListView.Selected.Index>0);
         ACMoveDown.Enabled:=(EffectsListView.Selected.Index<EffectsListView.Selected.Owner.Count-1);
         ACCut.Enabled:=False;
         ACCopy.Enabled:=false;
         ACPaste.Enabled:=False;
    end
      else
      begin
         ACDeleteObject.Enabled:=false;
         ACMoveUp.Enabled:=false;
         ACMoveDown.Enabled:=false;
         ACCut.Enabled:=false;
         ACCopy.Enabled:=false;
         ACPaste.Enabled:=false;
      end;
  end;

end;

procedure TGLSceneEditorForm.PopupMenuPopup(Sender: TObject);
var
  object1:TGLBaseSceneObject;
begin
   if (Tree.Selected)<>nil then
   begin
     object1:=TGLBaseSceneObject(Tree.Selected.Data);
     SetBehavioursSubItems(MIAddBehaviour,object1.Behaviours);
     SetEffectsSubItems(MIAddEffect,object1.Effects);
   end;
end;

procedure TGLSceneEditorForm.TBEffectsPanelClick(Sender: TObject);
begin
   PABehaviours.Visible:=TBEffectsPanel.Down;
   Splitter.Visible:=TBEffectsPanel.Down;
   if PABehaviours.Visible then
      Width:=Width+PABehaviours.Width
   else Width:=Width-PABehaviours.Width;
end;

initialization

finalization

   ReleaseGLSceneEditorForm;

end.
