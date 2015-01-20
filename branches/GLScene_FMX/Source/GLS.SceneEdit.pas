//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLS.SceneEdit<p>

  Scene Editor, for adding + removing scene objects within the Delphi IDE.<p>

  <b>History : </b><font size=-1><ul>
  <li>20/01/15 - PW - Converted to support FMX platform, added StyleBook to store icons
  <li>06/12/14 - PW -  Reduced doubled Camera and Expand/Collapse buttons, added GalleryListView
  <li>20/01/10 - Yar - TGLSceneEditorForm.IsPastePossible now uses CharInSet
  <li>20/01/10 - Yar - Added Expand and Collapse buttons (thanks to lolo)
  <li>14/03/09 - DanB - Removed Cameras node, instead cameras are now placed into scene
  <li>19/03/08 - mrqzzz - Little change to "stay on top" (references self, not GLSceneEditorForm )
  <li>17/03/08 - mrqzzz - By dAlex: Added "stay on top" button
  <li>12/07/07 - DaStr - Improved cross-platform compatibility (BugTrackerID=1684432)
  <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
  <li>25/03/07 - DaStr - Abstracted IsSubComponent for Delphi5 compatibility
  <li>17/03/07 - DaStr - Dropped Kylix support in favor of FPC (BugTrackerID=1681585)
  <li>07/02/07 - DaStr - TGLSceneEditorForm.ACDeleteObjectExecute bugfixed
                         TGLSceneEditorForm.AddNodes - removed warning (all for proper Subcomponent support)
  <li>20/01/07 - DaStr - TGLSceneEditorForm.ACCutExecute bugfixed
  <li>19/12/06 - DaStr - TGLSceneEditorForm.AddNodes bugfixed -
                         SubComponents are no longer displayed in the Editor (BugTraker ID = 1585913)
  <li>24/06/06 - PvD - Fixed bug with DELETE key when editing name in Treeview
  <li>03/07/04 - LR - Updated for Linux
  <li>18/12/04 - PhP - Added support for deleting objects/effects/behaviours by pressing "Delete"
  <li>03/07/04 - LR - Make change for Linux
  <li>14/12/03 - EG - Paste fix (Mrqzzz)
  <li>31/06/03 - EG - Cosmetic changes, form position/state now saved to the registry
  <li>21/06/03 - DanB - Added behaviours/effects listviews
  <li>22/01/02 - EG - Fixed controls state after drag/drop (Anton Zhuchkov)
  <li>06/08/00 - EG - Added basic Clipboard support
  <li>14/05/00 - EG - Added workaround for VCL DesignInfo bug (thx Nelson Chu)
  <li>28/04/00 - EG - Fixed new objects not being immediately reco by IDE
  <li>26/04/00 - EG - Added support for objects categories
  <li>17/04/00 - EG - Added access to TInfoForm
  <li>16/04/00 - EG - Fixed occasionnal crash when rebuilding GLScene dpk while GLSceneEdit is visible
  <li>10/04/00 - EG - Minor Create/Release change
  <li>24/03/00 - EG - Fixed SetScene not updating enablings
  <li>13/03/00 - EG - Object names (ie. node text) is now properly adjusted
                      when a GLScene object is renamed, Added Load/Save whole scene
  <li>07/02/00 - EG - Fixed notification logic
  <li>06/02/00 - EG - DragDrop now starts after moving the mouse a little,
                      Form is now auto-creating, fixed Notification, Added actionlist and moveUp/moveDown
  <li>05/02/00 - EG - Fixed DragDrop, added root nodes auto-expansion
  </ul></font>
}

unit GLS.SceneEdit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Actions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Menus,
  FMX.ActnList, FMX.StdCtrls, FMX.Layouts, FMX.TreeView,
  FMX.ListView.Types, FMX.ListView,

  GLS.Scene,
  GLS.SceneViewer,
  GLS.SceneRegister,
  GLS.Strings,
  FInfo,
  GLS.XCollection,
  GLS.CrossPlatform, FMX.Objects;

type
  TGLSceneEditorForm = class(TForm)
    ToolBar: TToolBar;
    PATree: TPanel;
    PAGallery: TPanel;
    PAEffects: TPanel;
    ActionList: TActionList;
    PMToolBar: TPopupMenu;
    PopupMenu: TPopupMenu;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    Tree: TTreeView;
    PABehaviours: TPanel;
    PMBehavioursToolBar: TPopupMenu;
    PMEffectsToolBar: TPopupMenu;
    BehavioursPopupMenu: TPopupMenu;
    ToolBarBehaviours: TToolBar;
    ToolBarEffects: TToolBar;
    GalleryListView: TListView;
    BehavioursListView: TListView;
    EffectsListView: TListView;
    ACLoadScene: TAction;
    ACSaveScene: TAction;
    ACStayOnTop: TAction;
    ACAddObject: TAction;
    ACAddBehaviour: TAction;
    ACAddEffect: TAction;
    ACMoveUp: TAction;
    ACMoveDown: TAction;
    ACExpand: TAction;
    ACDeleteObject: TAction;
    ACDeleteBehaviour: TAction;
    ACCut: TAction;
    ACCopy: TAction;
    ACPaste: TAction;
    ACInfo: TAction;
    TBLoadScene: TSpeedButton;
    ImLoadScene: TImage;
    TBInfo: TSpeedButton;
    ImInfo: TImage;
    TBPaste: TSpeedButton;
    ImPaste: TImage;
    TBCopy: TSpeedButton;
    ImCopy: TImage;
    TBCut: TSpeedButton;
    ImCut: TImage;
    TBDeleteObject: TSpeedButton;
    ImDeleteObject: TImage;
    TBExpand: TSpeedButton;
    ImExpand: TImage;
    TBMoveDown: TSpeedButton;
    ImMoveDown: TImage;
    TBMoveUp: TSpeedButton;
    ImMoveUp: TImage;
    TBCharacterPanels: TSpeedButton;
    ImCharacterPanels: TImage;
    TBGalleryPanel: TSpeedButton;
    ImGalleryPanel: TImage;
    TBAddObjects: TSpeedButton;
    ImAddObjects: TImage;
    TBStayOnTop: TSpeedButton;
    ImStayOnTop: TImage;
    TBSaveScene: TSpeedButton;
    ImSaveScene: TImage;
    TBSeparator1: TSpeedButton;
    TBSeparator2: TSpeedButton;
    TBSeparator3: TSpeedButton;
    TBSeparator4: TSpeedButton;
    ACGallery: TAction;
    ImArrowDown: TImage;
    ImArrowDownBeh: TImage;
    ImArrowDownEff: TImage;
    TBAddBehaviours: TSpeedButton;
    TBAddEffects: TSpeedButton;
    MIAddObject: TMenuItem;
    MIAddBehaviour: TMenuItem;
    MIAddEffect: TMenuItem;
    MICut: TMenuItem;
    MICopy: TMenuItem;
    MIPaste: TMenuItem;
    MIDelObject: TMenuItem;
    MIMoveUp: TMenuItem;
    MIMoveDown: TMenuItem;
    StyleBook: TStyleBook;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ACInfoExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function GLSceneEditorForm: TGLSceneEditorForm;
procedure ReleaseGLSceneEditorForm;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R *.fmx}

resourcestring
  cGLSceneEditor = 'GLScene FMX Editor';

const
  cRegistryKey = 'Software\GLScene_FMX\GLSceneEdit';

var
  vGLSceneEditorForm: TGLSceneEditorForm;

function GLSceneEditorForm: TGLSceneEditorForm;
begin
  if not Assigned(vGLSceneEditorForm) then
    vGLSceneEditorForm := TGLSceneEditorForm.Create(nil);
  Result := vGLSceneEditorForm;
end;

procedure ReleaseGLSceneEditorForm;
begin
  if Assigned(vGLSceneEditorForm) then
  begin
    vGLSceneEditorForm.Free;
    vGLSceneEditorForm := nil;
  end;
end;

function ReadRegistryInteger(reg: TRegistry; const Name: string;
  defaultValue: Integer): Integer;
begin
  if reg.ValueExists(name) then
    Result := reg.ReadInteger(name)
  else
    Result := defaultValue;
end;

// FormCreate
//
procedure TGLSceneEditorForm.FormCreate(Sender: TObject);
var
  CurrentNode: TTreeNode;
  reg: TRegistry;
begin
  RegisterGLBaseSceneObjectNameChangeEvent(OnBaseSceneObjectNameChanged);
  Tree.Images := ObjectManager.ObjectIcons;
  Tree.Indent := ObjectManager.ObjectIcons.Width;
  with Tree.Items do
  begin
    // first add the scene root
    CurrentNode := Add(nil, glsSceneRoot);
    with CurrentNode do
    begin
      ImageIndex := ObjectManager.SceneRootIndex;
      SelectedIndex := ImageIndex;
    end;
    // and the root for all objects
    FObjectNode := AddChild(CurrentNode, glsObjectRoot);
    FSceneObjects := FObjectNode;
    with FObjectNode do
    begin
      ImageIndex := ObjectManager.ObjectRootIndex;
      SelectedIndex := ImageIndex;
    end;
  end;
  // Build SubMenus
  SetObjectsSubItems(MIAddObject);
  MIAddObject.SubMenuImages := ObjectManager.ObjectIcons;
  SetObjectsSubItems(PMToolBar.Items);
  PMToolBar.Images := ObjectManager.ObjectIcons;

  SetBehavioursSubItems(MIAddBehaviour, nil);
  SetBehavioursSubItems(PMBehavioursToolbar.Items, nil);
  SetEffectsSubItems(MIAddEffect, nil);
  SetEffectsSubItems(PMEffectsToolbar.Items, nil);

  reg := TRegistry.Create;
  try
    if reg.OpenKey(cRegistryKey, true) then
    begin
      if reg.ValueExists('CharacterPanels') then
        TBCharacterPanels.Down := reg.ReadBool('CharacterPanels');
      TBCharacterPanelsClick(Self);

      if reg.ValueExists('ExpandTree') then
        TBExpand.Down := reg.ReadBool('ExpandTree');
      ACExpandExecute(Self);

      Left := ReadRegistryInteger(reg, 'Left', Left);
      Top := ReadRegistryInteger(reg, 'Top', Top);
      Width := ReadRegistryInteger(reg, 'Width', 250);
      Height := ReadRegistryInteger(reg, 'Height', Height);
    end;
  finally
    reg.Free;
  end;

  // Trigger the event OnEdited manualy
  Tree.OnEdited := TreeEdited;
end;

// FormDestroy
//
procedure TGLSceneEditorForm.FormDestroy(Sender: TObject);
var
  reg: TRegistry;
begin
  DeRegisterGLBaseSceneObjectNameChangeEvent(OnBaseSceneObjectNameChanged);

  reg := TRegistry.Create;
  try
    if reg.OpenKey(cRegistryKey, true) then
    begin
      reg.WriteBool('CharacterPanels', TBCharacterPanels.Down);
      reg.WriteBool('ExpandTree', TBExpand.Down);
      reg.WriteInteger('Left', Left);
      reg.WriteInteger('Top', Top);
      reg.WriteInteger('Width', Width);
      reg.WriteInteger('Height', Height);
    end;
  finally
    reg.Free;
  end;
end;

// ACInfoExecute
//
procedure TGLSceneEditorForm.ACInfoExecute(Sender: TObject);
var
  AScene: TGLSceneViewer;
begin
  AScene := TGLSceneViewer.Create(Self);
  AScene.Name := 'GLSceneEditor';
  AScene.Width := 0;
  AScene.Height := 0;
  AScene.parent := Self;
  try
    AScene.Buffer.ShowInfo;
  finally
    AScene.Free;
  end;
end;


end.
