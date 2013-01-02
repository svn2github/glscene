// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSceneEdit.pas' rev: 24.00 (Win32)

#ifndef GlsceneeditHPP
#define GlsceneeditHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <XCollection.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Win.Registry.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.ComCtrls.hpp>	// Pascal unit
#include <Vcl.ImgList.hpp>	// Pascal unit
#include <Vcl.Dialogs.hpp>	// Pascal unit
#include <Vcl.Menus.hpp>	// Pascal unit
#include <Vcl.ActnList.hpp>	// Pascal unit
#include <Vcl.ToolWin.hpp>	// Pascal unit
#include <Vcl.ExtCtrls.hpp>	// Pascal unit
#include <Vcl.StdCtrls.hpp>	// Pascal unit
#include <DesignIntf.hpp>	// Pascal unit
#include <VCLEditors.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit
#include <System.UITypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glsceneedit
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TSetSubItemsEvent)(System::TObject* Sender);

class DELPHICLASS TGLSceneEditorForm;
class PASCALIMPLEMENTATION TGLSceneEditorForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Comctrls::TTreeView* Tree;
	Vcl::Menus::TPopupMenu* PopupMenu;
	Vcl::Menus::TMenuItem* MIAddCamera;
	Vcl::Menus::TMenuItem* MIAddObject;
	Vcl::Menus::TMenuItem* N1;
	Vcl::Menus::TMenuItem* MIDelObject;
	Vcl::Comctrls::TToolBar* ToolBar;
	Vcl::Actnlist::TActionList* ActionList;
	Vcl::Comctrls::TToolButton* ToolButton1;
	Vcl::Comctrls::TToolButton* TBAddObjects;
	Vcl::Comctrls::TToolButton* ToolButton4;
	Vcl::Menus::TPopupMenu* PMToolBar;
	Vcl::Comctrls::TToolButton* ToolButton5;
	Vcl::Comctrls::TToolButton* ToolButton7;
	Vcl::Actnlist::TAction* ACAddCamera;
	Vcl::Actnlist::TAction* ACAddObject;
	Vcl::Controls::TImageList* ImageList;
	Vcl::Actnlist::TAction* ACDeleteObject;
	Vcl::Actnlist::TAction* ACMoveUp;
	Vcl::Actnlist::TAction* ACMoveDown;
	Vcl::Menus::TMenuItem* N2;
	Vcl::Menus::TMenuItem* Moveobjectup1;
	Vcl::Menus::TMenuItem* Moveobjectdown1;
	Vcl::Actnlist::TAction* ACSaveScene;
	Vcl::Actnlist::TAction* ACLoadScene;
	Vcl::Dialogs::TOpenDialog* OpenDialog;
	Vcl::Dialogs::TSaveDialog* SaveDialog;
	Vcl::Comctrls::TToolButton* ToolButton8;
	Vcl::Comctrls::TToolButton* ToolButton9;
	Vcl::Actnlist::TAction* ACInfo;
	Vcl::Actnlist::TAction* ACCopy;
	Vcl::Actnlist::TAction* ACCut;
	Vcl::Actnlist::TAction* ACPaste;
	Vcl::Menus::TMenuItem* Copy1;
	Vcl::Menus::TMenuItem* Paste1;
	Vcl::Menus::TMenuItem* Cut1;
	Vcl::Comctrls::TToolButton* ToolButton12;
	Vcl::Comctrls::TToolButton* ToolButton13;
	Vcl::Comctrls::TToolButton* ToolButton14;
	Vcl::Extctrls::TPanel* PABehaviours;
	Vcl::Comctrls::TListView* BehavioursListView;
	Vcl::Extctrls::TSplitter* Splitter3;
	Vcl::Comctrls::TListView* EffectsListView;
	Vcl::Extctrls::TSplitter* Splitter;
	Vcl::Menus::TPopupMenu* PMBehavioursToolbar;
	Vcl::Actnlist::TAction* ACAddBehaviour;
	Vcl::Menus::TMenuItem* MIAddBehaviour;
	Vcl::Menus::TMenuItem* MIAddEffect;
	Vcl::Menus::TMenuItem* MIBehaviourSeparator;
	Vcl::Actnlist::TAction* ACDeleteBehaviour;
	Vcl::Menus::TPopupMenu* BehavioursPopupMenu;
	Vcl::Menus::TMenuItem* Delete1;
	Vcl::Menus::TMenuItem* MoveUp1;
	Vcl::Menus::TMenuItem* MoveDown1;
	Vcl::Menus::TMenuItem* N4;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Menus::TPopupMenu* PMEffectsToolbar;
	Vcl::Actnlist::TAction* ACAddEffect;
	Vcl::Comctrls::TToolBar* ToolBar1;
	Vcl::Comctrls::TToolButton* TBAddBehaviours;
	Vcl::Comctrls::TToolButton* TBAddEffects;
	Vcl::Comctrls::TToolButton* TBEffectsPanel;
	Vcl::Comctrls::TToolButton* TBStayOnTop;
	Vcl::Actnlist::TAction* ACStayOnTop;
	Vcl::Comctrls::TToolButton* ToolButton10;
	Vcl::Comctrls::TToolButton* ToolButton15;
	Vcl::Comctrls::TToolButton* ToolButton16;
	Vcl::Actnlist::TAction* ACExpand;
	Vcl::Actnlist::TAction* ACColapse;
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall TreeEditing(System::TObject* Sender, Vcl::Comctrls::TTreeNode* Node, bool &AllowEdit);
	void __fastcall TreeDragOver(System::TObject* Sender, System::TObject* Source, int X, int Y, System::Uitypes::TDragState State, bool &Accept);
	void __fastcall TreeDragDrop(System::TObject* Sender, System::TObject* Source, int X, int Y);
	void __fastcall TreeChange(System::TObject* Sender, Vcl::Comctrls::TTreeNode* Node);
	void __fastcall TreeMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall TreeEnter(System::TObject* Sender);
	void __fastcall TreeMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall ACAddCameraExecute(System::TObject* Sender);
	void __fastcall ACDeleteObjectExecute(System::TObject* Sender);
	void __fastcall ACMoveUpExecute(System::TObject* Sender);
	void __fastcall ACMoveDownExecute(System::TObject* Sender);
	void __fastcall ACAddObjectExecute(System::TObject* Sender);
	void __fastcall ACSaveSceneExecute(System::TObject* Sender);
	void __fastcall ACLoadSceneExecute(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall ACInfoExecute(System::TObject* Sender);
	void __fastcall ACCopyExecute(System::TObject* Sender);
	void __fastcall ACCutExecute(System::TObject* Sender);
	void __fastcall ACPasteExecute(System::TObject* Sender);
	void __fastcall BehavioursListViewEnter(System::TObject* Sender);
	void __fastcall EffectsListViewEnter(System::TObject* Sender);
	void __fastcall ACAddBehaviourExecute(System::TObject* Sender);
	void __fastcall DeleteBaseBehaviour(Vcl::Comctrls::TListView* ListView);
	void __fastcall PMBehavioursToolbarPopup(System::TObject* Sender);
	void __fastcall PMEffectsToolbarPopup(System::TObject* Sender);
	void __fastcall BehavioursListViewSelectItem(System::TObject* Sender, Vcl::Comctrls::TListItem* Item, bool Selected);
	void __fastcall ACAddEffectExecute(System::TObject* Sender);
	void __fastcall PopupMenuPopup(System::TObject* Sender);
	void __fastcall TBEffectsPanelClick(System::TObject* Sender);
	void __fastcall TreeKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall ACStayOnTopExecute(System::TObject* Sender);
	void __fastcall FormKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall ACExpandExecute(System::TObject* Sender);
	void __fastcall ACColapseExecute(System::TObject* Sender);
	
private:
	int FSelectedItems;
	Glscene::TGLScene* FScene;
	Vcl::Comctrls::TTreeNode* FObjectNode;
	Vcl::Comctrls::TTreeNode* FSceneObjects;
	Designintf::_di_IDesigner FCurrentDesigner;
	System::Types::TPoint FLastMouseDownPos;
	System::Classes::TComponent* FPasteOwner;
	Designintf::_di_IDesignerSelections FPasteSelection;
	void __fastcall ReadScene(void);
	void __fastcall ResetTree(void);
	Vcl::Comctrls::TTreeNode* __fastcall AddNodes(Vcl::Comctrls::TTreeNode* ANode, Glscene::TGLBaseSceneObject* AObject);
	void __fastcall AddObjectClick(System::TObject* Sender);
	void __fastcall AddBehaviourClick(System::TObject* Sender);
	void __fastcall AddEffectClick(System::TObject* Sender);
	void __fastcall SetObjectsSubItems(Vcl::Menus::TMenuItem* parent);
	void __fastcall SetXCollectionSubItems(Vcl::Menus::TMenuItem* parent, Xcollection::TXCollection* XCollection, TSetSubItemsEvent Event);
	void __fastcall SetBehavioursSubItems(Vcl::Menus::TMenuItem* parent, Xcollection::TXCollection* XCollection);
	void __fastcall SetEffectsSubItems(Vcl::Menus::TMenuItem* parent, Xcollection::TXCollection* XCollection);
	void __fastcall OnBaseSceneObjectNameChanged(System::TObject* Sender);
	bool __fastcall IsValidClipBoardNode(void);
	bool __fastcall IsPastePossible(void);
	void __fastcall ShowBehaviours(Glscene::TGLBaseSceneObject* BaseSceneObject);
	void __fastcall ShowEffects(Glscene::TGLBaseSceneObject* BaseSceneObject);
	void __fastcall ShowBehavioursAndEffects(Glscene::TGLBaseSceneObject* BaseSceneObject);
	void __fastcall EnableAndDisableActions(void);
	bool __fastcall CanPaste(Glscene::TGLBaseSceneObject* obj, Glscene::TGLBaseSceneObject* destination);
	void __fastcall CopyComponents(System::Classes::TComponent* Root, const Designintf::_di_IDesignerSelections Components);
	void __fastcall MethodError(System::Classes::TReader* Reader, const System::UnicodeString MethodName, void * &Address, bool &Error);
	bool __fastcall PasteComponents(System::Classes::TComponent* AOwner, System::Classes::TComponent* AParent, const Designintf::_di_IDesignerSelections Components);
	void __fastcall ReaderSetName(System::Classes::TReader* Reader, System::Classes::TComponent* Component, System::UnicodeString &Name);
	void __fastcall ComponentRead(System::Classes::TComponent* Component);
	System::UnicodeString __fastcall UniqueName(System::Classes::TComponent* Component);
	void __fastcall TreeEdited(System::TObject* Sender, Vcl::Comctrls::TTreeNode* Node, System::UnicodeString &S);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	void __fastcall SetScene(Glscene::TGLScene* Scene, Designintf::_di_IDesigner Designer);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TGLSceneEditorForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TGLSceneEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TGLSceneEditorForm(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLSceneEditorForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 SCENE_SELECTED = System::Int8(0x0);
static const System::Int8 BEHAVIOURS_SELECTED = System::Int8(0x1);
static const System::Int8 EFFECTS_SELECTED = System::Int8(0x2);
extern PACKAGE TGLSceneEditorForm* __fastcall GLSceneEditorForm(void);
extern PACKAGE void __fastcall ReleaseGLSceneEditorForm(void);
}	/* namespace Glsceneedit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCENEEDIT)
using namespace Glsceneedit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsceneeditHPP
