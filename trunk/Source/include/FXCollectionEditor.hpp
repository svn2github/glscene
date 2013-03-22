// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCollectionEditor.pas' rev: 24.00 (Win32)

#ifndef FxcollectioneditorHPP
#define FxcollectioneditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <XCollection.hpp>	// Pascal unit
#include <Winapi.Messages.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.ImgList.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.ActnList.hpp>	// Pascal unit
#include <Vcl.Menus.hpp>	// Pascal unit
#include <Vcl.ComCtrls.hpp>	// Pascal unit
#include <Vcl.ToolWin.hpp>	// Pascal unit
#include <DesignEditors.hpp>	// Pascal unit
#include <DesignIntf.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxcollectioneditor
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TXCollectionEditor;
class PASCALIMPLEMENTATION TXCollectionEditor : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Comctrls::TListView* ListView;
	Vcl::Menus::TPopupMenu* PMListView;
	Vcl::Actnlist::TActionList* ActionList;
	Vcl::Actnlist::TAction* ACRemove;
	Vcl::Actnlist::TAction* ACMoveUp;
	Vcl::Actnlist::TAction* ACMoveDown;
	Vcl::Controls::TImageList* ImageList;
	Vcl::Menus::TMenuItem* MIAdd;
	Vcl::Menus::TMenuItem* N1;
	Vcl::Menus::TMenuItem* N2;
	Vcl::Menus::TMenuItem* Moveup1;
	Vcl::Menus::TMenuItem* Movedown1;
	Vcl::Comctrls::TToolBar* ToolBar1;
	Vcl::Comctrls::TToolButton* TBAdd;
	Vcl::Comctrls::TToolButton* ToolButton2;
	Vcl::Comctrls::TToolButton* ToolButton3;
	Vcl::Comctrls::TToolButton* ToolButton4;
	Vcl::Comctrls::TToolButton* ToolButton5;
	Vcl::Comctrls::TToolButton* ToolButton6;
	Vcl::Menus::TPopupMenu* PMToolBar;
	void __fastcall TBAddClick(System::TObject* Sender);
	void __fastcall ListViewChange(System::TObject* Sender, Vcl::Comctrls::TListItem* Item, Vcl::Comctrls::TItemChange Change);
	void __fastcall ACRemoveExecute(System::TObject* Sender);
	void __fastcall ACMoveUpExecute(System::TObject* Sender);
	void __fastcall ACMoveDownExecute(System::TObject* Sender);
	void __fastcall PMToolBarPopup(System::TObject* Sender);
	void __fastcall PMListViewPopup(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall FormHide(System::TObject* Sender);
	
private:
	Xcollection::TXCollection* FXCollection;
	Designintf::_di_IDesigner FDesigner;
	bool updatingListView;
	void __fastcall PrepareListView(void);
	void __fastcall PrepareXCollectionItemPopup(Vcl::Menus::TMenuItem* parent);
	void __fastcall OnAddXCollectionItemClick(System::TObject* Sender);
	void __fastcall OnNameChanged(System::TObject* Sender);
	void __fastcall OnXCollectionDestroyed(System::TObject* Sender);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	void __fastcall SetXCollection(Xcollection::TXCollection* aXCollection, Designintf::_di_IDesigner designer);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TXCollectionEditor(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TXCollectionEditor(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TXCollectionEditor(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TXCollectionEditor(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TXCollectionEditor* __fastcall XCollectionEditor(void);
extern PACKAGE void __fastcall ReleaseXCollectionEditor(void);
}	/* namespace Fxcollectioneditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FXCOLLECTIONEDITOR)
using namespace Fxcollectioneditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FxcollectioneditorHPP
