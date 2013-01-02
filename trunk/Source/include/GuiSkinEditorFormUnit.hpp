// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GuiSkinEditorFormUnit.pas' rev: 24.00 (Win32)

#ifndef GuiskineditorformunitHPP
#define GuiskineditorformunitHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <Winapi.Messages.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.Dialogs.hpp>	// Pascal unit
#include <Vcl.StdCtrls.hpp>	// Pascal unit
#include <Vcl.ComCtrls.hpp>	// Pascal unit
#include <Vcl.ExtCtrls.hpp>	// Pascal unit
#include <Vcl.Menus.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLObjects.hpp>	// Pascal unit
#include <GLWindows.hpp>	// Pascal unit
#include <GLHUDObjects.hpp>	// Pascal unit
#include <GLWin32Viewer.hpp>	// Pascal unit
#include <GLGui.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <GLUtils.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit
#include <System.UITypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Guiskineditorformunit
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGUISkinEditor;
class PASCALIMPLEMENTATION TGUISkinEditor : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Glscene::TGLScene* GLScene1;
	Glscene::TGLCamera* GLCamera1;
	Glwindows::TGLPanel* GLPanel1;
	Glhudobjects::TGLHUDSprite* HUDSprite1;
	Glscene::TGLMemoryViewer* GLMemoryViewer1;
	Glscene::TGLLightSource* GLLightSource1;
	Vcl::Comctrls::TStatusBar* StatusBar;
	Vcl::Extctrls::TPanel* panBottom;
	Vcl::Extctrls::TPanel* panZoomImage;
	Vcl::Extctrls::TImage* imgFull;
	Vcl::Stdctrls::TScrollBar* sbarHorizontal;
	Vcl::Stdctrls::TScrollBar* sbarVertical;
	Vcl::Stdctrls::TButton* Button5;
	Vcl::Stdctrls::TButton* Button6;
	Vcl::Extctrls::TPanel* panImageProperties;
	Vcl::Stdctrls::TLabel* Label5;
	Vcl::Stdctrls::TLabel* Label6;
	Vcl::Extctrls::TPanel* Panel2;
	Vcl::Extctrls::TImage* imgPreview;
	Vcl::Extctrls::TPanel* Panel3;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TButton* Button3;
	Vcl::Stdctrls::TButton* Button4;
	Vcl::Stdctrls::TCheckBox* CheckBox1;
	Vcl::Stdctrls::TEdit* WidthEdit;
	Vcl::Stdctrls::TEdit* HeightEdit;
	Vcl::Extctrls::TPanel* panElements;
	Vcl::Extctrls::TBevel* Bevel2;
	Vcl::Extctrls::TBevel* Bevel1;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Stdctrls::TLabel* Label11;
	Vcl::Stdctrls::TLabel* Label12;
	Vcl::Stdctrls::TLabel* Label13;
	Vcl::Stdctrls::TLabel* Label9;
	Vcl::Stdctrls::TLabel* Label10;
	Vcl::Stdctrls::TLabel* Label14;
	Vcl::Stdctrls::TListBox* lbElements;
	Vcl::Stdctrls::TButton* btnAdd;
	Vcl::Stdctrls::TButton* btnDelete;
	Vcl::Stdctrls::TComboBox* ComboBox1;
	Vcl::Stdctrls::TEdit* LeftEdit;
	Vcl::Stdctrls::TEdit* TopEdit;
	Vcl::Stdctrls::TEdit* RightEdit;
	Vcl::Stdctrls::TEdit* BottomEdit;
	Vcl::Stdctrls::TEdit* ScaleXEdit;
	Vcl::Stdctrls::TEdit* ScaleYEdit;
	Vcl::Menus::TPopupMenu* popElements;
	Vcl::Menus::TMenuItem* mnuTopLeft;
	Vcl::Menus::TMenuItem* mnuTop;
	Vcl::Menus::TMenuItem* mnuTopRight;
	Vcl::Menus::TMenuItem* mnuLeft;
	Vcl::Menus::TMenuItem* mnuCenter;
	Vcl::Menus::TMenuItem* mnuRight;
	Vcl::Menus::TMenuItem* mnuBottomLeft;
	Vcl::Menus::TMenuItem* mnuBottom;
	Vcl::Menus::TMenuItem* mnuBottomRight;
	Vcl::Menus::TMenuItem* N1;
	Vcl::Menus::TMenuItem* mnuAddAll;
	Vcl::Menus::TMenuItem* N2;
	Vcl::Menus::TMenuItem* mnuAllTop;
	Vcl::Menus::TMenuItem* mnuAllMiddle;
	Vcl::Menus::TMenuItem* mnuAllBottom;
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall Button3Click(System::TObject* Sender);
	void __fastcall Button4Click(System::TObject* Sender);
	void __fastcall ScrollBarScroll(System::TObject* Sender, System::Uitypes::TScrollCode ScrollCode, int &ScrollPos);
	void __fastcall ScrollbarChange(System::TObject* Sender);
	void __fastcall WidthEditChange(System::TObject* Sender);
	void __fastcall HeightEditChange(System::TObject* Sender);
	void __fastcall btnAddClick(System::TObject* Sender);
	void __fastcall lbElementsClick(System::TObject* Sender);
	void __fastcall ComboBox1Change(System::TObject* Sender);
	void __fastcall btnDeleteClick(System::TObject* Sender);
	void __fastcall imgFullMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall imgFullMouseUp(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall imgFullMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall lbElementsKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall CheckBox1Click(System::TObject* Sender);
	void __fastcall ScaleXEditChange(System::TObject* Sender);
	void __fastcall ScaleYEditChange(System::TObject* Sender);
	void __fastcall LeftEditChange(System::TObject* Sender);
	void __fastcall TopEditChange(System::TObject* Sender);
	void __fastcall RightEditChange(System::TObject* Sender);
	void __fastcall BottomEditChange(System::TObject* Sender);
	void __fastcall EditKeyPress(System::TObject* Sender, System::WideChar &Key);
	void __fastcall FormResize(System::TObject* Sender);
	void __fastcall MenuItemClick(System::TObject* Sender);
	void __fastcall mnuAddAllClick(System::TObject* Sender);
	void __fastcall mnuAllTopClick(System::TObject* Sender);
	void __fastcall mnuAllMiddleClick(System::TObject* Sender);
	void __fastcall mnuAllBottomClick(System::TObject* Sender);
	void __fastcall imgPreviewMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall imgPreviewMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	
private:
	System::Classes::TWndMethod FOriginalWndProc;
	System::Types::TRect FFocusRect;
	System::Types::TRect VisibleRect;
	System::Types::TPoint PreviewMousePoint;
	int PreviewWidth;
	int PreviewHeight;
	System::Types::TPoint FullMousePoint;
	bool MouseDown;
	void __fastcall ImageWndProc(Winapi::Messages::TMessage &Message);
	void __fastcall DrawImageFocusRect(const System::Types::TRect &ARect);
	void __fastcall AlignZoomPanel(void);
	void __fastcall UpdateRegionEdits(void);
	void __fastcall SetEditState(Vcl::Controls::TControl* Parent, bool Enabled);
	void __fastcall AddElement(int Index);
	void __fastcall DrawCrossair(const System::Types::TPoint &Point);
	
public:
	Glgui::TGLGuiElementList* TheGuiComponent;
	Glgui::TGLGuiElement* SelectedElement;
	Gltexture::TGLTexture* Tex;
	float Zoom;
	int Width;
	int Height;
	bool __fastcall Edit(Glgui::TGLGuiElementList* GuiComponent);
	void __fastcall Render(void);
	void __fastcall SetMax(Vcl::Stdctrls::TScrollBar* Scrollbar, int Val);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TGUISkinEditor(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TGUISkinEditor(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TGUISkinEditor(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGUISkinEditor(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TGUISkinEditor* GUISkinEditor;
extern PACKAGE bool __fastcall GUIComponentDialog(Glgui::TGLGuiElementList* GuiComponent);
}	/* namespace Guiskineditorformunit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GUISKINEDITORFORMUNIT)
using namespace Guiskineditorformunit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GuiskineditorformunitHPP
