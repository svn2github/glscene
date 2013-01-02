// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSimpleNavigation.pas' rev: 24.00 (Win32)

#ifndef GlsimplenavigationHPP
#define GlsimplenavigationHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.TypInfo.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.ExtCtrls.hpp>	// Pascal unit
#include <GLSceneForm.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLViewer.hpp>	// Pascal unit
#include <GLStrings.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLWin32Viewer.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glsimplenavigation
{
//-- type declarations -------------------------------------------------------
typedef System::Types::TPoint TPoint;

enum TGLSimpleNavigationOption : unsigned char { snoInvertMoveAroundX, snoInvertMoveAroundY, snoInvertZoom, snoInvertMouseWheel, snoInvertRotateX, snoInvertRotateY, snoMouseWheelHandled, snoShowFPS };

typedef System::Set<TGLSimpleNavigationOption, TGLSimpleNavigationOption::snoInvertMoveAroundX, TGLSimpleNavigationOption::snoShowFPS>  TGLSimpleNavigationOptions;

enum TGLSimpleNavigationAction : unsigned char { snaNone, snaMoveAroundTarget, snaZoom, snaRotateTarget, snaCustom };

class DELPHICLASS TGLSimpleNavigationKeyCombination;
typedef void __fastcall (__closure *TSimpleNavigationCustomActionEvent)(TGLSimpleNavigationKeyCombination* Sender, System::Classes::TShiftState Shift, int X, int Y);

class PASCALIMPLEMENTATION TGLSimpleNavigationKeyCombination : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	bool FExitOnMatch;
	TGLSimpleNavigationAction FAction;
	TSimpleNavigationCustomActionEvent FOnCustomAction;
	System::Classes::TShiftState FShiftState;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	virtual void __fastcall DoOnCustomAction(System::Classes::TShiftState Shift, int X, int Y);
	
public:
	__fastcall virtual TGLSimpleNavigationKeyCombination(System::Classes::TCollection* Collection);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property System::Classes::TShiftState ShiftState = {read=FShiftState, write=FShiftState, default=0};
	__property bool ExitOnMatch = {read=FExitOnMatch, write=FExitOnMatch, default=1};
	__property TGLSimpleNavigationAction Action = {read=FAction, write=FAction, default=0};
	__property TSimpleNavigationCustomActionEvent OnCustomAction = {read=FOnCustomAction, write=FOnCustomAction};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TGLSimpleNavigationKeyCombination(void) { }
	
};


class DELPHICLASS TGLSimpleNavigationKeyCombinations;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSimpleNavigationKeyCombinations : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLSimpleNavigationKeyCombination* operator[](int Index) { return Items[Index]; }
	
private:
	TGLSimpleNavigationKeyCombination* __fastcall GetItems(int Index);
	void __fastcall SetItems(int Index, TGLSimpleNavigationKeyCombination* const Value);
	
public:
	HIDESBASE TGLSimpleNavigationKeyCombination* __fastcall Add(void)/* overload */;
	HIDESBASE TGLSimpleNavigationKeyCombination* __fastcall Add(const System::Classes::TShiftState AShiftState, const TGLSimpleNavigationAction AAction, const bool AExitOnMatch = true)/* overload */;
	__property TGLSimpleNavigationKeyCombination* Items[int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TOwnedCollection.Create */ inline __fastcall TGLSimpleNavigationKeyCombinations(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass ItemClass) : System::Classes::TOwnedCollection(AOwner, ItemClass) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLSimpleNavigationKeyCombinations(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLSimpleNavigation;
class PASCALIMPLEMENTATION TGLSimpleNavigation : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Vcl::Extctrls::TTimer* FTimer;
	Vcl::Forms::TCustomForm* FForm;
	Glwin32viewer::TGLSceneViewer* FGLSceneViewer;
	int FOldX;
	int FOldY;
	System::UnicodeString FFormCaption;
	float FMoveAroundTargetSpeed;
	float FZoomSpeed;
	TGLSimpleNavigationOptions FOptions;
	TGLSimpleNavigationKeyCombinations* FKeyCombinations;
	float FRotateTargetSpeed;
	Vcl::Controls::TMouseMoveEvent FOnMouseMove;
	bool FSceneForm;
	void __fastcall ShowFPS(System::TObject* Sender);
	void __fastcall ViewerMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall ViewerMouseWheel(System::TObject* Sender, System::Classes::TShiftState Shift, int WheelDelta, const System::Types::TPoint &MousePos, bool &Handled);
	void __fastcall SetGLSceneViewer(Glwin32viewer::TGLSceneViewer* const Value);
	void __fastcall SetForm(Vcl::Forms::TCustomForm* const Value);
	bool __fastcall StoreFormCaption(void);
	bool __fastcall StoreMoveAroundTargetSpeed(void);
	bool __fastcall StoreZoomSpeed(void);
	void __fastcall SetKeyCombinations(TGLSimpleNavigationKeyCombinations* const Value);
	bool __fastcall StoreRotateTargetSpeed(void);
	void __fastcall SetOptions(const TGLSimpleNavigationOptions Value);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLSimpleNavigation(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSimpleNavigation(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Vcl::Forms::TCustomForm* Form = {read=FForm, write=SetForm};
	__property Glwin32viewer::TGLSceneViewer* GLSceneViewer = {read=FGLSceneViewer, write=SetGLSceneViewer};
	__property float ZoomSpeed = {read=FZoomSpeed, write=FZoomSpeed, stored=StoreZoomSpeed};
	__property float MoveAroundTargetSpeed = {read=FMoveAroundTargetSpeed, write=FMoveAroundTargetSpeed, stored=StoreMoveAroundTargetSpeed};
	__property float RotateTargetSpeed = {read=FRotateTargetSpeed, write=FRotateTargetSpeed, stored=StoreRotateTargetSpeed};
	__property System::UnicodeString FormCaption = {read=FFormCaption, write=FFormCaption, stored=StoreFormCaption};
	__property TGLSimpleNavigationOptions Options = {read=FOptions, write=SetOptions, default=192};
	__property TGLSimpleNavigationKeyCombinations* KeyCombinations = {read=FKeyCombinations, write=SetKeyCombinations};
	__property Vcl::Controls::TMouseMoveEvent OnMouseMove = {read=FOnMouseMove, write=FOnMouseMove};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glsimplenavigation */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSIMPLENAVIGATION)
using namespace Glsimplenavigation;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsimplenavigationHPP
