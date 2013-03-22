// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLObjectManager.pas' rev: 24.00 (Win32)

#ifndef GlobjectmanagerHPP
#define GlobjectmanagerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.Menus.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Globjectmanager
{
//-- type declarations -------------------------------------------------------
struct TGLSceneObjectEntry;
typedef TGLSceneObjectEntry *PSceneObjectEntry;

struct DECLSPEC_DRECORD TGLSceneObjectEntry
{
public:
	Glscene::TGLSceneObjectClass ObjectClass;
	System::UnicodeString Name;
	System::UnicodeString Category;
	int Index;
	int ImageIndex;
};


class DELPHICLASS TObjectManager;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TObjectManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Classes::TList* FSceneObjectList;
	Vcl::Controls::TImageList* FObjectIcons;
	int FOverlayIndex;
	int FSceneRootIndex;
	int FCameraRootIndex;
	int FLightsourceRootIndex;
	int FObjectRootIndex;
	
protected:
	void __fastcall DestroySceneObjectList(void);
	PSceneObjectEntry __fastcall FindSceneObjectClass(Glscene::TGLSceneObjectClass AObjectClass, const System::UnicodeString ASceneObject = System::UnicodeString());
	
public:
	__fastcall virtual TObjectManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TObjectManager(void);
	void __fastcall CreateDefaultObjectIcons(unsigned ResourceModule);
	Glscene::TGLSceneObjectClass __fastcall GetClassFromIndex(int Index);
	int __fastcall GetImageIndex(Glscene::TGLSceneObjectClass ASceneObject);
	System::UnicodeString __fastcall GetCategory(Glscene::TGLSceneObjectClass ASceneObject);
	void __fastcall GetRegisteredSceneObjects(System::Classes::TStringList* ObjectList);
	void __fastcall PopulateMenuWithRegisteredSceneObjects(Vcl::Menus::TMenuItem* AMenuItem, System::Classes::TNotifyEvent aClickEvent);
	void __fastcall RegisterSceneObject(Glscene::TGLSceneObjectClass ASceneObject, const System::UnicodeString aName, const System::UnicodeString aCategory)/* overload */;
	void __fastcall RegisterSceneObject(Glscene::TGLSceneObjectClass ASceneObject, const System::UnicodeString aName, const System::UnicodeString aCategory, Vcl::Graphics::TBitmap* aBitmap)/* overload */;
	void __fastcall RegisterSceneObject(Glscene::TGLSceneObjectClass ASceneObject, const System::UnicodeString aName, const System::UnicodeString aCategory, unsigned ResourceModule, System::UnicodeString ResourceName = System::UnicodeString())/* overload */;
	void __fastcall UnRegisterSceneObject(Glscene::TGLSceneObjectClass ASceneObject);
	__property Vcl::Controls::TImageList* ObjectIcons = {read=FObjectIcons};
	__property int SceneRootIndex = {read=FSceneRootIndex, nodefault};
	__property int LightsourceRootIndex = {read=FLightsourceRootIndex, nodefault};
	__property int CameraRootIndex = {read=FCameraRootIndex, nodefault};
	__property int ObjectRootIndex = {read=FObjectRootIndex, nodefault};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Globjectmanager */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLOBJECTMANAGER)
using namespace Globjectmanager;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlobjectmanagerHPP
