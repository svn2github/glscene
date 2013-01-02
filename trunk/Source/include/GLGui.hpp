// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLGui.pas' rev: 24.00 (Win32)

#ifndef GlguiHPP
#define GlguiHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLBitmapFont.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glgui
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLBaseGuiObject;
class PASCALIMPLEMENTATION TGLBaseGuiObject : public Glscene::TGLBaseSceneObject
{
	typedef Glscene::TGLBaseSceneObject inherited;
	
private:
	bool FRecursiveVisible;
	float FWidth;
	float FHeight;
	
protected:
	DYNAMIC void __fastcall NotifyHide(void);
	DYNAMIC void __fastcall NotifyShow(void);
	void __fastcall SetLeft(const float Value);
	float __fastcall GetLeft(void);
	void __fastcall SetTop(const float Value);
	float __fastcall GetTop(void);
	void __fastcall SetWidth(const float val);
	void __fastcall SetHeight(const float val);
	virtual void __fastcall SetVisible(bool aValue);
	
public:
	__fastcall virtual TGLBaseGuiObject(System::Classes::TComponent* AOwner);
	DYNAMIC void __fastcall AddChild(Glscene::TGLBaseSceneObject* AChild);
	DYNAMIC void __fastcall Insert(int aIndex, Glscene::TGLBaseSceneObject* aChild);
	__property float Width = {read=FWidth, write=SetWidth};
	__property float Height = {read=FHeight, write=SetHeight};
	__property float Left = {read=GetLeft, write=SetLeft};
	__property float Top = {read=GetTop, write=SetTop};
	__property bool RecursiveVisible = {read=FRecursiveVisible, nodefault};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLBaseGuiObject(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLBaseSceneObject(aParentOwner) { }
	/* TGLBaseSceneObject.Destroy */ inline __fastcall virtual ~TGLBaseGuiObject(void) { }
	
};


enum TGUIAlignments : unsigned char { GLAlTopLeft, GLAlTop, GLAlTopRight, GLAlLeft, GLAlCenter, GLAlRight, GLAlBottomLeft, GLAlBottom, GLAlBottomRight, GLAlBorder };

struct DECLSPEC_DRECORD TGUIRect
{
public:
	float X1;
	float Y1;
	float X2;
	float Y2;
	float XTiles;
	float YTiles;
};


typedef System::StaticArray<TGUIRect, 10> TGUIDrawResult;

typedef System::UnicodeString TGLGuiElementName;

class DELPHICLASS TGLGuiElement;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGuiElement : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Glcoordinates::TGLCoordinates2* FTopLeft;
	Glcoordinates::TGLCoordinates2* FBottomRight;
	Glcoordinates::TGLCoordinates2* FScale;
	TGUIAlignments FAlign;
	System::UnicodeString FName;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	void __fastcall SetName(const System::UnicodeString val);
	
public:
	__fastcall virtual TGLGuiElement(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLGuiElement(void);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
__published:
	__property Glcoordinates::TGLCoordinates2* TopLeft = {read=FTopLeft, write=FTopLeft};
	__property Glcoordinates::TGLCoordinates2* BottomRight = {read=FBottomRight, write=FBottomRight};
	__property Glcoordinates::TGLCoordinates2* Scale = {read=FScale, write=FScale};
	__property TGUIAlignments Align = {read=FAlign, write=FAlign, nodefault};
	__property System::UnicodeString Name = {read=FName, write=SetName};
};

#pragma pack(pop)

class DELPHICLASS TGLGuiElementList;
class DELPHICLASS TGLGuiComponent;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGuiElementList : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLGuiElement* operator[](int index) { return Items[index]; }
	
private:
	TGLGuiComponent* FGuiComponent;
	
protected:
	void __fastcall SetItems(int index, TGLGuiElement* const val);
	TGLGuiElement* __fastcall GetItems(int index);
	
public:
	__fastcall TGLGuiElementList(TGLGuiComponent* AOwner);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	int __fastcall IndexOf(TGLGuiElement* const Item);
	__property TGLGuiElement* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLGuiElementList(void) { }
	
};

#pragma pack(pop)

typedef System::UnicodeString TGLGuiComponentName;

class DELPHICLASS TGLGuiComponentList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGuiComponent : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TGLGuiElementList* FElements;
	System::UnicodeString FName;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	void __fastcall SetName(const System::UnicodeString val);
	
public:
	__fastcall virtual TGLGuiComponent(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLGuiComponent(void);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	void __fastcall RenderToArea(float X1, float Y1, float X2, float Y2, TGUIRect *Res, bool Refresh = true, float Scale = 1.000000E+00);
	TGLGuiComponentList* __fastcall GetOwnerList(void);
	__property TGLGuiComponentList* Owner = {read=GetOwnerList};
	
__published:
	__property TGLGuiElementList* Elements = {read=FElements, write=FElements};
	__property System::UnicodeString Name = {read=FName, write=SetName};
};

#pragma pack(pop)

class DELPHICLASS TGLGuiLayout;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGuiComponentList : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLGuiComponent* operator[](int index) { return Items[index]; }
	
private:
	TGLGuiLayout* FLayout;
	
protected:
	void __fastcall SetItems(int index, TGLGuiComponent* const val);
	TGLGuiComponent* __fastcall GetItems(int index);
	
public:
	__fastcall TGLGuiComponentList(TGLGuiLayout* AOwner);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	TGLGuiComponent* __fastcall FindItem(System::UnicodeString name);
	__property TGLGuiComponent* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLGuiComponentList(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGuiLayout : public Baseclasses::TGLUpdateAbleComponent
{
	typedef Baseclasses::TGLUpdateAbleComponent inherited;
	
private:
	Glbitmapfont::TGLCustomBitmapFont* FBitmapFont;
	Glmaterial::TGLMaterial* FMaterial;
	TGLGuiComponentList* FGuiComponents;
	System::UnicodeString FFileName;
	System::Classes::TList* FGuiComponentList;
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetFileName(System::UnicodeString newName);
	
public:
	__fastcall virtual TGLGuiLayout(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLGuiLayout(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	void __fastcall LoadFromFile(System::UnicodeString FN);
	void __fastcall Clear(void);
	void __fastcall SaveToStream(System::Classes::TStream* Stream);
	void __fastcall SaveToFile(System::UnicodeString FN);
	void __fastcall AddGuiComponent(Baseclasses::TGLUpdateAbleComponent* Component);
	void __fastcall RemoveGuiComponent(Baseclasses::TGLUpdateAbleComponent* Component);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	
__published:
	__property Glbitmapfont::TGLCustomBitmapFont* BitmapFont = {read=FBitmapFont, write=FBitmapFont};
	__property Glmaterial::TGLMaterial* Material = {read=FMaterial, write=FMaterial};
	__property TGLGuiComponentList* GuiComponents = {read=FGuiComponents, write=FGuiComponents};
	__property System::UnicodeString FileName = {read=FFileName, write=SetFileName};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TGUIRect GuiNullRect;
extern PACKAGE bool __fastcall IsInRect(const TGUIRect &R, float X, float Y);
}	/* namespace Glgui */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLGUI)
using namespace Glgui;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlguiHPP
