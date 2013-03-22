// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLGizmo.pas' rev: 24.00 (Win32)

#ifndef GlgizmoHPP
#define GlgizmoHPP

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
#include <GLColor.hpp>	// Pascal unit
#include <GLObjects.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLStrings.hpp>	// Pascal unit
#include <GLGeomObjects.hpp>	// Pascal unit
#include <GLBitmapFont.hpp>	// Pascal unit
#include <GLViewer.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLSelection.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <GLWin32Viewer.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glgizmo
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLGizmoUndoItem;
class DELPHICLASS TGLGizmoUndoCollection;
class DELPHICLASS TGLGizmo;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGizmoUndoItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FOldLibMaterialName;
	Glcoordinates::TGLCoordinates3* FOldAutoScaling;
	Glscene::TGLCustomSceneObject* FEffectedObject;
	Vectortypes::TMatrix4f FOldMatr;
	Vectortypes::TMatrix4f FOldMatrix;
	void __fastcall SetEffectedObject(Glscene::TGLCustomSceneObject* const Value);
	void __fastcall SetOldAutoScaling(Glcoordinates::TGLCoordinates3* const Value);
	void __fastcall SetOldMatrix(const Vectortypes::TMatrix4f &Value);
	
protected:
	virtual void __fastcall DoUndo(void);
	TGLGizmoUndoCollection* __fastcall GetParent(void);
	TGLGizmo* __fastcall GetGizmo(void);
	
public:
	__fastcall virtual TGLGizmoUndoItem(System::Classes::TCollection* AOwner);
	__fastcall virtual ~TGLGizmoUndoItem(void);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall AssignFromObject(Glscene::TGLCustomSceneObject* const AObject);
	__property Vectortypes::TMatrix4f OldMatrix = {read=FOldMatrix, write=SetOldMatrix};
	
__published:
	__property Glscene::TGLCustomSceneObject* EffectedObject = {read=FEffectedObject, write=SetEffectedObject};
	__property Glcoordinates::TGLCoordinates3* OldAutoScaling = {read=FOldAutoScaling, write=SetOldAutoScaling};
	__property System::UnicodeString OldLibMaterialName = {read=FOldLibMaterialName, write=FOldLibMaterialName};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGizmoUndoCollection : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLGizmoUndoItem* operator[](const int Index) { return Items[Index]; }
	
private:
	TGLGizmoUndoItem* __fastcall GetItems(const int Index);
	void __fastcall SetItems(const int Index, TGLGizmoUndoItem* const Value);
	
protected:
	TGLGizmo* __fastcall GetParent(void);
	
public:
	void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall RemoveByObject(Glscene::TGLCustomSceneObject* const AObject);
	HIDESBASE TGLGizmoUndoItem* __fastcall Add(void);
	__property TGLGizmoUndoItem* Items[const int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TOwnedCollection.Create */ inline __fastcall TGLGizmoUndoCollection(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass ItemClass) : System::Classes::TOwnedCollection(AOwner, ItemClass) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLGizmoUndoCollection(void) { }
	
};

#pragma pack(pop)

enum TGLGizmoElement : unsigned char { geMove, geRotate, geScale, geAxisLabel, geObjectInfos, geBoundingBox };

typedef System::Set<TGLGizmoElement, TGLGizmoElement::geMove, TGLGizmoElement::geBoundingBox>  TGLGizmoElements;

enum TGLGizmoVisibleInfoLabel : unsigned char { vliName, vliOperation, vliCoords };

typedef System::Set<TGLGizmoVisibleInfoLabel, TGLGizmoVisibleInfoLabel::vliName, TGLGizmoVisibleInfoLabel::vliCoords>  TGLGizmoVisibleInfoLabels;

enum TGLGizmoAxis : unsigned char { gaNone, gaX, gaY, gaZ, gaXY, gaXZ, gaYZ };

enum TGLGizmoOperation : unsigned char { gopMove, gopRotate, gopScale, gopNone, gpMoveGizmo, GpRotateGizmo };

typedef void __fastcall (__closure *TGLGizmoAcceptEvent)(System::TObject* Sender, Glscene::TGLBaseSceneObject* &Obj, bool &Accept, Vectortypes::TVector4f &Dimensions);

typedef void __fastcall (__closure *TGLGizmoUpdateEvent)(System::TObject* Sender, Glscene::TGLBaseSceneObject* Obj, TGLGizmoAxis Axis, TGLGizmoOperation Operation, Vectortypes::TVector4f &Vector);

enum TGLGizmoPickMode : unsigned char { pmGetPickedObjects, pmRayCast };

class DELPHICLASS TGLGizmoRayCastHitData;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGizmoRayCastHitData : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
public:
	Glscene::TGLBaseSceneObject* Obj;
	Vectortypes::TVector4f Point;
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLGizmoRayCastHitData(void) { }
	
public:
	/* TObject.Create */ inline __fastcall TGLGizmoRayCastHitData(void) : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLGizmoPickCube;
class PASCALIMPLEMENTATION TGLGizmoPickCube : public Globjects::TGLCube
{
	typedef Globjects::TGLCube inherited;
	
public:
	/* TGLCube.Create */ inline __fastcall virtual TGLGizmoPickCube(System::Classes::TComponent* AOwner) : Globjects::TGLCube(AOwner) { }
	
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLGizmoPickCube(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoPickCube(Glscene::TGLBaseSceneObject* aParentOwner) : Globjects::TGLCube(aParentOwner) { }
	
};


class DELPHICLASS TGLGizmoPickTorus;
class PASCALIMPLEMENTATION TGLGizmoPickTorus : public Glgeomobjects::TGLTorus
{
	typedef Glgeomobjects::TGLTorus inherited;
	
public:
	/* TGLTorus.Create */ inline __fastcall virtual TGLGizmoPickTorus(System::Classes::TComponent* AOwner) : Glgeomobjects::TGLTorus(AOwner) { }
	
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLGizmoPickTorus(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoPickTorus(Glscene::TGLBaseSceneObject* aParentOwner) : Glgeomobjects::TGLTorus(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmo : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Glscene::TGLBaseSceneObject* _GZObaseGizmo;
	Globjects::TGLCube* _GZOBoundingcube;
	Glscene::TGLBaseSceneObject* _GZOrootHelpers;
	Glscene::TGLBaseSceneObject* _GZOrootLines;
	Glscene::TGLBaseSceneObject* _GZOrootTorus;
	Glscene::TGLBaseSceneObject* _GZOrootCubes;
	Glscene::TGLBaseSceneObject* _GZORootAxisLabel;
	Glscene::TGLBaseSceneObject* _GZORootVisibleInfoLabels;
	Globjects::TGLLines* _GZOlineX;
	Globjects::TGLLines* _GZOlineY;
	Globjects::TGLLines* _GZOlineZ;
	Globjects::TGLLines* _GZOplaneXY;
	Globjects::TGLLines* _GZOplaneXZ;
	Globjects::TGLLines* _GZOplaneYZ;
	TGLGizmoPickTorus* _GZOTorusX;
	TGLGizmoPickTorus* _GZOTorusY;
	TGLGizmoPickTorus* _GZOTorusZ;
	TGLGizmoPickCube* _GZOCubeX;
	TGLGizmoPickCube* _GZOCubeY;
	TGLGizmoPickCube* _GZOCubeZ;
	Glbitmapfont::TGLFlatText* _GZOAxisLabelX;
	Glbitmapfont::TGLFlatText* _GZOAxisLabelY;
	Glbitmapfont::TGLFlatText* _GZOAxisLabelZ;
	Glbitmapfont::TGLFlatText* _GZOVisibleInfoLabels;
	Glscene::TGLBaseSceneObject* FRootGizmo;
	Glscene::TGLBaseSceneObject* FSelectedObj;
	TGLGizmoOperation FOperation;
	TGLGizmoAxis FSelAxis;
	Glcolor::TGLColor* FBoundingBoxColor;
	Glcolor::TGLColor* FSelectedColor;
	Glcolor::TGLColor* FVisibleInfoLabelsColor;
	bool FBoundingBoxColorChanged;
	bool FVisibleInfoLabelsColorChanged;
	bool FForceOperation;
	bool FForceAxis;
	bool FForceUniformScale;
	bool FAutoZoom;
	bool FExcludeObjects;
	bool FNoZWrite;
	bool FEnabled;
	float FAutoZoomFactor;
	float FZoomFactor;
	float FMoveCoef;
	float FRotationCoef;
	Glwin32viewer::TGLSceneViewer* FViewer;
	TGLGizmoElements FGizmoElements;
	TGLGizmoVisibleInfoLabels FVisibleVisibleInfoLabels;
	System::Classes::TStrings* FExcludeObjectsList;
	bool Moving;
	int Mx;
	int My;
	int Rx;
	int Ry;
	Glscene::TGLDirectOpenGL* dglEnable;
	Glscene::TGLDirectOpenGL* dglDisable;
	Glscene::TGLDirectOpenGL* dgtEnable;
	Glscene::TGLDirectOpenGL* dgtDisable;
	Glscene::TGLDirectOpenGL* dgcEnable;
	Glscene::TGLDirectOpenGL* dgcDisable;
	Glscene::TGLDirectOpenGL* dglaEnable;
	Glscene::TGLDirectOpenGL* dglaDisable;
	Glscene::TGLDirectOpenGL* dgliEnable;
	Glscene::TGLDirectOpenGL* dgliDisable;
	Vectortypes::TVector4f LastMousePos;
	Vectortypes::TVector4f ObjDimensions;
	TGLGizmoAcceptEvent FOnBeforeSelect;
	TGLGizmoUpdateEvent FOnBeforeUpdate;
	System::Classes::TNotifyEvent FOnSelectionLost;
	float FScaleCoef;
	float FGizmoThickness;
	TGLGizmoPickMode FPickMode;
	System::Classes::TList* FInternalRaycastHitData;
	TGLGizmoUndoCollection* FUndoHistory;
	Glbitmapfont::TGLCustomBitmapFont* FLabelFont;
	void __fastcall SetRootGizmo(Glscene::TGLBaseSceneObject* const AValue);
	void __fastcall SetGizmoElements(const TGLGizmoElements AValue);
	void __fastcall SeTGLGizmoVisibleInfoLabels(const TGLGizmoVisibleInfoLabels AValue);
	void __fastcall SetBoundingBoxColor(Glcolor::TGLColor* const AValue);
	void __fastcall SetSelectedColor(Glcolor::TGLColor* const AValue);
	void __fastcall SetVisibleInfoLabelsColor(Glcolor::TGLColor* const AValue);
	void __fastcall SetExcludeObjectsList(System::Classes::TStrings* const AValue);
	void __fastcall DirectGlDisable(System::TObject* Sender, Glrendercontextinfo::TRenderContextInfo &Rci);
	void __fastcall DirectGlEnable(System::TObject* Sender, Glrendercontextinfo::TRenderContextInfo &Rci);
	Vectortypes::TVector4f __fastcall MouseWorldPos(const int X, const int Y);
	bool __fastcall CheckObjectInExcludeList(Glscene::TGLBaseSceneObject* const Obj);
	void __fastcall UpdateVisibleInfoLabels(void);
	void __fastcall SetGLGizmoThickness(const float Value);
	Glselection::TGLPickList* __fastcall InternalGetPickedObjects(const int X1, const int Y1, const int X2, const int Y2, const int GuessCount = 0x8);
	void __fastcall ClearInternalRaycastHitData(void);
	void __fastcall SetViewer(Glwin32viewer::TGLSceneViewer* const Value);
	void __fastcall SetLabelFont(Glbitmapfont::TGLCustomBitmapFont* const Value);
	void __fastcall SetSelectedObj(Glscene::TGLBaseSceneObject* const Value);
	
public:
	System::Classes::TList* PickableObjectsWithRayCast;
	__fastcall virtual TGLGizmo(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLGizmo(void);
	virtual void __fastcall Loaded(void);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall ViewerMouseMove(const int X, const int Y);
	void __fastcall ViewerMouseDown(const int X, const int Y);
	void __fastcall ViewerMouseUp(const int X, const int Y);
	void __fastcall UpdateGizmo(void)/* overload */;
	void __fastcall UpdateGizmo(const Vectortypes::TVector4f &NewDimensions)/* overload */;
	void __fastcall SetVisible(const bool AValue);
	Vectortypes::TVector4f __fastcall GetPickedObjectPoint(Glscene::TGLBaseSceneObject* const Obj);
	virtual void __fastcall LooseSelection(void);
	void __fastcall UndoAdd(Glscene::TGLCustomSceneObject* const AObject);
	__property Glscene::TGLBaseSceneObject* RootGizmo = {read=FRootGizmo, write=SetRootGizmo};
	
__published:
	__property Glwin32viewer::TGLSceneViewer* Viewer = {read=FViewer, write=SetViewer};
	__property TGLGizmoElements GizmoElements = {read=FGizmoElements, write=SetGizmoElements, nodefault};
	__property Glcolor::TGLColor* BoundingBoxColor = {read=FBoundingBoxColor, write=SetBoundingBoxColor};
	__property Glcolor::TGLColor* SelectedColor = {read=FSelectedColor, write=SetSelectedColor};
	__property TGLGizmoAxis SelAxis = {read=FSelAxis, write=FSelAxis, nodefault};
	__property bool ForceAxis = {read=FForceAxis, write=FForceAxis, nodefault};
	__property Glscene::TGLBaseSceneObject* SelectedObj = {read=FSelectedObj, write=SetSelectedObj};
	__property TGLGizmoOperation Operation = {read=FOperation, write=FOperation, nodefault};
	__property bool ForceOperation = {read=FForceOperation, write=FForceOperation, nodefault};
	__property bool ForceUniformScale = {read=FForceUniformScale, write=FForceUniformScale, nodefault};
	__property bool ExcludeObjects = {read=FExcludeObjects, write=FExcludeObjects, nodefault};
	__property System::Classes::TStrings* ExcludeObjectsList = {read=FExcludeObjectsList, write=SetExcludeObjectsList};
	__property TGLGizmoVisibleInfoLabels VisibleInfoLabels = {read=FVisibleVisibleInfoLabels, write=SeTGLGizmoVisibleInfoLabels, nodefault};
	__property Glcolor::TGLColor* VisibleInfoLabelsColor = {read=FVisibleInfoLabelsColor, write=SetVisibleInfoLabelsColor};
	__property bool AutoZoom = {read=FAutoZoom, write=FAutoZoom, nodefault};
	__property float AutoZoomFactor = {read=FAutoZoomFactor, write=FAutoZoomFactor};
	__property float ZoomFactor = {read=FZoomFactor, write=FZoomFactor};
	__property float MoveCoef = {read=FMoveCoef, write=FMoveCoef};
	__property float RotationCoef = {read=FRotationCoef, write=FRotationCoef};
	__property float ScaleCoef = {read=FScaleCoef, write=FScaleCoef};
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, nodefault};
	__property float GizmoThickness = {read=FGizmoThickness, write=SetGLGizmoThickness};
	__property bool Enabled = {read=FEnabled, write=FEnabled, default=0};
	__property Glbitmapfont::TGLCustomBitmapFont* LabelFont = {read=FLabelFont, write=SetLabelFont, default=0};
	__property TGLGizmoAcceptEvent OnBeforeSelect = {read=FOnBeforeSelect, write=FOnBeforeSelect};
	__property System::Classes::TNotifyEvent OnSelectionLost = {read=FOnSelectionLost, write=FOnSelectionLost};
	__property TGLGizmoUpdateEvent OnBeforeUpdate = {read=FOnBeforeUpdate, write=FOnBeforeUpdate};
	__property TGLGizmoPickMode PickMode = {read=FPickMode, write=FPickMode, default=0};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glgizmo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLGIZMO)
using namespace Glgizmo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlgizmoHPP
