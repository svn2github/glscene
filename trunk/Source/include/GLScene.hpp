// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLScene.pas' rev: 24.00 (Win32)

#ifndef GlsceneHPP
#define GlsceneHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <XCollection.hpp>	// Pascal unit
#include <GLSilhouette.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <GeometryBB.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <GLSelection.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <System.UITypes.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glscene
{
//-- type declarations -------------------------------------------------------
enum TGLProxyObjectOption : unsigned char { pooEffects, pooObjects, pooTransformation };

typedef System::Set<TGLProxyObjectOption, TGLProxyObjectOption::pooEffects, TGLProxyObjectOption::pooTransformation>  TGLProxyObjectOptions;

enum TGLCameraInvarianceMode : unsigned char { cimNone, cimPosition, cimOrientation };

enum TGLSceneViewerMode : unsigned char { svmDisabled, svmDefault, svmNavigation, svmGizmo };

enum TNormalDirection : unsigned char { ndInside, ndOutside };

enum TObjectChange : unsigned char { ocTransformation, ocAbsoluteMatrix, ocInvAbsoluteMatrix, ocStructure };

typedef System::Set<TObjectChange, TObjectChange::ocTransformation, TObjectChange::ocStructure>  TObjectChanges;

enum TObjectBBChange : unsigned char { oBBcChild, oBBcStructure };

typedef System::Set<TObjectBBChange, TObjectBBChange::oBBcChild, TObjectBBChange::oBBcStructure>  TObjectBBChanges;

enum TSceneOperation : unsigned char { soAdd, soRemove, soMove, soRename, soSelect, soBeginUpdate, soEndUpdate };

enum TContextOption : unsigned char { roSoftwareMode, roDoubleBuffer, roStencilBuffer, roRenderToWindow, roTwoSideLighting, roStereo, roDestinationAlpha, roNoColorBuffer, roNoColorBufferClear, roNoSwapBuffers, roNoDepthBufferClear, roDebugContext, roForwardContext, roOpenGL_ES2_Context };

typedef System::Set<TContextOption, TContextOption::roSoftwareMode, TContextOption::roOpenGL_ES2_Context>  TContextOptions;

enum TLimitType : unsigned char { limClipPlanes, limEvalOrder, limLights, limListNesting, limModelViewStack, limNameStack, limPixelMapTable, limProjectionStack, limTextureSize, limTextureStack, limViewportDims, limAccumAlphaBits, limAccumBlueBits, limAccumGreenBits, limAccumRedBits, limAlphaBits, limAuxBuffers, limBlueBits, limGreenBits, limRedBits, limIndexBits, limStereo, limDoubleBuffer, limSubpixelBits, limDepthBits, limStencilBits, limNbTextureUnits };

typedef System::TMetaClass* TGLSceneObjectClass;

typedef System::TMetaClass* TGLBehaviourClass;

typedef System::TMetaClass* TGLObjectEffectClass;

enum TGLObjectStyle : unsigned char { osDirectDraw, osIgnoreDepthBuffer, osNoVisibilityCulling };

typedef System::Set<TGLObjectStyle, TGLObjectStyle::osDirectDraw, TGLObjectStyle::osNoVisibilityCulling>  TGLObjectStyles;

__interface IGLInitializable;
typedef System::DelphiInterface<IGLInitializable> _di_IGLInitializable;
__interface  INTERFACE_UUID("{EA40AE8E-79B3-42F5-ADF1-7A901B665E12}") IGLInitializable  : public System::IInterface 
{
	
public:
	virtual void __fastcall InitializeObject(System::TObject* ASender, const Glrendercontextinfo::TRenderContextInfo &ARci) = 0 ;
};

class DELPHICLASS TGLInitializableObjectList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLInitializableObjectList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	_di_IGLInitializable operator[](const int Index) { return Items[Index]; }
	
private:
	_di_IGLInitializable __fastcall GetItems(const int Index);
	void __fastcall PutItems(const int Index, const _di_IGLInitializable Value);
	
public:
	HIDESBASE int __fastcall Add(const _di_IGLInitializable Item);
	__property _di_IGLInitializable Items[const int Index] = {read=GetItems, write=PutItems/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TGLInitializableObjectList(void) { }
	
public:
	/* TObject.Create */ inline __fastcall TGLInitializableObjectList(void) : System::Classes::TList() { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLBaseSceneObject;
class DELPHICLASS TGLScene;
class DELPHICLASS TGLBehaviours;
class DELPHICLASS TGLObjectEffects;
class DELPHICLASS TGLBehaviour;
class DELPHICLASS TGLObjectEffect;
class PASCALIMPLEMENTATION TGLBaseSceneObject : public Glcoordinates::TGLCoordinatesUpdateAbleComponent
{
	typedef Glcoordinates::TGLCoordinatesUpdateAbleComponent inherited;
	
public:
	TGLBaseSceneObject* operator[](int Index) { return Children[Index]; }
	
private:
	Vectortypes::TMatrix4f *FAbsoluteMatrix;
	Vectortypes::TMatrix4f *FInvAbsoluteMatrix;
	Vectortypes::TMatrix4f *FLocalMatrix;
	TGLObjectStyles FObjectStyle;
	Glcontext::TGLListHandle* FListHandle;
	Glcoordinates::TGLCoordinates3* FPosition;
	Glcoordinates::TGLCoordinates3* FDirection;
	Glcoordinates::TGLCoordinates3* FUp;
	Glcoordinates::TGLCoordinates3* FScaling;
	TObjectChanges FChanges;
	TGLBaseSceneObject* FParent;
	TGLScene* FScene;
	TObjectBBChanges FBBChanges;
	Geometrybb::THmgBoundingBox FBoundingBoxPersonalUnscaled;
	Geometrybb::THmgBoundingBox FBoundingBoxOfChildren;
	Geometrybb::THmgBoundingBox FBoundingBoxIncludingChildren;
	Persistentclasses::TPersistentObjectList* FChildren;
	bool FVisible;
	int FUpdateCount;
	bool FShowAxes;
	Glcoordinates::TGLCoordinates3* FRotation;
	bool FIsCalculating;
	Glrendercontextinfo::TGLObjectsSorting FObjectsSorting;
	Glrendercontextinfo::TGLVisibilityCulling FVisibilityCulling;
	Baseclasses::TGLProgressEvent FOnProgress;
	System::Classes::TNotifyEvent FOnAddedToParent;
	TGLBehaviours* FGLBehaviours;
	TGLObjectEffects* FGLObjectEffects;
	bool FPickable;
	System::Classes::TNotifyEvent FOnPicked;
	System::TObject* FTagObject;
	float FTagFloat;
	TGLBaseSceneObject* __fastcall Get(int Index);
	int __fastcall GetCount(void);
	int __fastcall GetIndex(void);
	void __fastcall SetParent(TGLBaseSceneObject* const val);
	void __fastcall SetIndex(int aValue);
	void __fastcall SetDirection(Glcoordinates::TGLCoordinates3* AVector);
	void __fastcall SetUp(Glcoordinates::TGLCoordinates3* AVector);
	Vectortypes::TMatrix4f __fastcall GetMatrix(void);
	void __fastcall SetMatrix(const Vectortypes::TMatrix4f &aValue);
	void __fastcall SetPosition(Glcoordinates::TGLCoordinates3* APosition);
	void __fastcall SetPitchAngle(float AValue);
	void __fastcall SetRollAngle(float AValue);
	void __fastcall SetTurnAngle(float AValue);
	void __fastcall SetRotation(Glcoordinates::TGLCoordinates3* aRotation);
	float __fastcall GetPitchAngle(void);
	float __fastcall GetTurnAngle(void);
	float __fastcall GetRollAngle(void);
	void __fastcall SetShowAxes(bool AValue);
	void __fastcall SetScaling(Glcoordinates::TGLCoordinates3* AValue);
	void __fastcall SetObjectsSorting(const Glrendercontextinfo::TGLObjectsSorting val);
	void __fastcall SetVisibilityCulling(const Glrendercontextinfo::TGLVisibilityCulling val);
	void __fastcall SetBehaviours(TGLBehaviours* const val);
	TGLBehaviours* __fastcall GetBehaviours(void);
	void __fastcall SetEffects(TGLObjectEffects* const val);
	TGLObjectEffects* __fastcall GetEffects(void);
	Vectortypes::TVector3f __fastcall GetAbsoluteAffineScale(void);
	Vectortypes::TVector4f __fastcall GetAbsoluteScale(void);
	void __fastcall SetAbsoluteAffineScale(const Vectortypes::TVector3f &Value);
	void __fastcall SetAbsoluteScale(const Vectortypes::TVector4f &Value);
	Vectortypes::TMatrix4f __fastcall GetAbsoluteMatrix(void);
	void __fastcall SetAbsoluteMatrix(const Vectortypes::TMatrix4f &Value);
	void __fastcall SetBBChanges(const TObjectBBChanges Value);
	
protected:
	virtual void __fastcall Loaded(void);
	virtual void __fastcall SetScene(TGLScene* const Value);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall WriteBehaviours(System::Classes::TStream* stream);
	void __fastcall ReadBehaviours(System::Classes::TStream* stream);
	void __fastcall WriteEffects(System::Classes::TStream* stream);
	void __fastcall ReadEffects(System::Classes::TStream* stream);
	void __fastcall WriteRotations(System::Classes::TStream* stream);
	void __fastcall ReadRotations(System::Classes::TStream* stream);
	virtual bool __fastcall GetVisible(void);
	virtual bool __fastcall GetPickable(void);
	virtual void __fastcall SetVisible(bool aValue);
	virtual void __fastcall SetPickable(bool aValue);
	void __fastcall SetAbsolutePosition(const Vectortypes::TVector4f &v);
	Vectortypes::TVector4f __fastcall GetAbsolutePosition(void);
	void __fastcall SetAbsoluteUp(const Vectortypes::TVector4f &v);
	Vectortypes::TVector4f __fastcall GetAbsoluteUp(void);
	void __fastcall SetAbsoluteDirection(const Vectortypes::TVector4f &v);
	Vectortypes::TVector4f __fastcall GetAbsoluteDirection(void);
	Vectortypes::TVector3f __fastcall GetAbsoluteAffinePosition(void);
	void __fastcall SetAbsoluteAffinePosition(const Vectortypes::TVector3f &Value);
	void __fastcall SetAbsoluteAffineUp(const Vectortypes::TVector3f &v);
	Vectortypes::TVector3f __fastcall GetAbsoluteAffineUp(void);
	void __fastcall SetAbsoluteAffineDirection(const Vectortypes::TVector3f &v);
	Vectortypes::TVector3f __fastcall GetAbsoluteAffineDirection(void);
	void __fastcall RecTransformationChanged(void);
	void __fastcall DrawAxes(Glrendercontextinfo::TRenderContextInfo &rci, System::Word pattern);
	DYNAMIC void __fastcall GetChildren(System::Classes::TGetChildProc AProc, System::Classes::TComponent* Root);
	virtual bool __fastcall Blended(void);
	void __fastcall RebuildMatrix(void);
	virtual void __fastcall SetName(const System::Classes::TComponentName NewName);
	DYNAMIC void __fastcall SetParentComponent(System::Classes::TComponent* Value);
	DYNAMIC void __fastcall DestroyHandle(void);
	void __fastcall DestroyHandles(void);
	void __fastcall DeleteChildCameras(void);
	virtual void __fastcall DoOnAddedToParent(void);
	virtual void __fastcall CalculateBoundingBoxPersonalUnscaled(Geometrybb::THmgBoundingBox &ANewBoundingBox);
	
public:
	__fastcall virtual TGLBaseSceneObject(System::Classes::TComponent* AOwner);
	__fastcall TGLBaseSceneObject(TGLBaseSceneObject* aParentOwner);
	__fastcall virtual ~TGLBaseSceneObject(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property TGLObjectStyles ObjectStyle = {read=FObjectStyle, write=FObjectStyle, nodefault};
	virtual unsigned __fastcall GetHandle(Glrendercontextinfo::TRenderContextInfo &rci);
	bool __fastcall ListHandleAllocated(void);
	__property Vectortypes::TMatrix4f Matrix = {read=GetMatrix, write=SetMatrix};
	Vectorgeometry::PMatrix __fastcall MatrixAsAddress(void);
	__property Vectorgeometry::PMatrix LocalMatrix = {read=FLocalMatrix};
	void __fastcall ForceLocalMatrix(const Vectortypes::TMatrix4f &aMatrix);
	Vectorgeometry::PMatrix __fastcall AbsoluteMatrixAsAddress(void);
	__property Vectorgeometry::PMatrix DirectAbsoluteMatrix = {read=FAbsoluteMatrix};
	Vectortypes::TMatrix4f __fastcall InvAbsoluteMatrix(void);
	Vectorgeometry::PMatrix __fastcall InvAbsoluteMatrixAsAddress(void);
	__property Vectortypes::TMatrix4f AbsoluteMatrix = {read=GetAbsoluteMatrix, write=SetAbsoluteMatrix};
	__property Vectortypes::TVector4f AbsoluteDirection = {read=GetAbsoluteDirection, write=SetAbsoluteDirection};
	__property Vectortypes::TVector3f AbsoluteAffineDirection = {read=GetAbsoluteAffineDirection, write=SetAbsoluteAffineDirection};
	__property Vectortypes::TVector4f AbsoluteScale = {read=GetAbsoluteScale, write=SetAbsoluteScale};
	__property Vectortypes::TVector3f AbsoluteAffineScale = {read=GetAbsoluteAffineScale, write=SetAbsoluteAffineScale};
	__property Vectortypes::TVector4f AbsoluteUp = {read=GetAbsoluteUp, write=SetAbsoluteUp};
	__property Vectortypes::TVector3f AbsoluteAffineUp = {read=GetAbsoluteAffineUp, write=SetAbsoluteAffineUp};
	Vectortypes::TVector4f __fastcall AbsoluteRight(void);
	Vectortypes::TVector4f __fastcall AbsoluteLeft(void);
	__property Vectortypes::TVector4f AbsolutePosition = {read=GetAbsolutePosition, write=SetAbsolutePosition};
	__property Vectortypes::TVector3f AbsoluteAffinePosition = {read=GetAbsoluteAffinePosition, write=SetAbsoluteAffinePosition};
	Vectorgeometry::PVector __fastcall AbsolutePositionAsAddress(void);
	Vectortypes::TVector4f __fastcall AbsoluteXVector(void);
	Vectortypes::TVector4f __fastcall AbsoluteYVector(void);
	Vectortypes::TVector4f __fastcall AbsoluteZVector(void);
	Vectortypes::TVector4f __fastcall AbsoluteToLocal(const Vectortypes::TVector4f &v)/* overload */;
	Vectortypes::TVector3f __fastcall AbsoluteToLocal(const Vectortypes::TVector3f &v)/* overload */;
	Vectortypes::TVector4f __fastcall LocalToAbsolute(const Vectortypes::TVector4f &v)/* overload */;
	Vectortypes::TVector3f __fastcall LocalToAbsolute(const Vectortypes::TVector3f &v)/* overload */;
	Vectortypes::TVector4f __fastcall Right(void);
	Vectortypes::TVector4f __fastcall LeftVector(void);
	Vectortypes::TVector3f __fastcall AffineRight(void);
	Vectortypes::TVector3f __fastcall AffineLeftVector(void);
	float __fastcall SqrDistanceTo(TGLBaseSceneObject* anObject)/* overload */;
	float __fastcall SqrDistanceTo(const Vectortypes::TVector4f &pt)/* overload */;
	float __fastcall SqrDistanceTo(const Vectortypes::TVector3f &pt)/* overload */;
	float __fastcall DistanceTo(TGLBaseSceneObject* anObject)/* overload */;
	float __fastcall DistanceTo(const Vectortypes::TVector3f &pt)/* overload */;
	float __fastcall DistanceTo(const Vectortypes::TVector4f &pt)/* overload */;
	virtual Vectortypes::TVector4f __fastcall BarycenterAbsolutePosition(void);
	float __fastcall BarycenterSqrDistanceTo(const Vectortypes::TVector4f &pt);
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensions(void);
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled(void);
	Geometrybb::TAABB __fastcall AxisAlignedBoundingBox(const bool AIncludeChilden = true);
	Geometrybb::TAABB __fastcall AxisAlignedBoundingBoxUnscaled(const bool AIncludeChilden = true);
	Geometrybb::TAABB __fastcall AxisAlignedBoundingBoxAbsolute(const bool AIncludeChilden = true, const bool AUseBaryCenter = false);
	Geometrybb::TAABB __fastcall AxisAlignedBoundingBoxEx(void);
	Geometrybb::TAABB __fastcall AxisAlignedBoundingBoxAbsoluteEx(void);
	Geometrybb::THmgBoundingBox __fastcall BoundingBox(const bool AIncludeChilden = true, const bool AUseBaryCenter = false);
	Geometrybb::THmgBoundingBox __fastcall BoundingBoxUnscaled(const bool AIncludeChilden = true, const bool AUseBaryCenter = false);
	Geometrybb::THmgBoundingBox __fastcall BoundingBoxAbsolute(const bool AIncludeChilden = true, const bool AUseBaryCenter = false);
	Geometrybb::THmgBoundingBox __fastcall BoundingBoxPersonalUnscaledEx(void);
	Geometrybb::THmgBoundingBox __fastcall BoundingBoxOfChildrenEx(void);
	Geometrybb::THmgBoundingBox __fastcall BoundingBoxIncludingChildrenEx(void);
	float __fastcall BoundingSphereRadius(void);
	float __fastcall BoundingSphereRadiusUnscaled(void);
	virtual bool __fastcall PointInObject(const Vectortypes::TVector4f &point);
	virtual bool __fastcall RayCastIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, Vectorgeometry::PVector intersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector intersectNormal = (Vectorgeometry::PVector)(0x0));
	virtual Glsilhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Glsilhouette::TGLSilhouetteParameters &silhouetteParameters);
	__property TGLBaseSceneObject* Children[int Index] = {read=Get/*, default*/};
	__property int Count = {read=GetCount, nodefault};
	__property int Index = {read=GetIndex, write=SetIndex, nodefault};
	DYNAMIC TGLBaseSceneObject* __fastcall AddNewChild(TGLSceneObjectClass AChild);
	DYNAMIC TGLBaseSceneObject* __fastcall AddNewChildFirst(TGLSceneObjectClass AChild);
	DYNAMIC void __fastcall AddChild(TGLBaseSceneObject* AChild);
	TGLBehaviour* __fastcall GetOrCreateBehaviour(TGLBehaviourClass aBehaviour);
	TGLBehaviour* __fastcall AddNewBehaviour(TGLBehaviourClass aBehaviour);
	TGLObjectEffect* __fastcall GetOrCreateEffect(TGLObjectEffectClass anEffect);
	TGLObjectEffect* __fastcall AddNewEffect(TGLObjectEffectClass anEffect);
	bool __fastcall HasSubChildren(void);
	DYNAMIC void __fastcall DeleteChildren(void);
	HIDESBASEDYNAMIC void __fastcall Insert(int AIndex, TGLBaseSceneObject* AChild);
	HIDESBASEDYNAMIC void __fastcall Remove(TGLBaseSceneObject* aChild, bool keepChildren);
	int __fastcall IndexOfChild(TGLBaseSceneObject* aChild);
	TGLBaseSceneObject* __fastcall FindChild(const System::UnicodeString aName, bool ownChildrenOnly);
	void __fastcall ExchangeChildrenSafe(int anIndex1, int anIndex2);
	void __fastcall ExchangeChildren(int anIndex1, int anIndex2);
	void __fastcall MoveChildUp(int anIndex);
	void __fastcall MoveChildDown(int anIndex);
	void __fastcall MoveChildFirst(int anIndex);
	void __fastcall MoveChildLast(int anIndex);
	virtual void __fastcall DoProgress(const Baseclasses::TProgressTimes &progressTime);
	DYNAMIC void __fastcall MoveTo(TGLBaseSceneObject* newParent);
	void __fastcall MoveUp(void);
	void __fastcall MoveDown(void);
	void __fastcall MoveFirst(void);
	void __fastcall MoveLast(void);
	virtual void __fastcall BeginUpdate(void);
	virtual void __fastcall EndUpdate(void);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	DYNAMIC System::Classes::TComponent* __fastcall GetParentComponent(void);
	DYNAMIC bool __fastcall HasParent(void);
	bool __fastcall IsUpdating(void);
	void __fastcall Lift(float ADistance);
	void __fastcall Move(float ADistance);
	void __fastcall Translate(float tx, float ty, float tz);
	void __fastcall MoveObjectAround(TGLBaseSceneObject* anObject, float pitchDelta, float turnDelta);
	void __fastcall MoveObjectAllAround(TGLBaseSceneObject* anObject, float pitchDelta, float turnDelta);
	void __fastcall Pitch(float angle);
	void __fastcall Roll(float angle);
	void __fastcall Turn(float angle);
	void __fastcall ResetRotations(void);
	void __fastcall ResetAndPitchTurnRoll(const float degX, const float degY, const float degZ);
	void __fastcall RotateAbsolute(const float rx, const float ry, const float rz)/* overload */;
	void __fastcall RotateAbsolute(const Vectortypes::TVector3f &axis, float angle)/* overload */;
	void __fastcall Slide(float ADistance);
	void __fastcall PointTo(TGLBaseSceneObject* const ATargetObject, const Vectortypes::TVector4f &AUpVector)/* overload */;
	void __fastcall PointTo(const Vectortypes::TVector4f &AAbsolutePosition, const Vectortypes::TVector4f &AUpVector)/* overload */;
	void __fastcall Render(Glrendercontextinfo::TRenderContextInfo &ARci);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall RenderChildren(int firstChildIndex, int lastChildIndex, Glrendercontextinfo::TRenderContextInfo &rci);
	DYNAMIC void __fastcall StructureChanged(void);
	void __fastcall ClearStructureChanged(void);
	virtual void __fastcall CoordinateChanged(Glcoordinates::TGLCustomCoordinates* Sender);
	void __fastcall TransformationChanged(void);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	__property Glcoordinates::TGLCoordinates3* Rotation = {read=FRotation, write=SetRotation};
	__property float PitchAngle = {read=GetPitchAngle, write=SetPitchAngle};
	__property float RollAngle = {read=GetRollAngle, write=SetRollAngle};
	__property float TurnAngle = {read=GetTurnAngle, write=SetTurnAngle};
	__property bool ShowAxes = {read=FShowAxes, write=SetShowAxes, default=0};
	__property TObjectChanges Changes = {read=FChanges, nodefault};
	__property TObjectBBChanges BBChanges = {read=FBBChanges, write=SetBBChanges, nodefault};
	__property TGLBaseSceneObject* Parent = {read=FParent, write=SetParent};
	__property Glcoordinates::TGLCoordinates3* Position = {read=FPosition, write=SetPosition};
	__property Glcoordinates::TGLCoordinates3* Direction = {read=FDirection, write=SetDirection};
	__property Glcoordinates::TGLCoordinates3* Up = {read=FUp, write=SetUp};
	__property Glcoordinates::TGLCoordinates3* Scale = {read=FScaling, write=SetScaling};
	__property TGLScene* Scene = {read=FScene};
	__property bool Visible = {read=FVisible, write=SetVisible, default=1};
	__property bool Pickable = {read=FPickable, write=SetPickable, default=1};
	__property Glrendercontextinfo::TGLObjectsSorting ObjectsSorting = {read=FObjectsSorting, write=SetObjectsSorting, default=0};
	__property Glrendercontextinfo::TGLVisibilityCulling VisibilityCulling = {read=FVisibilityCulling, write=SetVisibilityCulling, default=0};
	__property Baseclasses::TGLProgressEvent OnProgress = {read=FOnProgress, write=FOnProgress};
	__property System::Classes::TNotifyEvent OnPicked = {read=FOnPicked, write=FOnPicked};
	__property System::Classes::TNotifyEvent OnAddedToParent = {read=FOnAddedToParent, write=FOnAddedToParent};
	__property TGLBehaviours* Behaviours = {read=GetBehaviours, write=SetBehaviours, stored=false};
	__property TGLObjectEffects* Effects = {read=GetEffects, write=SetEffects, stored=false};
	__property System::TObject* TagObject = {read=FTagObject, write=FTagObject};
	
__published:
	__property float TagFloat = {read=FTagFloat, write=FTagFloat};
};


class DELPHICLASS TGLBaseBehaviour;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBaseBehaviour : public Xcollection::TXCollectionItem
{
	typedef Xcollection::TXCollectionItem inherited;
	
protected:
	virtual void __fastcall SetName(const System::UnicodeString val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	TGLBaseSceneObject* __fastcall OwnerBaseSceneObject(void);
	
public:
	__fastcall virtual TGLBaseBehaviour(Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLBaseBehaviour(void);
	virtual void __fastcall DoProgress(const Baseclasses::TProgressTimes &progressTime);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBehaviour : public TGLBaseBehaviour
{
	typedef TGLBaseBehaviour inherited;
	
public:
	/* TGLBaseBehaviour.Create */ inline __fastcall virtual TGLBehaviour(Xcollection::TXCollection* aOwner) : TGLBaseBehaviour(aOwner) { }
	/* TGLBaseBehaviour.Destroy */ inline __fastcall virtual ~TGLBehaviour(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLBehaviours : public Xcollection::TXCollection
{
	typedef Xcollection::TXCollection inherited;
	
public:
	TGLBehaviour* operator[](int index) { return Behaviour[index]; }
	
protected:
	TGLBehaviour* __fastcall GetBehaviour(int index);
	
public:
	__fastcall virtual TGLBehaviours(System::Classes::TPersistent* aOwner);
	DYNAMIC System::UnicodeString __fastcall GetNamePath(void);
	__classmethod virtual Xcollection::TXCollectionItemClass __fastcall ItemsClass();
	__property TGLBehaviour* Behaviour[int index] = {read=GetBehaviour/*, default*/};
	virtual bool __fastcall CanAdd(Xcollection::TXCollectionItemClass aClass);
	void __fastcall DoProgress(const Baseclasses::TProgressTimes &progressTimes);
public:
	/* TXCollection.Destroy */ inline __fastcall virtual ~TGLBehaviours(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLObjectEffect : public TGLBaseBehaviour
{
	typedef TGLBaseBehaviour inherited;
	
protected:
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	
public:
	virtual void __fastcall Render(Glrendercontextinfo::TRenderContextInfo &rci);
public:
	/* TGLBaseBehaviour.Create */ inline __fastcall virtual TGLObjectEffect(Xcollection::TXCollection* aOwner) : TGLBaseBehaviour(aOwner) { }
	/* TGLBaseBehaviour.Destroy */ inline __fastcall virtual ~TGLObjectEffect(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLObjectPreEffect;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLObjectPreEffect : public TGLObjectEffect
{
	typedef TGLObjectEffect inherited;
	
public:
	/* TGLBaseBehaviour.Create */ inline __fastcall virtual TGLObjectPreEffect(Xcollection::TXCollection* aOwner) : TGLObjectEffect(aOwner) { }
	/* TGLBaseBehaviour.Destroy */ inline __fastcall virtual ~TGLObjectPreEffect(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLObjectPostEffect;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLObjectPostEffect : public TGLObjectEffect
{
	typedef TGLObjectEffect inherited;
	
public:
	/* TGLBaseBehaviour.Create */ inline __fastcall virtual TGLObjectPostEffect(Xcollection::TXCollection* aOwner) : TGLObjectEffect(aOwner) { }
	/* TGLBaseBehaviour.Destroy */ inline __fastcall virtual ~TGLObjectPostEffect(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLObjectAfterEffect;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLObjectAfterEffect : public TGLObjectEffect
{
	typedef TGLObjectEffect inherited;
	
public:
	/* TGLBaseBehaviour.Create */ inline __fastcall virtual TGLObjectAfterEffect(Xcollection::TXCollection* aOwner) : TGLObjectEffect(aOwner) { }
	/* TGLBaseBehaviour.Destroy */ inline __fastcall virtual ~TGLObjectAfterEffect(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLObjectEffects : public Xcollection::TXCollection
{
	typedef Xcollection::TXCollection inherited;
	
public:
	TGLObjectEffect* operator[](int index) { return ObjectEffect[index]; }
	
protected:
	TGLObjectEffect* __fastcall GetEffect(int index);
	
public:
	__fastcall virtual TGLObjectEffects(System::Classes::TPersistent* aOwner);
	DYNAMIC System::UnicodeString __fastcall GetNamePath(void);
	__classmethod virtual Xcollection::TXCollectionItemClass __fastcall ItemsClass();
	__property TGLObjectEffect* ObjectEffect[int index] = {read=GetEffect/*, default*/};
	virtual bool __fastcall CanAdd(Xcollection::TXCollectionItemClass aClass);
	void __fastcall DoProgress(const Baseclasses::TProgressTimes &progressTime);
	void __fastcall RenderPreEffects(Glrendercontextinfo::TRenderContextInfo &rci);
	void __fastcall RenderPostEffects(Glrendercontextinfo::TRenderContextInfo &rci);
public:
	/* TXCollection.Destroy */ inline __fastcall virtual ~TGLObjectEffects(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLCustomSceneObject;
class PASCALIMPLEMENTATION TGLCustomSceneObject : public TGLBaseSceneObject
{
	typedef TGLBaseSceneObject inherited;
	
private:
	Glmaterial::TGLMaterial* FMaterial;
	System::UnicodeString FHint;
	
protected:
	virtual bool __fastcall Blended(void);
	void __fastcall SetGLMaterial(Glmaterial::TGLMaterial* AValue);
	DYNAMIC void __fastcall DestroyHandle(void);
	virtual void __fastcall Loaded(void);
	
public:
	__fastcall virtual TGLCustomSceneObject(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomSceneObject(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	__property Glmaterial::TGLMaterial* Material = {read=FMaterial, write=SetGLMaterial};
	__property System::UnicodeString Hint = {read=FHint, write=FHint};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLCustomSceneObject(TGLBaseSceneObject* aParentOwner) : TGLBaseSceneObject(aParentOwner) { }
	
};


class DELPHICLASS TGLSceneRootObject;
class PASCALIMPLEMENTATION TGLSceneRootObject : public TGLBaseSceneObject
{
	typedef TGLBaseSceneObject inherited;
	
public:
	__fastcall virtual TGLSceneRootObject(System::Classes::TComponent* AOwner);
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSceneRootObject(TGLBaseSceneObject* aParentOwner) : TGLBaseSceneObject(aParentOwner) { }
	/* TGLBaseSceneObject.Destroy */ inline __fastcall virtual ~TGLSceneRootObject(void) { }
	
};


class DELPHICLASS TGLImmaterialSceneObject;
class PASCALIMPLEMENTATION TGLImmaterialSceneObject : public TGLCustomSceneObject
{
	typedef TGLCustomSceneObject inherited;
	
public:
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	
__published:
	__property ObjectsSorting = {default=0};
	__property VisibilityCulling = {default=0};
	__property Direction;
	__property PitchAngle = {default=0};
	__property Position;
	__property RollAngle = {default=0};
	__property Scale;
	__property ShowAxes = {default=0};
	__property TurnAngle = {default=0};
	__property Up;
	__property Visible = {default=1};
	__property Pickable = {default=1};
	__property OnProgress;
	__property OnPicked;
	__property Behaviours;
	__property Effects;
	__property Hint = {default=0};
public:
	/* TGLCustomSceneObject.Create */ inline __fastcall virtual TGLImmaterialSceneObject(System::Classes::TComponent* AOwner) : TGLCustomSceneObject(AOwner) { }
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLImmaterialSceneObject(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLImmaterialSceneObject(TGLBaseSceneObject* aParentOwner) : TGLCustomSceneObject(aParentOwner) { }
	
};


class DELPHICLASS TGLCameraInvariantObject;
class PASCALIMPLEMENTATION TGLCameraInvariantObject : public TGLImmaterialSceneObject
{
	typedef TGLImmaterialSceneObject inherited;
	
private:
	TGLCameraInvarianceMode FCamInvarianceMode;
	
protected:
	void __fastcall SetCamInvarianceMode(const TGLCameraInvarianceMode val);
	__property TGLCameraInvarianceMode CamInvarianceMode = {read=FCamInvarianceMode, write=SetCamInvarianceMode, nodefault};
	
public:
	__fastcall virtual TGLCameraInvariantObject(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLCameraInvariantObject(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLCameraInvariantObject(TGLBaseSceneObject* aParentOwner) : TGLImmaterialSceneObject(aParentOwner) { }
	
};


class DELPHICLASS TGLSceneObject;
class PASCALIMPLEMENTATION TGLSceneObject : public TGLCustomSceneObject
{
	typedef TGLCustomSceneObject inherited;
	
__published:
	__property Material;
	__property ObjectsSorting = {default=0};
	__property VisibilityCulling = {default=0};
	__property Direction;
	__property PitchAngle = {default=0};
	__property Position;
	__property RollAngle = {default=0};
	__property Scale;
	__property ShowAxes = {default=0};
	__property TurnAngle = {default=0};
	__property Up;
	__property Visible = {default=1};
	__property Pickable = {default=1};
	__property OnProgress;
	__property OnPicked;
	__property Behaviours;
	__property Effects;
	__property Hint = {default=0};
public:
	/* TGLCustomSceneObject.Create */ inline __fastcall virtual TGLSceneObject(System::Classes::TComponent* AOwner) : TGLCustomSceneObject(AOwner) { }
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLSceneObject(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSceneObject(TGLBaseSceneObject* aParentOwner) : TGLCustomSceneObject(aParentOwner) { }
	
};


typedef void __fastcall (__closure *TDirectRenderEvent)(System::TObject* Sender, Glrendercontextinfo::TRenderContextInfo &rci);

class DELPHICLASS TGLDirectOpenGL;
class PASCALIMPLEMENTATION TGLDirectOpenGL : public TGLImmaterialSceneObject
{
	typedef TGLImmaterialSceneObject inherited;
	
private:
	bool FUseBuildList;
	TDirectRenderEvent FOnRender;
	bool FBlend;
	
protected:
	void __fastcall SetUseBuildList(const bool val);
	virtual bool __fastcall Blended(void);
	void __fastcall SetBlend(const bool val);
	
public:
	__fastcall virtual TGLDirectOpenGL(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled(void);
	
__published:
	__property bool UseBuildList = {read=FUseBuildList, write=SetUseBuildList, nodefault};
	__property TDirectRenderEvent OnRender = {read=FOnRender, write=FOnRender};
	__property bool Blend = {read=FBlend, write=SetBlend, nodefault};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLDirectOpenGL(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLDirectOpenGL(TGLBaseSceneObject* aParentOwner) : TGLImmaterialSceneObject(aParentOwner) { }
	
};


class DELPHICLASS TGLRenderPoint;
class PASCALIMPLEMENTATION TGLRenderPoint : public TGLImmaterialSceneObject
{
	typedef TGLImmaterialSceneObject inherited;
	
private:
	typedef System::DynamicArray<TDirectRenderEvent> _TGLRenderPoint__1;
	
	typedef System::DynamicArray<System::Classes::TNotifyEvent> _TGLRenderPoint__2;
	
	
private:
	_TGLRenderPoint__1 FCallBacks;
	_TGLRenderPoint__2 FFreeCallBacks;
	
public:
	__fastcall virtual TGLRenderPoint(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLRenderPoint(void);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	void __fastcall RegisterCallBack(TDirectRenderEvent renderEvent, System::Classes::TNotifyEvent renderPointFreed);
	void __fastcall UnRegisterCallBack(TDirectRenderEvent renderEvent);
	void __fastcall Clear(void);
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLRenderPoint(TGLBaseSceneObject* aParentOwner) : TGLImmaterialSceneObject(aParentOwner) { }
	
};


class DELPHICLASS TGLProxyObject;
class PASCALIMPLEMENTATION TGLProxyObject : public TGLBaseSceneObject
{
	typedef TGLBaseSceneObject inherited;
	
private:
	TGLBaseSceneObject* FMasterObject;
	TGLProxyObjectOptions FProxyOptions;
	
protected:
	bool FRendering;
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall SetMasterObject(TGLBaseSceneObject* const val);
	void __fastcall SetProxyOptions(const TGLProxyObjectOptions val);
	
public:
	__fastcall virtual TGLProxyObject(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLProxyObject(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual Vectortypes::TVector4f __fastcall BarycenterAbsolutePosition(void);
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensions(void);
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled(void);
	virtual bool __fastcall RayCastIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, Vectorgeometry::PVector intersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector intersectNormal = (Vectorgeometry::PVector)(0x0));
	virtual Glsilhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Glsilhouette::TGLSilhouetteParameters &silhouetteParameters);
	
__published:
	__property TGLBaseSceneObject* MasterObject = {read=FMasterObject, write=SetMasterObject};
	__property TGLProxyObjectOptions ProxyOptions = {read=FProxyOptions, write=SetProxyOptions, default=7};
	__property ObjectsSorting = {default=0};
	__property Direction;
	__property PitchAngle = {default=0};
	__property Position;
	__property RollAngle = {default=0};
	__property Scale;
	__property ShowAxes = {default=0};
	__property TurnAngle = {default=0};
	__property Up;
	__property Visible = {default=1};
	__property Pickable = {default=1};
	__property OnProgress;
	__property OnPicked;
	__property Behaviours;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLProxyObject(TGLBaseSceneObject* aParentOwner) : TGLBaseSceneObject(aParentOwner) { }
	
};


typedef System::TMetaClass* TGLProxyObjectClass;

enum TLightStyle : unsigned char { lsSpot, lsOmni, lsParallel, lsParallelSpot };

class DELPHICLASS TGLLightSource;
class PASCALIMPLEMENTATION TGLLightSource : public TGLBaseSceneObject
{
	typedef TGLBaseSceneObject inherited;
	
private:
	unsigned FLightID;
	Glcoordinates::TGLCoordinates3* FSpotDirection;
	float FSpotExponent;
	float FSpotCutOff;
	float FConstAttenuation;
	float FLinearAttenuation;
	float FQuadraticAttenuation;
	bool FShining;
	Glcolor::TGLColor* FAmbient;
	Glcolor::TGLColor* FDiffuse;
	Glcolor::TGLColor* FSpecular;
	TLightStyle FLightStyle;
	
protected:
	void __fastcall SetAmbient(Glcolor::TGLColor* AValue);
	void __fastcall SetDiffuse(Glcolor::TGLColor* AValue);
	void __fastcall SetSpecular(Glcolor::TGLColor* AValue);
	void __fastcall SetConstAttenuation(float AValue);
	void __fastcall SetLinearAttenuation(float AValue);
	void __fastcall SetQuadraticAttenuation(float AValue);
	void __fastcall SetShining(bool AValue);
	void __fastcall SetSpotDirection(Glcoordinates::TGLCoordinates3* AVector);
	void __fastcall SetSpotExponent(float AValue);
	void __fastcall SetSpotCutOff(const float val);
	void __fastcall SetLightStyle(const TLightStyle val);
	
public:
	__fastcall virtual TGLLightSource(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLLightSource(void);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual unsigned __fastcall GetHandle(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual bool __fastcall RayCastIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, Vectorgeometry::PVector intersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector intersectNormal = (Vectorgeometry::PVector)(0x0));
	virtual void __fastcall CoordinateChanged(Glcoordinates::TGLCustomCoordinates* Sender);
	virtual Glsilhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Glsilhouette::TGLSilhouetteParameters &silhouetteParameters);
	__property unsigned LightID = {read=FLightID, nodefault};
	bool __fastcall Attenuated(void);
	
__published:
	__property Glcolor::TGLColor* Ambient = {read=FAmbient, write=SetAmbient};
	__property float ConstAttenuation = {read=FConstAttenuation, write=SetConstAttenuation};
	__property Glcolor::TGLColor* Diffuse = {read=FDiffuse, write=SetDiffuse};
	__property float LinearAttenuation = {read=FLinearAttenuation, write=SetLinearAttenuation};
	__property float QuadraticAttenuation = {read=FQuadraticAttenuation, write=SetQuadraticAttenuation};
	__property Position;
	__property TLightStyle LightStyle = {read=FLightStyle, write=SetLightStyle, default=0};
	__property bool Shining = {read=FShining, write=SetShining, default=1};
	__property Glcolor::TGLColor* Specular = {read=FSpecular, write=SetSpecular};
	__property float SpotCutOff = {read=FSpotCutOff, write=SetSpotCutOff};
	__property Glcoordinates::TGLCoordinates3* SpotDirection = {read=FSpotDirection, write=SetSpotDirection};
	__property float SpotExponent = {read=FSpotExponent, write=SetSpotExponent};
	__property OnProgress;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLLightSource(TGLBaseSceneObject* aParentOwner) : TGLBaseSceneObject(aParentOwner) { }
	
};


enum TGLCameraStyle : unsigned char { csPerspective, csOrthogonal, csOrtho2D, csCustom, csInfinitePerspective, csPerspectiveKeepFOV };

enum TGLCameraKeepFOVMode : unsigned char { ckmHorizontalFOV, ckmVerticalFOV };

typedef void __fastcall (__closure *TOnCustomPerspective)(const Vectorgeometry::TRectangle &viewport, int width, int height, int DPI, float &viewPortRadius);

class DELPHICLASS TGLCamera;
class DELPHICLASS TGLSceneBuffer;
class PASCALIMPLEMENTATION TGLCamera : public TGLBaseSceneObject
{
	typedef TGLBaseSceneObject inherited;
	
private:
	float FFocalLength;
	float FDepthOfView;
	float FNearPlane;
	float FNearPlaneBias;
	float FViewPortRadius;
	TGLBaseSceneObject* FTargetObject;
	Vectortypes::TVector4f FLastDirection;
	TGLCameraStyle FCameraStyle;
	TGLCameraKeepFOVMode FKeepFOVMode;
	float FSceneScale;
	System::Classes::TNotifyEvent FDeferredApply;
	TOnCustomPerspective FOnCustomPerspective;
	bool FDesign;
	double FFOVY;
	double FFOVX;
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetTargetObject(TGLBaseSceneObject* const val);
	void __fastcall SetDepthOfView(float AValue);
	void __fastcall SetFocalLength(float AValue);
	void __fastcall SetCameraStyle(const TGLCameraStyle val);
	void __fastcall SetKeepFOVMode(const TGLCameraKeepFOVMode val);
	void __fastcall SetSceneScale(float value);
	bool __fastcall StoreSceneScale(void);
	void __fastcall SetNearPlaneBias(float value);
	bool __fastcall StoreNearPlaneBias(void);
	
public:
	__fastcall virtual TGLCamera(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLCamera(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property float NearPlane = {read=FNearPlane};
	void __fastcall Apply(void);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual bool __fastcall RayCastIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, Vectorgeometry::PVector intersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector intersectNormal = (Vectorgeometry::PVector)(0x0));
	void __fastcall ApplyPerspective(const Vectorgeometry::TRectangle &AViewport, int AWidth, int AHeight, int ADPI);
	void __fastcall AutoLeveling(float Factor);
	void __fastcall Reset(TGLSceneBuffer* aSceneBuffer);
	void __fastcall ZoomAll(TGLSceneBuffer* aSceneBuffer);
	void __fastcall RotateObject(TGLBaseSceneObject* obj, float pitchDelta, float turnDelta, float rollDelta = 0.000000E+00);
	void __fastcall RotateTarget(float pitchDelta, float turnDelta, float rollDelta = 0.000000E+00);
	void __fastcall MoveAroundTarget(float pitchDelta, float turnDelta);
	void __fastcall MoveAllAroundTarget(float pitchDelta, float turnDelta);
	void __fastcall MoveInEyeSpace(float forwardDistance, float rightDistance, float upDistance);
	void __fastcall MoveTargetInEyeSpace(float forwardDistance, float rightDistance, float upDistance);
	Vectortypes::TVector4f __fastcall AbsoluteEyeSpaceVector(float forwardDistance, float rightDistance, float upDistance);
	void __fastcall AdjustDistanceToTarget(float distanceRatio);
	float __fastcall DistanceToTarget(void);
	Vectortypes::TVector4f __fastcall AbsoluteVectorToTarget(void);
	Vectortypes::TVector4f __fastcall AbsoluteRightVectorToTarget(void);
	Vectortypes::TVector4f __fastcall AbsoluteUpVectorToTarget(void);
	Vectortypes::TVector4f __fastcall ScreenDeltaToVector(int deltaX, int deltaY, float ratio, const Vectortypes::TVector4f &planeNormal);
	Vectortypes::TVector4f __fastcall ScreenDeltaToVectorXY(int deltaX, int deltaY, float ratio);
	Vectortypes::TVector4f __fastcall ScreenDeltaToVectorXZ(int deltaX, int deltaY, float ratio);
	Vectortypes::TVector4f __fastcall ScreenDeltaToVectorYZ(int deltaX, int deltaY, float ratio);
	bool __fastcall PointInFront(const Vectortypes::TVector4f &point)/* overload */;
	float __fastcall GetFieldOfView(const float AViewportDimension);
	void __fastcall SetFieldOfView(const float AFieldOfView, const float AViewportDimension);
	
__published:
	__property float DepthOfView = {read=FDepthOfView, write=SetDepthOfView};
	__property float FocalLength = {read=FFocalLength, write=SetFocalLength};
	__property float SceneScale = {read=FSceneScale, write=SetSceneScale, stored=StoreSceneScale};
	__property float NearPlaneBias = {read=FNearPlaneBias, write=SetNearPlaneBias, stored=StoreNearPlaneBias};
	__property TGLBaseSceneObject* TargetObject = {read=FTargetObject, write=SetTargetObject};
	__property TGLCameraStyle CameraStyle = {read=FCameraStyle, write=SetCameraStyle, default=0};
	__property TGLCameraKeepFOVMode KeepFOVMode = {read=FKeepFOVMode, write=SetKeepFOVMode, default=0};
	__property TOnCustomPerspective OnCustomPerspective = {read=FOnCustomPerspective, write=FOnCustomPerspective};
	__property Position;
	__property Direction;
	__property Up;
	__property OnProgress;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLCamera(TGLBaseSceneObject* aParentOwner) : TGLBaseSceneObject(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLScene : public Baseclasses::TGLUpdateAbleComponent
{
	typedef Baseclasses::TGLUpdateAbleComponent inherited;
	
private:
	int FUpdateCount;
	TGLSceneRootObject* FObjects;
	Glcontext::TGLContext* FBaseContext;
	Persistentclasses::TPersistentObjectList* FLights;
	Persistentclasses::TPersistentObjectList* FBuffers;
	TGLCamera* FCurrentGLCamera;
	TGLSceneBuffer* FCurrentBuffer;
	Glrendercontextinfo::TGLObjectsSorting FObjectsSorting;
	Glrendercontextinfo::TGLVisibilityCulling FVisibilityCulling;
	Baseclasses::TGLProgressEvent FOnBeforeProgress;
	Baseclasses::TGLProgressEvent FOnProgress;
	double FCurrentDeltaTime;
	TGLInitializableObjectList* FInitializableObjects;
	
protected:
	void __fastcall AddLight(TGLLightSource* aLight);
	void __fastcall RemoveLight(TGLLightSource* aLight);
	void __fastcall AddLights(TGLBaseSceneObject* anObj);
	void __fastcall RemoveLights(TGLBaseSceneObject* anObj);
	DYNAMIC void __fastcall GetChildren(System::Classes::TGetChildProc AProc, System::Classes::TComponent* Root);
	DYNAMIC void __fastcall SetChildOrder(System::Classes::TComponent* AChild, int Order);
	void __fastcall SetObjectsSorting(const Glrendercontextinfo::TGLObjectsSorting val);
	void __fastcall SetVisibilityCulling(const Glrendercontextinfo::TGLVisibilityCulling val);
	virtual void __fastcall ReadState(System::Classes::TReader* Reader);
	
public:
	__fastcall virtual TGLScene(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLScene(void);
	void __fastcall BeginUpdate(void);
	void __fastcall EndUpdate(void);
	bool __fastcall IsUpdating(void);
	void __fastcall AddBuffer(TGLSceneBuffer* aBuffer);
	void __fastcall RemoveBuffer(TGLSceneBuffer* aBuffer);
	void __fastcall SetupLights(int maxLights);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	void __fastcall Progress(const double deltaTime, const double newTime);
	TGLBaseSceneObject* __fastcall FindSceneObject(const System::UnicodeString AName);
	virtual TGLBaseSceneObject* __fastcall RayCastIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, Vectorgeometry::PVector intersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector intersectNormal = (Vectorgeometry::PVector)(0x0));
	void __fastcall ShutdownAllLights(void);
	void __fastcall SaveToFile(const System::UnicodeString fileName);
	void __fastcall LoadFromFile(const System::UnicodeString fileName);
	void __fastcall SaveToStream(System::Classes::TStream* aStream);
	void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	void __fastcall SaveToTextFile(const System::UnicodeString fileName);
	void __fastcall LoadFromTextFile(const System::UnicodeString fileName);
	__property TGLCamera* CurrentGLCamera = {read=FCurrentGLCamera};
	__property Persistentclasses::TPersistentObjectList* Lights = {read=FLights};
	__property TGLSceneRootObject* Objects = {read=FObjects};
	__property TGLSceneBuffer* CurrentBuffer = {read=FCurrentBuffer};
	__property TGLInitializableObjectList* InitializableObjects = {read=FInitializableObjects};
	__property double CurrentDeltaTime = {read=FCurrentDeltaTime};
	
__published:
	__property Glrendercontextinfo::TGLObjectsSorting ObjectsSorting = {read=FObjectsSorting, write=SetObjectsSorting, default=3};
	__property Glrendercontextinfo::TGLVisibilityCulling VisibilityCulling = {read=FVisibilityCulling, write=SetVisibilityCulling, default=1};
	__property Baseclasses::TGLProgressEvent OnBeforeProgress = {read=FOnBeforeProgress, write=FOnBeforeProgress};
	__property Baseclasses::TGLProgressEvent OnProgress = {read=FOnProgress, write=FOnProgress};
};


enum TFogMode : unsigned char { fmLinear, fmExp, fmExp2 };

enum TFogDistance : unsigned char { fdDefault, fdEyeRadial, fdEyePlane };

class DELPHICLASS TGLFogEnvironment;
class PASCALIMPLEMENTATION TGLFogEnvironment : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	TGLSceneBuffer* FSceneBuffer;
	Glcolor::TGLColor* FFogColor;
	float FFogStart;
	float FFogEnd;
	TFogMode FFogMode;
	TFogDistance FFogDistance;
	
protected:
	void __fastcall SetFogColor(Glcolor::TGLColor* Value);
	void __fastcall SetFogStart(float Value);
	void __fastcall SetFogEnd(float Value);
	void __fastcall SetFogMode(TFogMode Value);
	void __fastcall SetFogDistance(const TFogDistance val);
	
public:
	__fastcall virtual TGLFogEnvironment(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLFogEnvironment(void);
	void __fastcall ApplyFog(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	bool __fastcall IsAtDefaultValues(void);
	
__published:
	__property Glcolor::TGLColor* FogColor = {read=FFogColor, write=SetFogColor};
	__property float FogStart = {read=FFogStart, write=SetFogStart};
	__property float FogEnd = {read=FFogEnd, write=SetFogEnd};
	__property TFogMode FogMode = {read=FFogMode, write=SetFogMode, default=0};
	__property TFogDistance FogDistance = {read=FFogDistance, write=SetFogDistance, default=0};
};


enum TGLDepthPrecision : unsigned char { dpDefault, dp16bits, dp24bits, dp32bits };

enum TGLColorDepth : unsigned char { cdDefault, cd8bits, cd16bits, cd24bits, cdFloat64bits, cdFloat128bits };

enum TGLShadeModel : unsigned char { smDefault, smSmooth, smFlat };

class PASCALIMPLEMENTATION TGLSceneBuffer : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	typedef System::DynamicArray<Vectortypes::TMatrix4f> _TGLSceneBuffer__1;
	
	typedef System::DynamicArray<Vectortypes::TMatrix4f> _TGLSceneBuffer__2;
	
	
private:
	bool FRendering;
	Glcontext::TGLContext* FRenderingContext;
	Persistentclasses::TPersistentObjectList* FAfterRenderEffects;
	_TGLSceneBuffer__1 FViewMatrixStack;
	_TGLSceneBuffer__2 FProjectionMatrixStack;
	Vectortypes::TMatrix4f FBaseProjectionMatrix;
	Vectortypes::TVector4f FCameraAbsolutePosition;
	Vectorgeometry::TRectangle FViewPort;
	Glselection::TGLBaseSelectTechnique* FSelector;
	bool FFaceCulling;
	bool FFogEnable;
	bool FLighting;
	bool FDepthTest;
	System::Uitypes::TColor FBackgroundColor;
	float FBackgroundAlpha;
	Glcolor::TGLColor* FAmbientColor;
	Glcontext::TGLAntiAliasing FAntiAliasing;
	TGLDepthPrecision FDepthPrecision;
	TGLColorDepth FColorDepth;
	TContextOptions FContextOptions;
	TGLShadeModel FShadeModel;
	int FRenderDPI;
	TGLFogEnvironment* FFogEnvironment;
	int FAccumBufferBits;
	Glcontext::TGLContextLayer FLayer;
	TGLCamera* FCamera;
	void *FFreezeBuffer;
	bool FFreezed;
	Vectorgeometry::TRectangle FFreezedViewPort;
	int FFrameCount;
	float FFramesPerSecond;
	__int64 FFirstPerfCounter;
	float FLastFrameTime;
	System::Classes::TNotifyEvent FOnChange;
	System::Classes::TNotifyEvent FOnStructuralChange;
	System::Classes::TNotifyEvent FOnPrepareGLContext;
	System::Classes::TNotifyEvent FBeforeRender;
	System::Classes::TNotifyEvent FViewerBeforeRender;
	System::Classes::TNotifyEvent FPostRender;
	System::Classes::TNotifyEvent FAfterRender;
	TDirectRenderEvent FInitiateRendering;
	TDirectRenderEvent FWrapUpRendering;
	void __fastcall SetLayer(const Glcontext::TGLContextLayer Value);
	
protected:
	void __fastcall SetBackgroundColor(System::Uitypes::TColor AColor);
	void __fastcall SetBackgroundAlpha(float alpha);
	void __fastcall SetAmbientColor(Glcolor::TGLColor* AColor);
	int __fastcall GetLimit(TLimitType Which);
	void __fastcall SetCamera(TGLCamera* ACamera);
	void __fastcall SetContextOptions(TContextOptions Options);
	void __fastcall SetDepthTest(bool AValue);
	void __fastcall SetFaceCulling(bool AValue);
	void __fastcall SetLighting(bool AValue);
	void __fastcall SetAntiAliasing(const Glcontext::TGLAntiAliasing val);
	void __fastcall SetDepthPrecision(const TGLDepthPrecision val);
	void __fastcall SetColorDepth(const TGLColorDepth val);
	void __fastcall SetShadeModel(const TGLShadeModel val);
	void __fastcall SetFogEnable(bool AValue);
	void __fastcall SetGLFogEnvironment(TGLFogEnvironment* AValue);
	bool __fastcall StoreFog(void);
	void __fastcall SetAccumBufferBits(const int val);
	void __fastcall PrepareRenderingMatrices(const Vectorgeometry::TRectangle &aViewPort, int resolution, Glcrossplatform::PGLRect pickingRect = (Glcrossplatform::PGLRect)(0x0));
	void __fastcall DoBaseRender(const Vectorgeometry::TRectangle &aViewPort, int resolution, Glrendercontextinfo::TDrawState drawState, TGLBaseSceneObject* baseObject);
	void __fastcall SetupRenderingContext(Glcontext::TGLContext* context);
	void __fastcall SetupRCOptions(Glcontext::TGLContext* context);
	void __fastcall PrepareGLContext(void);
	void __fastcall DoChange(void);
	void __fastcall DoStructuralChange(void);
	__property int RenderDPI = {read=FRenderDPI, nodefault};
	__property System::Classes::TNotifyEvent OnPrepareGLContext = {read=FOnPrepareGLContext, write=FOnPrepareGLContext};
	
public:
	__fastcall virtual TGLSceneBuffer(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLSceneBuffer(void);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	void __fastcall CreateRC(HWND AWindowHandle, bool memoryContext, int BufferCount = 0x1)/* overload */;
	void __fastcall ClearBuffers(void);
	void __fastcall DestroyRC(void);
	bool __fastcall RCInstantiated(void);
	void __fastcall Resize(int newLeft, int newTop, int newWidth, int newHeight);
	Glcontext::TGLContextAcceleration __fastcall Acceleration(void);
	__property Vectorgeometry::TRectangle ViewPort = {read=FViewPort};
	void __fastcall PickObjects(const System::Types::TRect &rect, Glselection::TGLPickList* pickList, int objectCountGuess);
	Glselection::TGLPickList* __fastcall GetPickedObjects(const System::Types::TRect &rect, int objectCountGuess = 0x40);
	TGLBaseSceneObject* __fastcall GetPickedObject(int x, int y);
	System::Uitypes::TColor __fastcall GetPixelColor(int x, int y);
	float __fastcall GetPixelDepth(int x, int y);
	float __fastcall PixelDepthToDistance(float aDepth);
	float __fastcall PixelToDistance(int x, int y);
	void __fastcall NotifyMouseMove(System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall Render(TGLBaseSceneObject* baseObject)/* overload */;
	void __fastcall Render(void)/* overload */;
	void __fastcall RenderScene(TGLScene* aScene, const int viewPortSizeX, const int viewPortSizeY, Glrendercontextinfo::TDrawState drawState, TGLBaseSceneObject* baseObject);
	void __fastcall RenderToBitmap(Vcl::Graphics::TBitmap* ABitmap, int DPI = 0x0);
	void __fastcall RenderToFile(const System::UnicodeString AFile, int DPI = 0x0)/* overload */;
	void __fastcall RenderToFile(const System::UnicodeString AFile, int bmpWidth, int bmpHeight)/* overload */;
	Glgraphics::TGLImage* __fastcall CreateSnapShot(void);
	Vcl::Graphics::TBitmap* __fastcall CreateSnapShotBitmap(void);
	void __fastcall CopyToTexture(Gltexture::TGLTexture* aTexture)/* overload */;
	void __fastcall CopyToTexture(Gltexture::TGLTexture* aTexture, int xSrc, int ySrc, int AWidth, int AHeight, int xDest, int yDest, unsigned glCubeFace = (unsigned)(0x0))/* overload */;
	void __fastcall SaveAsFloatToFile(const System::UnicodeString aFilename);
	__property System::Classes::TNotifyEvent ViewerBeforeRender = {read=FViewerBeforeRender, write=FViewerBeforeRender, stored=false};
	void __fastcall SetViewPort(int X, int Y, int W, int H);
	int __fastcall Width(void);
	int __fastcall Height(void);
	__property bool Freezed = {read=FFreezed, nodefault};
	void __fastcall Freeze(void);
	void __fastcall Melt(void);
	void __fastcall ShowInfo(bool Modal = false);
	__property bool Rendering = {read=FRendering, nodefault};
	__property float BackgroundAlpha = {read=FBackgroundAlpha, write=SetBackgroundAlpha};
	Vectortypes::TMatrix4f __fastcall ProjectionMatrix _DEPRECATED_ATTRIBUTE0 (void);
	Vectortypes::TMatrix4f __fastcall ViewMatrix _DEPRECATED_ATTRIBUTE0 (void);
	Vectortypes::TMatrix4f __fastcall ModelMatrix _DEPRECATED_ATTRIBUTE0 (void);
	__property Vectortypes::TMatrix4f BaseProjectionMatrix = {read=FBaseProjectionMatrix};
	void __fastcall PushViewMatrix _DEPRECATED_ATTRIBUTE0 (const Vectortypes::TMatrix4f &newMatrix);
	void __fastcall PopViewMatrix _DEPRECATED_ATTRIBUTE0 (void);
	void __fastcall PushProjectionMatrix _DEPRECATED_ATTRIBUTE0 (const Vectortypes::TMatrix4f &newMatrix);
	void __fastcall PopProjectionMatrix _DEPRECATED_ATTRIBUTE0 (void);
	Vectortypes::TVector3f __fastcall OrthoScreenToWorld(int screenX, int screenY)/* overload */;
	Vectortypes::TVector3f __fastcall ScreenToWorld(const Vectortypes::TVector3f &aPoint)/* overload */;
	Vectortypes::TVector4f __fastcall ScreenToWorld(const Vectortypes::TVector4f &aPoint)/* overload */;
	Vectortypes::TVector3f __fastcall ScreenToWorld(int screenX, int screenY)/* overload */;
	Vectortypes::TVector3f __fastcall WorldToScreen(const Vectortypes::TVector3f &aPoint)/* overload */;
	Vectortypes::TVector4f __fastcall WorldToScreen(const Vectortypes::TVector4f &aPoint)/* overload */;
	void __fastcall WorldToScreen(Vectorgeometry::PVector points, int nbPoints)/* overload */;
	Vectortypes::TVector3f __fastcall ScreenToVector(const Vectortypes::TVector3f &aPoint)/* overload */;
	Vectortypes::TVector4f __fastcall ScreenToVector(const Vectortypes::TVector4f &aPoint)/* overload */;
	Vectortypes::TVector4f __fastcall ScreenToVector(const int x, const int y)/* overload */;
	Vectortypes::TVector3f __fastcall VectorToScreen(const Vectortypes::TVector3f &VectToCam);
	bool __fastcall ScreenVectorIntersectWithPlane(const Vectortypes::TVector4f &aScreenPoint, const Vectortypes::TVector4f &planePoint, const Vectortypes::TVector4f &planeNormal, Vectortypes::TVector4f &intersectPoint);
	bool __fastcall ScreenVectorIntersectWithPlaneXY(const Vectortypes::TVector4f &aScreenPoint, const float z, Vectortypes::TVector4f &intersectPoint);
	bool __fastcall ScreenVectorIntersectWithPlaneYZ(const Vectortypes::TVector4f &aScreenPoint, const float x, Vectortypes::TVector4f &intersectPoint);
	bool __fastcall ScreenVectorIntersectWithPlaneXZ(const Vectortypes::TVector4f &aScreenPoint, const float y, Vectortypes::TVector4f &intersectPoint);
	Vectortypes::TVector3f __fastcall PixelRayToWorld(int x, int y);
	__property float LastFrameTime = {read=FLastFrameTime};
	__property float FramesPerSecond = {read=FFramesPerSecond};
	void __fastcall ResetPerformanceMonitor(void);
	__property int LimitOf[TLimitType Which] = {read=GetLimit};
	__property Glcontext::TGLContext* RenderingContext = {read=FRenderingContext};
	__property TGLCamera* Camera = {read=FCamera, write=SetCamera};
	__property Glcontext::TGLContextLayer Layer = {read=FLayer, write=SetLayer, default=2};
	
__published:
	__property TGLFogEnvironment* FogEnvironment = {read=FFogEnvironment, write=SetGLFogEnvironment, stored=StoreFog};
	__property System::Uitypes::TColor BackgroundColor = {read=FBackgroundColor, write=SetBackgroundColor, default=-16777201};
	__property Glcolor::TGLColor* AmbientColor = {read=FAmbientColor, write=SetAmbientColor};
	__property TContextOptions ContextOptions = {read=FContextOptions, write=SetContextOptions, default=2058};
	__property int AccumBufferBits = {read=FAccumBufferBits, write=SetAccumBufferBits, default=0};
	__property bool DepthTest = {read=FDepthTest, write=SetDepthTest, default=1};
	__property bool FaceCulling = {read=FFaceCulling, write=SetFaceCulling, default=1};
	__property bool FogEnable = {read=FFogEnable, write=SetFogEnable, default=0};
	__property bool Lighting = {read=FLighting, write=SetLighting, default=1};
	__property Glcontext::TGLAntiAliasing AntiAliasing = {read=FAntiAliasing, write=SetAntiAliasing, default=0};
	__property TGLDepthPrecision DepthPrecision = {read=FDepthPrecision, write=SetDepthPrecision, default=0};
	__property TGLColorDepth ColorDepth = {read=FColorDepth, write=SetColorDepth, default=0};
	__property TGLShadeModel ShadeModel = {read=FShadeModel, write=SetShadeModel, default=0};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange, stored=false};
	__property System::Classes::TNotifyEvent OnStructuralChange = {read=FOnStructuralChange, write=FOnStructuralChange, stored=false};
	__property System::Classes::TNotifyEvent BeforeRender = {read=FBeforeRender, write=FBeforeRender, stored=false};
	__property TDirectRenderEvent InitiateRendering = {read=FInitiateRendering, write=FInitiateRendering, stored=false};
	__property TDirectRenderEvent WrapUpRendering = {read=FWrapUpRendering, write=FWrapUpRendering, stored=false};
	__property System::Classes::TNotifyEvent PostRender = {read=FPostRender, write=FPostRender, stored=false};
	__property System::Classes::TNotifyEvent AfterRender = {read=FAfterRender, write=FAfterRender, stored=false};
};


class DELPHICLASS TGLNonVisualViewer;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLNonVisualViewer : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TGLSceneBuffer* FBuffer;
	int FWidth;
	int FHeight;
	int FCubeMapRotIdx;
	float FCubeMapZNear;
	float FCubeMapZFar;
	Vectortypes::TVector3f FCubeMapTranslation;
	
protected:
	void __fastcall SetBeforeRender(const System::Classes::TNotifyEvent val);
	System::Classes::TNotifyEvent __fastcall GetBeforeRender(void);
	void __fastcall SetPostRender(const System::Classes::TNotifyEvent val);
	System::Classes::TNotifyEvent __fastcall GetPostRender(void);
	void __fastcall SetAfterRender(const System::Classes::TNotifyEvent val);
	System::Classes::TNotifyEvent __fastcall GetAfterRender(void);
	void __fastcall SetCamera(TGLCamera* const val);
	TGLCamera* __fastcall GetCamera(void);
	void __fastcall SetBuffer(TGLSceneBuffer* const val);
	void __fastcall SetWidth(const int val);
	void __fastcall SetHeight(const int val);
	void __fastcall SetupCubeMapCamera(System::TObject* Sender);
	void __fastcall DoOnPrepareGLContext(System::TObject* Sender);
	DYNAMIC void __fastcall PrepareGLContext(void);
	virtual void __fastcall DoBufferChange(System::TObject* Sender);
	virtual void __fastcall DoBufferStructuralChange(System::TObject* Sender);
	
public:
	__fastcall virtual TGLNonVisualViewer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLNonVisualViewer(void);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall Render(TGLBaseSceneObject* baseObject = (TGLBaseSceneObject*)(0x0)) = 0 ;
	virtual void __fastcall CopyToTexture(Gltexture::TGLTexture* aTexture)/* overload */;
	void __fastcall CopyToTexture(Gltexture::TGLTexture* aTexture, int xSrc, int ySrc, int width, int height, int xDest, int yDest)/* overload */;
	virtual void __fastcall CopyToTextureMRT(Gltexture::TGLTexture* aTexture, int BufferIndex)/* overload */;
	void __fastcall CopyToTextureMRT(Gltexture::TGLTexture* aTexture, int xSrc, int ySrc, int width, int height, int xDest, int yDest, int BufferIndex)/* overload */;
	void __fastcall RenderCubeMapTextures(Gltexture::TGLTexture* cubeMapTexture, float zNear = 0.000000E+00, float zFar = 0.000000E+00);
	
__published:
	__property TGLCamera* Camera = {read=GetCamera, write=SetCamera};
	__property int Width = {read=FWidth, write=SetWidth, default=256};
	__property int Height = {read=FHeight, write=SetHeight, default=256};
	__property System::Classes::TNotifyEvent BeforeRender = {read=GetBeforeRender, write=SetBeforeRender};
	__property System::Classes::TNotifyEvent PostRender = {read=GetPostRender, write=SetPostRender};
	__property System::Classes::TNotifyEvent AfterRender = {read=GetAfterRender, write=SetAfterRender};
	__property TGLSceneBuffer* Buffer = {read=FBuffer, write=SetBuffer};
};

#pragma pack(pop)

class DELPHICLASS TGLMemoryViewer;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMemoryViewer : public TGLNonVisualViewer
{
	typedef TGLNonVisualViewer inherited;
	
private:
	int FBufferCount;
	void __fastcall SetBufferCount(const int Value);
	
public:
	__fastcall virtual TGLMemoryViewer(System::Classes::TComponent* AOwner);
	void __fastcall InstantiateRenderingContext(void);
	virtual void __fastcall Render(TGLBaseSceneObject* baseObject = (TGLBaseSceneObject*)(0x0));
	
__published:
	__property int BufferCount = {read=FBufferCount, write=SetBufferCount, default=1};
public:
	/* TGLNonVisualViewer.Destroy */ inline __fastcall virtual ~TGLMemoryViewer(void) { }
	
};

#pragma pack(pop)

typedef void __fastcall (*TInvokeInfoForm)(TGLSceneBuffer* aSceneBuffer, bool Modal);

//-- var, const, procedure ---------------------------------------------------
#define cDefaultProxyOptions (System::Set<TGLProxyObjectOption, TGLProxyObjectOption::pooEffects, TGLProxyObjectOption::pooTransformation> () << TGLProxyObjectOption::pooEffects << TGLProxyObjectOption::pooObjects << TGLProxyObjectOption::pooTransformation )
#define GLSCENE_REVISION L"$Revision: 5600$"
#define GLSCENE_VERSION L"1.1.0.%s"
extern PACKAGE TGLBaseSceneObject* __fastcall GetCurrentRenderingObject(void);
extern PACKAGE void __fastcall AxesBuildList(Glrendercontextinfo::TRenderContextInfo &rci, System::Word pattern, float AxisLen);
extern PACKAGE void __fastcall RegisterInfoForm(TInvokeInfoForm infoForm);
extern PACKAGE void __fastcall InvokeInfoForm(TGLSceneBuffer* aSceneBuffer, bool Modal);
extern PACKAGE void __fastcall RegisterGLBaseSceneObjectNameChangeEvent(System::Classes::TNotifyEvent notifyEvent);
extern PACKAGE void __fastcall DeRegisterGLBaseSceneObjectNameChangeEvent(System::Classes::TNotifyEvent notifyEvent);
extern PACKAGE void __fastcall RegisterGLBehaviourNameChangeEvent(System::Classes::TNotifyEvent notifyEvent);
extern PACKAGE void __fastcall DeRegisterGLBehaviourNameChangeEvent(System::Classes::TNotifyEvent notifyEvent);
}	/* namespace Glscene */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCENE)
using namespace Glscene;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsceneHPP
