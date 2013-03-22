// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLObjects.pas' rev: 24.00 (Win32)

#ifndef GlobjectsHPP
#define GlobjectsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <OpenGLAdapter.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLSilhouette.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLNodes.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Globjects
{
//-- type declarations -------------------------------------------------------
typedef bool __fastcall (__closure *TGLVisibilityDeterminationEvent)(System::TObject* Sender, Glrendercontextinfo::TRenderContextInfo &rci);

struct TVertexRec;
typedef TVertexRec *PVertexRec;

struct DECLSPEC_DRECORD TVertexRec
{
public:
	Vectortypes::TVector3f Position;
	Vectortypes::TVector3f Normal;
	Vectortypes::TVector3f Binormal;
	Vectortypes::TVector3f Tangent;
	Vectortypes::TVector2f TexCoord;
};


class DELPHICLASS TGLDummyCube;
class PASCALIMPLEMENTATION TGLDummyCube : public Glscene::TGLCameraInvariantObject
{
	typedef Glscene::TGLCameraInvariantObject inherited;
	
private:
	float FCubeSize;
	Glcolor::TGLColor* FEdgeColor;
	bool FVisibleAtRunTime;
	bool FAmalgamate;
	Glcontext::TGLListHandle* FGroupList;
	TGLVisibilityDeterminationEvent FOnVisibilityDetermination;
	
protected:
	void __fastcall SetCubeSize(const float val);
	void __fastcall SetEdgeColor(Glcolor::TGLColor* const val);
	void __fastcall SetVisibleAtRunTime(const bool val);
	void __fastcall SetAmalgamate(const bool val);
	
public:
	__fastcall virtual TGLDummyCube(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLDummyCube(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled(void);
	virtual bool __fastcall RayCastIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, Vectorgeometry::PVector intersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector intersectNormal = (Vectorgeometry::PVector)(0x0));
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	DYNAMIC void __fastcall StructureChanged(void);
	virtual Vectortypes::TVector4f __fastcall BarycenterAbsolutePosition(void);
	
__published:
	__property float CubeSize = {read=FCubeSize, write=SetCubeSize};
	__property Glcolor::TGLColor* EdgeColor = {read=FEdgeColor, write=SetEdgeColor};
	__property bool VisibleAtRunTime = {read=FVisibleAtRunTime, write=SetVisibleAtRunTime, default=0};
	__property bool Amalgamate = {read=FAmalgamate, write=SetAmalgamate, default=0};
	__property CamInvarianceMode = {default=0};
	__property TGLVisibilityDeterminationEvent OnVisibilityDetermination = {read=FOnVisibilityDetermination, write=FOnVisibilityDetermination};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLDummyCube(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLCameraInvariantObject(aParentOwner) { }
	
};


enum TPlaneStyle : unsigned char { psSingleQuad, psTileTexture };

typedef System::Set<TPlaneStyle, TPlaneStyle::psSingleQuad, TPlaneStyle::psTileTexture>  TPlaneStyles;

class DELPHICLASS TGLPlane;
class PASCALIMPLEMENTATION TGLPlane : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	typedef System::DynamicArray<TVertexRec> _TGLPlane__1;
	
	typedef System::DynamicArray<System::DynamicArray<TVertexRec> > _TGLPlane__2;
	
	
private:
	float FXOffset;
	float FYOffset;
	float FXScope;
	float FYScope;
	float FWidth;
	float FHeight;
	unsigned FXTiles;
	unsigned FYTiles;
	TPlaneStyles FStyle;
	_TGLPlane__2 FMesh;
	
protected:
	void __fastcall SetHeight(const float aValue);
	void __fastcall SetWidth(const float aValue);
	void __fastcall SetXOffset(const float Value);
	void __fastcall SetXScope(const float Value);
	bool __fastcall StoreXScope(void);
	void __fastcall SetXTiles(const unsigned Value);
	void __fastcall SetYOffset(const float Value);
	void __fastcall SetYScope(const float Value);
	bool __fastcall StoreYScope(void);
	void __fastcall SetYTiles(const unsigned Value);
	void __fastcall SetStyle(const TPlaneStyles val);
	
public:
	__fastcall virtual TGLPlane(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual Glsilhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Glsilhouette::TGLSilhouetteParameters &silhouetteParameters);
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled(void);
	virtual bool __fastcall RayCastIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, Vectorgeometry::PVector intersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector intersectNormal = (Vectorgeometry::PVector)(0x0));
	System::Types::TRect __fastcall ScreenRect(Glscene::TGLSceneBuffer* aBuffer);
	float __fastcall PointDistance(const Vectortypes::TVector4f &aPoint);
	
__published:
	__property float Height = {read=FHeight, write=SetHeight};
	__property float Width = {read=FWidth, write=SetWidth};
	__property float XOffset = {read=FXOffset, write=SetXOffset};
	__property float XScope = {read=FXScope, write=SetXScope, stored=StoreXScope};
	__property unsigned XTiles = {read=FXTiles, write=SetXTiles, default=1};
	__property float YOffset = {read=FYOffset, write=SetYOffset};
	__property float YScope = {read=FYScope, write=SetYScope, stored=StoreYScope};
	__property unsigned YTiles = {read=FYTiles, write=SetYTiles, default=1};
	__property TPlaneStyles Style = {read=FStyle, write=SetStyle, default=3};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLPlane(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLPlane(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


class DELPHICLASS TGLSprite;
class PASCALIMPLEMENTATION TGLSprite : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	float FWidth;
	float FHeight;
	float FRotation;
	float FAlphaChannel;
	bool FMirrorU;
	bool FMirrorV;
	
protected:
	void __fastcall SetWidth(const float val);
	void __fastcall SetHeight(const float val);
	HIDESBASE void __fastcall SetRotation(const float val);
	void __fastcall SetAlphaChannel(const float val);
	bool __fastcall StoreAlphaChannel(void);
	void __fastcall SetMirrorU(const bool val);
	void __fastcall SetMirrorV(const bool val);
	
public:
	__fastcall virtual TGLSprite(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled(void);
	void __fastcall SetSize(const float Width, const float Height);
	void __fastcall SetSquareSize(const float size);
	
__published:
	__property float Width = {read=FWidth, write=SetWidth};
	__property float Height = {read=FHeight, write=SetHeight};
	__property float Rotation = {read=FRotation, write=SetRotation};
	__property float AlphaChannel = {read=FAlphaChannel, write=SetAlphaChannel, stored=StoreAlphaChannel};
	__property bool MirrorU = {read=FMirrorU, write=SetMirrorU, default=0};
	__property bool MirrorV = {read=FMirrorV, write=SetMirrorV, default=0};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLSprite(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSprite(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


enum TGLPointStyle : unsigned char { psSquare, psRound, psSmooth, psSmoothAdditive, psSquareAdditive };

class DELPHICLASS TGLPointParameters;
class PASCALIMPLEMENTATION TGLPointParameters : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	bool FEnabled;
	float FMinSize;
	float FMaxSize;
	float FFadeTresholdSize;
	Glcoordinates::TGLCoordinates3* FDistanceAttenuation;
	
protected:
	void __fastcall SetEnabled(const bool val);
	void __fastcall SetMinSize(const float val);
	void __fastcall SetMaxSize(const float val);
	void __fastcall SetFadeTresholdSize(const float val);
	void __fastcall SetDistanceAttenuation(Glcoordinates::TGLCoordinates3* const val);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadData(System::Classes::TStream* Stream);
	void __fastcall WriteData(System::Classes::TStream* Stream);
	
public:
	__fastcall virtual TGLPointParameters(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TGLPointParameters(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Apply(void);
	void __fastcall UnApply(void);
	
__published:
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=0};
	__property float MinSize = {read=FMinSize, write=SetMinSize, stored=false};
	__property float MaxSize = {read=FMaxSize, write=SetMaxSize, stored=false};
	__property float FadeTresholdSize = {read=FFadeTresholdSize, write=SetFadeTresholdSize, stored=false};
	__property Glcoordinates::TGLCoordinates3* DistanceAttenuation = {read=FDistanceAttenuation, write=SetDistanceAttenuation};
};


class DELPHICLASS TGLPoints;
class PASCALIMPLEMENTATION TGLPoints : public Glscene::TGLImmaterialSceneObject
{
	typedef Glscene::TGLImmaterialSceneObject inherited;
	
private:
	Vectorlists::TAffineVectorList* FPositions;
	Vectorlists::TVectorList* FColors;
	float FSize;
	TGLPointStyle FStyle;
	TGLPointParameters* FPointParameters;
	bool FStatic;
	bool FNoZWrite;
	
protected:
	bool __fastcall StoreSize(void);
	void __fastcall SetNoZWrite(const bool val);
	void __fastcall SetStatic(const bool val);
	void __fastcall SetSize(const float val);
	void __fastcall SetPositions(Vectorlists::TAffineVectorList* const val);
	void __fastcall SetColors(Vectorlists::TVectorList* const val);
	void __fastcall SetStyle(const TGLPointStyle val);
	void __fastcall SetPointParameters(TGLPointParameters* const val);
	
public:
	__fastcall virtual TGLPoints(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLPoints(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	__property Vectorlists::TAffineVectorList* Positions = {read=FPositions, write=SetPositions};
	__property Vectorlists::TVectorList* Colors = {read=FColors, write=SetColors};
	
__published:
	__property bool NoZWrite = {read=FNoZWrite, write=SetNoZWrite, nodefault};
	__property bool Static = {read=FStatic, write=SetStatic, nodefault};
	__property float size = {read=FSize, write=SetSize, stored=StoreSize};
	__property TGLPointStyle Style = {read=FStyle, write=SetStyle, default=0};
	__property TGLPointParameters* PointParameters = {read=FPointParameters, write=SetPointParameters};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLPoints(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


enum TLineNodesAspect : unsigned char { lnaInvisible, lnaAxes, lnaCube, lnaDodecahedron };

enum TLineSplineMode : unsigned char { lsmLines, lsmCubicSpline, lsmBezierSpline, lsmNURBSCurve, lsmSegments, lsmLoop };

class DELPHICLASS TGLLinesNode;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLLinesNode : public Glnodes::TGLNode
{
	typedef Glnodes::TGLNode inherited;
	
private:
	Glcolor::TGLColor* FColor;
	
protected:
	void __fastcall SetColor(Glcolor::TGLColor* const val);
	void __fastcall OnColorChange(System::TObject* Sender);
	bool __fastcall StoreColor(void);
	
public:
	__fastcall virtual TGLLinesNode(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLLinesNode(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Glcolor::TGLColor* Color = {read=FColor, write=SetColor, stored=StoreColor};
};

#pragma pack(pop)

class DELPHICLASS TGLLinesNodes;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLLinesNodes : public Glnodes::TGLNodes
{
	typedef Glnodes::TGLNodes inherited;
	
public:
	__fastcall TGLLinesNodes(System::Classes::TComponent* AOwner)/* overload */;
	virtual void __fastcall NotifyChange(void);
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLLinesNodes(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLLineBase;
class PASCALIMPLEMENTATION TGLLineBase : public Glscene::TGLImmaterialSceneObject
{
	typedef Glscene::TGLImmaterialSceneObject inherited;
	
private:
	Glcolor::TGLColor* FLineColor;
	System::Word FLinePattern;
	float FLineWidth;
	bool FAntiAliased;
	
protected:
	void __fastcall SetLineColor(Glcolor::TGLColor* const Value);
	void __fastcall SetLinePattern(const System::Word Value);
	void __fastcall SetLineWidth(const float val);
	bool __fastcall StoreLineWidth(void);
	void __fastcall SetAntiAliased(const bool val);
	void __fastcall SetupLineStyle(Glrendercontextinfo::TRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLLineBase(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLLineBase(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	
__published:
	__property bool AntiAliased = {read=FAntiAliased, write=SetAntiAliased, default=0};
	__property Glcolor::TGLColor* LineColor = {read=FLineColor, write=SetLineColor};
	__property System::Word LinePattern = {read=FLinePattern, write=SetLinePattern, default=65535};
	__property float LineWidth = {read=FLineWidth, write=SetLineWidth, stored=StoreLineWidth};
	__property Visible = {default=1};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLLineBase(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


class DELPHICLASS TGLNodedLines;
class PASCALIMPLEMENTATION TGLNodedLines : public TGLLineBase
{
	typedef TGLLineBase inherited;
	
private:
	TGLLinesNodes* FNodes;
	TLineNodesAspect FNodesAspect;
	Glcolor::TGLColor* FNodeColor;
	float FNodeSize;
	Vectortypes::TVector4f FOldNodeColor;
	
protected:
	void __fastcall SetNodesAspect(const TLineNodesAspect Value);
	void __fastcall SetNodeColor(Glcolor::TGLColor* const Value);
	void __fastcall OnNodeColorChanged(System::TObject* Sender);
	void __fastcall SetNodes(TGLLinesNodes* const aNodes);
	void __fastcall SetNodeSize(const float val);
	bool __fastcall StoreNodeSize(void);
	void __fastcall DrawNode(Glrendercontextinfo::TRenderContextInfo &rci, float X, float Y, float Z, Glcolor::TGLColor* Color);
	
public:
	__fastcall virtual TGLNodedLines(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLNodedLines(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled(void);
	void __fastcall AddNode(Glcoordinates::TGLCoordinates3* const coords)/* overload */;
	void __fastcall AddNode(const float X, const float Y, const float Z)/* overload */;
	void __fastcall AddNode(const Vectortypes::TVector4f &Value)/* overload */;
	void __fastcall AddNode(const Vectortypes::TVector3f &Value)/* overload */;
	
__published:
	__property Glcolor::TGLColor* NodeColor = {read=FNodeColor, write=SetNodeColor};
	__property TGLLinesNodes* Nodes = {read=FNodes, write=SetNodes};
	__property TLineNodesAspect NodesAspect = {read=FNodesAspect, write=SetNodesAspect, default=1};
	__property float NodeSize = {read=FNodeSize, write=SetNodeSize, stored=StoreNodeSize};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLNodedLines(Glscene::TGLBaseSceneObject* aParentOwner) : TGLLineBase(aParentOwner) { }
	
};


enum TLinesOption : unsigned char { loUseNodeColorForLines, loColorLogicXor };

typedef System::Set<TLinesOption, TLinesOption::loUseNodeColorForLines, TLinesOption::loColorLogicXor>  TLinesOptions;

class DELPHICLASS TGLLines;
class PASCALIMPLEMENTATION TGLLines : public TGLNodedLines
{
	typedef TGLNodedLines inherited;
	
private:
	int FDivision;
	TLineSplineMode FSplineMode;
	TLinesOptions FOptions;
	int FNURBSOrder;
	float FNURBSTolerance;
	Vectorlists::TSingleList* FNURBSKnots;
	
protected:
	void __fastcall SetSplineMode(const TLineSplineMode val);
	void __fastcall SetDivision(const int Value);
	void __fastcall SetOptions(const TLinesOptions val);
	void __fastcall SetNURBSOrder(const int val);
	void __fastcall SetNURBSTolerance(const float val);
	
public:
	__fastcall virtual TGLLines(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLLines(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	__property Vectorlists::TSingleList* NURBSKnots = {read=FNURBSKnots};
	__property int NURBSOrder = {read=FNURBSOrder, write=SetNURBSOrder, nodefault};
	__property float NURBSTolerance = {read=FNURBSTolerance, write=SetNURBSTolerance};
	
__published:
	__property int Division = {read=FDivision, write=SetDivision, default=10};
	__property TLineSplineMode SplineMode = {read=FSplineMode, write=SetSplineMode, default=0};
	__property TLinesOptions Options = {read=FOptions, write=SetOptions, nodefault};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLLines(Glscene::TGLBaseSceneObject* aParentOwner) : TGLNodedLines(aParentOwner) { }
	
};


enum TCubePart : unsigned char { cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight };

typedef System::Set<TCubePart, TCubePart::cpTop, TCubePart::cpRight>  TCubeParts;

class DELPHICLASS TGLCube;
class PASCALIMPLEMENTATION TGLCube : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	Vectortypes::TVector3f FCubeSize;
	TCubeParts FParts;
	Glscene::TNormalDirection FNormalDirection;
	float __fastcall GetCubeWHD(const int Index);
	void __fastcall SetCubeWHD(int Index, float AValue);
	void __fastcall SetParts(TCubeParts aValue);
	void __fastcall SetNormalDirection(Glscene::TNormalDirection aValue);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadData(System::Classes::TStream* Stream);
	void __fastcall WriteData(System::Classes::TStream* Stream);
	
public:
	__fastcall virtual TGLCube(System::Classes::TComponent* AOwner);
	virtual Glsilhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Glsilhouette::TGLSilhouetteParameters &silhouetteParameters);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled(void);
	virtual bool __fastcall RayCastIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, Vectorgeometry::PVector intersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector intersectNormal = (Vectorgeometry::PVector)(0x0));
	
__published:
	__property float CubeWidth = {read=GetCubeWHD, write=SetCubeWHD, stored=false, index=0};
	__property float CubeHeight = {read=GetCubeWHD, write=SetCubeWHD, stored=false, index=1};
	__property float CubeDepth = {read=GetCubeWHD, write=SetCubeWHD, stored=false, index=2};
	__property Glscene::TNormalDirection NormalDirection = {read=FNormalDirection, write=SetNormalDirection, default=1};
	__property TCubeParts Parts = {read=FParts, write=SetParts, default=63};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLCube(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLCube(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


enum TNormalSmoothing : unsigned char { nsFlat, nsSmooth, nsNone };

class DELPHICLASS TGLQuadricObject;
class PASCALIMPLEMENTATION TGLQuadricObject : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	TNormalSmoothing FNormals;
	Glscene::TNormalDirection FNormalDirection;
	
protected:
	void __fastcall SetNormals(TNormalSmoothing aValue);
	void __fastcall SetNormalDirection(Glscene::TNormalDirection aValue);
	void __fastcall SetupQuadricParams(Opengltokens::PGLUQuadric quadric);
	void __fastcall SetNormalQuadricOrientation(Opengltokens::PGLUQuadric quadric);
	void __fastcall SetInvertedQuadricOrientation(Opengltokens::PGLUQuadric quadric);
	
public:
	__fastcall virtual TGLQuadricObject(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property TNormalSmoothing Normals = {read=FNormals, write=SetNormals, default=1};
	__property Glscene::TNormalDirection NormalDirection = {read=FNormalDirection, write=SetNormalDirection, default=1};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLQuadricObject(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLQuadricObject(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


typedef System::Int8 TAngleLimit1;

typedef System::Word TAngleLimit2;

enum TCapType : unsigned char { ctNone, ctCenter, ctFlat };

class DELPHICLASS TGLSphere;
class PASCALIMPLEMENTATION TGLSphere : public TGLQuadricObject
{
	typedef TGLQuadricObject inherited;
	
private:
	float FRadius;
	int FSlices;
	int FStacks;
	TAngleLimit1 FTop;
	TAngleLimit1 FBottom;
	TAngleLimit2 FStart;
	TAngleLimit2 FStop;
	TCapType FTopCap;
	TCapType FBottomCap;
	void __fastcall SetBottom(TAngleLimit1 aValue);
	void __fastcall SetBottomCap(TCapType aValue);
	void __fastcall SetRadius(const float aValue);
	void __fastcall SetSlices(int aValue);
	void __fastcall SetStart(TAngleLimit2 aValue);
	void __fastcall SetStop(TAngleLimit2 aValue);
	void __fastcall SetStacks(int aValue);
	void __fastcall SetTop(TAngleLimit1 aValue);
	void __fastcall SetTopCap(TCapType aValue);
	
public:
	__fastcall virtual TGLSphere(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled(void);
	virtual bool __fastcall RayCastIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, Vectorgeometry::PVector intersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector intersectNormal = (Vectorgeometry::PVector)(0x0));
	virtual Glsilhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Glsilhouette::TGLSilhouetteParameters &silhouetteParameters);
	
__published:
	__property TAngleLimit1 Bottom = {read=FBottom, write=SetBottom, default=-90};
	__property TCapType BottomCap = {read=FBottomCap, write=SetBottomCap, default=0};
	__property float Radius = {read=FRadius, write=SetRadius};
	__property int Slices = {read=FSlices, write=SetSlices, default=16};
	__property int Stacks = {read=FStacks, write=SetStacks, default=16};
	__property TAngleLimit2 Start = {read=FStart, write=SetStart, default=0};
	__property TAngleLimit2 Stop = {read=FStop, write=SetStop, default=360};
	__property TAngleLimit1 Top = {read=FTop, write=SetTop, default=90};
	__property TCapType TopCap = {read=FTopCap, write=SetTopCap, default=0};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLSphere(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSphere(Glscene::TGLBaseSceneObject* aParentOwner) : TGLQuadricObject(aParentOwner) { }
	
};


class DELPHICLASS TGLPolygonBase;
class PASCALIMPLEMENTATION TGLPolygonBase : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	int FDivision;
	TLineSplineMode FSplineMode;
	
protected:
	Glnodes::TGLNodes* FNodes;
	DYNAMIC void __fastcall CreateNodes(void);
	void __fastcall SetSplineMode(const TLineSplineMode val);
	void __fastcall SetDivision(const int Value);
	void __fastcall SetNodes(Glnodes::TGLNodes* const aNodes);
	
public:
	__fastcall virtual TGLPolygonBase(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLPolygonBase(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	void __fastcall AddNode(Glcoordinates::TGLCoordinates3* const coords)/* overload */;
	void __fastcall AddNode(const float X, const float Y, const float Z)/* overload */;
	void __fastcall AddNode(const Vectortypes::TVector4f &Value)/* overload */;
	void __fastcall AddNode(const Vectortypes::TVector3f &Value)/* overload */;
	
__published:
	__property Glnodes::TGLNodes* Nodes = {read=FNodes, write=SetNodes};
	__property int Division = {read=FDivision, write=SetDivision, default=10};
	__property TLineSplineMode SplineMode = {read=FSplineMode, write=SetSplineMode, default=0};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLPolygonBase(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


class DELPHICLASS TGLSuperellipsoid;
class PASCALIMPLEMENTATION TGLSuperellipsoid : public TGLQuadricObject
{
	typedef TGLQuadricObject inherited;
	
private:
	float FRadius;
	float FxyCurve;
	float FzCurve;
	int FSlices;
	int FStacks;
	TAngleLimit1 FTop;
	TAngleLimit1 FBottom;
	TAngleLimit2 FStart;
	TAngleLimit2 FStop;
	TCapType FTopCap;
	TCapType FBottomCap;
	void __fastcall SetBottom(TAngleLimit1 aValue);
	void __fastcall SetBottomCap(TCapType aValue);
	void __fastcall SetRadius(const float aValue);
	void __fastcall SetxyCurve(const float aValue);
	void __fastcall SetzCurve(const float aValue);
	void __fastcall SetSlices(int aValue);
	void __fastcall SetStart(TAngleLimit2 aValue);
	void __fastcall SetStop(TAngleLimit2 aValue);
	void __fastcall SetStacks(int aValue);
	void __fastcall SetTop(TAngleLimit1 aValue);
	void __fastcall SetTopCap(TCapType aValue);
	
public:
	__fastcall virtual TGLSuperellipsoid(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled(void);
	virtual bool __fastcall RayCastIntersect(const Vectortypes::TVector4f &rayStart, const Vectortypes::TVector4f &rayVector, Vectorgeometry::PVector intersectPoint = (Vectorgeometry::PVector)(0x0), Vectorgeometry::PVector intersectNormal = (Vectorgeometry::PVector)(0x0));
	virtual Glsilhouette::TGLSilhouette* __fastcall GenerateSilhouette(const Glsilhouette::TGLSilhouetteParameters &silhouetteParameters);
	
__published:
	__property TAngleLimit1 Bottom = {read=FBottom, write=SetBottom, default=-90};
	__property TCapType BottomCap = {read=FBottomCap, write=SetBottomCap, default=0};
	__property float Radius = {read=FRadius, write=SetRadius};
	__property float xyCurve = {read=FxyCurve, write=SetxyCurve};
	__property float zCurve = {read=FzCurve, write=SetzCurve};
	__property int Slices = {read=FSlices, write=SetSlices, default=16};
	__property int Stacks = {read=FStacks, write=SetStacks, default=16};
	__property TAngleLimit2 Start = {read=FStart, write=SetStart, default=0};
	__property TAngleLimit2 Stop = {read=FStop, write=SetStop, default=360};
	__property TAngleLimit1 Top = {read=FTop, write=SetTop, default=90};
	__property TCapType TopCap = {read=FTopCap, write=SetTopCap, default=0};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLSuperellipsoid(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSuperellipsoid(Glscene::TGLBaseSceneObject* aParentOwner) : TGLQuadricObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::AnsiString TangentAttributeName;
extern PACKAGE System::AnsiString BinormalAttributeName;
extern PACKAGE void __fastcall CubeWireframeBuildList(Glrendercontextinfo::TRenderContextInfo &rci, float size, bool stipple, const Vectortypes::TVector4f &Color);
extern PACKAGE void __fastcall DodecahedronBuildList(void);
extern PACKAGE void __fastcall IcosahedronBuildList(void);
extern PACKAGE void __fastcall OctahedronBuildList(void);
extern PACKAGE void __fastcall TetrahedronBuildList(void);
}	/* namespace Globjects */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLOBJECTS)
using namespace Globjects;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlobjectsHPP
