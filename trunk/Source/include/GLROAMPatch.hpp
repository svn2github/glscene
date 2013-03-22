// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLROAMPatch.pas' rev: 24.00 (Win32)

#ifndef GlroampatchHPP
#define GlroampatchHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLHeightData.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glroampatch
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EGLROAMException;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLROAMException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLROAMException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLROAMException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EGLROAMException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLROAMException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLROAMException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLROAMException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EGLROAMException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLROAMException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLROAMException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLROAMException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLROAMException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLROAMException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLROAMException(void) { }
	
};

#pragma pack(pop)

struct TROAMTriangleNode;
typedef TROAMTriangleNode *PROAMTriangleNode;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TROAMTriangleNode
{
public:
	TROAMTriangleNode *base;
	TROAMTriangleNode *left;
	TROAMTriangleNode *right;
	TROAMTriangleNode *leftChild;
	TROAMTriangleNode *rightChild;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TROAMRenderPoint
{
public:
	int X;
	int Y;
	int idx;
};
#pragma pack(pop)


class DELPHICLASS TGLROAMPatch;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLROAMPatch : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	typedef System::DynamicArray<unsigned> _TGLROAMPatch__1;
	
	
private:
	int FID;
	Glheightdata::THeightData* FHeightData;
	Glheightdata::TSmallIntRaster *FHeightRaster;
	int FTLNode;
	int FBRNode;
	_TGLROAMPatch__1 FTLVariance;
	_TGLROAMPatch__1 FBRVariance;
	int FPatchSize;
	int FTriangleCount;
	Glcontext::TGLListHandle* FListHandle;
	int FTag;
	Vectortypes::TVector3f FObserverPosition;
	TGLROAMPatch* FNorth;
	TGLROAMPatch* FSouth;
	TGLROAMPatch* FWest;
	TGLROAMPatch* FEast;
	bool FHighRes;
	int FMaxDepth;
	Vectortypes::TVector3f FVertexScale;
	Vectortypes::TVector3f FVertexOffset;
	Vectortypes::TVector3f FTextureScale;
	Vectortypes::TVector3f FTextureOffset;
	int FMaxTLVarianceDepth;
	int FMaxBRVarianceDepth;
	Glcontext::TGLOcclusionQueryHandle* FOcclusionQuery;
	int FOcclusionSkip;
	int FOcclusionCounter;
	bool FLastOcclusionTestPassed;
	
protected:
	void __fastcall SetHeightData(Glheightdata::THeightData* val);
	void __fastcall SetOcclusionSkip(int val);
	void __fastcall RenderROAM(Vectorlists::TAffineVectorList* vertices, Vectorlists::TIntegerList* vertexIndices, Vectorlists::TTexPointList* texCoords);
	void __fastcall RenderAsStrips(Vectorlists::TAffineVectorList* vertices, Vectorlists::TIntegerList* vertexIndices, Vectorlists::TTexPointList* texCoords);
	bool __fastcall Tesselate(void);
	
public:
	__fastcall TGLROAMPatch(void);
	__fastcall virtual ~TGLROAMPatch(void);
	void __fastcall ComputeVariance(int variance);
	void __fastcall ResetTessellation(void);
	void __fastcall ConnectToTheWest(TGLROAMPatch* westPatch);
	void __fastcall ConnectToTheNorth(TGLROAMPatch* northPatch);
	bool __fastcall SafeTesselate(void);
	void __fastcall RenderHighRes(Vectorlists::TAffineVectorList* vertices, Vectorlists::TIntegerList* vertexIndices, Vectorlists::TTexPointList* texCoords, bool forceROAM);
	void __fastcall RenderAccum(Vectorlists::TAffineVectorList* vertices, Vectorlists::TIntegerList* vertexIndices, Vectorlists::TTexPointList* texCoords, int autoFlushVertexCount);
	__classmethod void __fastcall FlushAccum(Vectorlists::TAffineVectorList* vertices, Vectorlists::TIntegerList* vertexIndices, Vectorlists::TTexPointList* texCoords);
	__property Glheightdata::THeightData* HeightData = {read=FHeightData, write=SetHeightData};
	__property Vectortypes::TVector3f VertexScale = {read=FVertexScale, write=FVertexScale};
	__property Vectortypes::TVector3f VertexOffset = {read=FVertexOffset, write=FVertexOffset};
	__property Vectortypes::TVector3f ObserverPosition = {read=FObserverPosition, write=FObserverPosition};
	__property Vectortypes::TVector3f TextureScale = {read=FTextureScale, write=FTextureScale};
	__property Vectortypes::TVector3f TextureOffset = {read=FTextureOffset, write=FTextureOffset};
	__property bool HighRes = {read=FHighRes, write=FHighRes, nodefault};
	__property int OcclusionSkip = {read=FOcclusionSkip, write=SetOcclusionSkip, nodefault};
	__property int OcclusionCounter = {read=FOcclusionCounter, write=FOcclusionCounter, nodefault};
	__property bool LastOcclusionTestPassed = {read=FLastOcclusionTestPassed, nodefault};
	__property int ID = {read=FID, nodefault};
	__property int TriangleCount = {read=FTriangleCount, nodefault};
	__property int Tag = {read=FTag, write=FTag, nodefault};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall SetROAMTrianglesCapacity(int nb);
extern PACKAGE int __fastcall GetROAMTrianglesCapacity(void);
}	/* namespace Glroampatch */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLROAMPATCH)
using namespace Glroampatch;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlroampatchHPP
