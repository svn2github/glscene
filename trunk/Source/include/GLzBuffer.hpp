// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLzBuffer.pas' rev: 24.00 (Win32)

#ifndef GlzbufferHPP
#define GlzbufferHPP

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
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <GLObjects.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLViewer.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <GLWin32Viewer.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glzbuffer
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EZBufferException;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EZBufferException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EZBufferException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EZBufferException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EZBufferException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EZBufferException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EZBufferException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EZBufferException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EZBufferException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EZBufferException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EZBufferException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EZBufferException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EZBufferException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EZBufferException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EZBufferException(void) { }
	
};

#pragma pack(pop)

typedef System::StaticArray<float, 268435456> TZArray;

typedef TZArray *PZArray;

typedef System::DynamicArray<PZArray> TZArrayIdx;

typedef System::StaticArray<System::Byte, 268435456> TAArray;

typedef TAArray *PAArray;

typedef System::DynamicArray<PAArray> TAArrayIdx;

enum TOptimise : unsigned char { opNone, op4in1, op9in1, op16in1 };

class DELPHICLASS TGLzBuffer;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLzBuffer : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TZArray *FData;
	TZArrayIdx FDataIdx;
	TZArrayIdx FDataInvIdx;
	int FWidth;
	int FHeight;
	int FDataSize;
	float ang1;
	float ang2;
	float scal;
	float c1;
	float s1;
	float c2;
	float s2;
	float vw;
	float vh;
	Vectortypes::TVector3f lt;
	Vectortypes::TVector3f rt;
	Vectortypes::TVector3f lb;
	Vectortypes::TVector3f rb;
	Vectortypes::TVector3f UpVec;
	Vectortypes::TVector3f riVec;
	Vectortypes::TVector3f ltW;
	Vectortypes::TVector3f rtW;
	Vectortypes::TVector3f lbW;
	Vectortypes::TVector3f rbW;
	Vectortypes::TVector3f UpVecW;
	Vectortypes::TVector3f riVecW;
	float OrthInvDov;
	float OrthAddX;
	float OrthMulX;
	float OrthAddY;
	float OrthMulY;
	float dov;
	float np;
	float fp;
	float NpFp;
	float OneMinNp_Fp;
	float invOneMinNp_Fp;
	Glscene::TGLCamera* cam;
	void __fastcall DoCalcVectors(void);
	
protected:
	void __fastcall PrepareBufferMemory(void);
	void __fastcall SetWidth(int val);
	void __fastcall SetHeight(const int val);
	
public:
	Glwin32viewer::TGLSceneViewer* SceneViewer;
	Glscene::TGLMemoryViewer* MemoryViewer;
	Glscene::TGLSceneBuffer* Buffer;
	Vectortypes::TVector3f Normal;
	__fastcall TGLzBuffer(void);
	__fastcall virtual ~TGLzBuffer(void);
	void __fastcall LinkToViewer(Glwin32viewer::TGLSceneViewer* viewer)/* overload */;
	void __fastcall LinkToViewer(Glscene::TGLMemoryViewer* viewer)/* overload */;
	PZArray __fastcall GetDepthBuffer(bool CalcVectors, bool ContextIsActive);
	float __fastcall GetPixelzDepth(int x, int y);
	float __fastcall PixelToDistance_OLD(int x, int y);
	float __fastcall PixelToDistance(int x, int y);
	__property int Width = {read=FWidth, write=SetWidth, nodefault};
	__property int Height = {read=FHeight, write=SetHeight, nodefault};
	__property int DataSize = {read=FDataSize, nodefault};
	__property PZArray Data = {read=FData};
	__property TZArrayIdx DataIdx = {read=FDataIdx};
	__property TZArrayIdx DataInvIdx = {read=FDataIdx};
	void __fastcall Refresh(void);
	Vectortypes::TVector3f __fastcall FastScreenToVector(int x, int y);
	Vectortypes::TVector3f __fastcall FastVectorToScreen(const Vectortypes::TVector3f &vec);
	Vectortypes::TVector3f __fastcall PixelToWorld(const int x, const int y);
	bool __fastcall WorldToPixel(const Vectortypes::TVector3f &aPoint, /* out */ int &pixX, /* out */ int &pixY, /* out */ float &pixZ);
	bool __fastcall WorldToPixelZ(const Vectortypes::TVector3f &aPoint, /* out */ int &pixX, /* out */ int &pixY, /* out */ float &pixZ)/* overload */;
	bool __fastcall WorldToPixelZ(const Vectortypes::TVector3f &aPoint, /* out */ float &pixX, /* out */ float &pixY, /* out */ float &pixZ)/* overload */;
	bool __fastcall OrthWorldToPixelZ(const Vectortypes::TVector3f &aPoint, /* out */ float &pixX, /* out */ float &pixY, /* out */ float &pixZ);
};

#pragma pack(pop)

class DELPHICLASS TGLZShadows;
class PASCALIMPLEMENTATION TGLZShadows : public Glscene::TGLBaseSceneObject
{
	typedef Glscene::TGLBaseSceneObject inherited;
	
private:
	Glwin32viewer::TGLSceneViewer* FViewer;
	Glscene::TGLMemoryViewer* FCaster;
	bool FDepthFade;
	bool FFrustShadow;
	bool FSkyShadow;
	TOptimise FOptimise;
	TAArray *FData;
	TAArrayIdx FDataIdx;
	TAArrayIdx FDataInvIdx;
	int FDataSize;
	int FWidth;
	int FHeight;
	int FXRes;
	int FYRes;
	bool Fsoft;
	float FTolerance;
	Glcolor::TGLColor* FColor;
	Glgraphics::TGLPixel32 SCol;
	bool FTexturePrepared;
	Glcontext::TGLTextureHandle* FTexHandle;
	
protected:
	void __fastcall PrepareAlphaMemory(void);
	Glwin32viewer::TGLSceneViewer* __fastcall GetViewer(void);
	void __fastcall SetViewer(Glwin32viewer::TGLSceneViewer* const val);
	Glscene::TGLMemoryViewer* __fastcall GetCaster(void);
	void __fastcall SetCaster(Glscene::TGLMemoryViewer* const val);
	void __fastcall CalcShadowTexture(Glrendercontextinfo::TRenderContextInfo &rci);
	System::Byte __fastcall HardSet(const int x, const int y);
	System::Byte __fastcall SoftTest(const int x, const int y);
	void __fastcall SetWidth(const int val);
	void __fastcall SetHeight(const int val);
	void __fastcall SetXRes(const int val);
	void __fastcall SetYRes(const int val);
	void __fastcall SetSoft(const bool val);
	void __fastcall BindTexture(void);
	
public:
	TGLzBuffer* ViewerZBuf;
	TGLzBuffer* CasterZBuf;
	__fastcall virtual TGLZShadows(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLZShadows(void);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	
__published:
	__property Glwin32viewer::TGLSceneViewer* Viewer = {read=GetViewer, write=SetViewer};
	__property Glscene::TGLMemoryViewer* Caster = {read=GetCaster, write=SetCaster};
	__property bool FrustShadow = {read=FFrustShadow, write=FFrustShadow, nodefault};
	__property bool SkyShadow = {read=FSkyShadow, write=FSkyShadow, nodefault};
	__property TOptimise Optimise = {read=FOptimise, write=FOptimise, nodefault};
	__property int Width = {read=FWidth, write=SetWidth, nodefault};
	__property int Height = {read=FHeight, write=SetHeight, nodefault};
	__property Glcolor::TGLColor* Color = {read=FColor, write=FColor};
	__property bool Soft = {read=Fsoft, write=SetSoft, nodefault};
	__property float Tolerance = {read=FTolerance, write=FTolerance};
	__property ObjectsSorting = {default=0};
	__property Visible = {default=1};
	__property bool DepthFade = {read=FDepthFade, write=FDepthFade, nodefault};
	bool __fastcall CastShadow(void);
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLZShadows(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLBaseSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glzbuffer */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLZBUFFER)
using namespace Glzbuffer;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlzbufferHPP
