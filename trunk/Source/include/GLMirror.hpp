// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLMirror.pas' rev: 24.00 (Win32)

#ifndef GlmirrorHPP
#define GlmirrorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <OpenGLAdapter.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glmirror
{
//-- type declarations -------------------------------------------------------
enum TMirrorOption : unsigned char { moUseStencil, moOpaque, moMirrorPlaneClip, moClearZBuffer };

typedef System::Set<TMirrorOption, TMirrorOption::moUseStencil, TMirrorOption::moClearZBuffer>  TMirrorOptions;

enum TMirrorShapes : unsigned char { msRect, msDisk };

class DELPHICLASS TGLMirror;
class PASCALIMPLEMENTATION TGLMirror : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	bool FRendering;
	Glscene::TGLBaseSceneObject* FMirrorObject;
	float FWidth;
	float FHeight;
	TMirrorOptions FMirrorOptions;
	System::Classes::TNotifyEvent FOnBeginRenderingMirrors;
	System::Classes::TNotifyEvent FOnEndRenderingMirrors;
	TMirrorShapes FShape;
	float FRadius;
	int FSlices;
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetMirrorObject(Glscene::TGLBaseSceneObject* const val);
	void __fastcall SetMirrorOptions(const TMirrorOptions val);
	void __fastcall ClearZBufferArea(Glscene::TGLSceneBuffer* aBuffer);
	void __fastcall SetHeight(float AValue);
	void __fastcall SetWidth(float AValue);
	void __fastcall SetRadius(const float aValue);
	void __fastcall SetSlices(const int aValue);
	void __fastcall SetShape(TMirrorShapes aValue);
	float __fastcall GetRadius(void);
	int __fastcall GetSlices(void);
	
public:
	__fastcall virtual TGLMirror(System::Classes::TComponent* AOwner);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &ARci);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Vectortypes::TVector4f __fastcall AxisAlignedDimensionsUnscaled(void);
	
__published:
	__property Glscene::TGLBaseSceneObject* MirrorObject = {read=FMirrorObject, write=SetMirrorObject};
	__property TMirrorOptions MirrorOptions = {read=FMirrorOptions, write=SetMirrorOptions, default=1};
	__property float Height = {read=FHeight, write=SetHeight};
	__property float Width = {read=FWidth, write=SetWidth};
	__property System::Classes::TNotifyEvent OnBeginRenderingMirrors = {read=FOnBeginRenderingMirrors, write=FOnBeginRenderingMirrors};
	__property System::Classes::TNotifyEvent OnEndRenderingMirrors = {read=FOnEndRenderingMirrors, write=FOnEndRenderingMirrors};
	__property float Radius = {read=FRadius, write=SetRadius};
	__property int Slices = {read=FSlices, write=SetSlices, default=16};
	__property TMirrorShapes Shape = {read=FShape, write=SetShape, default=0};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLMirror(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLMirror(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define cDefaultMirrorOptions (System::Set<TMirrorOption, TMirrorOption::moUseStencil, TMirrorOption::moClearZBuffer> () << TMirrorOption::moUseStencil )
}	/* namespace Glmirror */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLMIRROR)
using namespace Glmirror;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlmirrorHPP
