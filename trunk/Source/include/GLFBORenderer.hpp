// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFBORenderer.pas' rev: 24.00 (Win32)

#ifndef GlfborendererHPP
#define GlfborendererHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLFBO.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLSLog.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glfborenderer
{
//-- type declarations -------------------------------------------------------
enum TGLEnabledRenderBuffer : unsigned char { erbDepth, erbStencil };

typedef System::Set<TGLEnabledRenderBuffer, TGLEnabledRenderBuffer::erbDepth, TGLEnabledRenderBuffer::erbStencil>  TGLEnabledRenderBuffers;

enum TGLFBOTargetVisibility : unsigned char { tvDefault, tvFBOOnly };

enum TGLFBOClearOption : unsigned char { coColorBufferClear, coDepthBufferClear, coStencilBufferClear, coUseBufferBackground };

typedef System::Set<TGLFBOClearOption, TGLFBOClearOption::coColorBufferClear, TGLFBOClearOption::coUseBufferBackground>  TGLFBOClearOptions;

class DELPHICLASS TGLFBORenderer;
class PASCALIMPLEMENTATION TGLFBORenderer : public Glscene::TGLBaseSceneObject
{
	typedef Glscene::TGLBaseSceneObject inherited;
	
private:
	Glfbo::TGLFrameBuffer* FFbo;
	Glfbo::TGLDepthRBO* FDepthRBO;
	Glfbo::TGLStencilRBO* FStencilRBO;
	int FColorAttachment;
	bool FRendering;
	bool FHasColor;
	bool FHasDepth;
	bool FHasStencil;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	System::UnicodeString FColorTextureName;
	System::UnicodeString FDepthTextureName;
	int FWidth;
	int FHeight;
	bool FForceTextureDimensions;
	Glfbo::TGLStencilPrecision FStencilPrecision;
	Glscene::TGLBaseSceneObject* FRootObject;
	bool FRootVisible;
	Glscene::TGLCamera* FCamera;
	TGLEnabledRenderBuffers FEnabledRenderBuffers;
	TGLFBOTargetVisibility FTargetVisibility;
	Glscene::TDirectRenderEvent FBeforeRender;
	System::Classes::TNotifyEvent FPostInitialize;
	Glscene::TDirectRenderEvent FAfterRender;
	System::Classes::TNotifyEvent FPreInitialize;
	Glcolor::TGLColor* FBackgroundColor;
	TGLFBOClearOptions FClearOptions;
	float FAspect;
	float FSceneScaleFactor;
	bool FUseLibraryAsMultiTarget;
	bool FPostGenerateMipmap;
	int FMaxSize;
	int FMaxAttachment;
	System::StaticArray<Vectortypes::TVector4f, 3> FStoreCamera;
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary(void);
	void __fastcall SetMaterialLibrary(Glmaterial::TGLAbstractMaterialLibrary* const Value);
	void __fastcall SetDepthTextureName(const System::UnicodeString Value);
	void __fastcall SetColorTextureName(const System::UnicodeString Value);
	void __fastcall SetForceTextureDimentions(const bool Value);
	void __fastcall SetHeight(int Value);
	void __fastcall SetWidth(int Value);
	void __fastcall SetLayer(const int Value);
	int __fastcall GetLayer(void);
	void __fastcall SetLevel(const int Value);
	int __fastcall GetLevel(void);
	void __fastcall SetStencilPrecision(const Glfbo::TGLStencilPrecision Value);
	void __fastcall SetRootObject(Glscene::TGLBaseSceneObject* const Value);
	Vectorgeometry::TRectangle __fastcall GetViewport(void);
	void __fastcall SetCamera(Glscene::TGLCamera* const Value);
	void __fastcall SetEnabledRenderBuffers(const TGLEnabledRenderBuffers Value);
	void __fastcall SetTargetVisibility(const TGLFBOTargetVisibility Value);
	void __fastcall SetBackgroundColor(Glcolor::TGLColor* const Value);
	bool __fastcall StoreSceneScaleFactor(void);
	bool __fastcall StoreAspect(void);
	void __fastcall SetUseLibraryAsMultiTarget(bool Value);
	void __fastcall SetPostGenerateMipmap(const bool Value);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall Initialize(void);
	void __fastcall ForceDimensions(Gltexture::TGLTexture* Texture);
	void __fastcall RenderToFBO(Glrendercontextinfo::TRenderContextInfo &ARci);
	void __fastcall ApplyCamera(Glrendercontextinfo::TRenderContextInfo &ARci);
	void __fastcall UnApplyCamera(Glrendercontextinfo::TRenderContextInfo &ARci);
	void __fastcall DoBeforeRender(Glrendercontextinfo::TRenderContextInfo &ARci);
	void __fastcall DoAfterRender(Glrendercontextinfo::TRenderContextInfo &ARci);
	void __fastcall DoPreInitialize(void);
	void __fastcall DoPostInitialize(void);
	__property bool HasColor = {read=FHasColor, nodefault};
	__property bool HasDepth = {read=FHasDepth, nodefault};
	__property bool HasStencil = {read=FHasStencil, nodefault};
	__property Vectorgeometry::TRectangle Viewport = {read=GetViewport};
	
public:
	__fastcall virtual TGLFBORenderer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLFBORenderer(void);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	__property int Layer = {read=GetLayer, write=SetLayer, nodefault};
	__property int Level = {read=GetLevel, write=SetLevel, nodefault};
	
__published:
	__property bool Active = {read=GetVisible, write=SetVisible, default=1};
	__property bool PickableTarget = {read=GetPickable, write=SetPickable, default=0};
	__property bool ForceTextureDimensions = {read=FForceTextureDimensions, write=SetForceTextureDimentions, default=1};
	__property int Width = {read=FWidth, write=SetWidth, default=256};
	__property int Height = {read=FHeight, write=SetHeight, default=256};
	__property float Aspect = {read=FAspect, write=FAspect, stored=StoreAspect};
	__property System::UnicodeString ColorTextureName = {read=FColorTextureName, write=SetColorTextureName};
	__property System::UnicodeString DepthTextureName = {read=FDepthTextureName, write=SetDepthTextureName};
	__property Glmaterial::TGLAbstractMaterialLibrary* MaterialLibrary = {read=GetMaterialLibrary, write=SetMaterialLibrary};
	__property Glcolor::TGLColor* BackgroundColor = {read=FBackgroundColor, write=SetBackgroundColor};
	__property TGLFBOClearOptions ClearOptions = {read=FClearOptions, write=FClearOptions, nodefault};
	__property Glscene::TGLCamera* Camera = {read=FCamera, write=SetCamera};
	__property float SceneScaleFactor = {read=FSceneScaleFactor, write=FSceneScaleFactor, stored=StoreSceneScaleFactor};
	__property Glscene::TGLBaseSceneObject* RootObject = {read=FRootObject, write=SetRootObject};
	__property TGLFBOTargetVisibility TargetVisibility = {read=FTargetVisibility, write=SetTargetVisibility, default=0};
	__property TGLEnabledRenderBuffers EnabledRenderBuffers = {read=FEnabledRenderBuffers, write=SetEnabledRenderBuffers, nodefault};
	__property Glfbo::TGLStencilPrecision StencilPrecision = {read=FStencilPrecision, write=SetStencilPrecision, default=0};
	__property Glscene::TDirectRenderEvent BeforeRender = {read=FBeforeRender, write=FBeforeRender};
	__property Glscene::TDirectRenderEvent AfterRender = {read=FAfterRender, write=FAfterRender};
	__property System::Classes::TNotifyEvent PreInitialize = {read=FPreInitialize, write=FPreInitialize};
	__property System::Classes::TNotifyEvent PostInitialize = {read=FPostInitialize, write=FPostInitialize};
	__property bool UseLibraryAsMultiTarget = {read=FUseLibraryAsMultiTarget, write=SetUseLibraryAsMultiTarget, default=0};
	__property bool PostGenerateMipmap = {read=FPostGenerateMipmap, write=SetPostGenerateMipmap, default=1};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFBORenderer(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLBaseSceneObject(aParentOwner) { }
	
private:
	void *__IGLMaterialLibrarySupported;	/* Glmaterial::IGLMaterialLibrarySupported */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator Glmaterial::_di_IGLMaterialLibrarySupported()
	{
		Glmaterial::_di_IGLMaterialLibrarySupported intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Glmaterial::IGLMaterialLibrarySupported*(void) { return (Glmaterial::IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glfborenderer */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFBORENDERER)
using namespace Glfborenderer;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfborendererHPP
