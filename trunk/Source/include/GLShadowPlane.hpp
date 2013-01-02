// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLShadowPlane.pas' rev: 24.00 (Win32)

#ifndef GlshadowplaneHPP
#define GlshadowplaneHPP

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
#include <GLObjects.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glshadowplane
{
//-- type declarations -------------------------------------------------------
enum TShadowPlaneOption : unsigned char { spoUseStencil, spoScissor, spoTransparent, spoIgnoreZ };

typedef System::Set<TShadowPlaneOption, TShadowPlaneOption::spoUseStencil, TShadowPlaneOption::spoIgnoreZ>  TShadowPlaneOptions;

class DELPHICLASS TGLShadowPlane;
class PASCALIMPLEMENTATION TGLShadowPlane : public Globjects::TGLPlane
{
	typedef Globjects::TGLPlane inherited;
	
private:
	bool FRendering;
	Glscene::TGLBaseSceneObject* FShadowingObject;
	Glscene::TGLLightSource* FShadowedLight;
	Glcolor::TGLColor* FShadowColor;
	TShadowPlaneOptions FShadowOptions;
	System::Classes::TNotifyEvent FOnBeginRenderingShadows;
	System::Classes::TNotifyEvent FOnEndRenderingShadows;
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetShadowingObject(Glscene::TGLBaseSceneObject* const val);
	void __fastcall SetShadowedLight(Glscene::TGLLightSource* const val);
	void __fastcall SetShadowColor(Glcolor::TGLColor* const val);
	void __fastcall SetShadowOptions(const TShadowPlaneOptions val);
	
public:
	__fastcall virtual TGLShadowPlane(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLShadowPlane(void);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Glscene::TGLBaseSceneObject* ShadowingObject = {read=FShadowingObject, write=SetShadowingObject};
	__property Glscene::TGLLightSource* ShadowedLight = {read=FShadowedLight, write=SetShadowedLight};
	__property Glcolor::TGLColor* ShadowColor = {read=FShadowColor, write=SetShadowColor};
	__property TShadowPlaneOptions ShadowOptions = {read=FShadowOptions, write=SetShadowOptions, default=3};
	__property System::Classes::TNotifyEvent OnBeginRenderingShadows = {read=FOnBeginRenderingShadows, write=FOnBeginRenderingShadows};
	__property System::Classes::TNotifyEvent OnEndRenderingShadows = {read=FOnEndRenderingShadows, write=FOnEndRenderingShadows};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLShadowPlane(Glscene::TGLBaseSceneObject* aParentOwner) : Globjects::TGLPlane(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define cDefaultShadowPlaneOptions (System::Set<TShadowPlaneOption, TShadowPlaneOption::spoUseStencil, TShadowPlaneOption::spoIgnoreZ> () << TShadowPlaneOption::spoUseStencil << TShadowPlaneOption::spoScissor )
}	/* namespace Glshadowplane */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSHADOWPLANE)
using namespace Glshadowplane;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlshadowplaneHPP
