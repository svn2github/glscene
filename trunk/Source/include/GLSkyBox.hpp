// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSkyBox.pas' rev: 24.00 (Win32)

#ifndef GlskyboxHPP
#define GlskyboxHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <XOpenGL.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glskybox
{
//-- type declarations -------------------------------------------------------
enum TGLSkyBoxStyle : unsigned char { sbsFull, sbsTopHalf, sbsBottomHalf, sbTopTwoThirds, sbsTopHalfClamped };

class DELPHICLASS TGLSkyBox;
class PASCALIMPLEMENTATION TGLSkyBox : public Glscene::TGLCameraInvariantObject
{
	typedef Glscene::TGLCameraInvariantObject inherited;
	
private:
	System::UnicodeString FMatNameTop;
	System::UnicodeString FMatNameRight;
	System::UnicodeString FMatNameFront;
	System::UnicodeString FMatNameLeft;
	System::UnicodeString FMatNameBack;
	System::UnicodeString FMatNameBottom;
	System::UnicodeString FMatNameClouds;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	float FCloudsPlaneOffset;
	float FCloudsPlaneSize;
	TGLSkyBoxStyle FStyle;
	Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary(void);
	
protected:
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Value);
	void __fastcall SetMatNameBack(const System::UnicodeString Value);
	void __fastcall SetMatNameBottom(const System::UnicodeString Value);
	void __fastcall SetMatNameFront(const System::UnicodeString Value);
	void __fastcall SetMatNameLeft(const System::UnicodeString Value);
	void __fastcall SetMatNameRight(const System::UnicodeString Value);
	void __fastcall SetMatNameTop(const System::UnicodeString Value);
	void __fastcall SetMatNameClouds(const System::UnicodeString Value);
	void __fastcall SetCloudsPlaneOffset(const float Value);
	void __fastcall SetCloudsPlaneSize(const float Value);
	void __fastcall SetStyle(const TGLSkyBoxStyle value);
	
public:
	__fastcall virtual TGLSkyBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSkyBox(void);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &ARci);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
__published:
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property System::UnicodeString MatNameTop = {read=FMatNameTop, write=SetMatNameTop};
	__property System::UnicodeString MatNameBottom = {read=FMatNameBottom, write=SetMatNameBottom};
	__property System::UnicodeString MatNameLeft = {read=FMatNameLeft, write=SetMatNameLeft};
	__property System::UnicodeString MatNameRight = {read=FMatNameRight, write=SetMatNameRight};
	__property System::UnicodeString MatNameFront = {read=FMatNameFront, write=SetMatNameFront};
	__property System::UnicodeString MatNameBack = {read=FMatNameBack, write=SetMatNameBack};
	__property System::UnicodeString MatNameClouds = {read=FMatNameClouds, write=SetMatNameClouds};
	__property float CloudsPlaneOffset = {read=FCloudsPlaneOffset, write=SetCloudsPlaneOffset};
	__property float CloudsPlaneSize = {read=FCloudsPlaneSize, write=SetCloudsPlaneSize};
	__property TGLSkyBoxStyle Style = {read=FStyle, write=FStyle, default=0};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSkyBox(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLCameraInvariantObject(aParentOwner) { }
	
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
}	/* namespace Glskybox */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSKYBOX)
using namespace Glskybox;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlskyboxHPP
