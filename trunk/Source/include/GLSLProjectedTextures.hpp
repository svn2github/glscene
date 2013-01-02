// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSLProjectedTextures.pas' rev: 24.00 (Win32)

#ifndef GlslprojectedtexturesHPP
#define GlslprojectedtexturesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glslprojectedtextures
{
//-- type declarations -------------------------------------------------------
enum TGLSLProjectedTexturesStyle : unsigned char { ptsLight, ptsShadow };

class DELPHICLASS TGLSLTextureEmitter;
class DELPHICLASS TGLSLProjectedTextures;
class PASCALIMPLEMENTATION TGLSLTextureEmitter : public Glscene::TGLBaseSceneObject
{
	typedef Glscene::TGLBaseSceneObject inherited;
	
private:
	float FFOV;
	float FAspect;
	float FBrightness;
	float FAttenuation;
	TGLSLProjectedTexturesStyle FStyle;
	Glcolor::TGLColor* FColor;
	bool FUseAttenuation;
	bool FAllowReverseProjection;
	bool FUseQuadraticAttenuation;
	
protected:
	TGLSLProjectedTextures* ProjectedTexturesObject;
	Vectortypes::TMatrix4f TexMatrix;
	void __fastcall SetupTexMatrix(void);
	void __fastcall SetStyle(TGLSLProjectedTexturesStyle val);
	void __fastcall SetUseAttenuation(bool val);
	void __fastcall SetUseQuadraticAttenuation(bool val);
	void __fastcall SetAllowReverseProjection(bool val);
	
public:
	__fastcall virtual TGLSLTextureEmitter(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSLTextureEmitter(void);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	
__published:
	__property float FOV = {read=FFOV, write=FFOV};
	__property float Aspect = {read=FAspect, write=FAspect};
	__property TGLSLProjectedTexturesStyle Style = {read=FStyle, write=SetStyle, nodefault};
	__property float Attenuation = {read=FAttenuation, write=FAttenuation};
	__property float Brightness = {read=FBrightness, write=FBrightness};
	__property Glcolor::TGLColor* Color = {read=FColor, write=FColor};
	__property bool UseAttenuation = {read=FUseAttenuation, write=SetUseAttenuation, nodefault};
	__property bool UseQuadraticAttenuation = {read=FUseQuadraticAttenuation, write=SetUseQuadraticAttenuation, nodefault};
	__property bool AllowReverseProjection = {read=FAllowReverseProjection, write=SetAllowReverseProjection, nodefault};
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
	__property OnProgress;
	__property Behaviours;
	__property Effects;
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSLTextureEmitter(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLBaseSceneObject(aParentOwner) { }
	
};


class DELPHICLASS TGLSLTextureEmitterItem;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSLTextureEmitterItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TGLSLTextureEmitter* FEmitter;
	
protected:
	void __fastcall SetEmitter(TGLSLTextureEmitter* const val);
	void __fastcall RemoveNotification(System::Classes::TComponent* aComponent);
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	
public:
	__fastcall virtual TGLSLTextureEmitterItem(System::Classes::TCollection* Collection);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property TGLSLTextureEmitter* Emitter = {read=FEmitter, write=SetEmitter};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TGLSLTextureEmitterItem(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLSLTextureEmitters;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSLTextureEmitters : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TGLSLTextureEmitterItem* operator[](int index) { return Items[index]; }
	
private:
	TGLSLProjectedTextures* FOwner;
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	TGLSLTextureEmitterItem* __fastcall GetItems(int index);
	void __fastcall RemoveNotification(System::Classes::TComponent* aComponent);
	
public:
	void __fastcall AddEmitter(TGLSLTextureEmitter* texEmitter);
	__property TGLSLTextureEmitterItem* Items[int index] = {read=GetItems/*, default*/};
public:
	/* TCollection.Create */ inline __fastcall TGLSLTextureEmitters(System::Classes::TCollectionItemClass ItemClass) : System::Classes::TCollection(ItemClass) { }
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLSLTextureEmitters(void) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLSLProjectedTextures : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	bool ShaderSupported;
	TGLSLTextureEmitters* FEmitters;
	bool FUseLightmaps;
	Glcontext::TGLProgramHandle* Shader;
	Glcolor::TGLColor* FAmbient;
	void __fastcall SetupShader(void);
	
protected:
	bool ShaderChanged;
	void __fastcall SetUseLightmaps(bool val);
	
public:
	__fastcall virtual TGLSLProjectedTextures(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSLProjectedTextures(void);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &rci, bool renderSelf, bool renderChildren);
	DYNAMIC void __fastcall StructureChanged(void);
	
__published:
	__property TGLSLTextureEmitters* Emitters = {read=FEmitters, write=FEmitters};
	__property Glcolor::TGLColor* Ambient = {read=FAmbient, write=FAmbient};
	__property bool UseLightmaps = {read=FUseLightmaps, write=SetUseLightmaps, nodefault};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLSLProjectedTextures(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glslprojectedtextures */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSLPROJECTEDTEXTURES)
using namespace Glslprojectedtextures;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlslprojectedtexturesHPP
