// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLProjectedTextures.pas' rev: 24.00 (Win32)

#ifndef GlprojectedtexturesHPP
#define GlprojectedtexturesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <XOpenGL.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glprojectedtextures
{
//-- type declarations -------------------------------------------------------
enum TGLProjectedTexturesStyle : unsigned char { ptsOriginal, ptsInverse };

class DELPHICLASS TGLTextureEmitter;
class PASCALIMPLEMENTATION TGLTextureEmitter : public Glscene::TGLSceneObject
{
	typedef Glscene::TGLSceneObject inherited;
	
private:
	float FFOVy;
	float FAspect;
	
protected:
	void __fastcall SetupTexMatrix(Glrendercontextinfo::TRenderContextInfo &ARci);
	
public:
	__fastcall virtual TGLTextureEmitter(System::Classes::TComponent* AOwner);
	
__published:
	__property float FOVy = {read=FFOVy, write=FFOVy};
	__property float Aspect = {read=FAspect, write=FAspect};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLTextureEmitter(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLTextureEmitter(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLSceneObject(aParentOwner) { }
	
};


class DELPHICLASS TGLTextureEmitterItem;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTextureEmitterItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TGLTextureEmitter* FEmitter;
	
protected:
	void __fastcall SetEmitter(TGLTextureEmitter* const val);
	void __fastcall RemoveNotification(System::Classes::TComponent* aComponent);
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	
public:
	__fastcall virtual TGLTextureEmitterItem(System::Classes::TCollection* ACollection);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property TGLTextureEmitter* Emitter = {read=FEmitter, write=SetEmitter};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TGLTextureEmitterItem(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLTextureEmitters;
class DELPHICLASS TGLProjectedTextures;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLTextureEmitters : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TGLTextureEmitterItem* operator[](int index) { return Items[index]; }
	
private:
	TGLProjectedTextures* FOwner;
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	TGLTextureEmitterItem* __fastcall GetItems(int index);
	void __fastcall RemoveNotification(System::Classes::TComponent* aComponent);
	
public:
	void __fastcall AddEmitter(TGLTextureEmitter* texEmitter);
	__property TGLTextureEmitterItem* Items[int index] = {read=GetItems/*, default*/};
public:
	/* TCollection.Create */ inline __fastcall TGLTextureEmitters(System::Classes::TCollectionItemClass ItemClass) : System::Classes::TCollection(ItemClass) { }
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLTextureEmitters(void) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLProjectedTextures : public Glscene::TGLImmaterialSceneObject
{
	typedef Glscene::TGLImmaterialSceneObject inherited;
	
private:
	TGLTextureEmitters* FEmitters;
	TGLProjectedTexturesStyle FStyle;
	
public:
	__fastcall virtual TGLProjectedTextures(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLProjectedTextures(void);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	
__published:
	__property TGLTextureEmitters* Emitters = {read=FEmitters, write=FEmitters};
	__property TGLProjectedTexturesStyle Style = {read=FStyle, write=FStyle, nodefault};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLProjectedTextures(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLImmaterialSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glprojectedtextures */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPROJECTEDTEXTURES)
using namespace Glprojectedtextures;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlprojectedtexturesHPP
