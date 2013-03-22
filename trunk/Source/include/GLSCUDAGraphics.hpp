// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSCUDAGraphics.pas' rev: 24.00 (Win32)

#ifndef GlscudagraphicsHPP
#define GlscudagraphicsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLS_CUDA_API.hpp>	// Pascal unit
#include <GLSCUDA.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLGraphics.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLSLShader.hpp>	// Pascal unit
#include <GLSLParameter.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLSCUDAContext.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glscudagraphics
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLVertexAttribute;
typedef void __fastcall (__closure *TOnBeforeKernelLaunch)(TGLVertexAttribute* Sender);

class DELPHICLASS TGLVertexAttributes;
class PASCALIMPLEMENTATION TGLVertexAttribute : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FName;
	Glslparameter::TGLSLDataType FType;
	Glscuda::TCUDAFunction* FFunc;
	int FLocation;
	TOnBeforeKernelLaunch FOnBeforeKernelLaunch;
	void __fastcall SetName(const System::UnicodeString AName);
	void __fastcall SetType(Glslparameter::TGLSLDataType AType);
	void __fastcall SetFunc(Glscuda::TCUDAFunction* AFunc);
	int __fastcall GetLocation(void);
	HIDESBASE TGLVertexAttributes* __fastcall GetOwner(void);
	
public:
	__fastcall virtual TGLVertexAttribute(System::Classes::TCollection* ACollection);
	void __fastcall NotifyChange(System::TObject* Sender);
	__property int Location = {read=GetLocation, nodefault};
	
__published:
	__property System::UnicodeString Name = {read=FName, write=SetName};
	__property Glslparameter::TGLSLDataType GLSLType = {read=FType, write=SetType, nodefault};
	__property Glscuda::TCUDAFunction* KernelFunction = {read=FFunc, write=SetFunc};
	__property TOnBeforeKernelLaunch OnBeforeKernelLaunch = {read=FOnBeforeKernelLaunch, write=FOnBeforeKernelLaunch};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TGLVertexAttribute(void) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLVertexAttributes : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLVertexAttribute* operator[](int Index) { return Attributes[Index]; }
	
private:
	void __fastcall SetItems(int Index, TGLVertexAttribute* const AValue);
	TGLVertexAttribute* __fastcall GetItems(int Index);
	
public:
	__fastcall TGLVertexAttributes(System::Classes::TComponent* AOwner);
	void __fastcall NotifyChange(System::TObject* Sender);
	System::UnicodeString __fastcall MakeUniqueName(const System::UnicodeString ANameRoot);
	TGLVertexAttribute* __fastcall GetAttributeByName(const System::UnicodeString AName);
	HIDESBASE TGLVertexAttribute* __fastcall Add(void);
	__property TGLVertexAttribute* Attributes[int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLVertexAttributes(void) { }
	
};

#pragma pack(pop)

enum TFeedBackMeshPrimitive : unsigned int { fbmpPoint, fbmpLine, fbmpTriangle };

enum TFeedBackMeshLaunching : unsigned int { fblCommon, fblOnePerAtttribute };

class DELPHICLASS TGLCustomFeedBackMesh;
class PASCALIMPLEMENTATION TGLCustomFeedBackMesh : public Glscene::TGLBaseSceneObject
{
	typedef Glscene::TGLBaseSceneObject inherited;
	
private:
	Glscuda::TCUDAGraphicResource* FGeometryResource;
	TGLVertexAttributes* FAttributes;
	Glcontext::TGLVertexArrayHandle* FVAO;
	Glcontext::TGLVBOArrayBufferHandle* FVBO;
	Glcontext::TGLVBOElementArrayHandle* FEBO;
	TFeedBackMeshPrimitive FPrimitiveType;
	int FVertexNumber;
	int FElementNumber;
	Glslshader::TGLSLShader* FShader;
	Glscuda::TCUDAFunction* FCommonFunc;
	TFeedBackMeshLaunching FLaunching;
	bool FBlend;
	void __fastcall SetAttributes(TGLVertexAttributes* AValue);
	void __fastcall SetPrimitiveType(TFeedBackMeshPrimitive AValue);
	void __fastcall SetVertexNumber(int AValue);
	void __fastcall SetElementNumber(int AValue);
	void __fastcall SetShader(Glslshader::TGLSLShader* AShader);
	void __fastcall SetCommonFunc(Glscuda::TCUDAFunction* AFunc);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall RefreshAttributes(void);
	void __fastcall AllocateHandles(void);
	void __fastcall LaunchKernels(void);
	__property TGLVertexAttributes* Attributes = {read=FAttributes, write=SetAttributes};
	__property Glslshader::TGLSLShader* Shader = {read=FShader, write=SetShader};
	__property TFeedBackMeshPrimitive PrimitiveType = {read=FPrimitiveType, write=SetPrimitiveType, default=0};
	__property int VertexNumber = {read=FVertexNumber, write=SetVertexNumber, default=1};
	__property int ElementNumber = {read=FElementNumber, write=SetElementNumber, default=0};
	__property Glscuda::TCUDAFunction* CommonKernelFunction = {read=FCommonFunc, write=SetCommonFunc};
	__property TFeedBackMeshLaunching Launching = {read=FLaunching, write=FLaunching, default=0};
	__property bool Blend = {read=FBlend, write=FBlend, default=0};
	
public:
	__fastcall virtual TGLCustomFeedBackMesh(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomFeedBackMesh(void);
	virtual void __fastcall DoRender(Glrendercontextinfo::TRenderContextInfo &ARci, bool ARenderSelf, bool ARenderChildren);
	__property Glcontext::TGLVBOArrayBufferHandle* ArrayBufferHandle = {read=FVBO};
	__property Glcontext::TGLVBOElementArrayHandle* ElementArrayHandle = {read=FEBO};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLCustomFeedBackMesh(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLBaseSceneObject(aParentOwner) { }
	
};


class DELPHICLASS TGLFeedBackMesh;
class PASCALIMPLEMENTATION TGLFeedBackMesh : public TGLCustomFeedBackMesh
{
	typedef TGLCustomFeedBackMesh inherited;
	
__published:
	__property Attributes;
	__property Shader;
	__property PrimitiveType = {default=0};
	__property VertexNumber = {default=1};
	__property ElementNumber = {default=0};
	__property CommonKernelFunction;
	__property Launching = {default=0};
	__property Blend = {default=0};
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
public:
	/* TGLCustomFeedBackMesh.Create */ inline __fastcall virtual TGLFeedBackMesh(System::Classes::TComponent* AOwner) : TGLCustomFeedBackMesh(AOwner) { }
	/* TGLCustomFeedBackMesh.Destroy */ inline __fastcall virtual ~TGLFeedBackMesh(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLFeedBackMesh(Glscene::TGLBaseSceneObject* aParentOwner) : TGLCustomFeedBackMesh(aParentOwner) { }
	
};


class DELPHICLASS TCUDAGLImageResource;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCUDAGLImageResource : public Glscuda::TCUDAGraphicResource
{
	typedef Glscuda::TCUDAGraphicResource inherited;
	
private:
	Glmaterial::TGLMaterialLibrary* fMaterialLibrary;
	System::UnicodeString fTextureName;
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const Value);
	void __fastcall SetTextureName(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall AllocateHandles(void);
	virtual void __fastcall DestroyHandles(void);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TCUDAGLImageResource(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCUDAGLImageResource(void);
	virtual void __fastcall MapResources(void);
	virtual void __fastcall UnMapResources(void);
	virtual void __fastcall BindArrayToTexture(Glscuda::TCUDAMemData* &cudaArray, unsigned ALeyer, unsigned ALevel);
	
__published:
	__property System::UnicodeString TextureName = {read=fTextureName, write=SetTextureName};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=fMaterialLibrary, write=SetMaterialLibrary};
	__property Mapping = {default=0};
};

#pragma pack(pop)

class DELPHICLASS TCUDAGLGeometryResource;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCUDAGLGeometryResource : public Glscuda::TCUDAGraphicResource
{
	typedef Glscuda::TCUDAGraphicResource inherited;
	
private:
	TGLCustomFeedBackMesh* FFeedBackMesh;
	void __fastcall SetFeedBackMesh(TGLCustomFeedBackMesh* const Value);
	unsigned __fastcall GetAttribArraySize(TGLVertexAttribute* AAttr);
	
protected:
	virtual void __fastcall AllocateHandles(void);
	virtual void __fastcall DestroyHandles(void);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual unsigned __fastcall GetAttributeArraySize(const System::UnicodeString AName);
	virtual void * __fastcall GetAttributeArrayAddress(const System::UnicodeString AName);
	virtual unsigned __fastcall GetElementArrayDataSize(void);
	virtual void * __fastcall GetElementArrayAddress(void);
	
public:
	__fastcall virtual TCUDAGLGeometryResource(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCUDAGLGeometryResource(void);
	virtual void __fastcall MapResources(void);
	virtual void __fastcall UnMapResources(void);
	__property unsigned AttributeDataSize[const System::UnicodeString AttribName] = {read=GetAttributeArraySize};
	__property void * AttributeDataAddress[const System::UnicodeString AttribName] = {read=GetAttributeArrayAddress};
	__property unsigned IndexDataSize = {read=GetElementArrayDataSize, nodefault};
	__property void * IndexDataAddress = {read=GetElementArrayAddress};
	
__published:
	__property TGLCustomFeedBackMesh* FeedBackMesh = {read=FFeedBackMesh, write=SetFeedBackMesh};
	__property Mapping = {default=0};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glscudagraphics */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCUDAGRAPHICS)
using namespace Glscudagraphics;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlscudagraphicsHPP
