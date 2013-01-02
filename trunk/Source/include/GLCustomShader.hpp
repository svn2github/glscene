// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLCustomShader.pas' rev: 24.00 (Win32)

#ifndef GlcustomshaderHPP
#define GlcustomshaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLCadencer.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLStrings.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <GLSLParameter.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glcustomshader
{
//-- type declarations -------------------------------------------------------
enum TGLShaderFogSupport : unsigned char { sfsEnabled, sfsDisabled, sfsAuto };

enum TGLTransformFeedBackMode : unsigned char { tfbmInterleaved, tfbmSeparate };

class DELPHICLASS EGLCustomShaderException;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLCustomShaderException : public Gltexture::EGLShaderException
{
	typedef Gltexture::EGLShaderException inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLCustomShaderException(const System::UnicodeString Msg) : Gltexture::EGLShaderException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLCustomShaderException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : Gltexture::EGLShaderException(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EGLCustomShaderException(NativeUInt Ident)/* overload */ : Gltexture::EGLShaderException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLCustomShaderException(System::PResStringRec ResStringRec)/* overload */ : Gltexture::EGLShaderException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLCustomShaderException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : Gltexture::EGLShaderException(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLCustomShaderException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : Gltexture::EGLShaderException(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EGLCustomShaderException(const System::UnicodeString Msg, int AHelpContext) : Gltexture::EGLShaderException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLCustomShaderException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : Gltexture::EGLShaderException(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLCustomShaderException(NativeUInt Ident, int AHelpContext)/* overload */ : Gltexture::EGLShaderException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLCustomShaderException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Gltexture::EGLShaderException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLCustomShaderException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : Gltexture::EGLShaderException(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLCustomShaderException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : Gltexture::EGLShaderException(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLCustomShaderException(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLCustomShader;
typedef void __fastcall (__closure *TGLShaderEvent)(TGLCustomShader* Shader);

typedef void __fastcall (__closure *TGLShaderUnAplyEvent)(TGLCustomShader* Shader, bool &ThereAreMorePasses);

typedef System::Int8 TGLLightSourceEnum;

typedef System::Set<TGLLightSourceEnum, 1, 8>  TGLLightSourceSet;

__interface IGLShaderDescription;
typedef System::DelphiInterface<IGLShaderDescription> _di_IGLShaderDescription;
__interface  INTERFACE_UUID("{04089C64-60C2-43F5-AC9C-38ED46264812}") IGLShaderDescription  : public System::IInterface 
{
	
public:
	virtual void __fastcall SetShaderTextures(Gltexture::TGLTexture* const *Textures, const int Textures_Size) = 0 ;
	virtual void __fastcall GetShaderTextures(Gltexture::TGLTexture* *Textures, const int Textures_Size) = 0 ;
	virtual void __fastcall SetShaderColorParams(const Vectortypes::TVector4f &AAmbientColor, const Vectortypes::TVector4f &ADiffuseColor, const Vectortypes::TVector4f &ASpecularcolor) = 0 ;
	virtual void __fastcall GetShaderColorParams(Vectortypes::TVector4f &AAmbientColor, Vectortypes::TVector4f &ADiffuseColor, Vectortypes::TVector4f &ASpecularcolor) = 0 ;
	virtual void __fastcall SetShaderMiscParameters(Glcadencer::TGLCadencer* const ACadencer, Glmaterial::TGLMaterialLibrary* const AMatLib, const TGLLightSourceSet ALightSources) = 0 ;
	virtual void __fastcall GetShaderMiscParameters(Glcadencer::TGLCadencer* &ACadencer, Glmaterial::TGLMaterialLibrary* &AMatLib, TGLLightSourceSet &ALightSources) = 0 ;
	virtual float __fastcall GetShaderAlpha(void) = 0 ;
	virtual void __fastcall SetShaderAlpha(const float Value) = 0 ;
	virtual System::UnicodeString __fastcall GetShaderDescription(void) = 0 ;
};

__interface IGLPostShader;
typedef System::DelphiInterface<IGLPostShader> _di_IGLPostShader;
__interface  INTERFACE_UUID("{68A62362-AF0A-4CE8-A9E1-714FE02AFA4A}") IGLPostShader  : public System::IInterface 
{
	
public:
	virtual void __fastcall DoUseTempTexture(Glcontext::TGLTextureHandle* const TempTexture, Gltextureformat::TGLTextureTarget TextureTarget) = 0 ;
	virtual Gltextureformat::TGLTextureTarget __fastcall GetTextureTarget(void) = 0 ;
};

class DELPHICLASS TGLFragmentProgram;
class DELPHICLASS TGLVertexProgram;
class DELPHICLASS TGLGeometryProgram;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLCustomShader : public Glmaterial::TGLShader
{
	typedef Glmaterial::TGLShader inherited;
	
private:
	TGLFragmentProgram* FFragmentProgram;
	TGLVertexProgram* FVertexProgram;
	TGLGeometryProgram* FGeometryProgram;
	System::TObject* FTagObject;
	void __fastcall SetFragmentProgram(TGLFragmentProgram* const Value);
	void __fastcall SetGeometryProgram(TGLGeometryProgram* const Value);
	void __fastcall SetVertexProgram(TGLVertexProgram* const Value);
	bool __fastcall StoreFragmentProgram(void);
	bool __fastcall StoreGeometryProgram(void);
	bool __fastcall StoreVertexProgram(void);
	
protected:
	bool FDebugMode;
	virtual void __fastcall SetDebugMode(const bool Value);
	__property TGLFragmentProgram* FragmentProgram = {read=FFragmentProgram, write=SetFragmentProgram, stored=StoreFragmentProgram};
	__property TGLVertexProgram* VertexProgram = {read=FVertexProgram, write=SetVertexProgram, stored=StoreVertexProgram};
	__property TGLGeometryProgram* GeometryProgram = {read=FGeometryProgram, write=SetGeometryProgram, stored=StoreGeometryProgram};
	__property bool DebugMode = {read=FDebugMode, write=SetDebugMode, default=0};
	__property System::TObject* TagObject = {read=FTagObject, write=FTagObject, default=0};
	
public:
	__fastcall virtual TGLCustomShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomShader(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall LoadShaderPrograms(const System::UnicodeString VPFilename, const System::UnicodeString FPFilename, System::UnicodeString GPFilename = System::UnicodeString());
};

#pragma pack(pop)

class DELPHICLASS TGLShaderProgram;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLShaderProgram : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TGLCustomShader* FParent;
	bool FEnabled;
	System::Classes::TStrings* FCode;
	void __fastcall SetCode(System::Classes::TStrings* const Value);
	void __fastcall SetEnabled(const bool Value);
	void __fastcall OnChangeCode(System::TObject* Sender);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	
public:
	void __fastcall LoadFromFile(const System::UnicodeString AFileName);
	virtual void __fastcall Apply(void);
	__fastcall virtual TGLShaderProgram(TGLCustomShader* const AParent);
	__fastcall virtual ~TGLShaderProgram(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property System::Classes::TStrings* Code = {read=FCode, write=SetCode};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=0};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLVertexProgram : public TGLShaderProgram
{
	typedef TGLShaderProgram inherited;
	
__published:
	__property Code;
	__property Enabled = {default=0};
public:
	/* TGLShaderProgram.Create */ inline __fastcall virtual TGLVertexProgram(TGLCustomShader* const AParent) : TGLShaderProgram(AParent) { }
	/* TGLShaderProgram.Destroy */ inline __fastcall virtual ~TGLVertexProgram(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLFragmentProgram : public TGLShaderProgram
{
	typedef TGLShaderProgram inherited;
	
__published:
	__property Code;
	__property Enabled = {default=0};
public:
	/* TGLShaderProgram.Create */ inline __fastcall virtual TGLFragmentProgram(TGLCustomShader* const AParent) : TGLShaderProgram(AParent) { }
	/* TGLShaderProgram.Destroy */ inline __fastcall virtual ~TGLFragmentProgram(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGeometryProgram : public TGLShaderProgram
{
	typedef TGLShaderProgram inherited;
	
private:
	Glslparameter::TGLgsInTypes FInputPrimitiveType;
	Glslparameter::TGLgsOutTypes FOutputPrimitiveType;
	int FVerticesOut;
	void __fastcall SetInputPrimitiveType(const Glslparameter::TGLgsInTypes Value);
	void __fastcall SetOutputPrimitiveType(const Glslparameter::TGLgsOutTypes Value);
	void __fastcall SetVerticesOut(const int Value);
	
public:
	__fastcall virtual TGLGeometryProgram(TGLCustomShader* const AParent);
	
__published:
	__property Code;
	__property Enabled = {default=0};
	__property Glslparameter::TGLgsInTypes InputPrimitiveType = {read=FInputPrimitiveType, write=SetInputPrimitiveType, default=0};
	__property Glslparameter::TGLgsOutTypes OutputPrimitiveType = {read=FOutputPrimitiveType, write=SetOutputPrimitiveType, default=0};
	__property int VerticesOut = {read=FVerticesOut, write=SetVerticesOut, default=0};
public:
	/* TGLShaderProgram.Destroy */ inline __fastcall virtual ~TGLGeometryProgram(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLCustomShaderParameter;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLCustomShaderParameter : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	virtual float __fastcall GetAsVector1f(void) = 0 ;
	virtual Vectortypes::TVector2f __fastcall GetAsVector2f(void) = 0 ;
	virtual Vectortypes::TVector3f __fastcall GetAsVector3f(void) = 0 ;
	virtual Vectortypes::TVector4f __fastcall GetAsVector4f(void) = 0 ;
	virtual int __fastcall GetAsVector1i(void) = 0 ;
	virtual Vectortypes::TVector2i __fastcall GetAsVector2i(void) = 0 ;
	virtual Vectortypes::TVector3i __fastcall GetAsVector3i(void) = 0 ;
	virtual Vectortypes::TVector4i __fastcall GetAsVector4i(void) = 0 ;
	virtual unsigned __fastcall GetAsVector1ui(void) = 0 ;
	virtual Vectortypes::TVector2ui __fastcall GetAsVector2ui(void) = 0 ;
	virtual Vectortypes::TVector3ui __fastcall GetAsVector3ui(void) = 0 ;
	virtual Vectortypes::TVector4ui __fastcall GetAsVector4ui(void) = 0 ;
	virtual void __fastcall SetAsVector1f(const float Value) = 0 ;
	virtual void __fastcall SetAsVector2f(const Vectortypes::TVector2f &Value) = 0 ;
	virtual void __fastcall SetAsVector3f(const Vectortypes::TVector3f &Value) = 0 ;
	virtual void __fastcall SetAsVector4f(const Vectortypes::TVector4f &Value) = 0 ;
	virtual void __fastcall SetAsVector1i(const int Value) = 0 ;
	virtual void __fastcall SetAsVector2i(const Vectortypes::TVector2i &Value) = 0 ;
	virtual void __fastcall SetAsVector3i(const Vectortypes::TVector3i &Value) = 0 ;
	virtual void __fastcall SetAsVector4i(const Vectortypes::TVector4i &Value) = 0 ;
	virtual void __fastcall SetAsVector1ui(const unsigned Value) = 0 ;
	virtual void __fastcall SetAsVector2ui(const Vectortypes::TVector2ui &Value) = 0 ;
	virtual void __fastcall SetAsVector3ui(const Vectortypes::TVector3ui &Value) = 0 ;
	virtual void __fastcall SetAsVector4ui(const Vectortypes::TVector4ui &Value) = 0 ;
	virtual Vectortypes::TMatrix2f __fastcall GetAsMatrix2f(void) = 0 ;
	virtual Vectortypes::TMatrix3f __fastcall GetAsMatrix3f(void) = 0 ;
	virtual Vectortypes::TMatrix4f __fastcall GetAsMatrix4f(void) = 0 ;
	virtual void __fastcall SetAsMatrix2f(const Vectortypes::TMatrix2f &Value) = 0 ;
	virtual void __fastcall SetAsMatrix3f(const Vectortypes::TMatrix3f &Value) = 0 ;
	virtual void __fastcall SetAsMatrix4f(const Vectortypes::TMatrix4f &Value) = 0 ;
	void __fastcall SetAsTexture(const int TextureIndex, Gltexture::TGLTexture* const Value);
	void __fastcall SetAsTexture1D(const int TextureIndex, Gltexture::TGLTexture* const Value);
	void __fastcall SetAsTexture2D(const int TextureIndex, Gltexture::TGLTexture* const Value);
	void __fastcall SetAsTexture3D(const int TextureIndex, Gltexture::TGLTexture* const Value);
	void __fastcall SetAsTextureCube(const int TextureIndex, Gltexture::TGLTexture* const Value);
	void __fastcall SetAsTextureRect(const int TextureIndex, Gltexture::TGLTexture* const Value);
	virtual unsigned __fastcall GetAsCustomTexture(const int TextureIndex, Gltextureformat::TGLTextureTarget TextureTarget) = 0 ;
	virtual void __fastcall SetAsCustomTexture(const int TextureIndex, Gltextureformat::TGLTextureTarget TextureTarget, const unsigned Value) = 0 ;
	virtual unsigned __fastcall GetAsUniformBuffer(void) = 0 ;
	virtual void __fastcall SetAsUniformBuffer(unsigned UBO) = 0 ;
	
public:
	void __fastcall SetAsVectorF(float const *Values, const int Values_Size)/* overload */;
	void __fastcall SetAsVectorI(int const *Values, const int Values_Size)/* overload */;
	void __fastcall SetToTextureOf(Glmaterial::TGLLibMaterial* const LibMaterial, const int TextureIndex)/* overload */;
	void __fastcall SetToTextureOf(Gltexture::TGLTexture* const Texture, const int TextureIndex)/* overload */;
	__property Vectortypes::TVector4f AsVector = {read=GetAsVector4f, write=SetAsVector4f};
	__property Vectortypes::TVector3f AsAffineVector = {read=GetAsVector3f, write=SetAsVector3f};
	__property float AsFloat = {read=GetAsVector1f, write=SetAsVector1f};
	__property int AsInteger = {read=GetAsVector1i, write=SetAsVector1i, nodefault};
	__property float AsVector1f = {read=GetAsVector1f, write=SetAsVector1f};
	__property Vectortypes::TVector2f AsVector2f = {read=GetAsVector2f, write=SetAsVector2f};
	__property Vectortypes::TVector3f AsVector3f = {read=GetAsVector3f, write=SetAsVector3f};
	__property Vectortypes::TVector4f AsVector4f = {read=GetAsVector4f, write=SetAsVector4f};
	__property int AsVector1i = {read=GetAsVector1i, write=SetAsVector1i, nodefault};
	__property Vectortypes::TVector2i AsVector2i = {read=GetAsVector2i, write=SetAsVector2i};
	__property Vectortypes::TVector3i AsVector3i = {read=GetAsVector3i, write=SetAsVector3i};
	__property Vectortypes::TVector4i AsVector4i = {read=GetAsVector4i, write=SetAsVector4i};
	__property unsigned AsVector1ui = {read=GetAsVector1ui, write=SetAsVector1ui, nodefault};
	__property Vectortypes::TVector2ui AsVector2ui = {read=GetAsVector2ui, write=SetAsVector2ui};
	__property Vectortypes::TVector3ui AsVector3ui = {read=GetAsVector3ui, write=SetAsVector3ui};
	__property Vectortypes::TVector4ui AsVector4ui = {read=GetAsVector4ui, write=SetAsVector4ui};
	__property Vectortypes::TMatrix2f AsMatrix2f = {read=GetAsMatrix2f, write=SetAsMatrix2f};
	__property Vectortypes::TMatrix3f AsMatrix3f = {read=GetAsMatrix3f, write=SetAsMatrix3f};
	__property Vectortypes::TMatrix4f AsMatrix4f = {read=GetAsMatrix4f, write=SetAsMatrix4f};
	__property Gltexture::TGLTexture* AsTexture[const int TextureIndex] = {write=SetAsTexture};
	__property Gltexture::TGLTexture* AsTexture1D[const int TextureIndex] = {write=SetAsTexture1D};
	__property Gltexture::TGLTexture* AsTexture2D[const int TextureIndex] = {write=SetAsTexture2D};
	__property Gltexture::TGLTexture* AsTexture3D[const int TextureIndex] = {write=SetAsTexture3D};
	__property Gltexture::TGLTexture* AsTextureRect[const int TextureIndex] = {write=SetAsTextureRect};
	__property Gltexture::TGLTexture* AsTextureCube[const int TextureIndex] = {write=SetAsTextureCube};
	__property unsigned AsCustomTexture[const int TextureIndex][Gltextureformat::TGLTextureTarget TextureTarget] = {read=GetAsCustomTexture, write=SetAsCustomTexture};
	__property unsigned AsUniformBuffer = {read=GetAsUniformBuffer, write=SetAsUniformBuffer, nodefault};
public:
	/* TObject.Create */ inline __fastcall TGLCustomShaderParameter(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLCustomShaderParameter(void) { }
	
};

#pragma pack(pop)

enum TGLBlendingModeEx : unsigned char { bmxOpaque, bmxTransparency, bmxAdditive, bmxAlphaTest50, bmxAlphaTest100, bmxModulate, bmxDestColorOne, bmxDestAlphaOne };

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 glsShaderMaxLightSources = System::Int8(0x8);
extern PACKAGE void __fastcall GetActiveLightsList(Vectorlists::TIntegerList* const ALightIDs);
extern PACKAGE bool __fastcall IsFogEnabled(const TGLShaderFogSupport AFogSupportMode, Glrendercontextinfo::TRenderContextInfo &rci);
extern PACKAGE void __fastcall CopyScreentoTexture(const Glrendercontextinfo::TGLSize &ViewPortSize, const System::Word TextureTarget = (System::Word)(0xde1));
extern PACKAGE void __fastcall CopyScreentoTexture2(const Glrendercontextinfo::TGLSize &ViewPortSize, const System::Word TextureTarget = (System::Word)(0xde1));
extern PACKAGE void __fastcall ApplyBlendingModeEx(const TGLBlendingModeEx BlendingMode);
extern PACKAGE void __fastcall UnApplyBlendingModeEx(void);
extern PACKAGE void __fastcall DrawTexturedScreenQuad(void);
extern PACKAGE void __fastcall DrawTexturedScreenQuad2(const Glrendercontextinfo::TGLSize &ViewPortSize);
extern PACKAGE void __fastcall DrawTexturedScreenQuad4(const Glrendercontextinfo::TGLSize &ViewPortSize);
extern PACKAGE void __fastcall DrawTexturedScreenQuad5(const Glrendercontextinfo::TGLSize &ViewPortSize);
extern PACKAGE void __fastcall DrawTexturedScreenQuad6(const Glrendercontextinfo::TGLSize &ViewPortSize);
extern PACKAGE void __fastcall DrawTexturedScreenQuad3(void);
extern PACKAGE void __fastcall InitTexture(const unsigned TextureHandle, const Glrendercontextinfo::TGLSize &TextureSize, const Gltextureformat::TGLTextureTarget TextureTarget = (Gltextureformat::TGLTextureTarget)(0x2));
}	/* namespace Glcustomshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLCUSTOMSHADER)
using namespace Glcustomshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlcustomshaderHPP
