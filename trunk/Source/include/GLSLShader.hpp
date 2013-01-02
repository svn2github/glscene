// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSLShader.pas' rev: 24.00 (Win32)

#ifndef GlslshaderHPP
#define GlslshaderHPP

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
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLCustomShader.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <GLSLParameter.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glslshader
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EGLSLShaderException;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLSLShaderException : public Glcustomshader::EGLCustomShaderException
{
	typedef Glcustomshader::EGLCustomShaderException inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLSLShaderException(const System::UnicodeString Msg) : Glcustomshader::EGLCustomShaderException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLSLShaderException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : Glcustomshader::EGLCustomShaderException(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EGLSLShaderException(NativeUInt Ident)/* overload */ : Glcustomshader::EGLCustomShaderException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLSLShaderException(System::PResStringRec ResStringRec)/* overload */ : Glcustomshader::EGLCustomShaderException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLSLShaderException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : Glcustomshader::EGLCustomShaderException(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLSLShaderException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : Glcustomshader::EGLCustomShaderException(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EGLSLShaderException(const System::UnicodeString Msg, int AHelpContext) : Glcustomshader::EGLCustomShaderException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLSLShaderException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : Glcustomshader::EGLCustomShaderException(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLSLShaderException(NativeUInt Ident, int AHelpContext)/* overload */ : Glcustomshader::EGLCustomShaderException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLSLShaderException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Glcustomshader::EGLCustomShaderException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLSLShaderException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : Glcustomshader::EGLCustomShaderException(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLSLShaderException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : Glcustomshader::EGLCustomShaderException(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLSLShaderException(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLCustomGLSLShader;
typedef void __fastcall (__closure *TGLSLShaderEvent)(TGLCustomGLSLShader* Shader);

typedef void __fastcall (__closure *TGLSLShaderUnApplyEvent)(TGLCustomGLSLShader* Shader, bool &ThereAreMorePasses);

struct DECLSPEC_DRECORD TGLActiveAttrib
{
public:
	System::UnicodeString Name;
	int Size;
	Glslparameter::TGLSLDataType AType;
	int Location;
};


typedef System::DynamicArray<TGLActiveAttrib> TGLActiveAttribArray;

class DELPHICLASS TGLSLShaderParameter;
class PASCALIMPLEMENTATION TGLCustomGLSLShader : public Glcustomshader::TGLCustomShader
{
	typedef Glcustomshader::TGLCustomShader inherited;
	
private:
	Glcontext::TGLProgramHandle* FGLSLProg;
	TGLSLShaderParameter* FParam;
	System::Classes::TStrings* FActiveVarying;
	Glcustomshader::TGLTransformFeedBackMode FTransformFeedBackMode;
	TGLSLShaderEvent FOnInitialize;
	TGLSLShaderEvent FOnApply;
	TGLSLShaderUnApplyEvent FOnUnApply;
	TGLSLShaderParameter* __fastcall GetParam(const System::UnicodeString Index);
	TGLSLShaderParameter* __fastcall GetDirectParam(const unsigned Index);
	void __fastcall OnChangeActiveVarying(System::TObject* Sender);
	
protected:
	__property TGLSLShaderEvent OnApply = {read=FOnApply, write=FOnApply};
	__property TGLSLShaderUnApplyEvent OnUnApply = {read=FOnUnApply, write=FOnUnApply};
	__property TGLSLShaderEvent OnInitialize = {read=FOnInitialize, write=FOnInitialize};
	virtual void __fastcall DoInitialPass(void);
	virtual Glcontext::TGLProgramHandle* __fastcall GetGLSLProg(void);
	virtual TGLSLShaderParameter* __fastcall GetCurrentParam(void);
	void __fastcall SetActiveVarying(System::Classes::TStrings* const Value);
	void __fastcall SetTransformFeedBackMode(const Glcustomshader::TGLTransformFeedBackMode Value);
	DYNAMIC void __fastcall DoInitialize(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	DYNAMIC void __fastcall DoFinalize(void);
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLCustomGLSLShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomGLSLShader(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual bool __fastcall ShaderSupported(void);
	TGLActiveAttribArray __fastcall GetActiveAttribs(void);
	__property TGLSLShaderParameter* Param[const System::UnicodeString Index] = {read=GetParam};
	__property TGLSLShaderParameter* DirectParam[const unsigned Index] = {read=GetDirectParam};
	__property System::Classes::TStrings* ActiveVarying = {read=FActiveVarying, write=SetActiveVarying};
	__property Glcustomshader::TGLTransformFeedBackMode TransformFeedBackMode = {read=FTransformFeedBackMode, write=SetTransformFeedBackMode, default=0};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSLShaderParameter : public Glcustomshader::TGLCustomShaderParameter
{
	typedef Glcustomshader::TGLCustomShaderParameter inherited;
	
private:
	Glcontext::TGLProgramHandle* FGLSLProg;
	int FParameterID;
	
protected:
	virtual float __fastcall GetAsVector1f(void);
	virtual Vectortypes::TVector2f __fastcall GetAsVector2f(void);
	virtual Vectortypes::TVector3f __fastcall GetAsVector3f(void);
	virtual Vectortypes::TVector4f __fastcall GetAsVector4f(void);
	virtual int __fastcall GetAsVector1i(void);
	virtual Vectortypes::TVector2i __fastcall GetAsVector2i(void);
	virtual Vectortypes::TVector3i __fastcall GetAsVector3i(void);
	virtual Vectortypes::TVector4i __fastcall GetAsVector4i(void);
	virtual unsigned __fastcall GetAsVector1ui(void);
	virtual Vectortypes::TVector2ui __fastcall GetAsVector2ui(void);
	virtual Vectortypes::TVector3ui __fastcall GetAsVector3ui(void);
	virtual Vectortypes::TVector4ui __fastcall GetAsVector4ui(void);
	virtual void __fastcall SetAsVector1f(const float Value);
	virtual void __fastcall SetAsVector2f(const Vectortypes::TVector2f &Value);
	virtual void __fastcall SetAsVector3f(const Vectortypes::TVector3f &Value);
	virtual void __fastcall SetAsVector4f(const Vectortypes::TVector4f &Value);
	virtual void __fastcall SetAsVector1i(const int Value);
	virtual void __fastcall SetAsVector2i(const Vectortypes::TVector2i &Value);
	virtual void __fastcall SetAsVector3i(const Vectortypes::TVector3i &Value);
	virtual void __fastcall SetAsVector4i(const Vectortypes::TVector4i &Value);
	virtual void __fastcall SetAsVector1ui(const unsigned Value);
	virtual void __fastcall SetAsVector2ui(const Vectortypes::TVector2ui &Value);
	virtual void __fastcall SetAsVector3ui(const Vectortypes::TVector3ui &Value);
	virtual void __fastcall SetAsVector4ui(const Vectortypes::TVector4ui &Value);
	virtual Vectortypes::TMatrix2f __fastcall GetAsMatrix2f(void);
	virtual Vectortypes::TMatrix3f __fastcall GetAsMatrix3f(void);
	virtual Vectortypes::TMatrix4f __fastcall GetAsMatrix4f(void);
	virtual void __fastcall SetAsMatrix2f(const Vectortypes::TMatrix2f &Value);
	virtual void __fastcall SetAsMatrix3f(const Vectortypes::TMatrix3f &Value);
	virtual void __fastcall SetAsMatrix4f(const Vectortypes::TMatrix4f &Value);
	virtual unsigned __fastcall GetAsCustomTexture(const int TextureIndex, Gltextureformat::TGLTextureTarget TextureTarget);
	virtual void __fastcall SetAsCustomTexture(const int TextureIndex, Gltextureformat::TGLTextureTarget TextureTarget, const unsigned Value);
	virtual unsigned __fastcall GetAsUniformBuffer(void);
	virtual void __fastcall SetAsUniformBuffer(unsigned UBO);
public:
	/* TObject.Create */ inline __fastcall TGLSLShaderParameter(void) : Glcustomshader::TGLCustomShaderParameter() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TGLSLShaderParameter(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLSLShader;
class PASCALIMPLEMENTATION TGLSLShader : public TGLCustomGLSLShader
{
	typedef TGLCustomGLSLShader inherited;
	
__published:
	__property FragmentProgram;
	__property VertexProgram;
	__property GeometryProgram;
	__property OnApply;
	__property OnUnApply;
	__property OnInitialize;
	__property ShaderStyle = {default=1};
	__property FailedInitAction = {default=1};
	__property ActiveVarying;
	__property TransformFeedBackMode = {default=0};
public:
	/* TGLCustomGLSLShader.Create */ inline __fastcall virtual TGLSLShader(System::Classes::TComponent* AOwner) : TGLCustomGLSLShader(AOwner) { }
	/* TGLCustomGLSLShader.Destroy */ inline __fastcall virtual ~TGLSLShader(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glslshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSLSHADER)
using namespace Glslshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlslshaderHPP
