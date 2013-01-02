// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLCgShader.pas' rev: 24.00 (Win32)

#ifndef GlcgshaderHPP
#define GlcgshaderHPP

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
#include <VectorLists.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLStrings.hpp>	// Pascal unit
#include <GLCadencer.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLTextureFormat.hpp>	// Pascal unit
#include <cg.hpp>	// Pascal unit
#include <cgGL.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glcgshader
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EGLCGShaderException;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLCGShaderException : public Gltexture::EGLShaderException
{
	typedef Gltexture::EGLShaderException inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLCGShaderException(const System::UnicodeString Msg) : Gltexture::EGLShaderException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLCGShaderException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : Gltexture::EGLShaderException(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EGLCGShaderException(NativeUInt Ident)/* overload */ : Gltexture::EGLShaderException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLCGShaderException(System::PResStringRec ResStringRec)/* overload */ : Gltexture::EGLShaderException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLCGShaderException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : Gltexture::EGLShaderException(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLCGShaderException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : Gltexture::EGLShaderException(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EGLCGShaderException(const System::UnicodeString Msg, int AHelpContext) : Gltexture::EGLShaderException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLCGShaderException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : Gltexture::EGLShaderException(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLCGShaderException(NativeUInt Ident, int AHelpContext)/* overload */ : Gltexture::EGLShaderException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLCGShaderException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Gltexture::EGLShaderException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLCGShaderException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : Gltexture::EGLShaderException(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLCGShaderException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : Gltexture::EGLShaderException(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLCGShaderException(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TCgProgram;
typedef void __fastcall (__closure *TCgApplyEvent)(TCgProgram* CgProgram, System::TObject* Sender);

typedef void __fastcall (__closure *TCgUnApplyEvent)(TCgProgram* CgProgram);

class DELPHICLASS TCustomCgShader;
typedef void __fastcall (__closure *TCgShaderEvent)(TCustomCgShader* CgShader);

enum TcgProgramType : unsigned char { ptVertex, ptFragment };

enum TCgVPProfile : unsigned char { vpDetectLatest, vp20, vp30, vp40, arbvp1 };

enum TCgFPProfile : unsigned char { fpDetectLatest, fp20, fp30, fp40, arbfp1 };

enum TPrecisionSetting : unsigned char { psFull, psFast };

class DELPHICLASS TCgParameter;
class PASCALIMPLEMENTATION TCgProgram : public Baseclasses::TGLUpdateAbleObject
{
	typedef Baseclasses::TGLUpdateAbleObject inherited;
	
private:
	Cg::_CGcontext *FCgContext;
	System::Classes::TStrings* FCode;
	System::UnicodeString FProgramName;
	Cg::_CGprogram *FHandle;
	System::Classes::TList* FParams;
	TCgApplyEvent FOnApply;
	TCgUnApplyEvent FOnUnApply;
	System::Classes::TNotifyEvent FOnProgramChanged;
	bool FEnabled;
	bool FDetectProfile;
	TPrecisionSetting FPrecision;
	void __fastcall SetPrecision(const TPrecisionSetting Value);
	bool __fastcall GetManualNotification(void);
	void __fastcall SetManualNotification(const bool Value);
	
protected:
	TcgProgramType FProgramType;
	Cg::TCGprofile FProfile;
	void __fastcall SetCode(System::Classes::TStrings* const val);
	void __fastcall SetProgramName(const System::UnicodeString val);
	TCgParameter* __fastcall GetParam(System::UnicodeString index);
	void __fastcall AddParamsItem(const Cg::PCGparameter Param);
	void __fastcall BuildParamsList(void);
	void __fastcall ClearParamsList(void);
	
public:
	__fastcall virtual TCgProgram(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TCgProgram(void);
	virtual Cg::TCGprofile __fastcall GetLatestProfile(void) = 0 ;
	DYNAMIC void __fastcall Initialize(void);
	void __fastcall Finalize(void);
	void __fastcall Apply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	void __fastcall UnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	TCgParameter* __fastcall ParamByName(const System::UnicodeString name);
	__property TCgParameter* Param[System::UnicodeString index] = {read=GetParam};
	__property System::Classes::TList* Params = {read=FParams};
	Cg::PCGparameter __fastcall DirectParamByName(const System::UnicodeString name);
	int __fastcall ParamCount(void);
	System::UnicodeString __fastcall GetProfileStringA(void);
	void __fastcall LoadFromFile(const System::UnicodeString fileName);
	void __fastcall ListCompilation(System::Classes::TStrings* Output);
	void __fastcall ListParameters(System::Classes::TStrings* Output);
	void __fastcall SetParam(System::UnicodeString ParamName, float SingleVal)/* overload */;
	void __fastcall SetParam(System::UnicodeString ParamName, const Vectortypes::TVector2f &Vector2fVal)/* overload */;
	void __fastcall SetParam(System::UnicodeString ParamName, const Vectortypes::TVector3f &Vector3fVal)/* overload */;
	void __fastcall SetParam(System::UnicodeString ParamName, const Vectortypes::TVector4f &Vector4fVal)/* overload */;
	void __fastcall SetStateMatrix(System::UnicodeString ParamName, unsigned matrix, unsigned Transform);
	void __fastcall SetTexture(System::UnicodeString ParamName, unsigned TextureID);
	System::UnicodeString __fastcall LongName(void);
	__property Cg::TCGprofile DirectProfile = {read=FProfile, write=FProfile, nodefault};
	__property System::Classes::TNotifyEvent OnProgramChanged = {read=FOnProgramChanged, write=FOnProgramChanged};
	__property bool ManualNotification = {read=GetManualNotification, write=SetManualNotification, default=0};
	
__published:
	__property System::Classes::TStrings* Code = {read=FCode, write=SetCode};
	__property System::UnicodeString ProgramName = {read=FProgramName, write=SetProgramName};
	__property bool Enabled = {read=FEnabled, write=FEnabled, default=1};
	__property TPrecisionSetting Precision = {read=FPrecision, write=SetPrecision, default=0};
	__property TCgApplyEvent OnApply = {read=FOnApply, write=FOnApply};
	__property TCgUnApplyEvent OnUnApply = {read=FOnUnApply, write=FOnUnApply};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TCgParameter : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TCgProgram* FOwner;
	System::UnicodeString FName;
	Cg::_CGparameter *FHandle;
	Cg::TCGtype FValueType;
	Cg::TCGenum FDirection;
	Cg::TCGenum FVariability;
	
protected:
	System::UnicodeString __fastcall TypeMismatchMessage(void);
	void __fastcall CheckValueType(Cg::TCGtype aType)/* overload */;
	void __fastcall CheckValueType(Cg::TCGtype const *types, const int types_Size)/* overload */;
	void __fastcall CheckAllTextureTypes(void);
	void __fastcall CheckAllScalarTypes(void);
	void __fastcall CheckAllVector2fTypes(void);
	void __fastcall CheckAllVector3fTypes(void);
	void __fastcall CheckAllVector4fTypes(void);
	void __fastcall SetAsVector2f(const Vectortypes::TVector2f &val);
	void __fastcall SetAsVector3f(const Vectortypes::TVector3f &val);
	void __fastcall SetAsVector4f(const Vectortypes::TVector4f &val);
	
public:
	__fastcall virtual TCgParameter(void);
	__fastcall virtual ~TCgParameter(void);
	void __fastcall SetAsScalar(const float val)/* overload */;
	void __fastcall SetAsScalar(const bool val)/* overload */;
	void __fastcall SetAsVector(const Vectortypes::TVector2f &val)/* overload */;
	void __fastcall SetAsVector(const Vectortypes::TVector3f &val)/* overload */;
	void __fastcall SetAsVector(const Vectortypes::TVector4f &val)/* overload */;
	void __fastcall SetAsVector(float const *val, const int val_Size)/* overload */;
	void __fastcall SetAsStateMatrix(unsigned matrix, unsigned Transform);
	void __fastcall SetAsMatrix(const Vectortypes::TMatrix4f &val);
	void __fastcall SetAsTexture(unsigned TextureID);
	void __fastcall SetAsTexture1D(unsigned TextureID);
	void __fastcall SetAsTexture2D(unsigned TextureID);
	void __fastcall SetAsTexture3D(unsigned TextureID);
	void __fastcall SetAsTextureCUBE(unsigned TextureID);
	void __fastcall SetAsTextureRECT(unsigned TextureID);
	void __fastcall SetToTextureOf(Glmaterial::TGLLibMaterial* LibMaterial);
	void __fastcall EnableTexture(void);
	void __fastcall DisableTexture(void);
	void __fastcall SetParameterPointer(Vectorlists::TVectorList* Values)/* overload */;
	void __fastcall SetParameterPointer(Vectorlists::TAffineVectorList* Values)/* overload */;
	void __fastcall EnableClientState(void);
	void __fastcall DisableClientState(void);
	System::UnicodeString __fastcall LongName(void);
	__property TCgProgram* Owner = {read=FOwner};
	__property System::UnicodeString Name = {read=FName};
	__property Cg::TCGtype ValueType = {read=FValueType, nodefault};
	__property Cg::PCGparameter Handle = {read=FHandle, write=FHandle};
	__property Cg::TCGenum Direction = {read=FDirection, write=FDirection, nodefault};
	__property Cg::TCGenum Variability = {read=FVariability, write=FVariability, nodefault};
	__property Vectortypes::TVector4f AsVector = {write=SetAsVector4f};
	__property Vectortypes::TVector3f AsAffineVector = {write=SetAsVector3f};
	__property Vectortypes::TVector2f AsVector2f = {write=SetAsVector2f};
};

#pragma pack(pop)

class DELPHICLASS TCgVertexProgram;
class PASCALIMPLEMENTATION TCgVertexProgram : public TCgProgram
{
	typedef TCgProgram inherited;
	
private:
	TCgVPProfile FVPProfile;
	void __fastcall SetVPProfile(TCgVPProfile v);
	
public:
	__fastcall virtual TCgVertexProgram(System::Classes::TPersistent* AOwner);
	virtual Cg::TCGprofile __fastcall GetLatestProfile(void);
	
__published:
	__property TCgVPProfile Profile = {read=FVPProfile, write=SetVPProfile, default=0};
public:
	/* TCgProgram.Destroy */ inline __fastcall virtual ~TCgVertexProgram(void) { }
	
};


class DELPHICLASS TCgFragmentProgram;
class PASCALIMPLEMENTATION TCgFragmentProgram : public TCgProgram
{
	typedef TCgProgram inherited;
	
private:
	TCgFPProfile FFPProfile;
	bool FManageTexture;
	void __fastcall SetFPProfile(TCgFPProfile v);
	void __fastcall SetManageTexture(const bool Value);
	
public:
	__fastcall virtual TCgFragmentProgram(System::Classes::TPersistent* AOwner);
	DYNAMIC void __fastcall Initialize(void);
	virtual Cg::TCGprofile __fastcall GetLatestProfile(void);
	
__published:
	__property TCgFPProfile Profile = {read=FFPProfile, write=SetFPProfile, default=0};
	__property bool ManageTexture = {read=FManageTexture, write=SetManageTexture, default=0};
public:
	/* TCgProgram.Destroy */ inline __fastcall virtual ~TCgFragmentProgram(void) { }
	
};


class PASCALIMPLEMENTATION TCustomCgShader : public Glmaterial::TGLShader
{
	typedef Glmaterial::TGLShader inherited;
	
private:
	TCgVertexProgram* FVertexProgram;
	TCgFragmentProgram* FFragmentProgram;
	TCgShaderEvent FOnInitialize;
	bool FDesignEnable;
	
protected:
	void __fastcall SetVertexProgram(TCgVertexProgram* const val);
	void __fastcall SetOnApplyVertexProgram(const TCgApplyEvent val);
	TCgApplyEvent __fastcall GetOnApplyVertexProgram(void);
	void __fastcall SetOnUnApplyVertexProgram(const TCgUnApplyEvent val);
	TCgUnApplyEvent __fastcall GetOnUnApplyVertexProgram(void);
	void __fastcall SetFragmentProgram(TCgFragmentProgram* const val);
	void __fastcall SetOnApplyFragmentProgram(const TCgApplyEvent val);
	TCgApplyEvent __fastcall GetOnApplyFragmentProgram(void);
	void __fastcall SetOnUnApplyFragmentProgram(const TCgUnApplyEvent val);
	TCgUnApplyEvent __fastcall GetOnUnApplyFragmentProgram(void);
	TCgShaderEvent __fastcall GetOnInitialize(void);
	void __fastcall SetOnInitialize(const TCgShaderEvent val);
	DYNAMIC void __fastcall DoInitialize(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	DYNAMIC void __fastcall DoFinalize(void);
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	bool __fastcall IsProfileSupported(Cg::TCGprofile Profile);
	__property TCgApplyEvent OnApplyVP = {read=GetOnApplyVertexProgram, write=SetOnApplyVertexProgram};
	__property TCgApplyEvent OnApplyFP = {read=GetOnApplyFragmentProgram, write=SetOnApplyFragmentProgram};
	__property TCgUnApplyEvent OnUnApplyVP = {read=GetOnUnApplyVertexProgram, write=SetOnUnApplyVertexProgram};
	__property TCgUnApplyEvent OnUnApplyFP = {read=GetOnUnApplyFragmentProgram, write=SetOnUnApplyFragmentProgram};
	__property TCgShaderEvent OnInitialize = {read=GetOnInitialize, write=SetOnInitialize};
	__property bool DesignEnable = {read=FDesignEnable, write=FDesignEnable, default=0};
	__property TCgVertexProgram* VertexProgram = {read=FVertexProgram, write=SetVertexProgram};
	__property TCgFragmentProgram* FragmentProgram = {read=FFragmentProgram, write=SetFragmentProgram};
	
public:
	__fastcall virtual TCustomCgShader(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomCgShader(void);
	void __fastcall LoadShaderPrograms(const System::UnicodeString VPFilename, const System::UnicodeString FPFilename);
	virtual bool __fastcall ShaderSupported(void);
};


class DELPHICLASS TCadencableCustomCgShader;
class PASCALIMPLEMENTATION TCadencableCustomCgShader : public TCustomCgShader
{
	typedef TCustomCgShader inherited;
	
private:
	Glcadencer::TGLCadencer* FCadencer;
	void __fastcall SetCadencer(Glcadencer::TGLCadencer* const Value);
	
protected:
	DYNAMIC void __fastcall DoInitialize(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__property Glcadencer::TGLCadencer* Cadencer = {read=FCadencer, write=SetCadencer};
public:
	/* TCustomCgShader.Create */ inline __fastcall virtual TCadencableCustomCgShader(System::Classes::TComponent* AOwner) : TCustomCgShader(AOwner) { }
	/* TCustomCgShader.Destroy */ inline __fastcall virtual ~TCadencableCustomCgShader(void) { }
	
};


class DELPHICLASS TCgShader;
class PASCALIMPLEMENTATION TCgShader : public TCustomCgShader
{
	typedef TCustomCgShader inherited;
	
__published:
	__property DesignEnable = {default=0};
	__property ShaderStyle = {default=1};
	__property FailedInitAction = {default=1};
	__property VertexProgram;
	__property FragmentProgram;
	__property OnApplyVP;
	__property OnApplyFP;
	__property OnUnApplyVP;
	__property OnUnApplyFP;
	__property OnInitialize;
public:
	/* TCustomCgShader.Create */ inline __fastcall virtual TCgShader(System::Classes::TComponent* AOwner) : TCustomCgShader(AOwner) { }
	/* TCustomCgShader.Destroy */ inline __fastcall virtual ~TCgShader(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::UnicodeString IncludeFilePath;
extern PACKAGE bool __fastcall IsCgProfileSupported(Cg::TCGprofile Profile);
}	/* namespace Glcgshader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLCGSHADER)
using namespace Glcgshader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlcgshaderHPP
