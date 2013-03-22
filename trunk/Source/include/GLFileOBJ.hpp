// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFileOBJ.pas' rev: 24.00 (Win32)

#ifndef GlfileobjHPP
#define GlfileobjHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <ApplicationFileIO.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLVectorFileObjects.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLColor.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glfileobj
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGLOBJVectorFile;
class PASCALIMPLEMENTATION TGLOBJVectorFile : public Glvectorfileobjects::TVectorFile
{
	typedef Glvectorfileobjects::TVectorFile inherited;
	
private:
	System::Classes::TStream* FSourceStream;
	System::AnsiString FBuffer;
	System::UnicodeString FLine;
	int FLineNo;
	bool FEof;
	int FBufPos;
	
protected:
	void __fastcall ReadLine(void);
	void __fastcall Error(const System::UnicodeString msg);
	void __fastcall CalcMissingOBJNormals(Glvectorfileobjects::TMeshObject* mesh);
	
public:
	__classmethod virtual Applicationfileio::TDataFileCapabilities __fastcall Capabilities();
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	DYNAMIC void __fastcall SaveToStream(System::Classes::TStream* aStream);
public:
	/* TVectorFile.Create */ inline __fastcall virtual TGLOBJVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLOBJVectorFile(void) { }
	
};


class DELPHICLASS EGLOBJFileError;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLOBJFileError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
private:
	int FLineNo;
	
public:
	__property int LineNo = {read=FLineNo, nodefault};
public:
	/* Exception.Create */ inline __fastcall EGLOBJFileError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLOBJFileError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EGLOBJFileError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLOBJFileError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLOBJFileError(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLOBJFileError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EGLOBJFileError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLOBJFileError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLOBJFileError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLOBJFileError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLOBJFileError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLOBJFileError(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLOBJFileError(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TGLMTLFile;
class PASCALIMPLEMENTATION TGLMTLFile : public System::Classes::TStringList
{
	typedef System::Classes::TStringList inherited;
	
public:
	void __fastcall Prepare(void);
	System::UnicodeString __fastcall MaterialStringProperty(const System::UnicodeString materialName, const System::UnicodeString propertyName);
	Vectortypes::TVector4f __fastcall MaterialVectorProperty(const System::UnicodeString materialName, const System::UnicodeString propertyName, const Vectortypes::TVector4f &defaultValue);
public:
	/* TStringList.Create */ inline __fastcall TGLMTLFile(void)/* overload */ : System::Classes::TStringList() { }
	/* TStringList.Create */ inline __fastcall TGLMTLFile(bool OwnsObjects)/* overload */ : System::Classes::TStringList(OwnsObjects) { }
	/* TStringList.Destroy */ inline __fastcall virtual ~TGLMTLFile(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word BufSize = System::Word(0x2800);
static const System::Int8 LineLen = System::Int8(0x64);
extern PACKAGE bool vGLFileOBJ_SplitMesh;
}	/* namespace Glfileobj */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILEOBJ)
using namespace Glfileobj;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlfileobjHPP
