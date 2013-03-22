// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSCUDACompiler.pas' rev: 24.00 (Win32)

#ifndef GlscudacompilerHPP
#define GlscudacompilerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <GLSCUDAParser.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glscudacompiler
{
//-- type declarations -------------------------------------------------------
enum TGLSCUDACompilerOutput : unsigned int { codeUndefined, codePtx, codeCubin, codeGpu };

enum TGLSCUDAVirtArch : unsigned int { compute_10, compute_11, compute_12, compute_13, compute_20 };

enum TGLSCUDARealArch : unsigned int { sm_10, sm_11, sm_12, sm_13, sm_20, sm_21 };

typedef System::Set<TGLSCUDARealArch, TGLSCUDARealArch::sm_10, TGLSCUDARealArch::sm_21>  TGLSCUDARealArchs;

class DELPHICLASS TGLSCUDACompiler;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLSCUDACompiler : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString FNVCCPath;
	System::UnicodeString FCppCompilerPath;
	System::Classes::TStringList* FProduct;
	System::UnicodeString FProjectModule;
	System::UnicodeString FSourceCodeFile;
	System::UnicodeString FConsoleContent;
	TGLSCUDACompilerOutput FOutputCodeType;
	TGLSCUDAVirtArch FVirtualArch;
	TGLSCUDARealArchs FRealArch;
	int FMaxRegisterCount;
	Glscudaparser::TCUDAModuleInfo* FModuleInfo;
	void __fastcall SetMaxRegisterCount(int Value);
	void __fastcall SetOutputCodeType(const TGLSCUDACompilerOutput Value);
	bool __fastcall StoreProjectModule(void);
	void __fastcall SetRealArch(TGLSCUDARealArchs AValue);
	void __fastcall SetNVCCPath(const System::UnicodeString AValue);
	void __fastcall SetCppCompilerPath(const System::UnicodeString AValue);
	
protected:
	virtual void __fastcall Loaded(void);
	
public:
	__fastcall virtual TGLSCUDACompiler(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSCUDACompiler(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall SetSourceCodeFile(const System::UnicodeString AFileName);
	bool __fastcall Compile(void);
	__property System::Classes::TStringList* Product = {read=FProduct, write=FProduct};
	__property Glscudaparser::TCUDAModuleInfo* ModuleInfo = {read=FModuleInfo};
	__property System::UnicodeString ConsoleContent = {read=FConsoleContent};
	
__published:
	__property System::UnicodeString NVCCPath = {read=FNVCCPath, write=SetNVCCPath};
	__property System::UnicodeString CppCompilerPath = {read=FCppCompilerPath, write=SetCppCompilerPath};
	__property System::UnicodeString SourceCodeFile = {read=FSourceCodeFile};
	__property System::UnicodeString ProjectModule = {read=FProjectModule, write=FProjectModule, stored=StoreProjectModule};
	__property TGLSCUDACompilerOutput OutputCodeType = {read=FOutputCodeType, write=SetOutputCodeType, default=1};
	__property TGLSCUDARealArchs RealArchitecture = {read=FRealArch, write=SetRealArch, default=8};
	__property TGLSCUDAVirtArch VirtualArchitecture = {read=FVirtualArch, write=FVirtualArch, default=3};
	__property int MaxRegisterCount = {read=FMaxRegisterCount, write=SetMaxRegisterCount, default=32};
};

#pragma pack(pop)

typedef bool __fastcall (*TFindCuFileFunc)(System::UnicodeString &AModuleName);

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TFindCuFileFunc vFindCuFileFunc;
}	/* namespace Glscudacompiler */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCUDACOMPILER)
using namespace Glscudacompiler;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlscudacompilerHPP
