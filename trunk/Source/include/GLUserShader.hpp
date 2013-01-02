// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLUserShader.pas' rev: 24.00 (Win32)

#ifndef GlusershaderHPP
#define GlusershaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glusershader
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TOnDoApplyEvent)(System::TObject* Sender, Glrendercontextinfo::TRenderContextInfo &rci);

typedef void __fastcall (__closure *TOnDoUnApplyEvent)(System::TObject* Sender, int Pass, Glrendercontextinfo::TRenderContextInfo &rci, bool &Continue);

class DELPHICLASS TGLUserShader;
class PASCALIMPLEMENTATION TGLUserShader : public Glmaterial::TGLShader
{
	typedef Glmaterial::TGLShader inherited;
	
private:
	int FPass;
	TOnDoApplyEvent FOnDoApply;
	TOnDoUnApplyEvent FOnDoUnApply;
	
protected:
	virtual void __fastcall DoApply(Glrendercontextinfo::TRenderContextInfo &rci, System::TObject* Sender);
	virtual bool __fastcall DoUnApply(Glrendercontextinfo::TRenderContextInfo &rci);
	
__published:
	__property TOnDoApplyEvent OnDoApply = {read=FOnDoApply, write=FOnDoApply};
	__property TOnDoUnApplyEvent OnDoUnApply = {read=FOnDoUnApply, write=FOnDoUnApply};
	__property ShaderStyle = {default=1};
public:
	/* TGLShader.Create */ inline __fastcall virtual TGLUserShader(System::Classes::TComponent* AOwner) : Glmaterial::TGLShader(AOwner) { }
	/* TGLShader.Destroy */ inline __fastcall virtual ~TGLUserShader(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glusershader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLUSERSHADER)
using namespace Glusershader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlusershaderHPP
