// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'XOpenGL.pas' rev: 24.00 (Win32)

#ifndef XopenglHPP
#define XopenglHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Xopengl
{
//-- type declarations -------------------------------------------------------
enum TMapTexCoordMode : unsigned char { mtcmUndefined, mtcmNull, mtcmMain, mtcmDual, mtcmSecond, mtcmArbitrary };

class DELPHICLASS TGLMultitextureCoordinator;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLMultitextureCoordinator : public Glcontext::TAbstractMultitextureCoordinator
{
	typedef Glcontext::TAbstractMultitextureCoordinator inherited;
	
private:
	typedef System::DynamicArray<TMapTexCoordMode> _TGLMultitextureCoordinator__1;
	
	typedef System::DynamicArray<unsigned> _TGLMultitextureCoordinator__2;
	
	
private:
	TMapTexCoordMode FMapTexCoordMode;
	bool FSecondTextureUnitForbidden;
	int FUpdCount;
	TMapTexCoordMode FUpdNewMode;
	_TGLMultitextureCoordinator__1 FStateStack;
	_TGLMultitextureCoordinator__2 FComplexMapping;
	int FComplexMappingN;
	
public:
	void __stdcall (*TexCoord2f)(float s, float t);
	void __stdcall (*TexCoord2fv)(System::PSingle v);
	void __stdcall (*TexCoord3f)(float s, float t, float r);
	void __stdcall (*TexCoord3fv)(System::PSingle v);
	void __stdcall (*TexCoord4f)(float s, float t, float r, float q);
	void __stdcall (*TexCoord4fv)(System::PSingle v);
	void __stdcall (*TexGenf)(unsigned coord, unsigned pname, float param);
	void __stdcall (*TexGenfv)(unsigned coord, unsigned pname, System::PSingle params);
	void __stdcall (*TexGeni)(unsigned coord, unsigned pname, int param);
	void __stdcall (*TexGeniv)(unsigned coord, unsigned pname, Opengltokens::PGLint params);
	void __stdcall (*TexCoordPointer)(int size, unsigned atype, int stride, void * data);
	void __stdcall (*EnableClientState)(unsigned aarray);
	void __stdcall (*DisableClientState)(unsigned aarray);
	void __stdcall (*Enable)(unsigned cap);
	void __stdcall (*Disable)(unsigned cap);
	__fastcall virtual TGLMultitextureCoordinator(Glcontext::TGLContext* AOwner);
	void __fastcall MapTexCoordToNull(void);
	void __fastcall MapTexCoordToMain(void);
	void __fastcall MapTexCoordToSecond(void);
	void __fastcall MapTexCoordToDual(void);
	void __fastcall MapTexCoordToArbitrary(unsigned const *units, const int units_Size)/* overload */;
	void __fastcall MapTexCoordToArbitrary(const unsigned bitWiseUnits)/* overload */;
	void __fastcall MapTexCoordToArbitraryAdd(const unsigned bitWiseUnits);
	void __fastcall BeginUpdate(void);
	void __fastcall EndUpdate(void);
	void __fastcall PushState(void);
	void __fastcall PopState(void);
	void __fastcall ForbidSecondTextureUnit(void);
	void __fastcall AllowSecondTextureUnit(void);
	unsigned __fastcall GetBitWiseMapping(void);
	__property TMapTexCoordMode MapTexCoordMode = {read=FMapTexCoordMode, write=FMapTexCoordMode, nodefault};
	__property bool SecondTextureUnitForbidden = {read=FSecondTextureUnitForbidden, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGLMultitextureCoordinator(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TGLMultitextureCoordinator* __fastcall xgl(void);
}	/* namespace Xopengl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_XOPENGL)
using namespace Xopengl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// XopenglHPP
