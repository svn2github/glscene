// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'cgGL.pas' rev: 24.00 (Win32)

#ifndef CgglHPP
#define CgglHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <cg.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Cggl
{
//-- type declarations -------------------------------------------------------
typedef unsigned TCGGLenum;

typedef unsigned CGGLenum;

//-- var, const, procedure ---------------------------------------------------
#define CgGLlibrary L"cgGL.dll"
static const System::Int8 CG_GL_MATRIX_IDENTITY = System::Int8(0x0);
static const System::Int8 CG_GL_MATRIX_TRANSPOSE = System::Int8(0x1);
static const System::Int8 CG_GL_MATRIX_INVERSE = System::Int8(0x2);
static const System::Int8 CG_GL_MATRIX_INVERSE_TRANSPOSE = System::Int8(0x3);
static const System::Int8 CG_GL_MODELVIEW_MATRIX = System::Int8(0x4);
static const System::Int8 CG_GL_PROJECTION_MATRIX = System::Int8(0x5);
static const System::Int8 CG_GL_TEXTURE_MATRIX = System::Int8(0x6);
static const System::Int8 CG_GL_MODELVIEW_PROJECTION_MATRIX = System::Int8(0x7);
static const System::Int8 CG_GL_VERTEX = System::Int8(0x8);
static const System::Int8 CG_GL_FRAGMENT = System::Int8(0x9);
extern "C" int __cdecl cgGLIsProfileSupported(Cg::TCGprofile profile);
extern "C" void __cdecl cgGLEnableProfile(Cg::TCGprofile profile);
extern "C" void __cdecl cgGLDisableProfile(Cg::TCGprofile profile);
extern "C" Cg::TCGprofile __cdecl cgGLGetLatestProfile(unsigned profile_type);
extern "C" void __cdecl cgGLSetOptimalOptions(Cg::TCGprofile profile);
extern "C" void __cdecl cgGLLoadProgram(Cg::PCGprogram _program);
extern "C" int __cdecl cgGLIsProgramLoaded(Cg::PCGprogram _program);
extern "C" void __cdecl cgGLBindProgram(Cg::PCGprogram _program);
extern "C" void __cdecl cgGLUnbindProgram(Cg::TCGprofile profile);
extern "C" unsigned __cdecl cgGLGetProgramID(Cg::PCGprogram _program);
extern "C" void __cdecl cgGLSetParameter1f(Cg::PCGparameter param, float x);
extern "C" void __cdecl cgGLSetParameter2f(Cg::PCGparameter param, float x, float y);
extern "C" void __cdecl cgGLSetParameter3f(Cg::PCGparameter param, float x, float y, float z);
extern "C" void __cdecl cgGLSetParameter4f(Cg::PCGparameter param, float x, float y, float z, float w);
extern "C" void __cdecl cgGLSetParameter1fv(Cg::PCGparameter param, const Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLSetParameter2fv(Cg::PCGparameter param, const Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLSetParameter3fv(Cg::PCGparameter param, const Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLSetParameter4fv(Cg::PCGparameter param, const Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLSetParameter1d(Cg::PCGparameter param, double x);
extern "C" void __cdecl cgGLSetParameter2d(Cg::PCGparameter param, double x, double y);
extern "C" void __cdecl cgGLSetParameter3d(Cg::PCGparameter param, double x, double y, double z);
extern "C" void __cdecl cgGLSetParameter4d(Cg::PCGparameter param, double x, double y, double z, double w);
extern "C" void __cdecl cgGLSetParameter1dv(Cg::PCGparameter param, const System::PDouble v);
extern "C" void __cdecl cgGLSetParameter2dv(Cg::PCGparameter param, const System::PDouble v);
extern "C" void __cdecl cgGLSetParameter3dv(Cg::PCGparameter param, const System::PDouble v);
extern "C" void __cdecl cgGLSetParameter4dv(Cg::PCGparameter param, const System::PDouble v);
extern "C" void __cdecl cgGLGetParameter1f(Cg::PCGparameter param, Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLGetParameter2f(Cg::PCGparameter param, Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLGetParameter3f(Cg::PCGparameter param, Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLGetParameter4f(Cg::PCGparameter param, Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLGetParameter1d(Cg::PCGparameter param, System::PDouble v);
extern "C" void __cdecl cgGLGetParameter2d(Cg::PCGparameter param, System::PDouble v);
extern "C" void __cdecl cgGLGetParameter3d(Cg::PCGparameter param, System::PDouble v);
extern "C" void __cdecl cgGLGetParameter4d(Cg::PCGparameter param, System::PDouble v);
extern "C" void __cdecl cgGLSetParameterArray1f(Cg::PCGparameter param, int offset, int nelements, const Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLSetParameterArray2f(Cg::PCGparameter param, int offset, int nelements, const Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLSetParameterArray3f(Cg::PCGparameter param, int offset, int nelements, const Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLSetParameterArray4f(Cg::PCGparameter param, int offset, int nelements, const Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLSetParameterArray1d(Cg::PCGparameter param, int offset, int nelements, const System::PDouble v);
extern "C" void __cdecl cgGLSetParameterArray2d(Cg::PCGparameter param, int offset, int nelements, const System::PDouble v);
extern "C" void __cdecl cgGLSetParameterArray3d(Cg::PCGparameter param, int offset, int nelements, const System::PDouble v);
extern "C" void __cdecl cgGLSetParameterArray4d(Cg::PCGparameter param, int offset, int nelements, const System::PDouble v);
extern "C" void __cdecl cgGLGetParameterArray1f(Cg::PCGparameter param, int offset, int nelements, Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLGetParameterArray2f(Cg::PCGparameter param, int offset, int nelements, Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLGetParameterArray3f(Cg::PCGparameter param, int offset, int nelements, Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLGetParameterArray4f(Cg::PCGparameter param, int offset, int nelements, Winapi::Windows::PSingle v);
extern "C" void __cdecl cgGLGetParameterArray1d(Cg::PCGparameter param, int offset, int nelements, System::PDouble v);
extern "C" void __cdecl cgGLGetParameterArray2d(Cg::PCGparameter param, int offset, int nelements, System::PDouble v);
extern "C" void __cdecl cgGLGetParameterArray3d(Cg::PCGparameter param, int offset, int nelements, System::PDouble v);
extern "C" void __cdecl cgGLGetParameterArray4d(Cg::PCGparameter param, int offset, int nelements, System::PDouble v);
extern "C" void __cdecl cgGLSetParameterPointer(Cg::PCGparameter param, int fsize, unsigned _type, int stride, const void * _pointer);
extern "C" void __cdecl cgGLEnableClientState(Cg::PCGparameter param);
extern "C" void __cdecl cgGLDisableClientState(Cg::PCGparameter param);
extern "C" void __cdecl cgGLSetMatrixParameterdr(Cg::PCGparameter param, const System::PDouble matrix);
extern "C" void __cdecl cgGLSetMatrixParameterfr(Cg::PCGparameter param, const Winapi::Windows::PSingle matrix);
extern "C" void __cdecl cgGLSetMatrixParameterdc(Cg::PCGparameter param, const System::PDouble matrix);
extern "C" void __cdecl cgGLSetMatrixParameterfc(Cg::PCGparameter param, const Winapi::Windows::PSingle matrix);
extern "C" void __cdecl cgGLGetMatrixParameterdr(Cg::PCGparameter param, System::PDouble matrix);
extern "C" void __cdecl cgGLGetMatrixParameterfr(Cg::PCGparameter param, Winapi::Windows::PSingle matrix);
extern "C" void __cdecl cgGLGetMatrixParameterdc(Cg::PCGparameter param, System::PDouble matrix);
extern "C" void __cdecl cgGLGetMatrixParameterfc(Cg::PCGparameter param, Winapi::Windows::PSingle matrix);
extern "C" void __cdecl cgGLSetStateMatrixParameter(Cg::PCGparameter param, unsigned matrix, unsigned transform);
extern "C" void __cdecl cgGLSetMatrixParameterArrayfc(Cg::PCGparameter param, int offset, int nelements, const Winapi::Windows::PSingle matrices);
extern "C" void __cdecl cgGLSetMatrixParameterArrayfr(Cg::PCGparameter param, int offset, int nelements, const Winapi::Windows::PSingle matrices);
extern "C" void __cdecl cgGLSetMatrixParameterArraydc(Cg::PCGparameter param, int offset, int nelements, const System::PDouble matrices);
extern "C" void __cdecl cgGLSetMatrixParameterArraydr(Cg::PCGparameter param, int offset, int nelements, const System::PDouble matrices);
extern "C" void __cdecl cgGLGetMatrixParameterArrayfc(Cg::PCGparameter param, int offset, int nelements, Winapi::Windows::PSingle matrices);
extern "C" void __cdecl cgGLGetMatrixParameterArrayfr(Cg::PCGparameter param, int offset, int nelements, Winapi::Windows::PSingle matrices);
extern "C" void __cdecl cgGLGetMatrixParameterArraydc(Cg::PCGparameter param, int offset, int nelements, System::PDouble matrices);
extern "C" void __cdecl cgGLGetMatrixParameterArraydr(Cg::PCGparameter param, int offset, int nelements, System::PDouble matrices);
extern "C" void __cdecl cgGLSetTextureParameter(Cg::PCGparameter param, unsigned texobj);
extern "C" unsigned __cdecl cgGLGetTextureParameter(Cg::PCGparameter param);
extern "C" void __cdecl cgGLEnableTextureParameter(Cg::PCGparameter param);
extern "C" void __cdecl cgGLDisableTextureParameter(Cg::PCGparameter param);
extern "C" unsigned __cdecl cgGLGetTextureEnum(Cg::PCGparameter param);
extern "C" void __cdecl cgGLSetManageTextureParameters(Cg::PCGcontext ctx, int flag);
extern "C" int __cdecl cgGLGetManageTextureParameters(Cg::PCGcontext ctx);
}	/* namespace Cggl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CGGL)
using namespace Cggl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CgglHPP
