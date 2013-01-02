// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS_CL_GL.pas' rev: 24.00 (Win32)

#ifndef Gls_cl_glHPP
#define Gls_cl_glHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <GLS_CL.hpp>	// Pascal unit
#include <GLS_CL_Platform.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gls_cl_gl
{
//-- type declarations -------------------------------------------------------
typedef unsigned Tcl_gl_object_type;

typedef unsigned *Pcl_gl_object_type;

typedef unsigned Tcl_gl_texture_info;

typedef unsigned *Pcl_gl_texture_info;

typedef unsigned Tcl_gl_platform_info;

typedef unsigned *Pcl_gl_platform_info;

//-- var, const, procedure ---------------------------------------------------
static const System::Word CL_GL_OBJECT_BUFFER = System::Word(0x2000);
static const System::Word CL_GL_OBJECT_TEXTURE2D = System::Word(0x2001);
static const System::Word CL_GL_OBJECT_TEXTURE3D = System::Word(0x2002);
static const System::Word CL_GL_OBJECT_RENDERBUFFER = System::Word(0x2003);
static const System::Word CL_GL_TEXTURE_TARGET = System::Word(0x2004);
static const System::Word CL_GL_MIPMAP_LEVEL = System::Word(0x2005);
extern "C" Gls_cl::Tcl_mem __stdcall clCreateFromGLBuffer(Gls_cl::Tcl_context context, unsigned __int64 flags, unsigned bufobj, Gls_cl_platform::Pcl_int errcode_ret);
extern "C" Gls_cl::Tcl_mem __stdcall clCreateFromGLTexture2D(Gls_cl::Tcl_context context, unsigned __int64 flags, unsigned target, int miplevel, unsigned texture, Gls_cl_platform::Pcl_int errcode_ret);
extern "C" Gls_cl::Tcl_mem __stdcall clCreateFromGLTexture3D(Gls_cl::Tcl_context context, unsigned __int64 flags, unsigned target, int miplevel, unsigned texture, Gls_cl_platform::Pcl_int errcode_ret);
extern "C" Gls_cl::Tcl_mem __stdcall clCreateFromGLRenderbuffer(Gls_cl::Tcl_context context, unsigned __int64 flags, unsigned renderbuffer, Gls_cl_platform::Pcl_int errcode_ret);
extern "C" int __stdcall clGetGLObjectInfo(Gls_cl::Tcl_mem memobj, Pcl_gl_object_type gl_object_type, Opengltokens::PGLuint gl_object_name);
extern "C" int __stdcall clGetGLTextureInfo(Gls_cl::Tcl_mem memobj, unsigned param_name, NativeUInt param_value_size, void * param_value, Gls_cl_platform::Psize_t param_value_size_ret);
extern "C" int __stdcall clEnqueueAcquireGLObjects(Gls_cl::Tcl_command_queue command_queue, unsigned num_objects, Gls_cl::Pcl_mem mem_objects, unsigned num_events_in_wait_list, Gls_cl::Pcl_event event_wait_list, Gls_cl::Pcl_event event);
extern "C" int __stdcall clEnqueueReleaseGLObjects(Gls_cl::Tcl_command_queue command_queue, unsigned num_objects, Gls_cl::Pcl_mem mem_objects, unsigned num_events_in_wait_list, Gls_cl::Pcl_event event_wait_list, Gls_cl::Pcl_event event);
}	/* namespace Gls_cl_gl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_CL_GL)
using namespace Gls_cl_gl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_cl_glHPP
