// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS_CL_Platform.pas' rev: 24.00 (Win32)

#ifndef Gls_cl_platformHPP
#define Gls_cl_platformHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gls_cl_platform
{
//-- type declarations -------------------------------------------------------
typedef NativeUInt TSize_T;

typedef NativeUInt *Psize_t;

typedef NativeUInt intptr_t;

typedef System::Int8 Tcl_char;

typedef System::Byte Tcl_uchar;

typedef short Tcl_short;

typedef System::Word Tcl_ushort;

typedef int Tcl_int;

typedef unsigned Tcl_uint;

typedef __int64 Tcl_long;

typedef unsigned __int64 Tcl_ulong;

typedef System::Word Tcl_half;

typedef float Tcl_float;

typedef double Tcl_double;

typedef System::Int8 *Pcl_char;

typedef System::Byte *Pcl_uchar;

typedef short *Pcl_short;

typedef System::Word *Pcl_ushort;

typedef int *Pcl_int;

typedef unsigned *Pcl_uint;

typedef __int64 *Pcl_long;

typedef unsigned __int64 *Pcl_ulong;

typedef System::Word *Pcl_half;

typedef float *Pcl_float;

typedef double *Pcl_double;

typedef System::StaticArray<System::Int8, 2> Tcl_char2;

typedef System::StaticArray<System::Int8, 4> Tcl_char4;

typedef System::StaticArray<System::Int8, 8> Tcl_char8;

typedef System::StaticArray<System::Int8, 16> Tcl_char16;

typedef System::StaticArray<System::Byte, 2> Tcl_uchar2;

typedef System::StaticArray<System::Byte, 4> Tcl_uchar4;

typedef System::StaticArray<System::Byte, 8> Tcl_uchar8;

typedef System::StaticArray<System::Byte, 16> Tcl_uchar16;

typedef System::StaticArray<short, 2> Tcl_short2;

typedef System::StaticArray<short, 4> Tcl_short4;

typedef System::StaticArray<short, 8> Tcl_short8;

typedef System::StaticArray<short, 16> Tcl_short16;

typedef System::StaticArray<System::Word, 2> Tcl_ushort2;

typedef System::StaticArray<System::Word, 4> Tcl_ushort4;

typedef System::StaticArray<System::Word, 8> Tcl_ushort8;

typedef System::StaticArray<System::Word, 16> Tcl_ushort16;

typedef System::StaticArray<int, 2> Tcl_int2;

typedef System::StaticArray<int, 4> Tcl_int4;

typedef System::StaticArray<int, 8> Tcl_int8;

typedef System::StaticArray<int, 16> Tcl_int16;

typedef System::StaticArray<unsigned, 2> Tcl_uint2;

typedef System::StaticArray<unsigned, 4> Tcl_uint4;

typedef System::StaticArray<unsigned, 8> Tcl_uint8;

typedef System::StaticArray<unsigned, 16> Tcl_uint16;

typedef System::StaticArray<__int64, 2> Tcl_long2;

typedef System::StaticArray<__int64, 4> Tcl_long4;

typedef System::StaticArray<__int64, 8> Tcl_long8;

typedef System::StaticArray<__int64, 16> Tcl_long16;

typedef System::StaticArray<unsigned __int64, 2> Tcl_ulong2;

typedef System::StaticArray<unsigned __int64, 4> Tcl_ulong4;

typedef System::StaticArray<unsigned __int64, 8> Tcl_ulong8;

typedef System::StaticArray<unsigned __int64, 16> Tcl_ulong16;

typedef System::StaticArray<float, 2> Tcl_float2;

typedef System::StaticArray<float, 4> Tcl_float4;

typedef System::StaticArray<float, 8> Tcl_float8;

typedef System::StaticArray<float, 16> Tcl_float16;

typedef System::StaticArray<double, 2> Tcl_double2;

typedef System::StaticArray<double, 4> Tcl_double4;

typedef System::StaticArray<double, 8> Tcl_double8;

typedef System::StaticArray<double, 16> Tcl_double16;

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 CL_CHAR_BIT = System::Int8(0x8);
static const System::Int8 CL_SCHAR_MAX = System::Int8(0x7f);
static const System::Int8 CL_SCHAR_MIN = System::Int8(-128);
static const System::Int8 CL_CHAR_MAX = System::Int8(0x7f);
static const System::Int8 CL_CHAR_MIN = System::Int8(-128);
static const System::Byte CL_UCHAR_MAX = System::Byte(0xff);
static const System::Word CL_SHRT_MAX = System::Word(0x7fff);
static const short CL_SHRT_MIN = short(-32768);
static const System::Word CL_USHRT_MAX = System::Word(0xffff);
static const int CL_INT_MAX = int(0x7fffffff);
static const int CL_INT_MIN = int(-2147483648);
static const unsigned CL_UINT_MAX = unsigned(0xffffffff);
static const __int64 CL_LONG_MAX = 0x7fffffffffffffffLL;
static const __int64 CL_LONG_MIN = (-0x7fffffffffffffffLL-1);
static const unsigned __int64 CL_ULONG_MAX = 0xffffffffffffffffULL;
static const System::Int8 CL_FLT_DIG = System::Int8(0x6);
static const System::Int8 CL_FLT_MANT_DIG = System::Int8(0x18);
static const int CL_FLT_MAX_10_EXP = int(0x26);
static const int CL_FLT_MAX_EXP = int(0x80);
static const System::Int8 CL_FLT_MIN_10_EXP = System::Int8(-37);
static const System::Int8 CL_FLT_MIN_EXP = System::Int8(-125);
static const System::Int8 CL_FLT_RADIX = System::Int8(0x2);
static const System::Extended CL_FLT_MAX = 1.700000E+38;
static const System::Extended CL_FLT_MIN = 1.170000E-38;
static const System::Extended CL_FLT_EPSILON = 1.000000E-07;
static const System::Int8 CL_DBL_DIG = System::Int8(0xf);
static const System::Int8 CL_DBL_MANT_DIG = System::Int8(0x35);
static const int CL_DBL_MAX_10_EXP = int(0x134);
static const int CL_DBL_MAX_EXP = int(0x400);
static const short CL_DBL_MIN_10_EXP = short(-307);
static const short CL_DBL_MIN_EXP = short(-1021);
static const System::Int8 CL_DBL_RADIX = System::Int8(0x2);
static const System::Extended CL_DBL_MAX = 8.980000E+307;
static const System::Extended CL_DBL_MIN = 2.200000E-308;
static const System::Extended CL_DBL_EPSILON = 2.200000E-26;
}	/* namespace Gls_cl_platform */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_CL_PLATFORM)
using namespace Gls_cl_platform;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_cl_platformHPP
