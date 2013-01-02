// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSZLibExApi.pas' rev: 24.00 (Win32)

#ifndef GlszlibexapiHPP
#define GlszlibexapiHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glszlibexapi
{
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<System::UnicodeString, 10> Glszlibexapi__1;

typedef void * __fastcall (*TZAlloc)(void * opaque, int items, int size);

typedef void __fastcall (*TZFree)(void * opaque, void * block);

#pragma pack(push,1)
struct DECLSPEC_DRECORD TZStreamRec
{
public:
	void *next_in;
	int avail_in;
	int total_in;
	void *next_out;
	int avail_out;
	int total_out;
	void *msg;
	void *state;
	TZAlloc zalloc;
	TZFree zfree;
	void *opaque;
	int data_type;
	int adler;
	int reserved;
};
#pragma pack(pop)


//-- var, const, procedure ---------------------------------------------------
#define ZLIB_VERSION L"1.2.5"
static const System::Word ZLIB_VERNUM = System::Word(0x1250);
static const System::Int8 ZLIB_VER_MAJOR = System::Int8(0x1);
static const System::Int8 ZLIB_VER_MINOR = System::Int8(0x2);
static const System::Int8 ZLIB_VER_REVISION = System::Int8(0x5);
static const System::Int8 ZLIB_VER_SUBREVISION = System::Int8(0x0);
static const System::Int8 Z_DEFLATED = System::Int8(0x8);
static const System::Int8 Z_INFO_FLAG_SIZE = System::Int8(0x1);
static const System::Int8 Z_INFO_FLAG_CRC = System::Int8(0x2);
static const System::Int8 Z_INFO_FLAG_ADLER = System::Int8(0x4);
static const System::Int8 Z_INFO_NONE = System::Int8(0x0);
static const System::Int8 Z_INFO_DEFAULT = System::Int8(0x3);
static const System::Int8 Z_NO_FLUSH = System::Int8(0x0);
static const System::Int8 Z_PARTIAL_FLUSH = System::Int8(0x1);
static const System::Int8 Z_SYNC_FLUSH = System::Int8(0x2);
static const System::Int8 Z_FULL_FLUSH = System::Int8(0x3);
static const System::Int8 Z_FINISH = System::Int8(0x4);
static const System::Int8 Z_BLOCK = System::Int8(0x5);
static const System::Int8 Z_TREES = System::Int8(0x6);
static const System::Int8 Z_OK = System::Int8(0x0);
static const System::Int8 Z_STREAM_END = System::Int8(0x1);
static const System::Int8 Z_NEED_DICT = System::Int8(0x2);
static const System::Int8 Z_ERRNO = System::Int8(-1);
static const System::Int8 Z_STREAM_ERROR = System::Int8(-2);
static const System::Int8 Z_DATA_ERROR = System::Int8(-3);
static const System::Int8 Z_MEM_ERROR = System::Int8(-4);
static const System::Int8 Z_BUF_ERROR = System::Int8(-5);
static const System::Int8 Z_VERSION_ERROR = System::Int8(-6);
static const System::Int8 Z_NO_COMPRESSION = System::Int8(0x0);
static const System::Int8 Z_BEST_SPEED = System::Int8(0x1);
static const System::Int8 Z_BEST_COMPRESSION = System::Int8(0x9);
static const System::Int8 Z_DEFAULT_COMPRESSION = System::Int8(-1);
static const System::Int8 Z_FILTERED = System::Int8(0x1);
static const System::Int8 Z_HUFFMAN_ONLY = System::Int8(0x2);
static const System::Int8 Z_RLE = System::Int8(0x3);
static const System::Int8 Z_FIXED = System::Int8(0x4);
static const System::Int8 Z_DEFAULT_STRATEGY = System::Int8(0x0);
static const System::Int8 Z_BINARY = System::Int8(0x0);
static const System::Int8 Z_ASCII = System::Int8(0x1);
static const System::Int8 Z_TEXT = System::Int8(0x1);
static const System::Int8 Z_UNKNOWN = System::Int8(0x2);
extern PACKAGE Glszlibexapi__1 _z_errmsg;
extern PACKAGE int __fastcall deflateInit(TZStreamRec &strm, int level);
extern PACKAGE int __fastcall deflateInit2(TZStreamRec &strm, int level, int method, int windowBits, int memLevel, int strategy);
extern PACKAGE int __fastcall inflateInit(TZStreamRec &strm);
extern PACKAGE int __fastcall inflateInit2(TZStreamRec &strm, int windowBits);
extern PACKAGE int __fastcall deflateInit_(TZStreamRec &strm, int level, char * version, int recsize);
extern PACKAGE int __fastcall deflateInit2_(TZStreamRec &strm, int level, int method, int windowBits, int memLevel, int strategy, char * version, int recsize);
extern PACKAGE int __fastcall deflateReset(TZStreamRec &strm);
extern PACKAGE int __fastcall deflate(TZStreamRec &strm, int flush);
extern PACKAGE int __fastcall deflateEnd(TZStreamRec &strm);
extern PACKAGE int __fastcall inflateReset(TZStreamRec &strm);
extern PACKAGE int __fastcall inflateInit2_(TZStreamRec &strm, int windowBits, char * version, int recsize);
extern PACKAGE int __fastcall inflateInit_(TZStreamRec &strm, char * version, int recsize);
extern PACKAGE int __fastcall inflate(TZStreamRec &strm, int flush);
extern PACKAGE int __fastcall inflateEnd(TZStreamRec &strm);
extern PACKAGE int __fastcall adler32(int adler, const void *buf, int len);
extern PACKAGE int __fastcall crc32(int crc, const void *buf, int len);
}	/* namespace Glszlibexapi */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSZLIBEXAPI)
using namespace Glszlibexapi;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlszlibexapiHPP
