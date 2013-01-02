// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSZLibEx.pas' rev: 24.00 (Win32)

#ifndef GlszlibexHPP
#define GlszlibexHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLSZLibExApi.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glszlibex
{
//-- type declarations -------------------------------------------------------
typedef System::WideChar UnicodeChar;

typedef __int64 TStreamPos;

enum TZCompressionLevel : unsigned char { zcNone, zcFastest, zcDefault, zcMax, zcLevel1, zcLevel2, zcLevel3, zcLevel4, zcLevel5, zcLevel6, zcLevel7, zcLevel8, zcLevel9 };

enum TZStrategy : unsigned char { zsDefault, zsFiltered, zsHuffman, zsRLE, zsFixed };

enum TZError : unsigned char { zeError, zeStreamError, zeDataError, zeMemoryError, zeBufferError, zeVersionError };

enum TZFlush : unsigned char { zfNoFlush, zfPartialFlush, zfSyncFlush, zfFullFlush, zfFinish, zfBlock, zfTrees };

typedef int __fastcall (*TZReadFunction)(void * param, void *buffer, int size);

typedef int __fastcall (*TZWriteFunction)(void * param, const void *buffer, int size);

#pragma pack(push,1)
struct DECLSPEC_DRECORD TZInformation
{
public:
	int CompressedFlags;
	__int64 CompressedSize;
	int CompressedCrc;
	int CompressedAdler;
	int UncompressedFlags;
	__int64 UncompressedSize;
	int UncompressedCrc;
	int UncompressedAdler;
};
#pragma pack(pop)


class DELPHICLASS TCustomZStream;
class PASCALIMPLEMENTATION TCustomZStream : public System::Classes::TStream
{
	typedef System::Classes::TStream inherited;
	
private:
	System::Classes::TStream* FStream;
	__int64 FStreamPos;
	System::Classes::TNotifyEvent FOnProgress;
	Glszlibexapi::TZStreamRec FZStream;
	System::StaticArray<System::Byte, 65536> FBuffer;
	__int64 __fastcall GetStreamPosition(void);
	void __fastcall SetStreamPosition(__int64 value);
	
protected:
	int __fastcall StreamRead(void *buffer, int count);
	int __fastcall StreamWrite(const void *buffer, int count);
	int __fastcall StreamSeek(int offset, System::Word origin);
	void __fastcall StreamReadBuffer(void *buffer, int count);
	void __fastcall StreamWriteBuffer(const void *buffer, int count);
	DYNAMIC void __fastcall DoProgress(void);
	__property __int64 StreamPosition = {read=GetStreamPosition, write=SetStreamPosition};
	__property System::Classes::TNotifyEvent OnProgress = {read=FOnProgress, write=FOnProgress};
	
public:
	__fastcall TCustomZStream(System::Classes::TStream* stream);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TCustomZStream(void) { }
	
};


class DELPHICLASS TZCompressionStream;
class PASCALIMPLEMENTATION TZCompressionStream : public TCustomZStream
{
	typedef TCustomZStream inherited;
	
private:
	float __fastcall GetCompressionRate(void);
	
public:
	__fastcall TZCompressionStream(System::Classes::TStream* dest, TZCompressionLevel compressionLevel)/* overload */;
	__fastcall TZCompressionStream(System::Classes::TStream* dest, TZCompressionLevel compressionLevel, int windowBits, int memLevel, TZStrategy strategy)/* overload */;
	__fastcall virtual ~TZCompressionStream(void);
	virtual int __fastcall Read(void *buffer, int count)/* overload */;
	virtual int __fastcall Write(const void *buffer, int count)/* overload */;
	virtual int __fastcall Seek(int offset, System::Word origin)/* overload */;
	__property float CompressionRate = {read=GetCompressionRate};
	__property OnProgress;
/* Hoisted overloads: */
	
public:
	inline int __fastcall  Read(System::DynamicArray<System::Byte> Buffer, int Offset, int Count){ return System::Classes::TStream::Read(Buffer, Offset, Count); }
	inline int __fastcall  Read(System::DynamicArray<System::Byte> &Buffer, int Count){ return System::Classes::TStream::Read(Buffer, Count); }
	inline int __fastcall  Write(const System::DynamicArray<System::Byte> Buffer, int Offset, int Count){ return System::Classes::TStream::Write(Buffer, Offset, Count); }
	inline int __fastcall  Write(const System::DynamicArray<System::Byte> Buffer, int Count){ return System::Classes::TStream::Write(Buffer, Count); }
	inline __int64 __fastcall  Seek(const __int64 Offset, System::Classes::TSeekOrigin Origin){ return System::Classes::TStream::Seek(Offset, Origin); }
	
};


class DELPHICLASS TZDecompressionStream;
class PASCALIMPLEMENTATION TZDecompressionStream : public TCustomZStream
{
	typedef TCustomZStream inherited;
	
public:
	__fastcall TZDecompressionStream(System::Classes::TStream* source)/* overload */;
	__fastcall TZDecompressionStream(System::Classes::TStream* source, int windowBits)/* overload */;
	__fastcall virtual ~TZDecompressionStream(void);
	virtual int __fastcall Read(void *buffer, int count)/* overload */;
	virtual int __fastcall Write(const void *buffer, int count)/* overload */;
	virtual int __fastcall Seek(int offset, System::Word origin)/* overload */;
	__property OnProgress;
/* Hoisted overloads: */
	
public:
	inline int __fastcall  Read(System::DynamicArray<System::Byte> Buffer, int Offset, int Count){ return System::Classes::TStream::Read(Buffer, Offset, Count); }
	inline int __fastcall  Read(System::DynamicArray<System::Byte> &Buffer, int Count){ return System::Classes::TStream::Read(Buffer, Count); }
	inline int __fastcall  Write(const System::DynamicArray<System::Byte> Buffer, int Offset, int Count){ return System::Classes::TStream::Write(Buffer, Offset, Count); }
	inline int __fastcall  Write(const System::DynamicArray<System::Byte> Buffer, int Count){ return System::Classes::TStream::Write(Buffer, Count); }
	inline __int64 __fastcall  Seek(const __int64 Offset, System::Classes::TSeekOrigin Origin){ return System::Classes::TStream::Seek(Offset, Origin); }
	
};


class DELPHICLASS TZCustomBuffer;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TZCustomBuffer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	void *FBuffer;
	int FBufferCapacity;
	int FBufferSize;
	
protected:
	Glszlibexapi::TZStreamRec FZStream;
	void __fastcall BufferWrite(const void * buffer, int size);
	void __fastcall BufferRead(void * &buffer, int size);
	void __fastcall BufferCapacity(int capacity);
	__property int BufferSize = {read=FBufferSize, nodefault};
	
public:
	__fastcall TZCustomBuffer(void);
	__fastcall virtual ~TZCustomBuffer(void);
	virtual void __fastcall Clear(void);
	virtual void __fastcall Flush(TZFlush flush);
	virtual int __fastcall Write(const void * buffer, int size) = 0 /* overload */;
	int __fastcall Write(const System::AnsiString s)/* overload */;
	int __fastcall Read(void * &buffer, int size)/* overload */;
	int __fastcall Read(System::AnsiString &s)/* overload */;
};

#pragma pack(pop)

class DELPHICLASS TZCompressionBuffer;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TZCompressionBuffer : public TZCustomBuffer
{
	typedef TZCustomBuffer inherited;
	
public:
	__fastcall TZCompressionBuffer(TZCompressionLevel level)/* overload */;
	__fastcall TZCompressionBuffer(TZCompressionLevel level, int windowBits, int memLevel, TZStrategy strategy)/* overload */;
	__fastcall virtual ~TZCompressionBuffer(void);
	virtual void __fastcall Clear(void);
	virtual void __fastcall Flush(TZFlush flush);
	virtual int __fastcall Write(const void * buffer, int size)/* overload */;
/* Hoisted overloads: */
	
public:
	inline int __fastcall  Write(const System::AnsiString s){ return TZCustomBuffer::Write(s); }
	
};

#pragma pack(pop)

class DELPHICLASS TZDecompressionBuffer;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TZDecompressionBuffer : public TZCustomBuffer
{
	typedef TZCustomBuffer inherited;
	
public:
	__fastcall TZDecompressionBuffer(void)/* overload */;
	__fastcall TZDecompressionBuffer(int windowBits)/* overload */;
	__fastcall virtual ~TZDecompressionBuffer(void);
	virtual void __fastcall Clear(void);
	virtual int __fastcall Write(const void * buffer, int size)/* overload */;
/* Hoisted overloads: */
	
public:
	inline int __fastcall  Write(const System::AnsiString s){ return TZCustomBuffer::Write(s); }
	
};

#pragma pack(pop)

typedef System::TMetaClass* EZLibErrorClass;

class DELPHICLASS EZLibError;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EZLibError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
private:
	int FErrorCode;
	
public:
	__fastcall EZLibError(int code, const System::UnicodeString dummy)/* overload */;
	__fastcall EZLibError(TZError error, const System::UnicodeString dummy)/* overload */;
	__property int ErrorCode = {read=FErrorCode, write=FErrorCode, nodefault};
public:
	/* Exception.CreateFmt */ inline __fastcall EZLibError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EZLibError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EZLibError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EZLibError(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EZLibError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EZLibError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EZLibError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EZLibError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EZLibError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EZLibError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EZLibError(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EZLibError(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS EZCompressionError;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EZCompressionError : public EZLibError
{
	typedef EZLibError inherited;
	
public:
	/* EZLibError.Create */ inline __fastcall EZCompressionError(int code, const System::UnicodeString dummy)/* overload */ : EZLibError(code, dummy) { }
	/* EZLibError.Create */ inline __fastcall EZCompressionError(TZError error, const System::UnicodeString dummy)/* overload */ : EZLibError(error, dummy) { }
	
public:
	/* Exception.CreateFmt */ inline __fastcall EZCompressionError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : EZLibError(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EZCompressionError(NativeUInt Ident)/* overload */ : EZLibError(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EZCompressionError(System::PResStringRec ResStringRec)/* overload */ : EZLibError(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EZCompressionError(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : EZLibError(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EZCompressionError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : EZLibError(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EZCompressionError(const System::UnicodeString Msg, int AHelpContext) : EZLibError(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EZCompressionError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : EZLibError(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EZCompressionError(NativeUInt Ident, int AHelpContext)/* overload */ : EZLibError(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EZCompressionError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EZLibError(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EZCompressionError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : EZLibError(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EZCompressionError(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : EZLibError(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EZCompressionError(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS EZDecompressionError;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EZDecompressionError : public EZLibError
{
	typedef EZLibError inherited;
	
public:
	/* EZLibError.Create */ inline __fastcall EZDecompressionError(int code, const System::UnicodeString dummy)/* overload */ : EZLibError(code, dummy) { }
	/* EZLibError.Create */ inline __fastcall EZDecompressionError(TZError error, const System::UnicodeString dummy)/* overload */ : EZLibError(error, dummy) { }
	
public:
	/* Exception.CreateFmt */ inline __fastcall EZDecompressionError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : EZLibError(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EZDecompressionError(NativeUInt Ident)/* overload */ : EZLibError(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EZDecompressionError(System::PResStringRec ResStringRec)/* overload */ : EZLibError(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EZDecompressionError(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : EZLibError(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EZDecompressionError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : EZLibError(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EZDecompressionError(const System::UnicodeString Msg, int AHelpContext) : EZLibError(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EZDecompressionError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : EZLibError(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EZDecompressionError(NativeUInt Ident, int AHelpContext)/* overload */ : EZLibError(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EZDecompressionError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EZLibError(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EZDecompressionError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : EZLibError(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EZDecompressionError(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : EZLibError(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EZDecompressionError(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::StaticArray<int, 13> ZLevels;
extern PACKAGE System::StaticArray<int, 5> ZStrategies;
extern PACKAGE System::StaticArray<int, 6> ZErrors;
extern PACKAGE System::StaticArray<int, 7> ZFlushes;
extern PACKAGE int __fastcall ZDeflateInit(Glszlibexapi::TZStreamRec &stream, TZCompressionLevel level);
extern PACKAGE int __fastcall ZDeflateInit2(Glszlibexapi::TZStreamRec &stream, TZCompressionLevel level, int windowBits, int memLevel, TZStrategy strategy);
extern PACKAGE int __fastcall ZDeflate(Glszlibexapi::TZStreamRec &stream, TZFlush flush);
extern PACKAGE int __fastcall ZDeflateEnd(Glszlibexapi::TZStreamRec &stream);
extern PACKAGE int __fastcall ZDeflateReset(Glszlibexapi::TZStreamRec &stream);
extern PACKAGE int __fastcall ZInflateInit(Glszlibexapi::TZStreamRec &stream);
extern PACKAGE int __fastcall ZInflateInit2(Glszlibexapi::TZStreamRec &stream, int windowBits);
extern PACKAGE int __fastcall ZInflate(Glszlibexapi::TZStreamRec &stream, TZFlush flush);
extern PACKAGE int __fastcall ZInflateEnd(Glszlibexapi::TZStreamRec &stream);
extern PACKAGE int __fastcall ZInflateReset(Glszlibexapi::TZStreamRec &stream);
extern PACKAGE int __fastcall ZAdler32(int adler, const void *buffer, int size);
extern PACKAGE int __fastcall ZCrc32(int crc, const void *buffer, int size);
extern PACKAGE void __fastcall ZDeflateEx(Glszlibexapi::TZStreamRec &stream, void * param, TZReadFunction read, TZWriteFunction write, TZFlush flush);
extern PACKAGE void __fastcall ZInflateEx(Glszlibexapi::TZStreamRec &stream, void * param, TZReadFunction read, TZWriteFunction write, TZFlush flush);
extern PACKAGE void __fastcall ZCompress(const void * inBuffer, int inSize, /* out */ void * &outBuffer, /* out */ int &outSize, TZCompressionLevel level = (TZCompressionLevel)(0x2));
extern PACKAGE void __fastcall ZCompress2(const void * inBuffer, int inSize, /* out */ void * &outBuffer, /* out */ int &outSize, TZCompressionLevel level, int windowBits, int memLevel, TZStrategy strategy);
extern PACKAGE void __fastcall ZDecompress(const void * inBuffer, int inSize, /* out */ void * &outBuffer, /* out */ int &outSize, int outEstimate = 0x0);
extern PACKAGE void __fastcall ZDecompress2(const void * inBuffer, int inSize, /* out */ void * &outBuffer, /* out */ int &outSize, int windowBits, int outEstimate = 0x0);
extern PACKAGE System::RawByteString __fastcall ZCompressStr(const System::AnsiString s, TZCompressionLevel level = (TZCompressionLevel)(0x2));
extern PACKAGE void __fastcall ZCompressString(System::RawByteString &result, const System::AnsiString s, TZCompressionLevel level = (TZCompressionLevel)(0x2))/* overload */;
extern PACKAGE void __fastcall ZCompressString(System::RawByteString &result, const System::UnicodeString s, TZCompressionLevel level = (TZCompressionLevel)(0x2))/* overload */;
extern PACKAGE System::RawByteString __fastcall ZCompressStrEx(const System::AnsiString s, TZCompressionLevel level = (TZCompressionLevel)(0x2));
extern PACKAGE void __fastcall ZCompressStringEx(System::RawByteString &result, const System::AnsiString s, TZCompressionLevel level = (TZCompressionLevel)(0x2))/* overload */;
extern PACKAGE void __fastcall ZCompressStringEx(System::RawByteString &result, const System::UnicodeString s, TZCompressionLevel level = (TZCompressionLevel)(0x2))/* overload */;
extern PACKAGE System::RawByteString __fastcall ZCompressStr2(const System::AnsiString s, TZCompressionLevel level, int windowBits, int memLevel, TZStrategy strategy);
extern PACKAGE void __fastcall ZCompressString2(System::RawByteString &result, const System::AnsiString s, TZCompressionLevel level, int windowBits, int memLevel, TZStrategy strategy)/* overload */;
extern PACKAGE void __fastcall ZCompressString2(System::RawByteString &result, const System::UnicodeString s, TZCompressionLevel level, int windowBits, int memLevel, TZStrategy strategy)/* overload */;
extern PACKAGE System::RawByteString __fastcall ZCompressStrWeb(const System::AnsiString s);
extern PACKAGE void __fastcall ZCompressStringWeb(System::RawByteString &result, const System::AnsiString s)/* overload */;
extern PACKAGE void __fastcall ZCompressStringWeb(System::RawByteString &result, const System::UnicodeString s)/* overload */;
extern PACKAGE System::AnsiString __fastcall ZDecompressStr(const System::RawByteString s);
extern PACKAGE void __fastcall ZDecompressString(System::AnsiString &result, const System::RawByteString s)/* overload */;
extern PACKAGE void __fastcall ZDecompressString(System::UnicodeString &result, const System::RawByteString s)/* overload */;
extern PACKAGE System::AnsiString __fastcall ZDecompressStrEx(const System::RawByteString s);
extern PACKAGE void __fastcall ZDecompressStringEx(System::AnsiString &result, const System::RawByteString s)/* overload */;
extern PACKAGE void __fastcall ZDecompressStringEx(System::UnicodeString &result, const System::RawByteString s)/* overload */;
extern PACKAGE System::AnsiString __fastcall ZDecompressStr2(const System::RawByteString s, int windowBits);
extern PACKAGE void __fastcall ZDecompressString2(System::AnsiString &result, const System::RawByteString s, int windowBits)/* overload */;
extern PACKAGE void __fastcall ZDecompressString2(System::UnicodeString &result, const System::RawByteString s, int windowBits)/* overload */;
extern PACKAGE void __fastcall ZCompressStream(System::Classes::TStream* inStream, System::Classes::TStream* outStream, TZCompressionLevel level = (TZCompressionLevel)(0x2));
extern PACKAGE void __fastcall ZCompressStream2(System::Classes::TStream* inStream, System::Classes::TStream* outStream, TZCompressionLevel level, int windowBits, int memLevel, TZStrategy strategy);
extern PACKAGE void __fastcall ZCompressStreamWeb(System::Classes::TStream* inStream, System::Classes::TStream* outStream);
extern PACKAGE void __fastcall ZDecompressStream(System::Classes::TStream* inStream, System::Classes::TStream* outStream);
extern PACKAGE void __fastcall ZDecompressStream2(System::Classes::TStream* inStream, System::Classes::TStream* outStream, int windowBits);
}	/* namespace Glszlibex */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSZLIBEX)
using namespace Glszlibex;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlszlibexHPP
