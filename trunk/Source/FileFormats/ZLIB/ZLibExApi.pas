{*****************************************************************************
*  ZLibExApi.pas                                                             *
*                                                                            *
*  copyright (c) 2000-2010 base2 technologies                                *
*  copyright (c) 1995-2002 Borland Software Corporation                      *
*                                                                            *
*  revision history                                                          *
*    2010.04.20  updated to zlib version 1.2.5                               *
*    2010.04.15  updated to zlib version 1.2.4                               *
*    2005.07.25  updated to zlib version 1.2.3                               *
*    2005.01.11  updated to zlib version 1.2.2                               *
*    2004.01.06  updated to zlib version 1.2.1                               *
*    2002.03.15  updated to zlib version 1.1.4                               *
*                                                                            *
*  acknowledgments                                                           *
*    burak kalayci                                                           *
*      2002.03.15  informing me about the zlib 1.1.4 update                  *
*      2004.01.06  informing me about the zlib 1.2.1 update                  *
*                                                                            *
*    vicente sanchez-alarcos                                                 *
*      2005.01.11  informing me about the zlib 1.2.2 update                  *
*                                                                            *
*    mathijs van veluw                                                       *
*      2005.07.25  informing me about the zlib 1.2.3 update                  *
*****************************************************************************}

unit ZLibExApi;

interface

{$I ZLibEx.inc}

const
  {** version ids ***********************************************************}

{$IFNDEF FPC}
  ZLIB_VERSION         = '1.2.5';
  ZLIB_VERNUM          = $1250;

  ZLIB_VER_MAJOR       = 1;
  ZLIB_VER_MINOR       = 2;
  ZLIB_VER_REVISION    = 5;
  ZLIB_VER_SUBREVISION = 0;
{$ENDIF}

  {** compression methods ***************************************************}

  Z_DEFLATED = 8;

  {** information flags *****************************************************}

  Z_INFO_FLAG_SIZE  = $1;
  Z_INFO_FLAG_CRC   = $2;
  Z_INFO_FLAG_ADLER = $4;

  Z_INFO_NONE       = 0;
  Z_INFO_DEFAULT    = Z_INFO_FLAG_SIZE or Z_INFO_FLAG_CRC;

  {** flush constants *******************************************************}

  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;
  Z_BLOCK         = 5;
  Z_TREES         = 6;

  {** return codes **********************************************************}

  Z_OK            = 0;
  Z_STREAM_END    = 1;
  Z_NEED_DICT     = 2;
  Z_ERRNO         = (-1);
  Z_STREAM_ERROR  = (-2);
  Z_DATA_ERROR    = (-3);
  Z_MEM_ERROR     = (-4);
  Z_BUF_ERROR     = (-5);
  Z_VERSION_ERROR = (-6);

  {** compression levels ****************************************************}

  Z_NO_COMPRESSION      =   0;
  Z_BEST_SPEED          =   1;
  Z_BEST_COMPRESSION    =   9;
  Z_DEFAULT_COMPRESSION = (-1);

  {** compression strategies ************************************************}

  Z_FILTERED         = 1;
  Z_HUFFMAN_ONLY     = 2;
  Z_RLE              = 3;
  Z_FIXED            = 4;
  Z_DEFAULT_STRATEGY = 0;

  {** data types ************************************************************}

  Z_BINARY  = 0;
  Z_ASCII   = 1;
  Z_TEXT    = Z_ASCII;
  Z_UNKNOWN = 2;

  {** return code messages **************************************************}

  _z_errmsg: Array [0..9] of String = (
    'Need dictionary',      // Z_NEED_DICT      (2)
    'Stream end',           // Z_STREAM_END     (1)
    'OK',                   // Z_OK             (0)
    'File error',           // Z_ERRNO          (-1)
    'Stream error',         // Z_STREAM_ERROR   (-2)
    'Data error',           // Z_DATA_ERROR     (-3)
    'Insufficient memory',  // Z_MEM_ERROR      (-4)
    'Buffer error',         // Z_BUF_ERROR      (-5)
    'Incompatible version', // Z_VERSION_ERROR  (-6)
    ''
  );
{$IFDEF FPC}
var
  ZLIB_VERSION            :PChar;
{$ENDIF}

type
  TZAlloc = function (opaque: Pointer; items, size: Integer): Pointer;
  TZFree  = procedure (opaque, block: Pointer);

  {** TZStreamRec ***********************************************************}

  TZStreamRec = packed record
    next_in  : Pointer;   // next input byte
    avail_in : Longint;   // number of bytes available at next_in
    total_in : Longint;   // total nb of input bytes read so far

    next_out : Pointer;   // next output byte should be put here
    avail_out: Longint;   // remaining free space at next_out
    total_out: Longint;   // total nb of bytes output so far

    msg      : Pointer;   // last error message, NULL if no error
    state    : Pointer;   // not visible by applications

    zalloc   : TZAlloc;   // used to allocate the internal state
    zfree    : TZFree;    // used to free the internal state
    opaque   : Pointer;   // private data object passed to zalloc and zfree

    data_type: Integer;   // best guess about the data type: ascii or binary
    adler    : Longint;   // adler32 value of the uncompressed data
    reserved : Longint;   // reserved for future use
  end;

{** macros ******************************************************************}

function deflateInit(var strm: TZStreamRec; level: Integer): Integer;
  {$ifdef Version2005Plus} inline; {$endif}

function deflateInit2(var strm: TZStreamRec; level, method, windowBits,
  memLevel, strategy: Integer): Integer;
  {$ifdef Version2005Plus} inline; {$endif}

function inflateInit(var strm: TZStreamRec): Integer;
  {$ifdef Version2005Plus} inline; {$endif}

function inflateInit2(var strm: TZStreamRec; windowBits: Integer): Integer;
  {$ifdef Version2005Plus} inline; {$endif}

{$IFDEF FPC}
  const
    {$IFDEF UNIX}
     libz = 'libz.so.1';
    {$ENDIF}
    {$ifdef netware}  {zlib.nlm comes with netware6}
     libz='zlib';
    {$ENDIF}
    {$IFDEF MSWindows}
     //Note that this is the official ZLIB1 .DLL from the http://www.zlib.net/
     libz='zlib1';
     {$ENDIF}
{$ENDIF}

{** external routines *******************************************************}
{$IFDEF FPC}
function zlibVersionpchar(): pchar; cdecl; external libz name 'zlibVersion';
function zlibVersion(): string;
function zErrorpchar(err: integer): pchar; cdecl; external libz name 'zError';
function inflateSyncPoint(z: TZstreamRec): integer; cdecl; external libz name 'inflateSyncPoint';
function get_crc_table(): pointer; cdecl; external libz name 'get_crc_table';
{$ENDIF}

function deflateInit_(var strm: TZStreamRec; level: Integer;
  version: PAnsiChar; recsize: Integer): Integer;
  {$IFDEF FPC}cdecl; external libz name 'deflateInit_';{$ENDIF}

function deflateInit2_(var strm: TZStreamRec; level, method, windowBits,
  memLevel, strategy: Integer; version: PAnsiChar; recsize: Integer): Integer;
  {$IFDEF FPC} cdecl; external libz name 'inflateInit2_';{$ENDIF}

function deflate(var strm: TZStreamRec; flush: Integer): Integer;
  {$IFDEF FPC} cdecl; external libz name 'deflate';{$ENDIF}

function deflateEnd(var strm: TZStreamRec): Integer;
  {$IFDEF FPC} cdecl; external libz name 'deflateEnd';{$ENDIF}

function deflateReset(var strm: TZStreamRec): Integer;
  {$IFDEF FPC} cdecl; external libz name 'deflateReset';{$ENDIF}

function inflateInit_(var strm: TZStreamRec; version: PAnsiChar;
  recsize: Integer): Integer;
  {$IFDEF FPC} cdecl; external libz name 'inflateInit_';{$ENDIF}

function inflateInit2_(var strm: TZStreamRec; windowBits: Integer;
  version: PAnsiChar; recsize: Integer): Integer;
  {$IFDEF FPC}cdecl; external libz name 'inflateInit2_';{$ENDIF}

function inflate(var strm: TZStreamRec; flush: Integer): Integer;
  {$IFDEF FPC} cdecl; external libz name 'inflate';{$ENDIF}

function inflateEnd(var strm: TZStreamRec): Integer;
  {$IFDEF FPC}cdecl; external libz name 'inflateEnd';{$ENDIF}

function inflateReset(var strm: TZStreamRec): Integer;
  {$IFDEF FPC} cdecl; external libz name 'inflateReset';{$ENDIF}

function adler32(adler: Longint; const buf; len: Integer): Longint;
  {$IFDEF FPC} cdecl; external libz name 'adler32';{$ENDIF}

function crc32(crc: Longint; const buf; len: Integer): Longint;
  {$IFDEF FPC} cdecl; external libz name 'crc32';{$ENDIF}

implementation

{*****************************************************************************
*  link zlib code                                                            *
*                                                                            *
*  bcc32 flags                                                               *
*    -c -O2 -Ve -X -pr -a8 -b -d -k- -vi -tWM                                *
*                                                                            *
*  note: do not reorder the following -- doing so will result in external    *
*  functions being undefined                                                 *
*****************************************************************************}
{$IFNDEF FPC}
  {$L ../LinkedObjects/deflate.obj}
  {$L ../LinkedObjects/inflate.obj}
  {$L ../LinkedObjects/inftrees.obj}
  {$L ../LinkedObjects/infback.obj}
  {$L ../LinkedObjects/inffast.obj}
  {$L ../LinkedObjects/trees.obj}
  {$L ../LinkedObjects/compress.obj}
  {$L ../LinkedObjects/adler32.obj}
  {$L ../LinkedObjects/crc32.obj}
{$ENDIF}

{** macros ******************************************************************}

function deflateInit(var strm: TZStreamRec; level: Integer): Integer;
begin
  result := deflateInit_(strm, level, ZLIB_VERSION, SizeOf(TZStreamRec));
end;

function deflateInit2(var strm: TZStreamRec; level, method, windowBits,
  memLevel, strategy: Integer): Integer;
begin
  result := deflateInit2_(strm, level, method, windowBits,
    memLevel, strategy, ZLIB_VERSION, SizeOf(TZStreamRec));
end;

function inflateInit(var strm: TZStreamRec): Integer;
begin
  result := inflateInit_(strm, ZLIB_VERSION, SizeOf(TZStreamRec));
end;

function inflateInit2(var strm: TZStreamRec; windowBits: Integer): Integer;
begin
  result := inflateInit2_(strm, windowBits, ZLIB_VERSION,
    SizeOf(TZStreamRec));
end;

{** external routines *******************************************************}
{$IFNDEF FPC}
function deflateInit_(var strm: TZStreamRec; level: Integer;
  version: PAnsiChar; recsize: Integer): Integer;
  external;

function deflateInit2_(var strm: TZStreamRec; level, method, windowBits,
  memLevel, strategy: Integer; version: PAnsiChar; recsize: Integer): Integer;
  external;

function deflate(var strm: TZStreamRec; flush: Integer): Integer;
  external;

function deflateEnd(var strm: TZStreamRec): Integer;
  external;

function deflateReset(var strm: TZStreamRec): Integer;
  external;

function inflateInit_(var strm: TZStreamRec; version: PAnsiChar;
  recsize: Integer): Integer;
  external;

function inflateInit2_(var strm: TZStreamRec; windowBits: Integer;
  version: PAnsiChar; recsize: Integer): Integer;
  external;

function inflate(var strm: TZStreamRec; flush: Integer): Integer;
  external;

function inflateEnd(var strm: TZStreamRec): Integer;
  external;

function inflateReset(var strm: TZStreamRec): Integer;
  external;

function adler32(adler: Longint; const buf; len: Integer): Longint;
  external;

function crc32(crc: Longint; const buf; len: Integer): Longint;
  external;
{$ELSE}
function zlibversion(): string;
begin
   zlibversion := string(zlibversionpchar);
end;
{$ENDIF}

{** zlib function implementations *******************************************}

function zcalloc(opaque: Pointer; items, size: Integer): Pointer;
begin
  GetMem(result,items * size);
end;

procedure zcfree(opaque, block: Pointer);
begin
  FreeMem(block);
end;

{** c function implementations **********************************************}

procedure _memset(p: Pointer; b: Byte; count: Integer); cdecl;
begin
  FillChar(p^,count,b);
end;

procedure _memcpy(dest, source: Pointer; count: Integer); cdecl;
begin
  Move(source^,dest^,count);
end;

initialization
 {$IFDEF FPC}
 ZLIB_VERSION := zlibVersionpchar;
 {$ENDIF}

end.
