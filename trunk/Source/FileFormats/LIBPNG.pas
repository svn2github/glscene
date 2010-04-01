//
// This unit is part of the GLScene Project, http://glscene.org
//
{: LIBPNG<p>

  <b>Historique : </b><font size=-1><ul>
      <li>01/04/10 - Yar - Bugfix when Delphi not use optimization (thanks Lampogolovii)
      <li>15/03/10 - Yar - Fixed memory leak (thanks Lampogolovii)
      <li>05/03/10 - Yar - Creation
  </ul></font>
}

unit LIBPNG;

interface

{$I GLScene.inc}

uses
{$IFDEF GLS_LOGGING}
  GLSLog,
{$ENDIF}
  Classes, SysUtils, VectorGeometry;

const
  ZLIB_VERSION = '1.2.3';
  LIBPNG_VERSION = '1.4.0';

  PNG_FLAG_ZLIB_CUSTOM_STRATEGY = $0001;
  PNG_FLAG_ZLIB_CUSTOM_LEVEL = $0002;
  PNG_FLAG_ZLIB_CUSTOM_MEM_LEVEL = $0004;
  PNG_FLAG_ZLIB_CUSTOM_WINDOW_BITS = $0008;
  PNG_FLAG_ZLIB_CUSTOM_METHOD = $0010;
  PNG_FLAG_ZLIB_FINISHED = $0020;
  PNG_FLAG_ROW_INIT = $0040;
  PNG_FLAG_FILLER_AFTER = $0080;
  PNG_FLAG_CRC_ANCILLARY_USE = $0100;
  PNG_FLAG_CRC_ANCILLARY_NOWARN = $0200;
  PNG_FLAG_CRC_CRITICAL_USE = $0400;
  PNG_FLAG_CRC_CRITICAL_IGNORE = $0800;
  PNG_FLAG_KEEP_UNKNOWN_CHUNKS = $8000;
  PNG_FLAG_KEEP_UNSAFE_CHUNKS = $10000;
  PNG_FLAG_LIBRARY_MISMATCH = $20000;
  PNG_FLAG_STRIP_ERROR_NUMBERS = $40000;
  PNG_FLAG_STRIP_ERROR_TEXT = $80000;
  PNG_FLAG_MALLOC_NULL_MEM_OK = $100000;
  PNG_FLAG_ADD_ALPHA = $200000;
  PNG_FLAG_STRIP_ALPHA = $400000;
  PNG_FLAG_BENIGN_ERRORS_WARN = $800000;
  PNG_FLAG_CRC_ANCILLARY_MASK = PNG_FLAG_CRC_ANCILLARY_USE or
    PNG_FLAG_CRC_ANCILLARY_NOWARN;

  PNG_FLAG_CRC_CRITICAL_MASK = PNG_FLAG_CRC_CRITICAL_USE or
    PNG_FLAG_CRC_CRITICAL_IGNORE;

  PNG_FLAG_CRC_MASK = PNG_FLAG_CRC_ANCILLARY_MASK or
    PNG_FLAG_CRC_CRITICAL_MASK;

  PNG_USER_WIDTH_MAX = 1000000;
  PNG_USER_HEIGHT_MAX = 1000000;
  PNG_UINT_31_MAX = $7FFFFFFF;
  PNG_UINT_32_MAX = $FFFFFFFF;

  PNG_COLOR_MASK_PALETTE = 1;
  PNG_COLOR_MASK_COLOR = 2;
  PNG_COLOR_MASK_ALPHA = 4;

  PNG_COLOR_TYPE_GRAY = 0;
  PNG_COLOR_TYPE_PALETTE = PNG_COLOR_MASK_COLOR or PNG_COLOR_MASK_PALETTE;
  PNG_COLOR_TYPE_RGB = PNG_COLOR_MASK_COLOR;
  PNG_COLOR_TYPE_RGB_ALPHA = PNG_COLOR_MASK_COLOR or PNG_COLOR_MASK_ALPHA;
  PNG_COLOR_TYPE_GRAY_ALPHA = PNG_COLOR_MASK_ALPHA;

  PNG_INTERLACE_NONE = 0;
  PNG_INTERLACE_ADAM7 = 1;
  PNG_INTERLACE_LAST = 2;

  PNG_COMPRESSION_TYPE_BASE = 0;

  PNG_HAVE_IHDR = $01;
  PNG_HAVE_PLTE = $02;
  PNG_HAVE_IDAT = $04;
  PNG_AFTER_IDAT = $08;
  PNG_HAVE_IEND = $10;
  PNG_HAVE_gAMA = $20;
  PNG_HAVE_cHRM = $40;
  PNG_HAVE_sRGB = $80;
  PNG_HAVE_CHUNK_HEADER = $0100;
  PNG_WROTE_tIME = $0200;
  PNG_WROTE_INFO_BEFORE_PLTE = $0400;
  PNG_BACKGROUND_IS_GRAY = $0800;
  PNG_HAVE_PNG_SIGNATURE = $1000;
  PNG_HAVE_CHUNK_AFTER_IDAT = $2000;

  PNG_INFO_gAMA = $0001;
  PNG_INFO_sBIT = $0002;
  PNG_INFO_cHRM = $0004;
  PNG_INFO_PLTE = $0008;
  PNG_INFO_tRNS = $0010;
  PNG_INFO_bKGD = $0020;
  PNG_INFO_hIST = $0040;
  PNG_INFO_pHYs = $0080;
  PNG_INFO_oFFs = $0100;
  PNG_INFO_tIME = $0200;
  PNG_INFO_pCAL = $0400;
  PNG_INFO_sRGB = $0800;
  PNG_INFO_iCCP = $1000;
  PNG_INFO_sPLT = $2000;
  PNG_INFO_sCAL = $4000;
  PNG_INFO_IDAT = $8000;

  PNG_FILTER_TYPE_BASE = 0;
  PNG_INTRAPIXEL_DIFFERENCING = 64;

  PNG_FLAG_MNG_EMPTY_PLTE = $01;
  PNG_FLAG_MNG_FILTER_64 = $04;
  PNG_ALL_MNG_FEATURES = $05;

  PNG_BGR = $0001;
  PNG_INTERLACE = $0002;
  PNG_PACK = $0004;
  PNG_SHIFT = $0008;
  PNG_SWAP_BYTES = $0010;
  PNG_INVERT_MONO = $0020;
  PNG_DITHER = $0040;
  PNG_BACKGROUND = $0080;
  PNG_BACKGROUND_EXPAND = $0100;

  PNG_16_TO_8 = $0400;
  PNG_RGBA = $0800;
  PNG_EXPAND = $1000;
  PNG_GAMMA = $2000;
  PNG_GRAY_TO_RGB = $4000;
  PNG_FILLER = $8000;
  PNG_PACKSWAP = $10000;
  PNG_SWAP_ALPHA = $20000;
  PNG_STRIP_ALPHA = $40000;
  PNG_INVERT_ALPHA = $80000;
  PNG_USER_TRANSFORM = $100000;
  PNG_RGB_TO_GRAY_ERR = $200000;
  PNG_RGB_TO_GRAY_WARN = $400000;
  PNG_RGB_TO_GRAY = $600000;

  PNG_ADD_ALPHA = $1000000;
  PNG_EXPAND_tRNS = $2000000;

  PNG_GAMMA_THRESHOLD = 0.05;

  PNG_BACKGROUND_GAMMA_UNKNOWN = 0;
  PNG_BACKGROUND_GAMMA_SCREEN = 1;
  PNG_BACKGROUND_GAMMA_FILE = 2;
  PNG_BACKGROUND_GAMMA_UNIQUE = 3;

  PNG_FREE_HIST = $0008;
  PNG_FREE_ICCP = $0010;
  PNG_FREE_SPLT = $0020;
  PNG_FREE_ROWS = $0040;
  PNG_FREE_PCAL = $0080;
  PNG_FREE_SCAL = $0100;
  PNG_FREE_UNKN = $0200;
  PNG_FREE_LIST = $0400;
  PNG_FREE_PLTE = $1000;
  PNG_FREE_TRNS = $2000;
  PNG_FREE_TEXT = $4000;
  PNG_FREE_ALL = $7FFF;
  PNG_FREE_MUL = $4220;

  PNG_COMPRESSION_TYPE_DEFAULT = PNG_COMPRESSION_TYPE_BASE;
  PNG_FILTER_TYPE_DEFAULT = PNG_FILTER_TYPE_BASE;

type
  png_size_t = Cardinal; // 32 bit unsigned integer
  png_byte = Byte;
  png_int_32 = LongInt;
  png_uint_32 = LongWord;
  png_uint_16 = Word;
  png_fixed_point = LongInt;

  png_voidp = Pointer;
  png_byte_array = array[0..MaxInt div 2 - 1] of png_byte;
  png_bytep = ^png_byte_array;
  png_bytep_array = array[0..MaxInt div (2 * SizeOf(png_bytep))] of png_bytep;
  png_bytep_arrayp = ^png_bytep_array;
  png_bytepp = ^png_bytep;
  png_uint_32p = ^png_uint_32;
  png_int_32p = PInteger;
  png_uint_16p = ^png_uint_16;
  png_uint_16pp = ^png_uint_16p;
  png_int_16p = PShortInt;
  png_const_charp = PAnsiChar;
  png_charp = PAnsiChar;
  png_charp_array = array[0..MaxInt div (2 * SizeOf(png_charp))] of png_charp;
  png_charp_arrayp = ^png_charp_array;
  png_charpp = ^png_charp;
  png_fixed_point_p = ^LongInt;

  jmp_buf = record
    j_ebp,
      j_ebx,
      j_edi,
      j_esi,
      j_esp,
      j_ret,
      j_excep,
      j_context: Cardinal;
  end;
  jmp_bufp = ^jmp_buf;

  {: Three color definitions.  The order of the red, green, and blue, (and the
    exact size) is not important, although the size of the fields need to
    be png_byte or png_uint_16 (as defined below). }
  png_color = record
    red: png_byte;
    green: png_byte;
    blue: png_byte;
  end;
  png_color_array = array[0..MaxInt div (2 * SizeOf(png_color))] of png_color;
  png_colorp = ^png_color_array;
  png_colorpp = ^png_colorp;

  png_color_8 = record
    red: png_byte; // for use in red green blue files
    green: png_byte;
    blue: png_byte;
    gray: png_byte; // for use in grayscale files
    alpha: png_byte; // for alpha channel files
  end;
  png_color_8p = ^png_color_8;
  png_color_8pp = ^png_color_8p;

  png_color_16 = record
    index: png_byte; // used for palette files
    red: png_uint_16; // for use in red green blue files
    green: png_uint_16;
    blue: png_uint_16;
    gray: png_uint_16; // for use in grayscale files
  end;
  png_color_16p = ^png_color_16;
  png_color_16pp = ^png_color_16p;

  {: png_unknown_chunk is a structure to hold queued chunks for which there is
     no specific support.  The idea is that we can use this to queue
     up private chunks for output even though the library doesn't actually
     know about their semantics. }

  png_unknown_chunk = record
    name: array[0..4] of png_byte;
    data: png_bytep;
    size: png_size_t;

    {: libpng-using applications should NOT directly modify this byte.
       png_byte location;  mode of operation at read time }
  end;
  png_unknown_chunk_array =
    array[0..MaxInt div (2 * SizeOf(png_unknown_chunk))] of png_unknown_chunk;
  png_unknown_chunk_arrayp = ^png_unknown_chunk_array;
  png_unknown_chunkp = ^png_unknown_chunk;
  png_unknown_chunkpp = ^png_unknown_chunkp;

  png_text = record
    {: compression value:
       -1: tEXt, none
        0: zTXt, deflate
        1: iTXt, none
        2: iTXt, deflate  }
    compression: Integer;
    {: keyword, 1-79 character description of "text" }
    key: png_charp;
    {: comment, may be an empty string (ie "")
       or a NULL pointer }
    text: png_charp;
    {: length of the text string }
    text_length: png_size_t;
    {: length of the itxt string }
    itxt_length: png_size_t;
    {: language code, 0-79 characters
       or a NULL pointer }
    lang: png_charp;
    {: keyword translated UTF-8 string, 0 or more
       chars or a NULL pointer }
    lang_key: png_charp;
  end;
  png_text_array = array[0..MaxInt div (2 * SizeOf(png_text))] of png_text;
  png_text_arrayp = ^png_text_array;
  png_textp = ^png_text;
  png_textpp = ^png_textp;

  png_time = record
    year: png_uint_16; {: full year, as in, 1995 }
    month: png_byte; {: month of year, 1 - 12 }
    day: png_byte; {: day of month, 1 - 31 }
    hour: png_byte; {: hour of day, 0 - 23 }
    minute: png_byte; {: minute of hour, 0 - 59 }
    second: png_byte; {: second of minute, 0 - 60 (for leap seconds) }
  end;
  png_timep = ^png_time;
  png_timepp = ^png_timep;

  png_sPLT_entry = record
    red: png_uint_16;
    green: png_uint_16;
    blue: png_uint_16;
    alpha: png_uint_16;
    frequency: png_uint_16;
  end;

  png_sPLT_entryp = ^png_sPLT_entry;
  png_sPLT_entrypp = ^png_sPLT_entryp;

  png_sPLT_t = record
    name: png_charp; {: palette name }
    depth: png_byte; {: depth of palette samples }
    entries: png_sPLT_entryp; {: palette entries }
    nentries: png_int_32; {: number of palette entries }
  end;
  png_sPLT_tp_array =
    array[0..MaxInt div (2 * SizeOf(png_sPLT_t))] of png_sPLT_t;
  png_sPLT_tp_arrayp = ^png_sPLT_tp_array;
  png_sPLT_tp = ^png_sPLT_t;
  png_sPLT_tpp = ^png_sPLT_tp;

  png_structp = ^png_struct;
  png_structpp = ^png_structp;
  png_info = record
    width: png_uint_32; {: width of image in pixels (from IHDR) }
    height: png_uint_32; {: height of image in pixels (from IHDR) }
    valid: png_uint_32; {: valid chunk data (see PNG_INFO_ below) }
    rowbytes: png_size_t; {: bytes needed to hold an untransformed row }
    palette: png_colorp; {: array of color values (valid & PNG_INFO_PLTE) }
    num_palette: png_uint_16; {: number of color entries in "palette" (PLTE) }
    num_trans: png_uint_16; {: number of transparent palette color (tRNS) }
    bit_depth: png_byte; {: 1, 2, 4, 8, or 16 bits/channel (from IHDR) }
    color_type: png_byte; {: see PNG_COLOR_TYPE_ below (from IHDR) }

    compression_type: png_byte; {: must be PNG_COMPRESSION_TYPE_BASE (IHDR) }
    filter_type: png_byte; {: must be PNG_FILTER_TYPE_BASE (from IHDR) }
    interlace_type: png_byte; {: One of PNG_INTERLACE_NONE, PNG_INTERLACE_ADAM7 }

    channels: png_byte; {: number of data channels per pixel (1, 2, 3, 4) }
    pixel_depth: png_byte; {: number of bits per pixel }
    spare_byte: png_byte; {: to align the data, and for future use }
    signature: array[0..7] of png_byte;
    {: magic bytes read by libpng from start of file }

    gamma: Single; {: gamma value of image, if (valid & PNG_INFO_gAMA) }
    srgb_intent: png_byte; {: sRGB rendering intent [0, 1, 2, or 3] }

    num_text: Integer; {: number of comments read/to write }
    max_text: Integer; {: current size of text array }
    text: png_textp; {: array of comments read/to write }

    mod_time: png_time;

    sig_bit: png_color_8; {: significant bits in color channels }

    trans_alpha: png_bytep; {: alpha values for paletted image }
    trans_color: png_color_16; {: transparent color for non-palette image }

    background: png_color_16;

    x_offset: png_int_32; {: x offset on page }
    y_offset: png_int_32; {: y offset on page }
    offset_unit_type: png_byte; {: offset units type }

    x_pixels_per_unit: png_uint_32; {: horizontal pixel density }
    y_pixels_per_unit: png_uint_32; {: vertical pixel density }
    phys_unit_type: png_byte; {: resolution type (see PNG_RESOLUTION_ below) }

    hist: png_uint_16p;

    x_white: Single;
    y_white: Single;
    x_red: Single;
    y_red: Single;
    x_green: Single;
    y_green: Single;
    x_blue: Single;
    y_blue: Single;

    pcal_purpose: png_charp; {: pCAL chunk description string }
    pcal_X0: png_int_32; {: minimum value }
    pcal_X1: png_int_32; {: maximum value }
    pcal_units: png_charp; {: Latin-1 string giving physical units }
    pcal_params: png_charpp; {: ASCII strings containing parameter values }
    pcal_type: png_byte; {: equation type (see PNG_EQUATION_ below) }
    pcal_nparams: png_byte; {: number of parameters given in pcal_params }

    free_me: png_uint_32; {: flags items libpng is responsible for freeing }

    unknown_chunks: png_unknown_chunkp;
    unknown_chunks_num: png_size_t;

    iccp_name: png_charp; {: profile name }
    iccp_profile: png_charp; {: International Color Consortium profile data }
    {: Note to maintainer: should be png_bytep }
    iccp_proflen: png_uint_32; {: ICC profile data length }
    iccp_compression: png_byte; {: Always zero }

    splt_palettes: png_sPLT_tp;
    splt_palettes_num: png_uint_32;

    scal_unit: png_byte; {: unit of physical scale }
    scal_pixel_width: double; {: width of one pixel }
    scal_pixel_height: double; {: height of one pixel }
    scal_s_width: png_charp; {: string containing height }
    scal_s_height: png_charp; {: string containing width }

    row_pointers: png_bytepp; {: the image bits }

    int_gamma: png_fixed_point; {: gamma of image, if (valid & PNG_INFO_gAMA) }

    int_x_white: png_fixed_point;
    int_y_white: png_fixed_point;
    int_x_red: png_fixed_point;
    int_y_red: png_fixed_point;
    int_x_green: png_fixed_point;
    int_y_green: png_fixed_point;
    int_x_blue: png_fixed_point;
    int_y_blue: png_fixed_point;
  end;
  png_infop = ^png_info;
  png_infopp = ^png_infop;

  png_row_info = record
    width: png_uint_32; // width of row
    rowbytes: png_uint_32; // number of bytes in row
    color_type: png_byte; // color type of row
    bit_depth: png_byte; // bit depth of row
    channels: png_byte; // number of channels (1, 2, 3, or 4)
    pixel_depth: png_byte; // bits per pixel (depth * channels)
  end;
  png_row_infop = ^png_row_info;

  TAlloc = function(AppData: Pointer; Items, Size: Integer): Pointer; cdecl;
  TFree = procedure(AppData, Block: Pointer); cdecl;
  TInFunc = function(opaque: Pointer; var buf: PByte): Integer; cdecl;
  TOutFunc = function(opaque: Pointer; buf: PByte; size: Integer): Integer;
  png_error_ptr = procedure(struct: png_structp; str: png_const_charp); cdecl;
  png_rw_ptr = procedure(struct: png_structp; data: png_bytep; size:
    png_size_t); cdecl;
  png_longjmp_ptr = procedure(jb: jmp_buf; i: Integer); cdecl;
  png_user_transform_ptr = procedure(struct: png_structp; row_info:
    png_row_infop; b: png_bytep); cdecl;
  png_flush_ptr = procedure(struct: png_structp); cdecl;
  png_read_status_ptr = procedure(struct: png_structp; ui: png_uint_32; i:
    Integer); cdecl;
  png_write_status_ptr = procedure(struct: png_structp; ui: png_uint_32; i:
    Integer); cdecl;
  png_progressive_info_ptr = procedure(struct: png_structp; info: png_infop);
  cdecl;
  png_progressive_end_ptr = procedure(struct: png_structp; info: png_infop);
  cdecl;
  png_progressive_row_ptr = procedure(struct: png_structp; bp: png_bytep; ui:
    png_uint_32; i: Integer); cdecl;
  png_malloc_ptr = function(struct: png_structp; size: png_size_t): png_voidp;
  png_free_ptr = procedure(struct: png_structp; ptr: png_voidp);
  png_user_chunk_ptr = function(struct: png_structp; chunk: png_unknown_chunkp):
    Integer;

  // Internal structure.  Ignore.
  TZStreamRec = packed record
    next_in: PAnsiChar; // next input byte
    avail_in: Integer; // number of bytes available at next_in
    total_in: Integer; // total nb of input bytes read so far

    next_out: PAnsiChar; // next output byte should be put here
    avail_out: Integer; // remaining free space at next_out
    total_out: Integer; // total nb of bytes output so far

    msg: PAnsiChar; // last error message, NULL if no error
    internal: Pointer; // not visible by applications

    zalloc: TAlloc; // used to allocate the internal state
    zfree: TFree; // used to free the internal state
    AppData: Pointer; // private data object passed to zalloc and zfree

    data_type: Integer; //  best guess about the data type: ascii or binary
    adler: Integer; // adler32 value of the uncompressed data
    reserved: Integer; // reserved for future use
  end;

  png_struct = record
    jmpbuf: jmp_buf; // used in png_error
    longjmp_fn: png_longjmp_ptr; // setjmp non-local goto function.
    error_fn: png_error_ptr; // function for printing errors and aborting
    warning_fn: png_error_ptr; // function for printing warnings
    error_ptr: png_voidp; // user supplied struct for error functions
    write_data_fn: png_rw_ptr; // function for writing output data
    read_data_fn: png_rw_ptr; // function for reading input data
    io_ptr: png_voidp; // ptr to application struct for I/O functions

    read_user_transform_fn: png_user_transform_ptr; // user read transform
    write_user_transform_fn: png_user_transform_ptr; // user write transform

    // These were added in libpng-1.0.2
    user_transform_ptr: png_voidp; // user supplied struct for user transform
    user_transform_depth: png_byte; // bit depth of user transformed pixels
    user_transform_channels: png_byte; // channels in user transformed pixels

    mode: png_uint_32; // tells us where we are in the PNG file
    flags: png_uint_32; // flags indicating various things to libpng
    transformations: png_uint_32; // which transformations to perform

    zstream: TZStreamRec; // pointer to decompression structure (below)
    zbuf: png_bytep; // buffer for zlib
    zbuf_size: png_size_t; // size of zbuf
    zlib_level: Integer; // holds zlib compression level
    zlib_method: Integer; // holds zlib compression method
    zlib_window_bits: Integer; // holds zlib compression window bits
    zlib_mem_level: Integer; // holds zlib compression memory level
    zlib_strategy: Integer; // holds zlib compression strategy

    width: png_uint_32; // width of image in pixels
    height: png_uint_32; // height of image in pixels
    num_rows: png_uint_32; // number of rows in current pass
    usr_width: png_uint_32; // width of row at start of write
    rowbytes: png_uint_32; // size of row in bytes
    irowbytes: png_uint_32; // size of current interlaced row in bytes
    iwidth: png_uint_32; // width of current interlaced row in pixels
    row_number: png_uint_32; // current row in interlace pass
    prev_row: png_bytep; // buffer to save previous (unfiltered) row
    row_buf: png_bytep; // buffer to save current (unfiltered) row
    sub_row: png_bytep; // buffer to save "sub" row when filtering
    up_row: png_bytep; // buffer to save "up" row when filtering
    avg_row: png_bytep; // buffer to save "avg" row when filtering
    paeth_row: png_bytep; // buffer to save "Paeth" row when filtering
    row_info: png_row_info; // used for transformation routines

    idat_size: png_uint_32; // current IDAT size for read
    crc: png_uint_32; // current chunk CRC value
    palette: png_colorp; // palette from the input file
    num_palette: png_uint_16; // number of color entries in palette
    num_trans: png_uint_16; // number of transparency values
    chunk_name: array[0..4] of png_byte; // null-terminated name of current chunk
    compression: png_byte; // file compression type (always 0)
    filter: png_byte; // file filter type (always 0)
    interlaced: png_byte; // PNG_INTERLACE_NONE, PNG_INTERLACE_ADAM7
    pass: png_byte; // current interlace pass (0 - 6)
    do_filter: png_byte; // row filter flags (see PNG_FILTER_ below )
    color_type: png_byte; // color type of file
    bit_depth: png_byte; // bit depth of file
    usr_bit_depth: png_byte; // bit depth of users row
    pixel_depth: png_byte; // number of bits per pixel
    channels: png_byte; // number of channels in file
    usr_channels: png_byte; // channels at start of write
    sig_bytes: png_byte; // magic bytes read/written from start of file

    filler: png_uint_16; // filler bytes for pixel expansion

    background_gamma_type: png_byte;
    background_gamma: Single;
    background: png_color_16; // background color in screen gamma space
    background_1: png_color_16; // background normalized to gamma 1.0

    output_flush_fn: png_flush_ptr; // Function for flushing output
    flush_dist: png_uint_32; // how many rows apart to flush, 0 - no flush
    flush_rows: png_uint_32; // number of rows written since last flush

    gamma_shift: Integer; // number of "insignificant" bits 16-bit gamma
    gamma: Single; // file gamma value
    screen_gamma: Single; // screen gamma value (display_exponent)

    gamma_table: png_bytep; // gamma table for 8-bit depth files
    gamma_from_1: png_bytep; // converts from 1.0 to screen
    gamma_to_1: png_bytep; // converts from file to 1.0
    gamma_16_table: png_uint_16pp; // gamma table for 16-bit depth files
    gamma_16_from_1: png_uint_16pp; // converts from 1.0 to screen
    gamma_16_to_1: png_uint_16pp; // converts from file to 1.0

    sig_bit: png_color_8; // significant bits in each available channel

    shift: png_color_8; // shift for significant bit tranformation

    trans: png_bytep; // transparency values for paletted files
    trans_values: png_color_16; // transparency values for non-paletted files

    trans_alpha: png_bytep; // alpha values for paletted files
    trans_color: png_color_16; // transparent color for non-paletted files

    read_row_fn: png_read_status_ptr; // called after each row is decoded
    write_row_fn: png_write_status_ptr; // called after each row is encoded

    info_fn: png_progressive_info_ptr; // called after header data fully read
    row_fn: png_progressive_row_ptr; // called after each prog. row is decoded
    end_fn: png_progressive_end_ptr; // called after image is complete
    save_buffer_ptr: png_bytep; // current location in save_buffer
    save_buffer: png_bytep; // buffer for previously read data
    current_buffer_ptr: png_bytep; // current location in current_buffer
    current_buffer: png_bytep; // buffer for recently used data
    push_length: png_uint_32; // size of current input chunk
    skip_length: png_uint_32; // bytes to skip in input data
    save_buffer_size: png_size_t; // amount of data now in save_buffer
    save_buffer_max: png_size_t; // total size of save_buffer
    buffer_size: png_size_t; // total amount of available input data
    current_buffer_size: png_size_t; // amount of data now in current_buffer
    process_mode: Integer; // what push library is currently doing
    cur_palette: Integer; // current push library palette index

    current_text_size: png_size_t; // current size of text input data
    current_text_left: png_size_t; // how much text left to read in input
    current_text: png_charp; // current text chunk buffer
    current_text_ptr: png_charp; // current location in current_text

    hist: png_uint_16p; // histogram }

    heuristic_method: png_byte; // heuristic for row filter selection }
    num_prev_filters: png_byte; // number of weights for previous rows }
    prev_filters: png_bytep; // filter type(s) of previous row(s) }
    filter_weights: png_uint_16p; // weight(s) for previous line(s) }
    inv_filter_weights: png_uint_16p; // 1/weight(s) for previous line(s) }
    filter_costs: png_uint_16p; // relative filter calculation cost }
    inv_filter_costs: png_uint_16p; // 1/relative filter calculation cost }

    time_buffer: png_charp; // String to hold RFC 1123 time text

    // New members added in libpng-1.0.6

    free_me: png_uint_32; // flags items libpng is responsible for freeing

    user_chunk_ptr: png_voidp;
    read_user_chunk_fn: png_user_chunk_ptr; // user read chunk handler

    num_chunk_list: Integer;
    chunk_list: png_bytep;

    // New members added in libpng-1.0.3
    rgb_to_gray_status: png_byte;
    // These were changed from png_byte in libpng-1.0.6
    rgb_to_gray_red_coeff: png_uint_16;
    rgb_to_gray_green_coeff: png_uint_16;
    rgb_to_gray_blue_coeff: png_uint_16;

    // New member added in libpng-1.0.4 (renamed in 1.0.9)
    // Changed from png_byte to png_uint_32 at version 1.2.0
    mng_features_permitted: png_uint_32;

    // New member added in libpng-1.0.7
    int_gamma: png_fixed_point;

    // New member added in libpng-1.0.9, ifdef'ed out in 1.0.12, enabled in 1.2.0
    filter_type: png_byte;

    // New members added in libpng-1.2.0

    // New members added in libpng-1.0.2 but first enabled by default in 1.2.0
    mem_ptr: png_voidp; // user supplied struct for mem functions
    malloc_fn: png_malloc_ptr; // function for allocating memory
    free_fn: png_free_ptr; // function for freeing memory

    // New member added in libpng-1.0.13 and 1.2.0
    big_row_buf: png_bytep; // buffer to save current (unfiltered) row

    // New members added in libpng-1.0.16 and 1.2.6
    compression_type: png_byte;

    //    user_width_max: png_uint_32;
    //    user_height_max: png_uint_32;
        // Added in libpng-1.4.0: Total number of sPLT, text, and unknown
        // chunks that can be stored ( $7fffffff means unlimited).

    //    user_chunk_cache_max: png_uint_32;

        // New member added in libpng-1.0.25 and 1.2.17
        // Storage for unknown chunk that the library doesn't recognize.
    unknown_chunk: png_unknown_chunk;

    // New members added in libpng-1.2.26
    old_big_row_buf_size: png_uint_32;
    old_prev_row_size: png_uint_32;

    // New member added in libpng-1.2.30
    chunkdata: png_charp; // buffer for reading chunk data

    // New member added in libpng-1.4.0
    io_state: png_uint_32;
  end;

  (* stream read/write function *)
procedure pngReadFn(png_ptr: png_structp; data: png_bytep; length: png_size_t);
cdecl;
procedure pngWriteFn(png_ptr: png_structp; data: png_bytep; length: png_size_t);
cdecl;
procedure pngErrorFn(struct: png_structp; str: png_const_charp);
cdecl;
procedure pngWarnFn(struct: png_structp; str: png_const_charp);
cdecl;
(* basic functions *)
function deflateInit(var strm: TZStreamRec; level: Integer): Integer;
forward;
function deflate(var strm: TZStreamRec; flush: Integer): Integer;
forward;
function deflateEnd(var strm: TZStreamRec): Integer;
forward;
function inflate(var strm: TZStreamRec; flush: Integer): Integer;
forward;
function inflateEnd(var strm: TZStreamRec): Integer;
forward;

(* advanced functions *)
function deflateSetDictionary(var strm: TZStreamRec; const dictionary:
  PAnsiChar;
  dictLength: Integer): Integer;
forward;
function deflateCopy(var dest, source: TZStreamRec): Integer;
forward;
function deflateReset(var strm: TZStreamRec): Integer;
cdecl; forward;
function deflateParams(var strm: TZStreamRec; level, strategy: Integer):
  Integer;
forward;
function deflateBound(var strm: TZStreamRec; sourceLen: LongInt): LongInt;
forward;
function deflatePrime(var strm: TZStreamRec; bits, value: Integer): Integer;
forward;
function inflateSetDictionary(var strm: TZStreamRec; const dictionary:
  PAnsiChar; dictLength: Integer): Integer;
forward;
function inflateSync(var strm: TZStreamRec): Integer;
forward;
function inflateCopy(var dest, source: TZStreamRec): Integer;
forward;
function inflateReset(var strm: TZStreamRec): Integer;
forward;
function inflateBack(var strm: TZStreamRec; in_fn: TInFunc; in_desc: Pointer;
  out_fn: TOutFunc; out_desc: Pointer): Integer;
forward;
function inflateBackEnd(var strm: TZStreamRec): Integer;
forward;

(* utility functions *)
function compress(dest: PAnsiChar; var destLen: LongInt;
  const source: PAnsiChar; sourceLen: LongInt): Integer; forward;
function compress2(dest: PAnsiChar; var destLen: LongInt;
  const source: PAnsiChar; sourceLen: LongInt;
  level: Integer): Integer; forward;
function compressBound(sourceLen: LongInt): LongInt; forward;

(* checksum functions *)
function adler32(adler: LongInt; const buf: PAnsiChar; len: Integer): LongInt;
forward;
function crc32(crc: LongInt; const buf: PAnsiChar; len: Integer): LongInt;
forward;

(* various hacks, don't look :) *)
function deflateInit_(var strm: TZStreamRec; level: Integer;
  const version: PAnsiChar; stream_size: Integer): Integer;
function inflateInit_(var strm: TZStreamRec; const version: PAnsiChar;
  stream_size: Integer): Integer;
function deflateInit2_(var strm: TZStreamRec;
  level, method, windowBits, memLevel, strategy: Integer;
  const version: PAnsiChar; stream_size: Integer): Integer;
function inflateInit2_(var strm: TZStreamRec; windowBits: Integer;
  const version: PAnsiChar; stream_size: Integer): Integer;
function inflateBackInit_(var strm: TZStreamRec;
  windowBits: Integer; window: PAnsiChar;
  const version: PAnsiChar; stream_size: Integer): Integer;

{: Main libpng functions }
function _png_sig_cmp(sig: png_bytep; start: png_size_t; num_to_check:
  png_size_t): Integer; cdecl; forward;
function _png_create_read_struct(
  user_png_ver: png_const_charp;
  error_ptr: png_voidp;
  error_fn: png_error_ptr;
  warn_fn: png_error_ptr): png_structp; cdecl; forward;
procedure _png_destroy_read_struct(png_ptr_ptr: png_structpp; info_ptr_ptr:
  png_infopp;
  end_info_ptr_ptr: png_infopp); cdecl; forward;
function _png_create_info_struct(png_ptr: png_structp): png_infop; cdecl;
forward;
procedure _png_destroy_info_struct(png_ptr: png_structp; info_ptr_ptr:
  png_infopp); cdecl; forward;
procedure _png_set_read_fn(png_ptr: png_structp; io_ptr: png_voidp;
  read_data_fn: png_rw_ptr); cdecl; forward;
procedure _png_set_sig_bytes(png_ptr: png_structp; num_bytes: Integer);
cdecl; forward;
function _png_get_image_width(png_ptr: png_structp; info_ptr: png_infop):
  png_uint_32; cdecl; forward;
function _png_get_image_height(png_ptr: png_structp; info_ptr: png_infop):
  png_uint_32; cdecl; forward;
function _png_get_color_type(png_ptr: png_structp; info_ptr: png_infop):
  png_byte; cdecl; forward;
function _png_get_bit_depth(png_ptr: png_structp; info_ptr: png_infop):
  png_byte; cdecl; forward;
function _png_get_filter_type(png_ptr: png_structp; info_ptr: png_infop):
  png_byte; cdecl; forward;
function _png_get_interlace_type(png_ptr: png_structp; info_ptr: png_infop):
  png_byte; cdecl; forward;
function _png_get_compression_type(png_ptr: png_structp; info_ptr: png_infop):
  png_byte; cdecl; forward;
procedure _png_read_info(png_ptr: png_structp; info_ptr: png_infop);
cdecl; forward;
function _png_get_io_ptr(png_ptr: png_structp): png_voidp;
cdecl; forward;
procedure _png_warning(png_ptr: png_structp; warning_message: png_const_charp);
cdecl; forward;
procedure _png_error(png_ptr: png_structp; error_message: png_const_charp);
cdecl; forward;
procedure _png_set_palette_to_rgb(png_ptr: png_structp);
cdecl; forward;
procedure _png_set_expand_gray_1_2_4_to_8(png_ptr: png_structp);
cdecl; forward;
procedure _png_set_tRNS_to_alpha(png_ptr: png_structp);
cdecl; forward;
function _png_get_valid(png_ptr: png_structp; info_ptr: png_infop; flag:
  png_uint_32): png_uint_32;
cdecl; forward;
procedure _png_read_update_info(png_ptr: png_structp; info_ptr: png_infop);
cdecl; forward;
procedure _png_read_image(png_ptr: png_structp; image: png_bytepp);
cdecl; forward;
function _png_get_rowbytes(png_ptr: png_structp; info_ptr: png_infop):
  png_size_t;
cdecl; forward;
procedure _png_build_gamma_table(png_ptr: png_structp; bit_depth: png_byte);
cdecl; forward;
procedure _png_read_end(png_ptr: png_structp; info_ptr: png_infop);
cdecl; forward;
procedure _png_free(png_ptr: png_structp; ptr: png_voidp);
cdecl; forward;
procedure _png_info_init_3(ptr_ptr: png_infopp; png_info_struct_size:
  png_size_t); cdecl; forward;
function _png_get_channels(png_ptr: png_structp; info_ptr: png_infop): png_byte;
cdecl; forward;
function _png_create_write_struct(user_png_ver: png_const_charp; error_ptr:
  png_voidp;
  error_fn: png_error_ptr; warn_fn: png_error_ptr): png_structp;
cdecl; forward;
procedure _png_destroy_write_struct(png_ptr_ptr: png_structpp;
  info_ptr_ptr: png_infopp);
cdecl; forward;
procedure _png_set_IHDR(png_ptr: png_structp; info_ptr: png_infop;
  width: png_uint_32; height: png_uint_32; bit_depth: Integer;
  color_type: Integer; interlace_type: Integer; compression_type: Integer;
  filter_type: Integer);
cdecl; forward;
procedure _png_write_image(png_ptr: png_structp; image: png_bytepp);
cdecl; forward;
procedure _png_write_end(png_ptr: png_structp; info_ptr: png_infop);
cdecl; forward;
procedure _png_set_write_fn(png_ptr: png_structp; io_ptr: png_voidp;
  write_data_fn: png_rw_ptr; output_flush_fn: png_flush_ptr);
cdecl;
procedure _png_write_info(png_ptr: png_structp; info_ptr: png_infop);
cdecl; forward;
function _png_malloc(png_ptr: png_structp; size: png_size_t): png_voidp;
cdecl; forward;

implementation

{$L LinkedObjects\adler32.obj}
{$L LinkedObjects\deflate.obj}
{$L LinkedObjects\infback.obj}
{$L LinkedObjects\inffast.obj}
{$L LinkedObjects\inflate.obj}
{$L LinkedObjects\inftrees.obj}
{$L LinkedObjects\trees.obj}
{$L LinkedObjects\compress.obj}
{$L LinkedObjects\crc32.obj}
{$L LinkedObjects\png_check_sig.obj}

function adler32; external;
function compress; external;
function compress2; external;
function compressBound; external;
function crc32; external;
function deflate; external;
function deflateBound; external;
function deflateCopy; external;
function deflateEnd; external;
function deflateInit_; external;
function deflateInit2_; external;
function deflateParams; external;
function deflatePrime; external;
function deflateReset; external;
function deflateSetDictionary; external;
function inflate; external;
function inflateBack; external;
function inflateBackEnd; external;
function inflateBackInit_; external;
function inflateCopy; external;
function inflateEnd; external;
function inflateInit_; external;
function inflateInit2_; external;
function inflateReset; external;
function inflateSetDictionary; external;
function inflateSync; external;

function zcalloc(AppData: Pointer; Items, Size: Integer): Pointer;
begin
  GetMem(Result, Items * Size);
end;

procedure zcfree(AppData, Block: Pointer);
begin
  FreeMem(Block);
end;

{$LINK LinkedObjects\png.obj}
{$LINK LinkedObjects\pngerror.obj}
{$LINK LinkedObjects\pngget.obj}
{$LINK LinkedObjects\pngmem.obj}
{$LINK LinkedObjects\pngpread.obj}
{$LINK LinkedObjects\pngread.obj}
{$LINK LinkedObjects\pngrio.obj}
{$LINK LinkedObjects\pngrtran.obj}
{$LINK LinkedObjects\pngrutil.obj}
{$LINK LinkedObjects\pngset.obj}
{$LINK LinkedObjects\pngtrans.obj}
{$LINK LinkedObjects\pngwio.obj}
{$LINK LinkedObjects\pngwrite.obj}
{$LINK LinkedObjects\pngwtran.obj}
{$LINK LinkedObjects\pngwutil.obj}

function _malloc(Size: Cardinal): Pointer; cdecl;
begin
  GetMem(Result, Size);
end;

procedure _free(Block: Pointer); cdecl;
begin
  FreeMem(Block);
end;

procedure _memset(P: Pointer; B: Byte; count: Integer); cdecl;
begin
  FillChar(P^, count, B);
end;

procedure _memcpy(dest, source: Pointer; count: Integer); cdecl;
begin
  Move(source^, dest^, count);
end;

function deflateInit(var strm: TZStreamRec; level: Integer): Integer;
begin
  Result := deflateInit_(strm, level, ZLIB_VERSION, sizeof(TZStreamRec));
end;

function _deflateInit2_(var strm: TZStreamRec; level, method, windowBits,
  memLevel,
  strategy: Integer): Integer; cdecl;
begin
  Result := deflateInit2_(strm, level, method, windowBits, memLevel, strategy,
    ZLIB_VERSION, sizeof(TZStreamRec));
end;

function _crc32(crc: LongInt; const buf: PAnsiChar; len: Integer): LongInt;
  cdecl;
begin
  Result := crc32(crc, buf, len);
end;

function __ftol: Integer; cdecl;
var
  f: double;
begin
  asm
    lea    eax, f             //  BC++ passes Singles on the FPU stack
    fstp  qword ptr [eax]     //  Delphi passes Singles on the CPU stack
  end;
  Result := Trunc(f);
end;

procedure __streams; cdecl;
begin
end;

var
  __turboFloat: LongBool = False;

procedure _abort; cdecl;
begin
  Abort;
end;

function _deflate(var strm: TZStreamRec; flush: Integer): Integer; cdecl;
begin
  Result := deflate(strm, flush);
end;

function _deflateEnd(var strm: TZStreamRec): Integer; cdecl;
begin
  Result := deflateEnd(strm);
end;

function _deflateReset(var strm: TZStreamRec): Integer; cdecl;
begin
  Result := deflateReset(strm);
end;

function _fabs(x: Double): Double; cdecl;
begin
  Result := Abs(x);
end;

function _fflush(S: TStream): Integer; cdecl;
begin
  Result := 0;
end;

procedure _fprintf; cdecl;
begin
end;

function _fread(var buf; recsize, reccount: Integer; S: TStream): Integer;
  cdecl;
begin
  Result := S.Read(buf, recsize * reccount);
end;

function _fwrite(const buf; recsize, reccount: Integer; S: TStream): Integer;
  cdecl;
begin
  Result := S.Write(buf, recsize * reccount);
end;

procedure _gmtime; cdecl;
begin
end;

function _inflate(var strm: TZStreamRec; flush: Integer): Integer; cdecl;
begin
  Result := inflate(strm, flush);
end;

function _inflateEnd(var strm: TZStreamRec): Integer; cdecl;
begin
  Result := inflateEnd(strm);
end;

function _inflateInit_(var strm: TZStreamRec;
  const version: PAnsiChar; stream_size: Integer): Integer; cdecl;
begin
  Result := inflateInit_(strm, version, stream_size);
end;

function _inflateReset(var strm: TZStreamRec): Integer; cdecl;
begin
  Result := inflateReset(strm);
end;

procedure _longjmp(__jmpb: jmp_buf; __retval: Integer); cdecl;
begin
end;

function _memcmp(P1, P2: Pointer; len: Integer): Boolean; cdecl;
begin
  Result := not CompareMem(P1, P2, len);
end;

procedure _png_calculate_crc(png_ptr: png_structp; ptr: png_bytep;
  length: png_size_t); cdecl;
var
  need_crc: Integer;
begin
  need_crc := 1;

  if (png_ptr.chunk_name[0] and $20) <> 0 then {: ancillary }
  begin
    if (png_ptr.flags and PNG_FLAG_CRC_ANCILLARY_MASK) =
      (PNG_FLAG_CRC_ANCILLARY_USE or PNG_FLAG_CRC_ANCILLARY_NOWARN) then
      need_crc := 0;
  end
  else
  begin {: critical }
    if (png_ptr.flags and PNG_FLAG_CRC_CRITICAL_IGNORE) <> 0 then
      need_crc := 0;
  end;

  if need_crc <> 0 then
    png_ptr.crc := crc32(png_ptr.crc, PAnsiChar(ptr), length);
end;

function _png_calloc(png_ptr: png_structp; size: png_size_t): png_voidp; cdecl;
begin
  GetMem(Result, size);
  FillChar(Result^, size, 0);
end;

function _png_check_cHRM_fixed(png_ptr: png_structp;
  white_x, white_y, red_x, red_y, green_x, green_y,
  blue_x, blue_y: png_fixed_point): Integer; cdecl;
var
  ret: Integer;
  xy, yx: UInt64;
begin
  if png_ptr = nil then
  begin
    Result := 0;
    exit;
  end;
  ret := 1;

  if (white_x < 0) or (white_y <= 0) or
    (red_x < 0) or (red_y < 0) or
    (green_x < 0) or (green_y < 0) or
    (blue_x < 0) or (blue_y < 0) then
  begin
    _png_warning(png_ptr,
      'Ignoring attempt to set negative chromaticity value');
    ret := 0;
  end;
  if white_x > 100000 - white_y then
  begin
    _png_warning(png_ptr, 'Invalid cHRM white point');
    ret := 0;
  end;
  if red_x > 100000 - red_y then
  begin
    _png_warning(png_ptr, 'Invalid cHRM red point');
    ret := 0;
  end;
  if green_x > 100000 - green_y then
  begin
    _png_warning(png_ptr, 'Invalid cHRM green point');
    ret := 0;
  end;
  if blue_x > 100000 - blue_y then
  begin
    _png_warning(png_ptr, 'Invalid cHRM blue point');
    ret := 0;
  end;

  xy := (green_x - red_x) * (blue_y - red_y);
  yx := (green_y - red_y) * (blue_x - red_x);

  if xy = yx then
  begin
    _png_warning(png_ptr,
      'Ignoring attempt to set cHRM RGB triangle with zero area');
    ret := 0;
  end;

  Result := ret;
end;

procedure _png_check_IHDR(png_ptr: png_structp;
  width: png_uint_32; height: png_uint_32; bit_depth: Integer;
  color_type: Integer; interlace_type: Integer; compression_type: Integer;
  filter_type: Integer); cdecl;
var
  error: Integer;
begin
  error := 0;

  {: Check for width and height valid values }
  if width = 0 then
  begin
    _png_warning(png_ptr, 'Image width is zero in IHDR');
    error := 1;
  end;

  if height = 0 then
  begin
    _png_warning(png_ptr, 'Image height is zero in IHDR');
    error := 1;
  end;

  if width > PNG_USER_WIDTH_MAX then
  begin
    _png_warning(png_ptr, 'Image width exceeds user limit in IHDR');
    error := 1;
  end;

  if height > PNG_USER_HEIGHT_MAX then
  begin
    _png_warning(png_ptr, 'Image height exceeds user limit in IHDR');
    error := 1;
  end;

  if width > PNG_UINT_31_MAX then
  begin
    _png_warning(png_ptr, 'Invalid image width in IHDR');
    error := 1;
  end;

  if height > PNG_UINT_31_MAX then
  begin
    _png_warning(png_ptr, 'Invalid image height in IHDR');
    error := 1;
  end;

  if width > (PNG_UINT_32_MAX
    shr 3) {: 8-byte RGBA pixels  }
  - 64 {: bigrowbuf hack  }
  - 1 {: filter byte  }
  - 7 * 8 {: rounding of width to multiple of 8 pixels  }
  - 8 then {: extra max_pixel_depth pad  }
    _png_warning(png_ptr, 'Width is too large for libpng to process pixels');

  {: Check other values  }
  if (bit_depth <> 1) and (bit_depth <> 2) and (bit_depth <> 4) and
    (bit_depth <> 8) and (bit_depth <> 16) then
  begin
    _png_warning(png_ptr, 'Invalid bit depth in IHDR');
    error := 1;
  end;

  if (color_type < 0) or (color_type = 1) or
    (color_type = 5) or (color_type > 6) then
  begin
    _png_warning(png_ptr, 'Invalid color type in IHDR');
    error := 1;
  end;

  if ((color_type = PNG_COLOR_TYPE_PALETTE) and (bit_depth > 8)) or
    (((color_type = PNG_COLOR_TYPE_RGB) or
    (color_type = PNG_COLOR_TYPE_GRAY_ALPHA) or
    (color_type = PNG_COLOR_TYPE_RGB_ALPHA)) and (bit_depth < 8)) then
  begin
    _png_warning(png_ptr, 'Invalid color type/bit depth combination in IHDR');
    error := 1;
  end;

  if interlace_type >= PNG_INTERLACE_LAST then
  begin
    _png_warning(png_ptr, 'Unknown interlace method in IHDR');
    error := 1;
  end;

  if compression_type <> PNG_COMPRESSION_TYPE_BASE then
  begin
    _png_warning(png_ptr, 'Unknown compression method in IHDR');
    error := 1;
  end;

  {: Accept filter_method 64 (intrapixel differencing) only if
     1. Libpng was compiled with PNG_MNG_FEATURES_SUPPORTED and
     2. Libpng did not read a PNG signature (this filter_method is only
        used in PNG datastreams that are embedded in MNG datastreams) and
     3. The application called png_permit_mng_features with a mask that
        included PNG_FLAG_MNG_FILTER_64 and
     4. The filter_method is 64 and
     5. The color_type is RGB or RGBA }
  if (png_ptr.mode and PNG_HAVE_PNG_SIGNATURE <> 0) and
    (png_ptr.mng_features_permitted <> 0) then
    _png_warning(png_ptr, 'MNG features are not allowed in a PNG datastream');

  if filter_type <> PNG_FILTER_TYPE_BASE then
  begin
    if not ((png_ptr.mng_features_permitted and PNG_FLAG_MNG_FILTER_64 <> 0) and
      (filter_type = PNG_INTRAPIXEL_DIFFERENCING) and
      (png_ptr.mode and PNG_HAVE_PNG_SIGNATURE = 0) and
      (color_type = PNG_COLOR_TYPE_RGB) or
      (color_type = PNG_COLOR_TYPE_RGB_ALPHA)) then
    begin
      _png_warning(png_ptr, 'Unknown filter method in IHDR');
      error := 1;
    end;

    if png_ptr.mode and PNG_HAVE_PNG_SIGNATURE <> 0 then
    begin
      _png_warning(png_ptr, 'Invalid filter method in IHDR');
      error := 1;
    end;
  end;

  if error = 1 then
    _png_error(png_ptr, 'Invalid IHDR data');
end;

procedure _png_chunk_error; cdecl;
begin
end;

procedure _png_chunk_warning; cdecl;
begin
end;

procedure _png_flush; cdecl;
begin
end;

procedure _png_free_data(png_ptr: png_structp; info_ptr: png_infop;
  mask: png_uint_32; num: Integer); cdecl;
var
  i: Integer;
begin
  if (png_ptr = nil) or (info_ptr = nil) then
    exit;

  {: Free text item num or (if num = -1) all text items }
  if (mask and PNG_FREE_TEXT <> 0) and (info_ptr.free_me <> 0) then
  begin
    if num <> -1 then
    begin
      if (info_ptr.text <> nil) and (png_text_arrayp(info_ptr.text)[num].key <>
        nil) then
      begin
        _png_free(png_ptr, png_text_arrayp(info_ptr.text)[num].key);
        png_text_arrayp(info_ptr.text)[num].key := nil;
      end;
    end
    else begin
      for i := 0 to info_ptr.num_text - 1 do
        _png_free_data(png_ptr, info_ptr, PNG_FREE_TEXT, i);
      _png_free(png_ptr, info_ptr.text);
      info_ptr.text := nil;
      info_ptr.num_text := 0;
    end;
  end;

  {: Free any tRNS entry }
  if (mask and PNG_FREE_TRNS <> 0) and (info_ptr.free_me <> 0) then
  begin
    _png_free(png_ptr, info_ptr.trans_alpha);
    info_ptr.trans_alpha := nil;
    info_ptr.valid := info_ptr.valid and not PNG_INFO_tRNS;
  end;

  {: Free any pCAL entry }
  if (mask and PNG_FREE_PCAL <> 0) and (info_ptr.free_me <> 0) then
  begin
    _png_free(png_ptr, info_ptr.pcal_purpose);
    _png_free(png_ptr, info_ptr.pcal_units);
    info_ptr.pcal_purpose := nil;
    info_ptr.pcal_units := nil;
    if info_ptr.pcal_params <> nil then
    begin
      for i := 0 to info_ptr.pcal_nparams - 1 do
      begin
        _png_free(png_ptr, png_charp_arrayp(info_ptr.pcal_params)[i]);
        png_charp_arrayp(info_ptr.pcal_params)[i] := nil;
      end;
      _png_free(png_ptr, info_ptr.pcal_params);
      info_ptr.pcal_params := nil;
    end;
    info_ptr.valid := info_ptr.valid and not PNG_INFO_pCAL;
  end;

  {: Free any sCAL entry }
  if (mask and PNG_FREE_SCAL <> 0) and (info_ptr.free_me <> 0) then
  begin
    _png_free(png_ptr, info_ptr.scal_s_width);
    _png_free(png_ptr, info_ptr.scal_s_height);
    info_ptr.scal_s_width := nil;
    info_ptr.scal_s_height := nil;
    info_ptr.valid := info_ptr.valid and not PNG_INFO_sCAL;
  end;

  {: Free any iCCP entry }
  if (mask and PNG_FREE_ICCP <> 0) and (info_ptr.free_me <> 0) then
  begin
    _png_free(png_ptr, info_ptr.iccp_name);
    _png_free(png_ptr, info_ptr.iccp_profile);
    info_ptr.iccp_name := nil;
    info_ptr.iccp_profile := nil;
    info_ptr.valid := info_ptr.valid and not PNG_INFO_iCCP;
  end;

  {: Free a given sPLT entry, or (if num == -1) all sPLT entries }
  if (mask and PNG_FREE_SPLT <> 0) and (info_ptr.free_me <> 0) then
  begin
    if num <> -1 then
    begin
      if info_ptr.splt_palettes <> nil then
      begin
        _png_free(png_ptr,
          png_sPLT_tp_arrayp(info_ptr.splt_palettes)[num].name);
        _png_free(png_ptr,
          png_sPLT_tp_arrayp(info_ptr.splt_palettes)[num].entries);
        png_sPLT_tp_arrayp(info_ptr.splt_palettes)[num].name := nil;
        png_sPLT_tp_arrayp(info_ptr.splt_palettes)[num].entries := nil;
      end;
    end
    else
    begin
      if info_ptr.splt_palettes_num <> 0 then
      begin
        for i := 0 to info_ptr.splt_palettes_num - 1 do
          _png_free_data(png_ptr, info_ptr, PNG_FREE_SPLT, i);

        _png_free(png_ptr, info_ptr.splt_palettes);
        info_ptr.splt_palettes := nil;
        info_ptr.splt_palettes_num := 0;
      end;
      info_ptr.valid := info_ptr.valid and not PNG_INFO_sPLT;
    end;
  end;

  if png_ptr.unknown_chunk.data <> nil then
  begin
    _png_free(png_ptr, png_ptr.unknown_chunk.data);
    png_ptr.unknown_chunk.data := nil;
  end;

  if (mask and PNG_FREE_UNKN <> 0) and (info_ptr.free_me <> 0) then
  begin
    if num <> -1 then
    begin
      if info_ptr.unknown_chunks <> nil then
      begin
        _png_free(png_ptr,
          png_unknown_chunk_arrayp(info_ptr.unknown_chunks)[num].data);
        png_unknown_chunk_arrayp(info_ptr.unknown_chunks)[num].data := nil;
      end;
    end
    else
    begin
      if info_ptr.unknown_chunks_num <> 0 then
      begin
        for i := 0 to info_ptr.unknown_chunks_num - 1 do
          _png_free_data(png_ptr, info_ptr, PNG_FREE_UNKN, i);

        _png_free(png_ptr, info_ptr.unknown_chunks);
        info_ptr.unknown_chunks := nil;
        info_ptr.unknown_chunks_num := 0;
      end;
    end;
  end;

  {: Free any hIST entry }
  if (mask and PNG_FREE_HIST <> 0) and (info_ptr.free_me <> 0) then
  begin
    _png_free(png_ptr, info_ptr.hist);
    info_ptr.hist := nil;
    info_ptr.valid := info_ptr.valid and not PNG_INFO_hIST;
  end;

  {: Free any PLTE entry that was internally allocated }
  if (mask and PNG_FREE_PLTE <> 0) and (info_ptr.free_me <> 0) then
  begin
    FreeMem(info_ptr.palette);
    info_ptr.palette := nil;
    info_ptr.valid := info_ptr.valid and not PNG_INFO_PLTE;
    info_ptr.num_palette := 0;
  end;

  {: Free any image bits attached to the info structure }
  if (mask and PNG_FREE_ROWS <> 0) and (info_ptr.free_me <> 0) then
  begin
    if info_ptr.row_pointers <> nil then
    begin
      for i := 0 to info_ptr.height - 1 do
      begin
        _png_free(png_ptr, png_bytep_arrayp(info_ptr.row_pointers)[i]);
        png_bytep_arrayp(info_ptr.row_pointers)[i] := nil;
      end;
      _png_free(png_ptr, info_ptr.row_pointers);
      info_ptr.row_pointers := nil;
    end;
    info_ptr.valid := info_ptr.valid and not PNG_INFO_IDAT;
  end;

  if num = -1 then
    info_ptr.free_me := info_ptr.free_me and not mask
  else
    info_ptr.free_me := info_ptr.free_me and not (mask and not PNG_FREE_MUL);
end;

function _png_get_header_ver(png_ptr: png_structp): png_charp; cdecl;
begin
  Result := png_charp(ZLIB_VERSION);
end;

function _png_get_rowbytes(png_ptr: png_structp; info_ptr: png_infop):
  png_size_t; cdecl;
begin
  if (png_ptr <> nil) and (info_ptr <> nil) then
    Result := info_ptr.rowbytes
  else
    Result := 0;
end;

procedure _png_get_sBIT; cdecl;
begin
end;

function _png_get_valid(png_ptr: png_structp; info_ptr: png_infop; flag:
  png_uint_32): png_uint_32; cdecl;
begin
  if (png_ptr <> nil) and (info_ptr <> nil) then
    Result := info_ptr.valid and flag
  else
    Result := 0;
end;

function _png_handle_as_unknown(png_ptr: png_structp; chunk_name: png_bytep):
  Integer; cdecl;
var
  i: Integer;
  p: png_bytep;
begin
  {: Check chunk_name and return "keep" value if it's on the list, else 0 }
  if (png_ptr = nil) or (chunk_name = nil) or (png_ptr.num_chunk_list <= 0) then
  begin
    Result := 0;
    exit;
  end;
  p := png_ptr.chunk_list;
  Inc(p, png_ptr.num_chunk_list * 5 - 5);
  for i := png_ptr.num_chunk_list downto 0 do
  begin
    if CompareMem(chunk_name, p, 4) then
    begin
      Inc(PByte(p), 4);
      Result := PInteger(p)^;
      exit;
    end;
    Dec(PByte(p), 5);
  end;
  Result := 0;
end;

procedure _png_info_destroy(png_ptr: png_structp; info_ptr: png_infop); cdecl;
begin
  _png_free_data(png_ptr, info_ptr, PNG_FREE_ALL, -1);

  if png_ptr.num_chunk_list > 0 then
  begin
    _png_free(png_ptr, png_ptr.chunk_list);
    png_ptr.chunk_list := nil;
    png_ptr.num_chunk_list := 0;
  end;
  _png_info_init_3(@info_ptr, SizeOf(png_info));
end;

procedure _png_init_read_transformations(png_ptr: png_structp); cdecl;
var
  color_type: Integer;
  i, istop, k: Integer;
  back, back_1: png_color;
  palette: png_colorp;
  num_palette: Integer;
  g, gs, m: double;
  v, w: png_byte;
  sr, sg, sb: Integer;

  procedure png_composite(out composite: png_byte; fg, alpha, bg:
    png_uint_16);
  var
    temp: png_uint_16;
  begin
    temp := fg * alpha + bg * (255 - alpha) + 128;
    composite := png_byte((temp + (temp shr 8)) shr 8);
  end;

begin
  color_type := png_ptr.color_type;

  if (png_ptr.transformations and PNG_BACKGROUND_EXPAND <> 0) and
    (color_type and PNG_COLOR_MASK_COLOR = 0) then
    png_ptr.mode := png_ptr.mode or PNG_BACKGROUND_IS_GRAY
  else if (png_ptr.transformations and PNG_BACKGROUND <> 0) and
    (png_ptr.transformations and PNG_BACKGROUND_EXPAND = 0) and
    (png_ptr.transformations and PNG_GRAY_TO_RGB <> 0) and
    (png_ptr.background.red = png_ptr.background.green) and
    (png_ptr.background.red = png_ptr.background.blue) then
  begin
    png_ptr.mode := png_ptr.mode or PNG_BACKGROUND_IS_GRAY;
    png_ptr.background.gray := png_ptr.background.red;
  end;

  if (png_ptr.transformations and PNG_BACKGROUND_EXPAND <> 0) and
    (png_ptr.transformations and PNG_EXPAND <> 0) then
  begin
    if color_type and PNG_COLOR_MASK_COLOR = 0 then
    begin
      case png_ptr.bit_depth of
        1:
          begin
            png_ptr.background.gray := png_ptr.background.gray * $FF;
            png_ptr.background.red := png_ptr.background.gray;
            png_ptr.background.green := png_ptr.background.gray;
            png_ptr.background.blue := png_ptr.background.gray;
            if png_ptr.transformations and PNG_EXPAND_tRNS = 0 then
            begin
              png_ptr.trans_color.gray := png_ptr.trans_color.gray * $FF;
              png_ptr.trans_color.red := png_ptr.trans_color.gray;
              png_ptr.trans_color.green := png_ptr.trans_color.gray;
              png_ptr.trans_color.blue := png_ptr.trans_color.gray;
            end;
          end;

        2:
          begin
            png_ptr.background.gray := png_ptr.background.gray * $55;
            png_ptr.background.red := png_ptr.background.gray;
            png_ptr.background.green := png_ptr.background.gray;
            png_ptr.background.blue := png_ptr.background.gray;
            if png_ptr.transformations and PNG_EXPAND_tRNS = 0 then
            begin
              png_ptr.trans_color.gray := png_ptr.trans_color.gray * $55;
              png_ptr.trans_color.red := png_ptr.trans_color.gray;
              png_ptr.trans_color.green := png_ptr.trans_color.gray;
              png_ptr.trans_color.blue := png_ptr.trans_color.gray;
            end;
          end;

        4:
          begin
            png_ptr.background.gray := png_ptr.background.gray * $11;
            png_ptr.background.red := png_ptr.background.gray;
            png_ptr.background.green := png_ptr.background.gray;
            png_ptr.background.blue := png_ptr.background.gray;
            if png_ptr.transformations and PNG_EXPAND_tRNS = 0 then
            begin
              png_ptr.trans_color.gray := png_ptr.trans_color.gray * $11;
              png_ptr.trans_color.red := png_ptr.trans_color.gray;
              png_ptr.trans_color.green := png_ptr.trans_color.gray;
              png_ptr.trans_color.blue := png_ptr.trans_color.gray;
            end;
          end;

        8: ;

        16:
          begin
            png_ptr.background.red := png_ptr.background.gray;
            png_ptr.background.green := png_ptr.background.gray;
            png_ptr.background.blue := png_ptr.background.gray;
          end;
      end;
    end
    else if color_type = PNG_COLOR_TYPE_PALETTE then
    begin
      png_ptr.background.red :=
        png_ptr.palette[png_ptr.background.index].red;
      png_ptr.background.green :=
        png_ptr.palette[png_ptr.background.index].green;
      png_ptr.background.blue :=
        png_ptr.palette[png_ptr.background.index].blue;

      if png_ptr.transformations and PNG_INVERT_ALPHA <> 0 then
      begin
        if png_ptr.transformations and PNG_EXPAND_tRNS = 0 then
        begin
          istop := png_ptr.num_trans;
          for i := 0 to istop - 1 do
            png_ptr.trans_alpha[i] := 255 - png_ptr.trans_alpha[i];
        end;
      end;

    end;
  end;

  png_ptr.background_1 := png_ptr.background;

  if (color_type = PNG_COLOR_TYPE_PALETTE) and (png_ptr.num_trans <> 0)
    and (abs(png_ptr.screen_gamma * png_ptr.gamma - 1.0)
    < PNG_GAMMA_THRESHOLD) then
  begin
    k := 0;
    for i := 0 to png_ptr.num_trans - 1 do
      if (png_ptr.trans_alpha[i] <> 0) and (png_ptr.trans_alpha[i] <> $FF) then
        k := 1;
    if k = 0 then
      png_ptr.transformations := png_ptr.transformations and not PNG_GAMMA;

    if (png_ptr.transformations and (PNG_GAMMA or PNG_RGB_TO_GRAY) <> 0) and
      (png_ptr.gamma <> 0.0) then
      _png_build_gamma_table(png_ptr, png_ptr.bit_depth);

    if png_ptr.transformations and PNG_BACKGROUND <> 0 then
    begin
      if color_type = PNG_COLOR_TYPE_PALETTE then
      begin
        palette := png_ptr.palette;
        num_palette := png_ptr.num_palette;
        if png_ptr.background_gamma_type = PNG_BACKGROUND_GAMMA_FILE then
        begin
          back.red := png_ptr.gamma_table[png_ptr.background.red];
          back.green := png_ptr.gamma_table[png_ptr.background.green];
          back.blue := png_ptr.gamma_table[png_ptr.background.blue];

          back_1.red := png_ptr.gamma_to_1[png_ptr.background.red];
          back_1.green := png_ptr.gamma_to_1[png_ptr.background.green];
          back_1.blue := png_ptr.gamma_to_1[png_ptr.background.blue];
        end
        else
        begin

          case png_ptr.background_gamma_type of
            PNG_BACKGROUND_GAMMA_SCREEN:
              begin
                g := png_ptr.screen_gamma;
                gs := 1.0;
              end;

            PNG_BACKGROUND_GAMMA_FILE:
              begin
                g := 1.0 / (png_ptr.gamma);
                gs := 1.0 / (png_ptr.gamma * png_ptr.screen_gamma);
              end;

            PNG_BACKGROUND_GAMMA_UNIQUE:
              begin
                g := 1.0 / (png_ptr.background_gamma);
                gs := 1.0 / (png_ptr.background_gamma *
                  png_ptr.screen_gamma);
              end;
          else
            g := 1.0;
            gs := 1.0;
          end;

          if abs(gs - 1.0) < PNG_GAMMA_THRESHOLD then
          begin
            back.red := png_ptr.background.red;
            back.green := png_ptr.background.green;
            back.blue := png_ptr.background.blue;
          end
          else
          begin
            back.red := Floor(power(
              png_ptr.background.red / 255.0, gs) * 255.0 + 0.5);
            back.green := Floor(power(
              png_ptr.background.green / 255.0, gs) * 255.0 + 0.5);
            back.blue := Floor(power(
              png_ptr.background.blue / 255.0, gs) * 255.0 + 0.5);
          end;

          back_1.red := Floor(power(
            png_ptr.background.red / 255.0, g) * 255.0 + 0.5);
          back_1.green := Floor(power(
            png_ptr.background.green / 255.0, g) * 255.0 + 0.5);
          back_1.blue := Floor(power(
            png_ptr.background.blue / 255.0, g) * 255.0 + 0.5);
        end;
        for i := 0 to num_palette - 1 do
        begin
          if (i < png_ptr.num_trans) and (png_ptr.trans_alpha[i] <> $FF) then
          begin
            if png_ptr.trans_alpha[i] = 0 then
              palette[i] := back
            else
            begin
              v := png_ptr.gamma_to_1[palette[i].red];
              png_composite(w, v, png_ptr.trans_alpha[i], back_1.red);
              palette[i].red := png_ptr.gamma_from_1[w];

              v := png_ptr.gamma_to_1[palette[i].green];
              png_composite(w, v, png_ptr.trans_alpha[i], back_1.green);
              palette[i].green := png_ptr.gamma_from_1[w];

              v := png_ptr.gamma_to_1[palette[i].blue];
              png_composite(w, v, png_ptr.trans_alpha[i], back_1.blue);
              palette[i].blue := png_ptr.gamma_from_1[w];
            end;
          end
          else
          begin
            palette[i].red := png_ptr.gamma_table[palette[i].red];
            palette[i].green := png_ptr.gamma_table[palette[i].green];
            palette[i].blue := png_ptr.gamma_table[palette[i].blue];
          end;
          png_ptr.transformations := png_ptr.transformations and not
            PNG_BACKGROUND;
          png_ptr.transformations := png_ptr.transformations and not PNG_GAMMA;
          png_ptr.transformations := png_ptr.transformations or PNG_STRIP_ALPHA;
        end;
      end
      else
      begin

        m := (1 shl png_ptr.bit_depth) - 1;
        g := 1.0;
        gs := 1.0;

        case png_ptr.background_gamma_type of
          PNG_BACKGROUND_GAMMA_SCREEN:
            begin
              g := png_ptr.screen_gamma;
              gs := 1.0;
            end;

          PNG_BACKGROUND_GAMMA_FILE:
            begin
              g := 1.0 / png_ptr.gamma;
              gs := 1.0 / png_ptr.gamma * png_ptr.screen_gamma;
            end;

          PNG_BACKGROUND_GAMMA_UNIQUE:
            begin
              g := 1.0 / (png_ptr.background_gamma);
              gs := 1.0 / (png_ptr.background_gamma * png_ptr.screen_gamma);
            end;
        end;

        png_ptr.background_1.gray := Floor(power(
          png_ptr.background.gray / m, g) * m + 0.5);
        png_ptr.background.gray := Floor(power(
          png_ptr.background.gray / m, gs) * m + 0.5);

        if (png_ptr.background.red <> png_ptr.background.green) or
          (png_ptr.background.red <> png_ptr.background.blue) or
          (png_ptr.background.red <> png_ptr.background.gray) then
        begin
          png_ptr.background_1.red := Floor(power(
            png_ptr.background.red / m, g) * m + 0.5);
          png_ptr.background_1.green := Floor(power(
            png_ptr.background.green / m, g) * m + 0.5);
          png_ptr.background_1.blue := Floor(power(
            png_ptr.background.blue / m, g) * m + 0.5);
          png_ptr.background.red := Floor(power(
            png_ptr.background.red / m, gs) * m + 0.5);
          png_ptr.background.green := Floor(power(
            png_ptr.background.green / m, gs) * m + 0.5);
          png_ptr.background.blue := Floor(power(
            png_ptr.background.blue / m, gs) * m + 0.5);
        end
        else
        begin
          png_ptr.background_1.red := png_ptr.background_1.gray;
          png_ptr.background_1.green := png_ptr.background_1.gray;
          png_ptr.background_1.blue := png_ptr.background_1.gray;
          png_ptr.background.red := png_ptr.background.gray;
          png_ptr.background.green := png_ptr.background.gray;
          png_ptr.background.blue := png_ptr.background.gray;
        end;
      end;
    end
    else if color_type = PNG_COLOR_TYPE_PALETTE then
    begin
      palette := png_ptr.palette;
      num_palette := png_ptr.num_palette;

      for i := 0 to num_palette - 1 do
      begin
        palette[i].red := png_ptr.gamma_table[palette[i].red];
        palette[i].green := png_ptr.gamma_table[palette[i].green];
        palette[i].blue := png_ptr.gamma_table[palette[i].blue];
      end;

      png_ptr.transformations := png_ptr.transformations and not PNG_GAMMA;
    end;
  end
  else if (png_ptr.transformations and PNG_BACKGROUND <> 0) and
    (color_type = PNG_COLOR_TYPE_PALETTE) then
  begin
    istop := png_ptr.num_trans;
    palette := png_ptr.palette;

    back.red := png_ptr.background.red;
    back.green := png_ptr.background.green;
    back.blue := png_ptr.background.blue;

    for i := 0 to istop - 1 do
    begin
      if png_ptr.trans_alpha[i] = 0 then
        palette[i] := back

      else if png_ptr.trans_alpha[i] <> $FF then
      begin
        png_composite(palette[i].red, palette[i].red,
          png_ptr.trans_alpha[i], back.red);
        png_composite(palette[i].green, palette[i].green,
          png_ptr.trans_alpha[i], back.green);
        png_composite(palette[i].blue, palette[i].blue,
          png_ptr.trans_alpha[i], back.blue);
      end;
    end;

    png_ptr.transformations := png_ptr.transformations and not PNG_BACKGROUND;
    png_ptr.transformations := png_ptr.transformations or PNG_STRIP_ALPHA;
  end;
  if (png_ptr.transformations and PNG_SHIFT <> 0) and
    (color_type = PNG_COLOR_TYPE_PALETTE) then
  begin
    istop := png_ptr.num_palette;
    sr := 8 - png_ptr.sig_bit.red;
    sg := 8 - png_ptr.sig_bit.green;
    sb := 8 - png_ptr.sig_bit.blue;

    if (sr < 0) or (sr > 8) then
      sr := 0;
    if (sg < 0) or (sg > 8) then
      sg := 0;
    if (sb < 0) or (sb > 8) then
      sb := 0;
    for i := 0 to istop - 1 do
    begin
      png_ptr.palette[i].red := png_ptr.palette[i].red shr sr;
      png_ptr.palette[i].green := png_ptr.palette[i].green shr sg;
      png_ptr.palette[i].blue := png_ptr.palette[i].blue shr sb;
    end;
  end;
end;

function _png_malloc_warn(png_ptr: png_structp; size: png_size_t): png_voidp;
  cdecl;
var
  save_flags: png_uint_32;
begin
  Result := nil;
  if png_ptr = nil then
    exit;
  save_flags := png_ptr.flags;
  png_ptr.flags := png_ptr.flags or PNG_FLAG_MALLOC_NULL_MEM_OK;
  Result := _png_malloc(png_ptr, size);
  png_ptr.flags := save_flags;
end;

procedure _png_reset_crc(png_ptr: png_structp); cdecl;
begin
  png_ptr.crc := crc32(0, nil, 0);
end;

procedure _png_set_error_fn(png_ptr: png_structp; error_ptr: png_voidp;
  error_fn: png_error_ptr; warning_fn: png_error_ptr); cdecl;
begin
  if png_ptr = nil then
    exit;
  png_ptr.error_ptr := error_ptr;
  png_ptr.error_fn := error_fn;
  png_ptr.warning_fn := warning_fn;
end;

procedure _png_set_filler; cdecl;
begin
end;

function _png_set_longjmp_fn(png_ptr: png_structp; longjmp_fn: png_longjmp_ptr;
  jmp_buf_size: png_size_t): jmp_bufp; cdecl;
begin
  if (png_ptr = nil) or (jmp_buf_size <> SizeOf(jmp_buf)) then
  begin
    Result := nil;
    exit;
  end;

  png_ptr.longjmp_fn := longjmp_fn;
  Result := @png_ptr.jmpbuf;
end;

procedure _png_set_mem_fn(png_ptr: png_structp; mem_ptr: png_voidp; malloc_fn:
  png_malloc_ptr; free_fn: png_free_ptr); cdecl;
begin
  if png_ptr <> nil then
  begin
    png_ptr.mem_ptr := mem_ptr;
    png_ptr.malloc_fn := malloc_fn;
    png_ptr.free_fn := free_fn;
  end;
end;

procedure _png_set_packing; cdecl;
begin
end;

procedure _png_set_write_fn(png_ptr: png_structp; io_ptr: png_voidp;
  write_data_fn: png_rw_ptr; output_flush_fn: png_flush_ptr); cdecl;
begin
  if png_ptr = nil then
    exit;

  png_ptr.io_ptr := io_ptr;

  if Assigned(write_data_fn) then
    png_ptr.write_data_fn := write_data_fn
  else
    png_ptr.write_data_fn := pngWriteFn;
  png_ptr.write_data_fn := write_data_fn;

  if Assigned(png_ptr.read_data_fn) then
  begin
    png_ptr.read_data_fn := nil;
    _png_warning(png_ptr,
      'Attempted to set both read_data_fn and write_data_fn in');
    _png_warning(png_ptr,
      'the same structure. Resetting read_data_fn to NULL');
  end;
end;

procedure _png_write_data(png_ptr: png_structp; data: png_bytep;
  length: png_size_t); cdecl;
begin
  if Assigned(png_ptr.write_data_fn) then
    png_ptr.write_data_fn(png_ptr, data, length)
  else
    _png_error(png_ptr, 'Call to NULL write function');
end;

procedure _png_write_flush(png_ptr: png_structp); cdecl;
begin

end;

// Function to allocate memory for zlib.

function _png_zalloc(png_ptr: Pointer; items: Cardinal; size: Cardinal):
  Pointer;
var
  ptr: png_voidp;
  num_bytes: png_size_t;
begin
  if png_ptr = nil then
  begin
    Result := nil;
    exit;
  end;
  if items > Cardinal(-1) div size then
  begin
    _png_warning(png_ptr, 'Potential overflow in png_zalloc()');
    Result := nil;
    exit;
  end;
  num_bytes := png_size_t(items) * size;
  GetMem(ptr, num_bytes);
  Result := ptr;
end;

procedure _png_zfree(png_ptr: Pointer; ptr: Pointer);
begin
  FreeMem(ptr);
end;

function _pow(x, y: double): double; cdecl;
begin
  Result := Power(x, y);
end;

function _setjmp(__jmpb: jmp_buf): Integer; cdecl;
begin
  Result := 0;
end;

procedure _snprintf; cdecl;
begin
end;

function _strlen(str: png_charp): Integer; cdecl;
begin
  Result := length(str);
end;

procedure _strtod; cdecl;
begin
end;

procedure _wcscpy; cdecl;
begin
end;

procedure _z_errmsg; cdecl;
begin
end;

procedure pngReadFn(png_ptr: png_structp; data: png_bytep; length: png_size_t);
var
  fs: TStream;
begin
  fs := TStream(_png_get_io_ptr(png_ptr));
  Assert(Assigned(data), 'Attempt to read from null file pointer');
  fs.Read(data^, length)
end;

procedure pngWriteFn(png_ptr: png_structp; data: png_bytep; length: png_size_t);
var
  fs: TStream;
begin
  fs := TStream(_png_get_io_ptr(png_ptr));
  Assert(Assigned(data), 'Attempt to write to null file pointer');
  fs.Write(data^, length);
end;

procedure pngErrorFn(struct: png_structp; str: png_const_charp); cdecl;
begin
{$IFDEF GLS_LOGGING}
  Log.Log(string(str), lkError);
{$ENDIF}
end;

procedure pngWarnFn(struct: png_structp; str: png_const_charp); cdecl;
begin
{$IFDEF GLS_LOGGING}
  Log.Log(string(str), lkWarning);
{$ENDIF}
end;

function _png_sig_cmp; external;
function _png_create_read_struct; external;
procedure _png_destroy_read_struct; external;
function _png_create_info_struct; external;
procedure _png_destroy_info_struct; external;
procedure _png_set_read_fn; external;
procedure _png_set_sig_bytes; external;
function _png_get_image_width; external;
function _png_get_image_height; external;
function _png_get_color_type; external;
function _png_get_bit_depth; external;
function _png_get_filter_type; external;
function _png_get_interlace_type; external;
function _png_get_compression_type; external;
procedure _png_read_info; external;
function _png_get_io_ptr; external;
procedure _png_warning; external;
procedure _png_error; external;
procedure _png_set_palette_to_rgb; external;
procedure _png_set_expand_gray_1_2_4_to_8; external;
procedure _png_set_tRNS_to_alpha; external;
procedure _png_read_update_info; external;
procedure _png_read_image; external;
procedure _png_build_gamma_table; external;
procedure _png_read_end; external;
procedure _png_free; external;
procedure _png_info_init_3; external;
function _png_get_channels; external;
function _png_create_write_struct; external;
procedure _png_destroy_write_struct; external;
procedure _png_set_IHDR; external;
procedure _png_write_image; external;
procedure _png_write_end; external;
procedure _png_write_info; external;
function _png_malloc; external;

end.

