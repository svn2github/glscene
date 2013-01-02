// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'LIBPNG.pas' rev: 24.00 (Win32)

#ifndef LibpngHPP
#define LibpngHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <GLSLog.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Libpng
{
//-- type declarations -------------------------------------------------------
typedef int time_t;

typedef int Int;

typedef unsigned png_uint_32;

typedef int png_int_32;

typedef System::Word png_uint_16;

typedef short png_int_16;

typedef System::Byte png_byte;

typedef unsigned *ppng_uint_32;

typedef int *ppng_int_32;

typedef System::Word *ppng_uint_16;

typedef short *ppng_int_16;

typedef System::Byte *ppng_byte;

typedef ppng_uint_32 *pppng_uint_32;

typedef ppng_int_32 *pppng_int_32;

typedef ppng_uint_16 *pppng_uint_16;

typedef System::Word *png_uint_16p;

typedef png_uint_16p *ppng_uint_16p;

typedef ppng_int_16 *pppng_int_16;

typedef ppng_byte *pppng_byte;

typedef unsigned png_size_t;

typedef int png_fixed_point;

typedef int *ppng_fixed_point;

typedef ppng_fixed_point *pppng_fixed_point;

typedef void * png_voidp;

typedef System::StaticArray<System::Byte, 1073741823> png_byte_array;

typedef png_byte_array *png_bytep;

typedef png_bytep *ppng_bytep;

typedef System::WideChar * png_const_charp;

typedef System::WideChar * png_charp;

typedef System::WideChar * *ppng_charp;

typedef ppng_fixed_point png_fixed_point_p;

typedef void * TFile;

typedef System::file *png_FILE_p;

typedef System::PDouble png_doublep;

typedef System::StaticArray<png_bytep, 268435456> png_bytep_array;

typedef png_bytep_array *png_bytep_arrayp;

typedef png_bytep *png_bytepp;

typedef unsigned *png_uint_32p;

typedef System::PInteger png_int_32p;

typedef png_uint_16p *png_uint_16pp;

typedef System::PShortInt png_int_16p;

typedef System::StaticArray<System::WideChar *, 268435456> png_charp_array;

typedef png_charp_array *png_charp_arrayp;

typedef System::WideChar * *png_charpp;

typedef png_charpp *ppng_charpp;

struct DECLSPEC_DRECORD jmp_buf
{
public:
	unsigned j_ebp;
	unsigned j_ebx;
	unsigned j_edi;
	unsigned j_esi;
	unsigned j_esp;
	unsigned j_ret;
	unsigned j_excep;
	unsigned j_context;
};


typedef jmp_buf *jmp_bufp;

struct DECLSPEC_DRECORD png_color
{
public:
	System::Byte red;
	System::Byte green;
	System::Byte blue;
};


typedef System::StaticArray<png_color, 357913942> png_color_array;

typedef png_color_array *png_colorp;

typedef png_colorp *png_colorpp;

struct DECLSPEC_DRECORD png_color_8
{
public:
	System::Byte red;
	System::Byte green;
	System::Byte blue;
	System::Byte gray;
	System::Byte alpha;
};


typedef png_color_8 *png_color_8p;

typedef png_color_8p *png_color_8pp;

typedef png_color_8p *ppng_color_8p;

typedef png_colorp *ppng_colorp;

struct DECLSPEC_DRECORD png_color_16
{
public:
	System::Byte index;
	System::Word red;
	System::Word green;
	System::Word blue;
	System::Word gray;
};


typedef png_color_16 *png_color_16p;

typedef png_color_16p *png_color_16pp;

typedef png_color_16p *ppng_color_16p;

struct DECLSPEC_DRECORD png_unknown_chunk
{
public:
	System::StaticArray<System::Byte, 5> Name;
	png_byte_array *Data;
	unsigned size;
};


typedef System::StaticArray<png_unknown_chunk, 67108864> png_unknown_chunk_array;

typedef png_unknown_chunk_array *png_unknown_chunk_arrayp;

typedef png_unknown_chunk *png_unknown_chunkp;

typedef png_unknown_chunkp *png_unknown_chunkpp;

struct DECLSPEC_DRECORD png_text
{
public:
	int compression;
	System::WideChar *key;
	System::WideChar *Text;
	unsigned text_length;
	unsigned itxt_length;
	System::WideChar *lang;
	System::WideChar *lang_key;
};


typedef System::StaticArray<png_text, 38347923> png_text_array;

typedef png_text_array *png_text_arrayp;

typedef png_text *png_textp;

typedef png_textp *png_textpp;

typedef png_textp *ppng_textp;

struct DECLSPEC_DRECORD png_time
{
public:
	System::Word year;
	System::Byte month;
	System::Byte day;
	System::Byte hour;
	System::Byte minute;
	System::Byte second;
};


typedef png_time *png_timep;

typedef png_timep *png_timepp;

typedef png_timep *ppng_timep;

struct DECLSPEC_DRECORD png_sPLT_entry
{
public:
	System::Word red;
	System::Word green;
	System::Word blue;
	System::Word alpha;
	System::Word frequency;
};


typedef png_sPLT_entry *png_sPLT_entryp;

typedef png_sPLT_entryp *png_sPLT_entrypp;

struct DECLSPEC_DRECORD png_sPLT_t
{
public:
	System::WideChar *Name;
	System::Byte depth;
	png_sPLT_entry *entries;
	int nentries;
};


typedef System::StaticArray<png_sPLT_t, 67108864> png_sPLT_tp_array;

typedef png_sPLT_tp_array *png_sPLT_tp_arrayp;

typedef png_sPLT_t *png_sPLT_tp;

typedef png_sPLT_tp *png_sPLT_tpp;

struct png_struct;
typedef png_struct *png_structp;

typedef png_structp *png_structpp;

struct DECLSPEC_DRECORD png_info
{
public:
	unsigned Width;
	unsigned Height;
	unsigned valid;
	unsigned rowbytes;
	png_color_array *palette;
	System::Word num_palette;
	System::Word num_trans;
	System::Byte bit_depth;
	System::Byte color_type;
	System::Byte compression_type;
	System::Byte filter_type;
	System::Byte interlace_type;
	System::Byte channels;
	System::Byte pixel_depth;
	System::Byte spare_byte;
	System::StaticArray<System::Byte, 8> signature;
	float gamma;
	System::Byte srgb_intent;
	int num_text;
	int max_text;
	png_text *Text;
	png_time mod_time;
	png_color_8 sig_bit;
	png_byte_array *trans_alpha;
	png_color_16 trans_color;
	png_color_16 background;
	int x_offset;
	int y_offset;
	System::Byte offset_unit_type;
	unsigned x_pixels_per_unit;
	unsigned y_pixels_per_unit;
	System::Byte phys_unit_type;
	System::Word *hist;
	float x_white;
	float y_white;
	float x_red;
	float y_red;
	float x_green;
	float y_green;
	float x_blue;
	float y_blue;
	System::WideChar *pcal_purpose;
	int pcal_X0;
	int pcal_X1;
	System::WideChar *pcal_units;
	System::WideChar * *pcal_params;
	System::Byte pcal_type;
	System::Byte pcal_nparams;
	unsigned free_me;
	png_unknown_chunk *unknown_chunks;
	unsigned unknown_chunks_num;
	System::WideChar *iccp_name;
	System::WideChar *iccp_profile;
	unsigned iccp_proflen;
	System::Byte iccp_compression;
	png_sPLT_t *splt_palettes;
	unsigned splt_palettes_num;
	System::Byte scal_unit;
	double scal_pixel_width;
	double scal_pixel_height;
	System::WideChar *scal_s_width;
	System::WideChar *scal_s_height;
	png_bytep *row_pointers;
	int int_gamma;
	int int_x_white;
	int int_y_white;
	int int_x_red;
	int int_y_red;
	int int_x_green;
	int int_y_green;
	int int_x_blue;
	int int_y_blue;
};


typedef png_info *png_infop;

typedef png_infop *png_infopp;

struct DECLSPEC_DRECORD png_row_info
{
public:
	unsigned Width;
	unsigned rowbytes;
	System::Byte color_type;
	System::Byte bit_depth;
	System::Byte channels;
	System::Byte pixel_depth;
};


typedef png_row_info *png_row_infop;

typedef void * __cdecl (*TAlloc)(void * AppData, int Items, int Size);

typedef void __cdecl (*TFree)(void * AppData, void * Block);

typedef int __cdecl (*TInFunc)(void * opaque, System::PByte &buf);

typedef int __fastcall (*TOutFunc)(void * opaque, System::PByte buf, int size);

typedef void __cdecl (*png_error_ptr)(png_structp Struct, System::WideChar * str);

typedef void __cdecl (*png_rw_ptr)(png_structp Struct, png_bytep Data, unsigned size);

typedef void __cdecl (*png_longjmp_ptr)(const jmp_buf jb, int i);

typedef void __cdecl (*png_user_transform_ptr)(png_structp Struct, png_row_infop row_info, png_bytep b);

typedef void __cdecl (*png_flush_ptr)(png_structp Struct);

typedef void __cdecl (*png_read_status_ptr)(png_structp Struct, unsigned ui, int i);

typedef void __cdecl (*png_write_status_ptr)(png_structp Struct, unsigned ui, int i);

typedef void __cdecl (*png_progressive_info_ptr)(png_structp Struct, png_infop info);

typedef void __cdecl (*png_progressive_end_ptr)(png_structp Struct, png_infop info);

typedef void __cdecl (*png_progressive_row_ptr)(png_structp Struct, png_bytep bp, unsigned ui, int i);

typedef void * __fastcall (*png_malloc_ptr)(png_structp Struct, unsigned size);

typedef void __fastcall (*png_free_ptr)(png_structp Struct, void * ptr);

typedef int __fastcall (*png_user_chunk_ptr)(png_structp Struct, png_unknown_chunkp chunk);

#pragma pack(push,1)
struct DECLSPEC_DRECORD TZStreamRec
{
public:
	char *next_in;
	int avail_in;
	int total_in;
	char *next_out;
	int avail_out;
	int total_out;
	char *msg;
	void *internal;
	TAlloc zalloc;
	TFree zfree;
	void *AppData;
	int data_type;
	int adler;
	int reserved;
};
#pragma pack(pop)


struct DECLSPEC_DRECORD png_struct
{
public:
	jmp_buf jmpbuf;
	png_longjmp_ptr longjmp_fn;
	png_error_ptr error_fn;
	png_error_ptr warning_fn;
	void *error_ptr;
	png_rw_ptr write_data_fn;
	png_rw_ptr read_data_fn;
	void *io_ptr;
	png_user_transform_ptr read_user_transform_fn;
	png_user_transform_ptr write_user_transform_fn;
	void *user_transform_ptr;
	System::Byte user_transform_depth;
	System::Byte user_transform_channels;
	unsigned mode;
	unsigned flags;
	unsigned transformations;
	TZStreamRec zstream;
	png_byte_array *zbuf;
	unsigned zbuf_size;
	int zlib_level;
	int zlib_method;
	int zlib_window_bits;
	int zlib_mem_level;
	int zlib_strategy;
	unsigned Width;
	unsigned Height;
	unsigned num_rows;
	unsigned usr_width;
	unsigned rowbytes;
	unsigned irowbytes;
	unsigned iwidth;
	unsigned row_number;
	png_byte_array *prev_row;
	png_byte_array *row_buf;
	png_byte_array *sub_row;
	png_byte_array *up_row;
	png_byte_array *avg_row;
	png_byte_array *paeth_row;
	png_row_info row_info;
	unsigned idat_size;
	unsigned crc;
	png_color_array *palette;
	System::Word num_palette;
	System::Word num_trans;
	System::StaticArray<System::Byte, 5> chunk_name;
	System::Byte compression;
	System::Byte filter;
	System::Byte interlaced;
	System::Byte pass;
	System::Byte do_filter;
	System::Byte color_type;
	System::Byte bit_depth;
	System::Byte usr_bit_depth;
	System::Byte pixel_depth;
	System::Byte channels;
	System::Byte usr_channels;
	System::Byte sig_bytes;
	System::Word filler;
	System::Byte background_gamma_type;
	float background_gamma;
	png_color_16 background;
	png_color_16 background_1;
	png_flush_ptr output_flush_fn;
	unsigned flush_dist;
	unsigned flush_rows;
	int gamma_shift;
	float gamma;
	float screen_gamma;
	png_byte_array *gamma_table;
	png_byte_array *gamma_from_1;
	png_byte_array *gamma_to_1;
	png_uint_16p *gamma_16_table;
	png_uint_16p *gamma_16_from_1;
	png_uint_16p *gamma_16_to_1;
	png_color_8 sig_bit;
	png_color_8 shift;
	png_byte_array *trans;
	png_color_16 trans_values;
	png_byte_array *trans_alpha;
	png_color_16 trans_color;
	png_read_status_ptr read_row_fn;
	png_write_status_ptr write_row_fn;
	png_progressive_info_ptr info_fn;
	png_progressive_row_ptr row_fn;
	png_progressive_end_ptr end_fn;
	png_byte_array *save_buffer_ptr;
	png_byte_array *save_buffer;
	png_byte_array *current_buffer_ptr;
	png_byte_array *current_buffer;
	unsigned push_length;
	unsigned skip_length;
	unsigned save_buffer_size;
	unsigned save_buffer_max;
	unsigned buffer_size;
	unsigned current_buffer_size;
	int process_mode;
	int cur_palette;
	unsigned current_text_size;
	unsigned current_text_left;
	System::WideChar *current_text;
	System::WideChar *current_text_ptr;
	System::Word *hist;
	System::Byte heuristic_method;
	System::Byte num_prev_filters;
	png_byte_array *prev_filters;
	System::Word *filter_weights;
	System::Word *inv_filter_weights;
	System::Word *filter_costs;
	System::Word *inv_filter_costs;
	System::WideChar *time_buffer;
	unsigned free_me;
	void *user_chunk_ptr;
	png_user_chunk_ptr read_user_chunk_fn;
	int num_chunk_list;
	png_byte_array *chunk_list;
	System::Byte rgb_to_gray_status;
	System::Word rgb_to_gray_red_coeff;
	System::Word rgb_to_gray_green_coeff;
	System::Word rgb_to_gray_blue_coeff;
	unsigned mng_features_permitted;
	int int_gamma;
	System::Byte filter_type;
	void *mem_ptr;
	png_malloc_ptr malloc_fn;
	png_free_ptr free_fn;
	png_byte_array *big_row_buf;
	System::Byte compression_type;
	png_unknown_chunk unknown_chunk;
	unsigned old_big_row_buf_size;
	unsigned old_prev_row_size;
	System::WideChar *chunkdata;
	unsigned io_state;
};


//-- var, const, procedure ---------------------------------------------------
#define LibPng13 L"libpng13"
#define ZLIB_VERSION L"1.2.3"
#define LIBPNG_VERSION L"1.4.0"
static const System::Int8 PNG_FLAG_ZLIB_CUSTOM_STRATEGY = System::Int8(0x1);
static const System::Int8 PNG_FLAG_ZLIB_CUSTOM_LEVEL = System::Int8(0x2);
static const System::Int8 PNG_FLAG_ZLIB_CUSTOM_MEM_LEVEL = System::Int8(0x4);
static const System::Int8 PNG_FLAG_ZLIB_CUSTOM_WINDOW_BITS = System::Int8(0x8);
static const System::Int8 PNG_FLAG_ZLIB_CUSTOM_METHOD = System::Int8(0x10);
static const System::Int8 PNG_FLAG_ZLIB_FINISHED = System::Int8(0x20);
static const System::Int8 PNG_FLAG_ROW_INIT = System::Int8(0x40);
static const System::Byte PNG_FLAG_FILLER_AFTER = System::Byte(0x80);
static const System::Word PNG_FLAG_CRC_ANCILLARY_USE = System::Word(0x100);
static const System::Word PNG_FLAG_CRC_ANCILLARY_NOWARN = System::Word(0x200);
static const System::Word PNG_FLAG_CRC_CRITICAL_USE = System::Word(0x400);
static const System::Word PNG_FLAG_CRC_CRITICAL_IGNORE = System::Word(0x800);
static const System::Word PNG_FLAG_KEEP_UNKNOWN_CHUNKS = System::Word(0x8000);
static const int PNG_FLAG_KEEP_UNSAFE_CHUNKS = int(0x10000);
static const int PNG_FLAG_LIBRARY_MISMATCH = int(0x20000);
static const int PNG_FLAG_STRIP_ERROR_NUMBERS = int(0x40000);
static const int PNG_FLAG_STRIP_ERROR_TEXT = int(0x80000);
static const int PNG_FLAG_MALLOC_NULL_MEM_OK = int(0x100000);
static const int PNG_FLAG_ADD_ALPHA = int(0x200000);
static const int PNG_FLAG_STRIP_ALPHA = int(0x400000);
static const int PNG_FLAG_BENIGN_ERRORS_WARN = int(0x800000);
static const System::Word PNG_FLAG_CRC_ANCILLARY_MASK = System::Word(0x300);
static const System::Word PNG_FLAG_CRC_CRITICAL_MASK = System::Word(0xc00);
static const System::Word PNG_FLAG_CRC_MASK = System::Word(0xf00);
static const int PNG_USER_WIDTH_MAX = int(0xf4240);
static const int PNG_USER_HEIGHT_MAX = int(0xf4240);
static const int PNG_UINT_31_MAX = int(0x7fffffff);
static const unsigned PNG_UINT_32_MAX = unsigned(0xffffffff);
static const System::Int8 PNG_COLOR_MASK_PALETTE = System::Int8(0x1);
static const System::Int8 PNG_COLOR_MASK_COLOR = System::Int8(0x2);
static const System::Int8 PNG_COLOR_MASK_ALPHA = System::Int8(0x4);
static const System::Int8 PNG_COLOR_TYPE_GRAY = System::Int8(0x0);
static const System::Int8 PNG_COLOR_TYPE_PALETTE = System::Int8(0x3);
static const System::Int8 PNG_COLOR_TYPE_RGB = System::Int8(0x2);
static const System::Int8 PNG_COLOR_TYPE_RGB_ALPHA = System::Int8(0x6);
static const System::Int8 PNG_COLOR_TYPE_GRAY_ALPHA = System::Int8(0x4);
static const System::Int8 PNG_INTERLACE_NONE = System::Int8(0x0);
static const System::Int8 PNG_INTERLACE_ADAM7 = System::Int8(0x1);
static const System::Int8 PNG_INTERLACE_LAST = System::Int8(0x2);
static const System::Int8 PNG_COMPRESSION_TYPE_BASE = System::Int8(0x0);
static const System::Int8 PNG_HAVE_IHDR = System::Int8(0x1);
static const System::Int8 PNG_HAVE_PLTE = System::Int8(0x2);
static const System::Int8 PNG_HAVE_IDAT = System::Int8(0x4);
static const System::Int8 PNG_AFTER_IDAT = System::Int8(0x8);
static const System::Int8 PNG_HAVE_IEND = System::Int8(0x10);
static const System::Int8 PNG_HAVE_gAMA = System::Int8(0x20);
static const System::Int8 PNG_HAVE_cHRM = System::Int8(0x40);
static const System::Byte PNG_HAVE_sRGB = System::Byte(0x80);
static const System::Word PNG_HAVE_CHUNK_HEADER = System::Word(0x100);
static const System::Word PNG_WROTE_tIME = System::Word(0x200);
static const System::Word PNG_WROTE_INFO_BEFORE_PLTE = System::Word(0x400);
static const System::Word PNG_BACKGROUND_IS_GRAY = System::Word(0x800);
static const System::Word PNG_HAVE_PNG_SIGNATURE = System::Word(0x1000);
static const System::Word PNG_HAVE_CHUNK_AFTER_IDAT = System::Word(0x2000);
static const System::Int8 PNG_INFO_gAMA = System::Int8(0x1);
static const System::Int8 PNG_INFO_sBIT = System::Int8(0x2);
static const System::Int8 PNG_INFO_cHRM = System::Int8(0x4);
static const System::Int8 PNG_INFO_PLTE = System::Int8(0x8);
static const System::Int8 PNG_INFO_tRNS = System::Int8(0x10);
static const System::Int8 PNG_INFO_bKGD = System::Int8(0x20);
static const System::Int8 PNG_INFO_hIST = System::Int8(0x40);
static const System::Byte PNG_INFO_pHYs = System::Byte(0x80);
static const System::Word PNG_INFO_oFFs = System::Word(0x100);
static const System::Word PNG_INFO_tIME = System::Word(0x200);
static const System::Word PNG_INFO_pCAL = System::Word(0x400);
static const System::Word PNG_INFO_sRGB = System::Word(0x800);
static const System::Word PNG_INFO_iCCP = System::Word(0x1000);
static const System::Word PNG_INFO_sPLT = System::Word(0x2000);
static const System::Word PNG_INFO_sCAL = System::Word(0x4000);
static const System::Word PNG_INFO_IDAT = System::Word(0x8000);
static const System::Int8 PNG_FILTER_TYPE_BASE = System::Int8(0x0);
static const System::Int8 PNG_INTRAPIXEL_DIFFERENCING = System::Int8(0x40);
static const System::Int8 PNG_FLAG_MNG_EMPTY_PLTE = System::Int8(0x1);
static const System::Int8 PNG_FLAG_MNG_FILTER_64 = System::Int8(0x4);
static const System::Int8 PNG_ALL_MNG_FEATURES = System::Int8(0x5);
static const System::Int8 PNG_BGR = System::Int8(0x1);
static const System::Int8 PNG_INTERLACE = System::Int8(0x2);
static const System::Int8 PNG_PACK = System::Int8(0x4);
static const System::Int8 PNG_SHIFT = System::Int8(0x8);
static const System::Int8 PNG_SWAP_BYTES = System::Int8(0x10);
static const System::Int8 PNG_INVERT_MONO = System::Int8(0x20);
static const System::Int8 PNG_DITHER = System::Int8(0x40);
static const System::Byte PNG_BACKGROUND = System::Byte(0x80);
static const System::Word PNG_BACKGROUND_EXPAND = System::Word(0x100);
static const System::Word PNG_16_TO_8 = System::Word(0x400);
static const System::Word PNG_RGBA = System::Word(0x800);
static const System::Word PNG_EXPAND = System::Word(0x1000);
static const System::Word PNG_GAMMA = System::Word(0x2000);
static const System::Word PNG_GRAY_TO_RGB = System::Word(0x4000);
static const System::Word PNG_FILLER = System::Word(0x8000);
static const int PNG_PACKSWAP = int(0x10000);
static const int PNG_SWAP_ALPHA = int(0x20000);
static const int PNG_STRIP_ALPHA = int(0x40000);
static const int PNG_INVERT_ALPHA = int(0x80000);
static const int PNG_USER_TRANSFORM = int(0x100000);
static const int PNG_RGB_TO_GRAY_ERR = int(0x200000);
static const int PNG_RGB_TO_GRAY_WARN = int(0x400000);
static const int PNG_RGB_TO_GRAY = int(0x600000);
static const int PNG_ADD_ALPHA = int(0x1000000);
static const int PNG_EXPAND_tRNS = int(0x2000000);
#define PNG_GAMMA_THRESHOLD  (5.000000E-02)
static const System::Int8 PNG_BACKGROUND_GAMMA_UNKNOWN = System::Int8(0x0);
static const System::Int8 PNG_BACKGROUND_GAMMA_SCREEN = System::Int8(0x1);
static const System::Int8 PNG_BACKGROUND_GAMMA_FILE = System::Int8(0x2);
static const System::Int8 PNG_BACKGROUND_GAMMA_UNIQUE = System::Int8(0x3);
static const System::Int8 PNG_FREE_HIST = System::Int8(0x8);
static const System::Int8 PNG_FREE_ICCP = System::Int8(0x10);
static const System::Int8 PNG_FREE_SPLT = System::Int8(0x20);
static const System::Int8 PNG_FREE_ROWS = System::Int8(0x40);
static const System::Byte PNG_FREE_PCAL = System::Byte(0x80);
static const System::Word PNG_FREE_SCAL = System::Word(0x100);
static const System::Word PNG_FREE_UNKN = System::Word(0x200);
static const System::Word PNG_FREE_LIST = System::Word(0x400);
static const System::Word PNG_FREE_PLTE = System::Word(0x1000);
static const System::Word PNG_FREE_TRNS = System::Word(0x2000);
static const System::Word PNG_FREE_TEXT = System::Word(0x4000);
static const System::Word PNG_FREE_ALL = System::Word(0x7fff);
static const System::Word PNG_FREE_MUL = System::Word(0x4220);
static const System::Int8 PNG_COMPRESSION_TYPE_DEFAULT = System::Int8(0x0);
static const System::Int8 PNG_FILTER_TYPE_DEFAULT = System::Int8(0x0);
extern PACKAGE int __fastcall deflateInit(TZStreamRec &strm, int level);
extern PACKAGE unsigned __cdecl _png_get_rowbytes(png_structp png_ptr, png_infop info_ptr);
extern PACKAGE unsigned __cdecl _png_get_valid(png_structp png_ptr, png_infop info_ptr, unsigned flag);
extern PACKAGE void __cdecl _png_set_write_fn(png_structp png_ptr, void * io_ptr, png_rw_ptr write_data_fn, png_flush_ptr output_flush_fn);
extern PACKAGE void __cdecl pngReadFn(png_structp png_ptr, png_bytep Data, unsigned length);
extern PACKAGE void __cdecl pngWriteFn(png_structp png_ptr, png_bytep Data, unsigned length);
extern PACKAGE void __cdecl pngErrorFn(png_structp Struct, System::WideChar * str);
extern PACKAGE void __cdecl pngWarnFn(png_structp Struct, System::WideChar * str);
extern PACKAGE int __fastcall adler32(int adler, const char * buf, int len);
extern PACKAGE int __fastcall deflateInit_(TZStreamRec &strm, int level, const char * version, int stream_size);
extern PACKAGE int __fastcall deflateInit2_(TZStreamRec &strm, int level, int method, int windowBits, int memLevel, int strategy, const char * version, int stream_size);
extern PACKAGE int __fastcall deflateSetDictionary(TZStreamRec &strm, const char * dictionary, int dictLength);
extern PACKAGE int __cdecl deflateReset(TZStreamRec &strm);
extern PACKAGE int __fastcall deflatePrime(TZStreamRec &strm, int bits, int Value);
extern PACKAGE int __fastcall deflateParams(TZStreamRec &strm, int level, int strategy);
extern PACKAGE int __fastcall deflateBound(TZStreamRec &strm, int sourceLen);
extern PACKAGE int __fastcall deflate(TZStreamRec &strm, int flush);
extern PACKAGE int __fastcall deflateEnd(TZStreamRec &strm);
extern PACKAGE int __fastcall deflateCopy(TZStreamRec &dest, TZStreamRec &Source);
extern PACKAGE int __fastcall inflateBackInit_(TZStreamRec &strm, int windowBits, char * window, const char * version, int stream_size);
extern PACKAGE int __fastcall inflateBack(TZStreamRec &strm, TInFunc in_fn, void * in_desc, TOutFunc out_fn, void * out_desc);
extern PACKAGE int __fastcall inflateBackEnd(TZStreamRec &strm);
extern PACKAGE int __fastcall inflateReset(TZStreamRec &strm);
extern PACKAGE int __fastcall inflateInit2_(TZStreamRec &strm, int windowBits, const char * version, int stream_size);
extern PACKAGE int __fastcall inflateInit_(TZStreamRec &strm, const char * version, int stream_size);
extern PACKAGE int __fastcall inflate(TZStreamRec &strm, int flush);
extern PACKAGE int __fastcall inflateEnd(TZStreamRec &strm);
extern PACKAGE int __fastcall inflateSetDictionary(TZStreamRec &strm, const char * dictionary, int dictLength);
extern PACKAGE int __fastcall inflateSync(TZStreamRec &strm);
extern PACKAGE int __fastcall inflateCopy(TZStreamRec &dest, TZStreamRec &Source);
extern PACKAGE int __fastcall compress2(char * dest, int &destLen, const char * Source, int sourceLen, int level);
extern PACKAGE int __fastcall compress(char * dest, int &destLen, const char * Source, int sourceLen);
extern PACKAGE int __fastcall compressBound(int sourceLen);
extern PACKAGE int __fastcall crc32(unsigned crc, const char * buf, unsigned len);
extern PACKAGE void __cdecl _png_set_sig_bytes(png_structp png_ptr, int num_bytes);
extern PACKAGE int __cdecl _png_sig_cmp(png_bytep sig, unsigned start, unsigned num_to_check);
extern PACKAGE png_infop __cdecl _png_create_info_struct(png_structp png_ptr);
extern PACKAGE void __cdecl _png_destroy_info_struct(png_structp png_ptr, png_infopp info_ptr_ptr);
extern PACKAGE void __cdecl _png_info_init_3(png_infopp ptr_ptr, unsigned png_info_struct_size);
extern PACKAGE void * __cdecl _png_get_io_ptr(png_structp png_ptr);
extern PACKAGE void __cdecl _png_error(png_structp png_ptr, System::WideChar * error_message);
extern PACKAGE void __cdecl _png_warning(png_structp png_ptr, System::WideChar * warning_message);
extern PACKAGE unsigned __cdecl _png_get_image_width(png_structp png_ptr, png_infop info_ptr);
extern PACKAGE unsigned __cdecl _png_get_image_height(png_structp png_ptr, png_infop info_ptr);
extern PACKAGE System::Byte __cdecl _png_get_bit_depth(png_structp png_ptr, png_infop info_ptr);
extern PACKAGE System::Byte __cdecl _png_get_color_type(png_structp png_ptr, png_infop info_ptr);
extern PACKAGE System::Byte __cdecl _png_get_filter_type(png_structp png_ptr, png_infop info_ptr);
extern PACKAGE System::Byte __cdecl _png_get_interlace_type(png_structp png_ptr, png_infop info_ptr);
extern PACKAGE System::Byte __cdecl _png_get_compression_type(png_structp png_ptr, png_infop info_ptr);
extern PACKAGE System::Byte __cdecl _png_get_channels(png_structp png_ptr, png_infop info_ptr);
extern PACKAGE void * __cdecl _png_malloc(png_structp png_ptr, unsigned size);
extern PACKAGE void __cdecl _png_free(png_structp png_ptr, void * ptr);
extern PACKAGE png_structp __cdecl _png_create_read_struct(System::WideChar * user_png_ver, void * error_ptr, png_error_ptr error_fn, png_error_ptr warn_fn);
extern PACKAGE void __cdecl _png_read_info(png_structp png_ptr, png_infop info_ptr);
extern PACKAGE void __cdecl _png_read_update_info(png_structp png_ptr, png_infop info_ptr);
extern PACKAGE void __cdecl _png_read_image(png_structp png_ptr, png_bytepp image);
extern PACKAGE void __cdecl _png_read_end(png_structp png_ptr, png_infop info_ptr);
extern PACKAGE void __cdecl _png_destroy_read_struct(png_structpp png_ptr_ptr, png_infopp info_ptr_ptr, png_infopp end_info_ptr_ptr);
extern PACKAGE void __cdecl _png_set_read_fn(png_structp png_ptr, void * io_ptr, png_rw_ptr read_data_fn);
extern PACKAGE void __cdecl _png_set_palette_to_rgb(png_structp png_ptr);
extern PACKAGE void __cdecl _png_set_expand_gray_1_2_4_to_8(png_structp png_ptr);
extern PACKAGE void __cdecl _png_set_tRNS_to_alpha(png_structp png_ptr);
extern PACKAGE void __cdecl _png_build_gamma_table(png_structp png_ptr, System::Byte bit_depth);
extern PACKAGE void __cdecl _png_set_IHDR(png_structp png_ptr, png_infop info_ptr, unsigned Width, unsigned Height, int bit_depth, int color_type, int interlace_type, int compression_type, int filter_type);
extern PACKAGE void __cdecl _png_write_info(png_structp png_ptr, png_infop info_ptr);
extern PACKAGE void __cdecl _png_write_end(png_structp png_ptr, png_infop info_ptr);
extern PACKAGE png_structp __cdecl _png_create_write_struct(System::WideChar * user_png_ver, void * error_ptr, png_error_ptr error_fn, png_error_ptr warn_fn);
extern PACKAGE void __cdecl _png_write_image(png_structp png_ptr, png_bytepp image);
extern PACKAGE void __cdecl _png_destroy_write_struct(png_structpp png_ptr_ptr, png_infopp info_ptr_ptr);
}	/* namespace Libpng */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_LIBPNG)
using namespace Libpng;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// LibpngHPP
