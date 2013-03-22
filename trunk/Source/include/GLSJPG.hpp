// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSJPG.pas' rev: 24.00 (Win32)

#ifndef GlsjpgHPP
#define GlsjpgHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glsjpg
{
//-- type declarations -------------------------------------------------------
enum TJPGColorDepth : unsigned int { jpgAuto, jpgGray, jpg8Bit, jpg24Bit };

enum TJPEGPixelFormat : unsigned int { jf24Bit, jf8Bit };

class DELPHICLASS EInvalidGraphic;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EInvalidGraphic : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EInvalidGraphic(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EInvalidGraphic(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidGraphic(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidGraphic(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidGraphic(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidGraphic(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EInvalidGraphic(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EInvalidGraphic(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidGraphic(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidGraphic(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidGraphic(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidGraphic(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EInvalidGraphic(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS EInvalidGraphicOperation;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EInvalidGraphicOperation : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EInvalidGraphicOperation(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EInvalidGraphicOperation(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidGraphicOperation(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidGraphicOperation(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidGraphicOperation(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidGraphicOperation(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EInvalidGraphicOperation(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EInvalidGraphicOperation(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidGraphicOperation(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidGraphicOperation(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidGraphicOperation(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidGraphicOperation(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EInvalidGraphicOperation(void) { }
	
};

#pragma pack(pop)

typedef System::Byte JSAMPLE;

typedef int GETJSAMPLE;

typedef int JCOEF;

typedef int *JCOEF_PTR;

typedef System::Byte UINT8;

typedef System::Word UINT16;

typedef unsigned UINT32;

typedef short INT16;

typedef int INT32;

typedef int *INT32PTR;

typedef unsigned JDIMENSION;

typedef System::Byte JOCTET;

typedef int jTOctet;

typedef System::StaticArray<System::Byte, 2147483647> JOCTET_FIELD;

typedef JOCTET_FIELD *JOCTET_FIELD_PTR;

typedef System::Byte *JOCTETPTR;

typedef System::Byte *JSAMPLE_PTR;

typedef System::StaticArray<System::Byte, 2147483647> JSAMPLE_ARRAY;

typedef JSAMPLE_ARRAY *JSAMPROW;

typedef JSAMPROW *JSAMPROW_PTR;

typedef int jTSample;

typedef int jTRow;

typedef System::StaticArray<JSAMPROW, 536870911> JSAMPROW_ARRAY;

typedef JSAMPROW_ARRAY *JSAMPARRAY;

typedef int jTArray;

typedef System::StaticArray<JSAMPARRAY, 536870911> JSAMP_ARRAY;

typedef JSAMP_ARRAY *JSAMPIMAGE;

enum J_COLOR_SPACE : unsigned int { JCS_UNKNOWN, JCS_GRAYSCALE, JCS_RGB, JCS_YCbCr, JCS_CMYK, JCS_YCCK };

enum J_DCT_METHOD : unsigned int { JDCT_ISLOW, JDCT_IFAST, JDCT_FLOAT };

enum J_DITHER_MODE : unsigned int { JDITHER_NONE, JDITHER_ORDERED, JDITHER_FS };

struct JQUANT_TBL;
typedef JQUANT_TBL *JQUANT_TBL_ptr;

struct DECLSPEC_DRECORD JQUANT_TBL
{
public:
	System::StaticArray<System::Word, 64> quantval;
	BOOL sent_table;
};


struct jpeg_component_info;
typedef jpeg_component_info *jpeg_component_info_ptr;

struct DECLSPEC_DRECORD jpeg_component_info
{
public:
	int component_id;
	int component_index;
	int h_samp_factor;
	int v_samp_factor;
	int quant_tbl_no;
	int dc_tbl_no;
	int ac_tbl_no;
	unsigned width_in_blocks;
	unsigned height_in_blocks;
	int DCT_scaled_size;
	unsigned downsampled_width;
	unsigned downsampled_height;
	BOOL component_needed;
	int MCU_width;
	int MCU_height;
	int MCU_blocks;
	int MCU_sample_width;
	int last_col_width;
	int last_row_height;
	JQUANT_TBL *quant_table;
	void *dct_table;
};


struct jpeg_error_mgr;
typedef jpeg_error_mgr *jpeg_error_mgr_ptr;

struct jpeg_progress_mgr;
typedef jpeg_progress_mgr *jpeg_progress_mgr_ptr;

struct jpeg_common_struct;
typedef jpeg_common_struct *j_common_ptr;

struct jpeg_decompress_struct;
typedef jpeg_decompress_struct *j_decompress_ptr;

typedef BOOL __fastcall (*jpeg_marker_parser_method)(j_decompress_ptr cinfo);

struct jpeg_marker_reader;
typedef jpeg_marker_reader *jpeg_marker_reader_ptr;

struct DECLSPEC_DRECORD jpeg_marker_reader
{
public:
	void __fastcall (*reset_marker_reader)(j_decompress_ptr cinfo);
	int __fastcall (*read_markers)(j_decompress_ptr cinfo);
	jpeg_marker_parser_method read_restart_marker;
	jpeg_marker_parser_method process_COM;
	System::StaticArray<jpeg_marker_parser_method, 16> process_APPn;
	BOOL saw_SOI;
	BOOL saw_SOF;
	int next_restart_num;
	unsigned discarded_bytes;
};


typedef System::StaticArray<int, 8> int8array;

struct DECLSPEC_DRECORD jpeg_error_mgr
{
private:
	struct DECLSPEC_DRECORD _jpeg_error_mgr__1
	{
		#pragma pack(push,1)
		union
		{
			struct 
			{
				System::SmallStringBase<80>  s;
			};
			struct 
			{
				int8array i;
			};
			
		};
		#pragma pack(pop)
	};
	
	
	
public:
	void __fastcall (*error_exit)(j_common_ptr cinfo);
	void __fastcall (*emit_message)(j_common_ptr cinfo, int msg_level);
	void __fastcall (*output_message)(j_common_ptr cinfo);
	void __fastcall (*format_message)(j_common_ptr cinfo, char * buffer);
	void __fastcall (*reset_error_mgr)(j_common_ptr cinfo);
	int msg_code;
	_jpeg_error_mgr__1 msg_parm;
	int trace_level;
	int num_warnings;
};


struct jpeg_source_mgr;
typedef jpeg_source_mgr *jpeg_source_mgr_ptr;

struct DECLSPEC_DRECORD jpeg_source_mgr
{
public:
	System::Byte *next_input_byte;
	int bytes_in_buffer;
	void __fastcall (*init_source)(j_decompress_ptr cinfo);
	BOOL __fastcall (*fill_input_buffer)(j_decompress_ptr cinfo);
	void __fastcall (*skip_input_data)(j_decompress_ptr cinfo, int num_bytes);
	BOOL __fastcall (*resync_to_restart)(j_decompress_ptr cinfo, int desired);
	void __fastcall (*term_source)(j_decompress_ptr cinfo);
};


struct jpeg_memory_mgr;
typedef jpeg_memory_mgr *jpeg_memory_mgr_ptr;

struct DECLSPEC_DRECORD jpeg_memory_mgr
{
public:
	void * __fastcall (*alloc_small)(j_common_ptr cinfo, int pool_id, int sizeofobject);
	void * __fastcall (*alloc_large)(j_common_ptr cinfo, int pool_id, int sizeofobject);
	JSAMPARRAY __fastcall (*alloc_sarray)(j_common_ptr cinfo, int pool_id, unsigned samplesperrow, unsigned numrows);
	void *alloc_barray;
	void *request_virt_sarray;
	void *request_virt_barray;
	void *realize_virt_arrays;
	void *access_virt_sarray;
	void *access_virt_barray;
	void *free_pool;
	void *self_destruct;
	int max_memory_to_use;
};


#pragma pack(push,1)
struct DECLSPEC_DRECORD jpeg_common_struct
{
public:
	jpeg_error_mgr *err;
	jpeg_memory_mgr *mem;
	jpeg_progress_mgr *progress;
	BOOL is_decompressor;
	int global_state;
};
#pragma pack(pop)


struct DECLSPEC_DRECORD jpeg_progress_mgr
{
public:
	void __fastcall (*progress_monitor)(const jpeg_common_struct &cinfo);
	int pass_counter;
	int pass_limit;
	int completed_passes;
	int total_passes;
	System::Classes::TPersistent* instance;
	int last_pass;
	int last_pct;
	int last_time;
	int last_scanline;
};


#pragma pack(push,1)
struct DECLSPEC_DRECORD jpeg_decompress_struct
{
public:
	jpeg_common_struct common;
	jpeg_source_mgr *src;
	unsigned image_width;
	unsigned image_height;
	int num_components;
	J_COLOR_SPACE jpeg_color_space;
	J_COLOR_SPACE out_color_space;
	unsigned scale_num;
	unsigned scale_denom;
	double output_gamma;
	BOOL buffered_image;
	BOOL raw_data_out;
	J_DCT_METHOD dct_method;
	BOOL do_fancy_upsampling;
	BOOL do_block_smoothing;
	BOOL quantize_colors;
	J_DITHER_MODE dither_mode;
	BOOL two_pass_quantize;
	int desired_number_of_colors;
	BOOL enable_1pass_quant;
	BOOL enable_external_quant;
	BOOL enable_2pass_quant;
	unsigned output_width;
	unsigned output_height;
	int out_color_components;
	int output_components;
	int rec_outbuf_height;
	int actual_number_of_colors;
	JSAMPROW_ARRAY *colormap;
	unsigned output_scanline;
	int input_scan_number;
	unsigned input_iMCU_row;
	int output_scan_number;
	int output_iMCU_row;
	void *coef_bits;
	System::StaticArray<void *, 4> quant_tbl_ptrs;
	System::StaticArray<void *, 4> dc_huff_tbl_ptrs;
	System::StaticArray<void *, 4> ac_huff_tbl_ptrs;
	int data_precision;
	jpeg_component_info *comp_info;
	BOOL progressive_mode;
	BOOL arith_code;
	System::StaticArray<System::Byte, 16> arith_dc_L;
	System::StaticArray<System::Byte, 16> arith_dc_U;
	System::StaticArray<System::Byte, 16> arith_ac_K;
	unsigned restart_interval;
	BOOL saw_JFIF_marker;
	System::Byte density_unit;
	System::Word X_density;
	System::Word Y_density;
	BOOL saw_Adobe_marker;
	System::Byte Adobe_transform;
	BOOL CCIR601_sampling;
	int max_h_samp_factor;
	int max_v_samp_factor;
	int min_DCT_scaled_size;
	unsigned total_iMCU_rows;
	void *sample_range_limit;
	int comps_in_scan;
	System::StaticArray<void *, 4> cur_comp_info;
	unsigned MCUs_per_row;
	unsigned MCU_rows_in_scan;
	unsigned blocks_in_MCU;
	System::StaticArray<int, 10> MCU_membership;
	int Ss;
	int Se;
	int Ah;
	int Al;
	int unread_marker;
	void *master;
	void *main;
	void *coef;
	void *post;
	void *inputctl;
	void *marker;
	void *entropy;
	void *idct;
	void *upsample;
	void *cconvert;
	void *cquantize;
};
#pragma pack(pop)


struct jpeg_compress_struct;
typedef jpeg_compress_struct *j_compress_ptr;

struct jpeg_destination_mgr;
typedef jpeg_destination_mgr *jpeg_destination_mgr_ptr;

struct DECLSPEC_DRECORD jpeg_destination_mgr
{
public:
	System::Byte *next_output_byte;
	int free_in_buffer;
	void __fastcall (*init_destination)(j_compress_ptr cinfo);
	BOOL __fastcall (*empty_output_buffer)(j_compress_ptr cinfo);
	void __fastcall (*term_destination)(j_compress_ptr cinfo);
};


#pragma pack(push,1)
struct DECLSPEC_DRECORD jpeg_compress_struct
{
public:
	jpeg_common_struct common;
	jpeg_destination_mgr *dest;
	unsigned image_width;
	unsigned image_height;
	int input_components;
	J_COLOR_SPACE in_color_space;
	double input_gamma;
	int data_precision;
	int num_components;
	J_COLOR_SPACE jpeg_color_space;
	jpeg_component_info *comp_info;
	System::StaticArray<void *, 4> quant_tbl_ptrs;
	System::StaticArray<void *, 4> dc_huff_tbl_ptrs;
	System::StaticArray<void *, 4> ac_huff_tbl_ptrs;
	System::StaticArray<System::Byte, 16> arith_dc_L;
	System::StaticArray<System::Byte, 16> arith_dc_U;
	System::StaticArray<System::Byte, 16> arith_ac_K;
	int num_scans;
	void *scan_info;
	BOOL raw_data_in;
	BOOL arith_code;
	BOOL optimize_coding;
	BOOL CCIR601_sampling;
	int smoothing_factor;
	J_DCT_METHOD dct_method;
	unsigned restart_interval;
	int restart_in_rows;
	BOOL write_JFIF_header;
	System::Byte density_unit;
	System::Word X_density;
	System::Word Y_density;
	BOOL write_Adobe_marker;
	unsigned next_scanline;
	BOOL progressive_mode;
	int max_h_samp_factor;
	int max_v_samp_factor;
	unsigned total_iMCU_rows;
	int comps_in_scan;
	System::StaticArray<void *, 4> cur_comp_info;
	unsigned MCUs_per_row;
	unsigned MCU_rows_in_scan;
	int blocks_in_MCU;
	System::StaticArray<int, 10> MCU_membership;
	int Ss;
	int Se;
	int Ah;
	int Al;
	void *master;
	void *main;
	void *prep;
	void *coef;
	void *marker;
	void *cconvert;
	void *downsample;
	void *fdct;
	void *entropy;
};
#pragma pack(pop)


struct DECLSPEC_DRECORD TJPEGContext
{
public:
	jpeg_error_mgr err;
	jpeg_progress_mgr progress;
	J_DCT_METHOD FinalDCT;
	bool FinalTwoPassQuant;
	J_DITHER_MODE FinalDitherMode;
	#pragma pack(push,1)
	union
	{
		struct 
		{
			jpeg_compress_struct c;
		};
		struct 
		{
			jpeg_decompress_struct d;
		};
		struct 
		{
			jpeg_common_struct common;
		};
		
	};
	#pragma pack(pop)
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 JPEG_SUSPENDED = System::Int8(0x0);
static const System::Int8 JPEG_HEADER_OK = System::Int8(0x1);
static const System::Int8 JPEG_HEADER_TABLES_ONLY = System::Int8(0x2);
static const System::Int8 JPEG_REACHED_SOS = System::Int8(0x1);
static const System::Int8 JPEG_REACHED_EOI = System::Int8(0x2);
static const System::Int8 JPEG_ROW_COMPLETED = System::Int8(0x3);
static const System::Int8 JPEG_SCAN_COMPLETED = System::Int8(0x4);
static const System::Int8 CSTATE_START = System::Int8(0x64);
static const System::Int8 CSTATE_SCANNING = System::Int8(0x65);
static const System::Int8 CSTATE_RAW_OK = System::Int8(0x66);
static const System::Int8 CSTATE_WRCOEFS = System::Int8(0x67);
static const System::Int8 JPEG_LIB_VERSION = System::Int8(0x3d);
static const System::Byte JPEG_RST0 = System::Byte(0xd0);
static const System::Byte JPEG_EOI = System::Byte(0xd9);
static const System::Byte JPEG_APP0 = System::Byte(0xe0);
static const System::Byte JPEG_COM = System::Byte(0xfe);
static const System::Int8 DCTSIZE = System::Int8(0x8);
static const System::Int8 DCTSIZE2 = System::Int8(0x40);
static const System::Int8 NUM_QUANT_TBLS = System::Int8(0x4);
static const System::Int8 NUM_HUFF_TBLS = System::Int8(0x4);
static const System::Int8 NUM_ARITH_TBLS = System::Int8(0x10);
static const System::Int8 MAX_COMPS_IN_SCAN = System::Int8(0x4);
static const System::Int8 MAX_SAMP_FACTOR = System::Int8(0x4);
static const System::Int8 C_MAX_BLOCKS_IN_MCU = System::Int8(0xa);
static const System::Int8 D_MAX_BLOCKS_IN_MCU = System::Int8(0xa);
static const System::Int8 MAX_COMPONENTS = System::Int8(0xa);
static const System::Byte MAXJSAMPLE = System::Byte(0xff);
static const System::Byte CENTERJSAMPLE = System::Byte(0x80);
static const System::Byte DSTATE_START = System::Byte(0xc8);
static const System::Byte DSTATE_INHEADER = System::Byte(0xc9);
static const System::Byte DSTATE_READY = System::Byte(0xca);
static const System::Byte DSTATE_PRELOAD = System::Byte(0xcb);
static const System::Byte DSTATE_PRESCAN = System::Byte(0xcc);
static const System::Byte DSTATE_SCANNING = System::Byte(0xcd);
static const System::Byte DSTATE_RAW_OK = System::Byte(0xce);
static const System::Byte DSTATE_BUFIMAGE = System::Byte(0xcf);
static const System::Byte DSTATE_BUFPOST = System::Byte(0xd0);
static const System::Byte DSTATE_RDCOEFS = System::Byte(0xd1);
static const System::Byte DSTATE_STOPPING = System::Byte(0xd2);
static const System::Byte JMSG_LENGTH_MAX = System::Byte(0xc8);
static const System::Int8 JMSG_STR_PARM_MAX = System::Int8(0x50);
static const System::Int8 JPOOL_PERMANENT = System::Int8(0x0);
static const System::Int8 JPOOL_IMAGE = System::Int8(0x1);
extern PACKAGE jpeg_error_mgr jpeg_std_error;
extern PACKAGE void __fastcall JPEGLIBCallback(const jpeg_common_struct &cinfo);
extern PACKAGE void __fastcall GetJPEGInfo(System::Classes::TStream* Stream, unsigned &Width, unsigned &Height)/* overload */;
extern PACKAGE void __fastcall GetJPEGInfo(System::UnicodeString FileName, unsigned &Width, unsigned &Height)/* overload */;
extern PACKAGE void __fastcall jpeg_CreateDecompress(j_decompress_ptr cinfo, int version, int structsize);
extern PACKAGE void __fastcall jpeg_destroy_decompress(j_decompress_ptr cinfo);
extern PACKAGE int __fastcall jpeg_read_header(j_decompress_ptr cinfo, BOOL RequireImage);
extern PACKAGE int __fastcall jpeg_consume_input(j_decompress_ptr cinfo);
extern PACKAGE BOOL __fastcall jpeg_has_multiple_scans(j_decompress_ptr cinfo);
extern PACKAGE BOOL __fastcall jpeg_finish_decompress(j_decompress_ptr cinfo);
extern PACKAGE void __fastcall jpeg_stdio_src(j_decompress_ptr cinfo, System::Classes::TStream* input_file);
extern PACKAGE BOOL __fastcall jpeg_start_decompress(j_decompress_ptr cinfo);
extern PACKAGE unsigned __fastcall jpeg_read_scanlines(j_decompress_ptr cinfo, JSAMPARRAY scanlines, unsigned max_lines);
extern PACKAGE unsigned __fastcall jpeg_read_raw_data(j_decompress_ptr cinfo, JSAMPIMAGE data, unsigned max_lines);
extern PACKAGE BOOL __fastcall jpeg_start_output(j_decompress_ptr cinfo, int scan_number);
extern PACKAGE BOOL __fastcall jpeg_finish_output(j_decompress_ptr cinfo);
extern PACKAGE void __fastcall jpeg_calc_output_dimensions(j_decompress_ptr cinfo);
extern PACKAGE BOOL __fastcall jpeg_resync_to_restart(j_decompress_ptr cinfo, int desired);
extern PACKAGE void __fastcall jpeg_abort(j_decompress_ptr cinfo);
extern PACKAGE void __fastcall jpeg_destroy(j_decompress_ptr cinfo);
extern PACKAGE void __fastcall jpeg_stdio_dest(j_compress_ptr cinfo, System::Classes::TStream* output_file);
extern PACKAGE void __fastcall jpeg_set_quality(j_compress_ptr cinfo, int Quality, BOOL Baseline);
extern PACKAGE void __fastcall jpeg_set_defaults(j_compress_ptr cinfo);
extern PACKAGE void __fastcall jpeg_set_colorspace(j_compress_ptr cinfo, J_COLOR_SPACE colorspace);
extern PACKAGE void __fastcall jpeg_simple_progression(j_compress_ptr cinfo);
extern PACKAGE void __fastcall jpeg_start_compress(j_compress_ptr cinfo, BOOL WriteAllTables);
extern PACKAGE unsigned __fastcall jpeg_write_scanlines(j_compress_ptr cinfo, JSAMPARRAY scanlines, unsigned max_lines);
extern PACKAGE void __fastcall jpeg_CreateCompress(j_compress_ptr cinfo, int version, int structsize);
extern PACKAGE void __fastcall jpeg_finish_compress(j_compress_ptr cinfo);
}	/* namespace Glsjpg */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSJPG)
using namespace Glsjpg;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsjpgHPP
