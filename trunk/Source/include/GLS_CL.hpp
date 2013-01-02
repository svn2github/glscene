// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS_CL.pas' rev: 24.00 (Win32)

#ifndef Gls_clHPP
#define Gls_clHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <GLS_CL_Platform.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gls_cl
{
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD T_cl_platform_id
{
};


struct DECLSPEC_DRECORD T_cl_device_id
{
};


struct DECLSPEC_DRECORD T_cl_context
{
};


struct DECLSPEC_DRECORD T_cl_command_queue
{
};


struct DECLSPEC_DRECORD T_cl_mem
{
};


struct DECLSPEC_DRECORD T_cl_program
{
};


struct DECLSPEC_DRECORD T_cl_kernel
{
};


struct DECLSPEC_DRECORD T_cl_event
{
};


struct DECLSPEC_DRECORD T_cl_sampler
{
};


typedef T_cl_platform_id *Tcl_platform_id;

typedef T_cl_device_id *Tcl_device_id;

typedef T_cl_context *Tcl_context;

typedef T_cl_command_queue *Tcl_command_queue;

typedef T_cl_mem *Tcl_mem;

typedef T_cl_program *Tcl_program;

typedef T_cl_kernel *Tcl_kernel;

typedef T_cl_event *Tcl_event;

typedef T_cl_sampler *Tcl_sampler;

typedef Tcl_platform_id *Pcl_platform_id;

typedef Tcl_device_id *Pcl_device_id;

typedef Tcl_context *Pcl_context;

typedef Tcl_command_queue *Pcl_command_queue;

typedef Tcl_mem *Pcl_mem;

typedef Tcl_program *Pcl_program;

typedef Tcl_kernel *Pcl_kernel;

typedef Tcl_event *Pcl_event;

typedef Tcl_sampler *Pcl_sampler;

typedef unsigned Tcl_bool;

typedef unsigned __int64 Tcl_bitfield;

typedef unsigned __int64 Tcl_device_type;

typedef unsigned Tcl_platform_info;

typedef unsigned Tcl_device_info;

typedef unsigned __int64 Tcl_device_address_info;

typedef unsigned __int64 Tcl_device_fp_config;

typedef unsigned Tcl_device_mem_cache_type;

typedef unsigned Tcl_device_local_mem_type;

typedef unsigned __int64 Tcl_device_exec_capabilities;

typedef unsigned __int64 Tcl_command_queue_properties;

typedef unsigned *Pcl_bool;

typedef unsigned __int64 *Pcl_bitfield;

typedef unsigned __int64 *Pcl_device_type;

typedef unsigned *Pcl_platform_info;

typedef unsigned *Pcl_device_info;

typedef unsigned __int64 *Pcl_device_address_info;

typedef unsigned __int64 *Pcl_device_fp_config;

typedef unsigned *Pcl_device_mem_cache_type;

typedef unsigned *Pcl_device_local_mem_type;

typedef unsigned __int64 *Pcl_device_exec_capabilities;

typedef unsigned __int64 *Pcl_command_queue_properties;

typedef NativeUInt Tcl_context_properties;

typedef unsigned Tcl_context_info;

typedef unsigned Tcl_command_queue_info;

typedef unsigned Tcl_channel_order;

typedef unsigned Tcl_channel_type;

typedef unsigned __int64 Tcl_mem_flags;

typedef unsigned Tcl_mem_object_type;

typedef unsigned Tcl_mem_info;

typedef unsigned Tcl_image_info;

typedef unsigned Tcl_addressing_mode;

typedef unsigned Tcl_filter_mode;

typedef unsigned Tcl_sampler_info;

typedef unsigned __int64 Tcl_map_flags;

typedef unsigned Tcl_program_info;

typedef unsigned Tcl_program_build_info;

typedef int Tcl_build_status;

typedef unsigned Tcl_kernel_info;

typedef unsigned Tcl_kernel_work_group_info;

typedef unsigned Tcl_event_info;

typedef unsigned Tcl_command_type;

typedef unsigned Tcl_profiling_info;

typedef NativeUInt *Pcl_context_properties;

typedef unsigned *Pcl_context_info;

typedef unsigned *Pcl_command_queue_info;

typedef unsigned *Pcl_channel_order;

typedef unsigned *Pcl_channel_type;

typedef unsigned __int64 *Pcl_mem_flags;

typedef unsigned *Pcl_mem_object_type;

typedef unsigned *Pcl_mem_info;

typedef unsigned *Pcl_image_info;

typedef unsigned *Pcl_addressing_mode;

typedef unsigned *Pcl_filter_mode;

typedef unsigned *Pcl_sampler_info;

typedef unsigned __int64 *Pcl_map_flags;

typedef unsigned *Pcl_program_info;

typedef unsigned *Pcl_program_build_info;

typedef int *Pcl_build_status;

typedef unsigned *Pcl_kernel_info;

typedef unsigned *Pcl_kernel_work_group_info;

typedef unsigned *Pcl_event_info;

typedef unsigned *Pcl_command_type;

typedef unsigned *Pcl_profiling_info;

struct DECLSPEC_DRECORD Tcl_image_format
{
public:
	unsigned image_channel_order;
	unsigned image_channel_data_type;
};


typedef Tcl_image_format *Pcl_image_format;

typedef void __stdcall (*logging_fn)(char * errinfo, void * private_info, NativeUInt cb, void * user_data);

typedef void __stdcall (*ProgramBuilt_notify_fn)(Tcl_program _program, void * user_data);

typedef void __stdcall (*EnqueueNativeKernel_user_func)(void);

//-- var, const, procedure ---------------------------------------------------
#define LibOpenCL L"OpenCL.dll"
static const System::Int8 CL_SUCCESS = System::Int8(0x0);
static const System::Int8 CL_DEVICE_NOT_FOUND = System::Int8(-1);
static const System::Int8 CL_DEVICE_NOT_AVAILABLE = System::Int8(-2);
static const System::Int8 CL_COMPILER_NOT_AVAILABLE = System::Int8(-3);
static const System::Int8 CL_MEM_OBJECT_ALLOCATION_FAILURE = System::Int8(-4);
static const System::Int8 CL_OUT_OF_RESOURCES = System::Int8(-5);
static const System::Int8 CL_OUT_OF_HOST_MEMORY = System::Int8(-6);
static const System::Int8 CL_PROFILING_INFO_NOT_AVAILABLE = System::Int8(-7);
static const System::Int8 CL_MEM_COPY_OVERLAP = System::Int8(-8);
static const System::Int8 CL_IMAGE_FORMAT_MISMATCH = System::Int8(-9);
static const System::Int8 CL_IMAGE_FORMAT_NOT_SUPPORTED = System::Int8(-10);
static const System::Int8 CL_BUILD_PROGRAM_FAILURE = System::Int8(-11);
static const System::Int8 CL_MAP_FAILURE = System::Int8(-12);
static const System::Int8 CL_INVALID_VALUE = System::Int8(-30);
static const System::Int8 CL_INVALID_DEVICE_TYPE = System::Int8(-31);
static const System::Int8 CL_INVALID_PLATFORM = System::Int8(-32);
static const System::Int8 CL_INVALID_DEVICE = System::Int8(-33);
static const System::Int8 CL_INVALID_CONTEXT = System::Int8(-34);
static const System::Int8 CL_INVALID_QUEUE_PROPERTIES = System::Int8(-35);
static const System::Int8 CL_INVALID_COMMAND_QUEUE = System::Int8(-36);
static const System::Int8 CL_INVALID_HOST_PTR = System::Int8(-37);
static const System::Int8 CL_INVALID_MEM_OBJECT = System::Int8(-38);
static const System::Int8 CL_INVALID_IMAGE_FORMAT_DESCRIPTOR = System::Int8(-39);
static const System::Int8 CL_INVALID_IMAGE_SIZE = System::Int8(-40);
static const System::Int8 CL_INVALID_SAMPLER = System::Int8(-41);
static const System::Int8 CL_INVALID_BINARY = System::Int8(-42);
static const System::Int8 CL_INVALID_BUILD_OPTIONS = System::Int8(-43);
static const System::Int8 CL_INVALID_PROGRAM = System::Int8(-44);
static const System::Int8 CL_INVALID_PROGRAM_EXECUTABLE = System::Int8(-45);
static const System::Int8 CL_INVALID_KERNEL_NAME = System::Int8(-46);
static const System::Int8 CL_INVALID_KERNEL_DEFINITION = System::Int8(-47);
static const System::Int8 CL_INVALID_KERNEL = System::Int8(-48);
static const System::Int8 CL_INVALID_ARG_INDEX = System::Int8(-49);
static const System::Int8 CL_INVALID_ARG_VALUE = System::Int8(-50);
static const System::Int8 CL_INVALID_ARG_SIZE = System::Int8(-51);
static const System::Int8 CL_INVALID_KERNEL_ARGS = System::Int8(-52);
static const System::Int8 CL_INVALID_WORK_DIMENSION = System::Int8(-53);
static const System::Int8 CL_INVALID_WORK_GROUP_SIZE = System::Int8(-54);
static const System::Int8 CL_INVALID_WORK_ITEM_SIZE = System::Int8(-55);
static const System::Int8 CL_INVALID_GLOBAL_OFFSET = System::Int8(-56);
static const System::Int8 CL_INVALID_EVENT_WAIT_LIST = System::Int8(-57);
static const System::Int8 CL_INVALID_EVENT = System::Int8(-58);
static const System::Int8 CL_INVALID_OPERATION = System::Int8(-59);
static const System::Int8 CL_INVALID_GL_OBJECT = System::Int8(-60);
static const System::Int8 CL_INVALID_BUFFER_SIZE = System::Int8(-61);
static const System::Int8 CL_INVALID_MIP_LEVEL = System::Int8(-62);
static const System::Int8 CL_INVALID_GLOBAL_WORK_SIZE = System::Int8(-63);
static const System::Int8 CL_VERSION_1_0 = System::Int8(0x1);
static const System::Int8 CL_FALSE = System::Int8(0x0);
static const System::Int8 CL_TRUE = System::Int8(0x1);
static const System::Word CL_PLATFORM_PROFILE = System::Word(0x900);
static const System::Word CL_PLATFORM_VERSION = System::Word(0x901);
static const System::Word CL_PLATFORM_NAME = System::Word(0x902);
static const System::Word CL_PLATFORM_VENDOR = System::Word(0x903);
static const System::Word CL_PLATFORM_EXTENSIONS = System::Word(0x904);
static const System::Int8 CL_DEVICE_TYPE_DEFAULT = System::Int8(0x1);
static const System::Int8 CL_DEVICE_TYPE_CPU = System::Int8(0x2);
static const System::Int8 CL_DEVICE_TYPE_GPU = System::Int8(0x4);
static const System::Int8 CL_DEVICE_TYPE_ACCELERATOR = System::Int8(0x8);
static const unsigned CL_DEVICE_TYPE_ALL = unsigned(0xffffffff);
static const System::Word CL_DEVICE_TYPE = System::Word(0x1000);
static const System::Word CL_DEVICE_VENDOR_ID = System::Word(0x1001);
static const System::Word CL_DEVICE_MAX_COMPUTE_UNITS = System::Word(0x1002);
static const System::Word CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS = System::Word(0x1003);
static const System::Word CL_DEVICE_MAX_WORK_GROUP_SIZE = System::Word(0x1004);
static const System::Word CL_DEVICE_MAX_WORK_ITEM_SIZES = System::Word(0x1005);
static const System::Word CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR = System::Word(0x1006);
static const System::Word CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT = System::Word(0x1007);
static const System::Word CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT = System::Word(0x1008);
static const System::Word CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG = System::Word(0x1009);
static const System::Word CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT = System::Word(0x100a);
static const System::Word CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE = System::Word(0x100b);
static const System::Word CL_DEVICE_MAX_CLOCK_FREQUENCY = System::Word(0x100c);
static const System::Word CL_DEVICE_ADDRESS_BITS = System::Word(0x100d);
static const System::Word CL_DEVICE_MAX_READ_IMAGE_ARGS = System::Word(0x100e);
static const System::Word CL_DEVICE_MAX_WRITE_IMAGE_ARGS = System::Word(0x100f);
static const System::Word CL_DEVICE_MAX_MEM_ALLOC_SIZE = System::Word(0x1010);
static const System::Word CL_DEVICE_IMAGE2D_MAX_WIDTH = System::Word(0x1011);
static const System::Word CL_DEVICE_IMAGE2D_MAX_HEIGHT = System::Word(0x1012);
static const System::Word CL_DEVICE_IMAGE3D_MAX_WIDTH = System::Word(0x1013);
static const System::Word CL_DEVICE_IMAGE3D_MAX_HEIGHT = System::Word(0x1014);
static const System::Word CL_DEVICE_IMAGE3D_MAX_DEPTH = System::Word(0x1015);
static const System::Word CL_DEVICE_IMAGE_SUPPORT = System::Word(0x1016);
static const System::Word CL_DEVICE_MAX_PARAMETER_SIZE = System::Word(0x1017);
static const System::Word CL_DEVICE_MAX_SAMPLERS = System::Word(0x1018);
static const System::Word CL_DEVICE_MEM_BASE_ADDR_ALIGN = System::Word(0x1019);
static const System::Word CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE = System::Word(0x101a);
static const System::Word CL_DEVICE_SINGLE_FP_CONFIG = System::Word(0x101b);
static const System::Word CL_DEVICE_GLOBAL_MEM_CACHE_TYPE = System::Word(0x101c);
static const System::Word CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE = System::Word(0x101d);
static const System::Word CL_DEVICE_GLOBAL_MEM_CACHE_SIZE = System::Word(0x101e);
static const System::Word CL_DEVICE_GLOBAL_MEM_SIZE = System::Word(0x101f);
static const System::Word CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE = System::Word(0x1020);
static const System::Word CL_DEVICE_MAX_CONSTANT_ARGS = System::Word(0x1021);
static const System::Word CL_DEVICE_LOCAL_MEM_TYPE = System::Word(0x1022);
static const System::Word CL_DEVICE_LOCAL_MEM_SIZE = System::Word(0x1023);
static const System::Word CL_DEVICE_ERROR_CORRECTION_SUPPORT = System::Word(0x1024);
static const System::Word CL_DEVICE_PROFILING_TIMER_RESOLUTION = System::Word(0x1025);
static const System::Word CL_DEVICE_ENDIAN_LITTLE = System::Word(0x1026);
static const System::Word CL_DEVICE_AVAILABLE = System::Word(0x1027);
static const System::Word CL_DEVICE_COMPILER_AVAILABLE = System::Word(0x1028);
static const System::Word CL_DEVICE_EXECUTION_CAPABILITIES = System::Word(0x1029);
static const System::Word CL_DEVICE_QUEUE_PROPERTIES = System::Word(0x102a);
static const System::Word CL_DEVICE_NAME = System::Word(0x102b);
static const System::Word CL_DEVICE_VENDOR = System::Word(0x102c);
static const System::Word CL_DRIVER_VERSION = System::Word(0x102d);
static const System::Word CL_DEVICE_PROFILE = System::Word(0x102e);
static const System::Word CL_DEVICE_VERSION = System::Word(0x102f);
static const System::Word CL_DEVICE_EXTENSIONS = System::Word(0x1030);
static const System::Word CL_DEVICE_PLATFORM = System::Word(0x1031);
static const System::Int8 CL_FP_DENORM = System::Int8(0x1);
static const System::Int8 CL_FP_INF_NAN = System::Int8(0x2);
static const System::Int8 CL_FP_ROUND_TO_NEAREST = System::Int8(0x4);
static const System::Int8 CL_FP_ROUND_TO_ZERO = System::Int8(0x8);
static const System::Int8 CL_FP_ROUND_TO_INF = System::Int8(0x10);
static const System::Int8 CL_FP_FMA = System::Int8(0x20);
static const System::Int8 CL_NONE = System::Int8(0x0);
static const System::Int8 CL_READ_ONLY_CACHE = System::Int8(0x1);
static const System::Int8 CL_READ_WRITE_CACHE = System::Int8(0x2);
static const System::Int8 CL_LOCAL = System::Int8(0x1);
static const System::Int8 CL_GLOBAL = System::Int8(0x2);
static const System::Int8 CL_EXEC_KERNEL = System::Int8(0x1);
static const System::Int8 CL_EXEC_NATIVE_KERNEL = System::Int8(0x2);
static const System::Int8 CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE = System::Int8(0x1);
static const System::Int8 CL_QUEUE_PROFILING_ENABLE = System::Int8(0x2);
static const System::Word CL_CONTEXT_REFERENCE_COUNT = System::Word(0x1080);
static const System::Word CL_CONTEXT_DEVICES = System::Word(0x1081);
static const System::Word CL_CONTEXT_PROPERTIES = System::Word(0x1082);
static const System::Word CL_CONTEXT_PLATFORM = System::Word(0x1084);
static const System::Word CL_QUEUE_CONTEXT = System::Word(0x1090);
static const System::Word CL_QUEUE_DEVICE = System::Word(0x1091);
static const System::Word CL_QUEUE_REFERENCE_COUNT = System::Word(0x1092);
static const System::Word CL_QUEUE_PROPERTIES = System::Word(0x1093);
static const System::Int8 CL_MEM_READ_WRITE = System::Int8(0x1);
static const System::Int8 CL_MEM_WRITE_ONLY = System::Int8(0x2);
static const System::Int8 CL_MEM_READ_ONLY = System::Int8(0x4);
static const System::Int8 CL_MEM_USE_HOST_PTR = System::Int8(0x8);
static const System::Int8 CL_MEM_ALLOC_HOST_PTR = System::Int8(0x10);
static const System::Int8 CL_MEM_COPY_HOST_PTR = System::Int8(0x20);
static const System::Word CL_R = System::Word(0x10b0);
static const System::Word CL_A = System::Word(0x10b1);
static const System::Word CL_RG = System::Word(0x10b2);
static const System::Word CL_RA = System::Word(0x10b3);
static const System::Word CL_RGB = System::Word(0x10b4);
static const System::Word CL_RGBA = System::Word(0x10b5);
static const System::Word CL_BGRA = System::Word(0x10b6);
static const System::Word CL_ARGB = System::Word(0x10b7);
static const System::Word CL_INTENSITY = System::Word(0x10b8);
static const System::Word CL_LUMINANCE = System::Word(0x10b9);
static const System::Word CL_SNORM_INT8 = System::Word(0x10d0);
static const System::Word CL_SNORM_INT16 = System::Word(0x10d1);
static const System::Word CL_UNORM_INT8 = System::Word(0x10d2);
static const System::Word CL_UNORM_INT16 = System::Word(0x10d3);
static const System::Word CL_UNORM_SHORT_565 = System::Word(0x10d4);
static const System::Word CL_UNORM_SHORT_555 = System::Word(0x10d5);
static const System::Word CL_UNORM_INT_101010 = System::Word(0x10d6);
static const System::Word CL_SIGNED_INT8 = System::Word(0x10d7);
static const System::Word CL_SIGNED_INT16 = System::Word(0x10d8);
static const System::Word CL_SIGNED_INT32 = System::Word(0x10d9);
static const System::Word CL_UNSIGNED_INT8 = System::Word(0x10da);
static const System::Word CL_UNSIGNED_INT16 = System::Word(0x10db);
static const System::Word CL_UNSIGNED_INT32 = System::Word(0x10dc);
static const System::Word CL_HALF_FLOAT = System::Word(0x10dd);
static const System::Word CL_FLOAT = System::Word(0x10de);
static const System::Word CL_MEM_OBJECT_BUFFER = System::Word(0x10f0);
static const System::Word CL_MEM_OBJECT_IMAGE2D = System::Word(0x10f1);
static const System::Word CL_MEM_OBJECT_IMAGE3D = System::Word(0x10f2);
static const System::Word CL_MEM_TYPE = System::Word(0x1100);
static const System::Word CL_MEM_FLAGS = System::Word(0x1101);
static const System::Word CL_MEM_SIZE = System::Word(0x1102);
static const System::Word CL_MEM_HOST_PTR = System::Word(0x1103);
static const System::Word CL_MEM_MAP_COUNT = System::Word(0x1104);
static const System::Word CL_MEM_REFERENCE_COUNT = System::Word(0x1105);
static const System::Word CL_MEM_CONTEXT = System::Word(0x1106);
static const System::Word CL_IMAGE_FORMAT = System::Word(0x1110);
static const System::Word CL_IMAGE_ELEMENT_SIZE = System::Word(0x1111);
static const System::Word CL_IMAGE_ROW_PITCH = System::Word(0x1112);
static const System::Word CL_IMAGE_SLICE_PITCH = System::Word(0x1113);
static const System::Word CL_IMAGE_WIDTH = System::Word(0x1114);
static const System::Word CL_IMAGE_HEIGHT = System::Word(0x1115);
static const System::Word CL_IMAGE_DEPTH = System::Word(0x1116);
static const System::Word CL_ADDRESS_NONE = System::Word(0x1130);
static const System::Word CL_ADDRESS_CLAMP_TO_EDGE = System::Word(0x1131);
static const System::Word CL_ADDRESS_CLAMP = System::Word(0x1132);
static const System::Word CL_ADDRESS_REPEAT = System::Word(0x1133);
static const System::Word CL_FILTER_NEAREST = System::Word(0x1140);
static const System::Word CL_FILTER_LINEAR = System::Word(0x1141);
static const System::Word CL_SAMPLER_REFERENCE_COUNT = System::Word(0x1150);
static const System::Word CL_SAMPLER_CONTEXT = System::Word(0x1151);
static const System::Word CL_SAMPLER_NORMALIZED_COORDS = System::Word(0x1152);
static const System::Word CL_SAMPLER_ADDRESSING_MODE = System::Word(0x1153);
static const System::Word CL_SAMPLER_FILTER_MODE = System::Word(0x1154);
static const System::Int8 CL_MAP_READ = System::Int8(0x1);
static const System::Int8 CL_MAP_WRITE = System::Int8(0x2);
static const System::Word CL_PROGRAM_REFERENCE_COUNT = System::Word(0x1160);
static const System::Word CL_PROGRAM_CONTEXT = System::Word(0x1161);
static const System::Word CL_PROGRAM_NUM_DEVICES = System::Word(0x1162);
static const System::Word CL_PROGRAM_DEVICES = System::Word(0x1163);
static const System::Word CL_PROGRAM_SOURCE = System::Word(0x1164);
static const System::Word CL_PROGRAM_BINARY_SIZES = System::Word(0x1165);
static const System::Word CL_PROGRAM_BINARIES = System::Word(0x1166);
static const System::Word CL_PROGRAM_BUILD_STATUS = System::Word(0x1181);
static const System::Word CL_PROGRAM_BUILD_OPTIONS = System::Word(0x1182);
static const System::Word CL_PROGRAM_BUILD_LOG = System::Word(0x1183);
static const System::Int8 CL_BUILD_SUCCESS = System::Int8(0x0);
static const System::Int8 CL_BUILD_NONE = System::Int8(-1);
static const System::Int8 CL_BUILD_ERROR = System::Int8(-2);
static const System::Int8 CL_BUILD_IN_PROGRESS = System::Int8(-3);
static const System::Word CL_KERNEL_FUNCTION_NAME = System::Word(0x1190);
static const System::Word CL_KERNEL_NUM_ARGS = System::Word(0x1191);
static const System::Word CL_KERNEL_REFERENCE_COUNT = System::Word(0x1192);
static const System::Word CL_KERNEL_CONTEXT = System::Word(0x1193);
static const System::Word CL_KERNEL_PROGRAM = System::Word(0x1194);
static const System::Word CL_KERNEL_WORK_GROUP_SIZE = System::Word(0x11b0);
static const System::Word CL_KERNEL_COMPILE_WORK_GROUP_SIZE = System::Word(0x11b1);
static const System::Word CL_KERNEL_LOCAL_MEM_SIZE = System::Word(0x11b2);
static const System::Word CL_EVENT_COMMAND_QUEUE = System::Word(0x11d0);
static const System::Word CL_EVENT_COMMAND_TYPE = System::Word(0x11d1);
static const System::Word CL_EVENT_REFERENCE_COUNT = System::Word(0x11d2);
static const System::Word CL_EVENT_COMMAND_EXECUTION_STATUS = System::Word(0x11d3);
static const System::Word CL_COMMAND_NDRANGE_KERNEL = System::Word(0x11f0);
static const System::Word CL_COMMAND_TASK = System::Word(0x11f1);
static const System::Word CL_COMMAND_NATIVE_KERNEL = System::Word(0x11f2);
static const System::Word CL_COMMAND_READ_BUFFER = System::Word(0x11f3);
static const System::Word CL_COMMAND_WRITE_BUFFER = System::Word(0x11f4);
static const System::Word CL_COMMAND_COPY_BUFFER = System::Word(0x11f5);
static const System::Word CL_COMMAND_READ_IMAGE = System::Word(0x11f6);
static const System::Word CL_COMMAND_WRITE_IMAGE = System::Word(0x11f7);
static const System::Word CL_COMMAND_COPY_IMAGE = System::Word(0x11f8);
static const System::Word CL_COMMAND_COPY_IMAGE_TO_BUFFER = System::Word(0x11f9);
static const System::Word CL_COMMAND_COPY_BUFFER_TO_IMAGE = System::Word(0x11fa);
static const System::Word CL_COMMAND_MAP_BUFFER = System::Word(0x11fb);
static const System::Word CL_COMMAND_MAP_IMAGE = System::Word(0x11fc);
static const System::Word CL_COMMAND_UNMAP_MEM_OBJECT = System::Word(0x11fd);
static const System::Word CL_COMMAND_MARKER = System::Word(0x11fe);
static const System::Word CL_COMMAND_ACQUIRE_GL_OBJECTS = System::Word(0x11ff);
static const System::Word CL_COMMAND_RELEASE_GL_OBJECTS = System::Word(0x1200);
static const System::Int8 CL_COMPLETE = System::Int8(0x0);
static const System::Int8 CL_RUNNING = System::Int8(0x1);
static const System::Int8 CL_SUBMITTED = System::Int8(0x2);
static const System::Int8 CL_QUEUED = System::Int8(0x3);
static const System::Word CL_PROFILING_COMMAND_QUEUED = System::Word(0x1280);
static const System::Word CL_PROFILING_COMMAND_SUBMIT = System::Word(0x1281);
static const System::Word CL_PROFILING_COMMAND_START = System::Word(0x1282);
static const System::Word CL_PROFILING_COMMAND_END = System::Word(0x1283);
extern PACKAGE int __stdcall (*clGetPlatformIDs)(unsigned num_entries, Pcl_platform_id platforms, Gls_cl_platform::Pcl_uint num_platforms);
extern PACKAGE int __stdcall (*clGetPlatformInfo)(Tcl_platform_id _platform, unsigned param_name, NativeUInt param_value_size, void * param_value, Gls_cl_platform::Psize_t param_value_size_ret);
extern PACKAGE int __stdcall (*clGetDeviceIDs)(Tcl_platform_id _platform, unsigned __int64 device_type, unsigned num_entries, Pcl_device_id devices, Gls_cl_platform::Pcl_uint num_devices);
extern PACKAGE int __stdcall (*clGetDeviceInfo)(Tcl_device_id device, unsigned param_name, NativeUInt param_value_size, void * param_value, Gls_cl_platform::Psize_t param_value_size_ret);
extern PACKAGE Tcl_context __stdcall (*clCreateContext)(Pcl_context_properties properties, unsigned num_devices, Pcl_device_id devices, logging_fn pfn_notify, void * user_data, Gls_cl_platform::Pcl_int errcode_ret);
extern PACKAGE Tcl_context __stdcall (*clCreateContextFromType)(Pcl_context_properties properties, unsigned __int64 device_type, logging_fn pfn_notify, void * user_data, Gls_cl_platform::Pcl_int errcode_ret);
extern PACKAGE int __stdcall (*clRetainContext)(Tcl_context context);
extern PACKAGE int __stdcall (*clReleaseContext)(Tcl_context context);
extern PACKAGE int __stdcall (*clGetContextInfo)(Tcl_context context, unsigned param_name, NativeUInt param_value_size, void * param_value, Gls_cl_platform::Psize_t param_value_size_ret);
extern PACKAGE Tcl_command_queue __stdcall (*clCreateCommandQueue)(Tcl_context context, Tcl_device_id device, unsigned __int64 properties, Gls_cl_platform::Pcl_int errcode_ret);
extern PACKAGE int __stdcall (*clRetainCommandQueue)(Tcl_command_queue command_queue);
extern PACKAGE int __stdcall (*clReleaseCommandQueue)(Tcl_command_queue command_queue);
extern PACKAGE int __stdcall (*clGetCommandQueueInfo)(Tcl_command_queue command_queue, unsigned param_name, NativeUInt param_value_size, void * param_value, Gls_cl_platform::Psize_t param_value_size_ret);
extern PACKAGE int __stdcall (*clSetCommandQueueProperty)(Tcl_command_queue command_queue, unsigned __int64 properties, unsigned enable, Pcl_command_queue_properties old_properties);
extern PACKAGE Tcl_mem __stdcall (*clCreateBuffer)(Tcl_context context, unsigned __int64 flags, NativeUInt size, void * host_ptr, Gls_cl_platform::Pcl_int errcode_ret);
extern PACKAGE Tcl_mem __stdcall (*clCreateImage2D)(Tcl_context context, unsigned __int64 flags, Pcl_image_format image_format, NativeUInt image_width, NativeUInt image_height, NativeUInt image_row_pitch, void * host_ptr, Gls_cl_platform::Pcl_int errcode_ret);
extern PACKAGE Tcl_mem __stdcall (*clCreateImage3D)(Tcl_context context, unsigned __int64 flags, Pcl_image_format image_format, NativeUInt image_width, NativeUInt image_height, NativeUInt image_depth, NativeUInt image_row_pitch, NativeUInt image_slice_pitch, void * host_ptr, Gls_cl_platform::Pcl_int errcode_ret);
extern PACKAGE int __stdcall (*clRetainMemObject)(Tcl_mem memobj);
extern PACKAGE int __stdcall (*clReleaseMemObject)(Tcl_mem memobj);
extern PACKAGE int __stdcall (*clGetSupportedImageFormats)(Tcl_context context, unsigned __int64 flags, unsigned image_type, unsigned num_entries, Pcl_image_format image_formats, Gls_cl_platform::Pcl_uint num_image_formats);
extern PACKAGE int __stdcall (*clGetMemObjectInfo)(Tcl_mem memobj, unsigned param_name, NativeUInt param_value_size, void * param_value, Gls_cl_platform::Psize_t param_value_size_ret);
extern PACKAGE int __stdcall (*clGetImageInfo)(Tcl_mem image, unsigned param_name, NativeUInt param_value_size, void * param_value, Gls_cl_platform::Psize_t param_value_size_ret);
extern PACKAGE Tcl_sampler __stdcall (*clCreateSampler)(Tcl_context context, unsigned normalized_coords, unsigned addressing_mode, unsigned filter_mode, Gls_cl_platform::Pcl_int errcode_ret);
extern PACKAGE int __stdcall (*clRetainSampler)(Tcl_sampler sampler);
extern PACKAGE int __stdcall (*clReleaseSampler)(Tcl_sampler sampler);
extern PACKAGE int __stdcall (*clGetSamplerInfo)(Tcl_sampler sampler, unsigned param_name, NativeUInt param_value_size, void * param_value, Gls_cl_platform::Psize_t param_value_size_ret);
extern PACKAGE Tcl_program __stdcall (*clCreateProgramWithSource)(Tcl_context context, unsigned count, System::PPAnsiChar strings, Gls_cl_platform::Psize_t lengths, Gls_cl_platform::Pcl_int errcode_ret);
extern PACKAGE Tcl_program __stdcall (*clCreateProgramWithBinary)(Tcl_context context, unsigned num_devices, Pcl_device_id device_list, Gls_cl_platform::Psize_t lengths, System::PPointer binaries, Gls_cl_platform::Pcl_int binary_status, Gls_cl_platform::Pcl_int errcode_ret);
extern PACKAGE int __stdcall (*clRetainProgram)(Tcl_program _program);
extern PACKAGE int __stdcall (*clReleaseProgram)(Tcl_program _program);
extern PACKAGE int __stdcall (*clBuildProgram)(Tcl_program _program, unsigned num_devices, Pcl_device_id device_list, char * options, ProgramBuilt_notify_fn pfn_notify, void * user_data);
extern PACKAGE int __stdcall (*clUnloadCompiler)(void);
extern PACKAGE int __stdcall (*clGetProgramInfo)(Tcl_program _program, unsigned param_name, NativeUInt param_value_size, void * param_value, Gls_cl_platform::Psize_t param_value_size_ret);
extern PACKAGE int __stdcall (*clGetProgramBuildInfo)(Tcl_program _program, Tcl_device_id device, unsigned param_name, NativeUInt param_value_size, void * param_value, Gls_cl_platform::Psize_t param_value_size_ret);
extern PACKAGE Tcl_kernel __stdcall (*clCreateKernel)(Tcl_program _program, char * kernel_name, Gls_cl_platform::Pcl_int errcode_ret);
extern PACKAGE int __stdcall (*clCreateKernelsInProgram)(Tcl_program _program, unsigned num_kernels, Pcl_kernel kernels, Gls_cl_platform::Pcl_uint num_kernels_ret);
extern PACKAGE int __stdcall (*clRetainKernel)(Tcl_kernel kernel);
extern PACKAGE int __stdcall (*clReleaseKernel)(Tcl_kernel kernel);
extern PACKAGE int __stdcall (*clSetKernelArg)(Tcl_kernel kernel, unsigned arg_index, NativeUInt arg_size, void * arg_value);
extern PACKAGE int __stdcall (*clGetKernelInfo)(Tcl_kernel kernel, unsigned param_name, NativeUInt param_value_size, void * param_value, Gls_cl_platform::Psize_t param_value_size_ret);
extern PACKAGE int __stdcall (*clGetKernelWorkGroupInfo)(Tcl_kernel kernel, Tcl_device_id device, unsigned param_name, NativeUInt param_value_size, void * param_value, Gls_cl_platform::Psize_t param_value_size_ret);
extern PACKAGE int __stdcall (*clWaitForEvents)(unsigned num_events, Pcl_event event_list);
extern PACKAGE int __stdcall (*clGetEventInfo)(Tcl_event event, unsigned param_name, NativeUInt param_value_size, void * param_value, Gls_cl_platform::Psize_t param_value_size_ret);
extern PACKAGE int __stdcall (*clRetainEvent)(Tcl_event event);
extern PACKAGE int __stdcall (*clReleaseEvent)(Tcl_event event);
extern PACKAGE int __stdcall (*clGetEventProfilingInfo)(Tcl_event event, unsigned param_name, NativeUInt param_value_size, void * param_value, Gls_cl_platform::Psize_t param_value_size_ret);
extern PACKAGE int __stdcall (*clFlush)(Tcl_command_queue command_queue);
extern PACKAGE int __stdcall (*clFinish)(Tcl_command_queue command_queue);
extern PACKAGE int __stdcall (*clEnqueueReadBuffer)(Tcl_command_queue command_queue, Tcl_mem buffer, unsigned blocking_read, NativeUInt offset, NativeUInt cb, void * ptr, unsigned num_events_in_wait_list, Pcl_event event_wait_list, Pcl_event event);
extern PACKAGE int __stdcall (*clEnqueueWriteBuffer)(Tcl_command_queue command_queue, Tcl_mem buffer, unsigned blocking_write, NativeUInt offset, NativeUInt cb, void * ptr, unsigned num_events_in_wait_list, Pcl_event event_wait_list, Pcl_event event);
extern PACKAGE int __stdcall (*clEnqueueCopyBuffer)(Tcl_command_queue command_queue, Tcl_mem src_buffer, Tcl_mem dst_buffer, NativeUInt src_offset, NativeUInt dst_offset, NativeUInt cb, unsigned num_events_in_wait_list, Pcl_event event_wait_list, Pcl_event event);
extern PACKAGE int __stdcall (*clEnqueueReadImage)(Tcl_command_queue command_queue, Tcl_mem image, unsigned blocking_read, Gls_cl_platform::Psize_t origin, Gls_cl_platform::Psize_t region, NativeUInt row_pitch, NativeUInt slice_pitch, void * ptr, unsigned num_events_in_wait_list, Pcl_event event_wait_list, Pcl_event event);
extern PACKAGE int __stdcall (*clEnqueueWriteImage)(Tcl_command_queue command_queue, Tcl_mem image, unsigned blocking_write, Gls_cl_platform::Psize_t origin, Gls_cl_platform::Psize_t region, NativeUInt input_row_pitch, NativeUInt input_slice_pitch, void * ptr, unsigned num_events_in_wait_list, Pcl_event event_wait_list, Pcl_event event);
extern PACKAGE int __stdcall (*clEnqueueCopyImage)(Tcl_command_queue command_queue, Tcl_mem src_image, Tcl_mem dst_image, Gls_cl_platform::Psize_t src_origin, Gls_cl_platform::Psize_t dst_origin, Gls_cl_platform::Psize_t region, unsigned num_events_in_wait_list, Pcl_event event_wait_list, Pcl_event event);
extern PACKAGE int __stdcall (*clEnqueueCopyImageToBuffer)(Tcl_command_queue command_queue, Tcl_mem src_image, Tcl_mem dst_buffer, Gls_cl_platform::Psize_t src_origin, Gls_cl_platform::Psize_t region, NativeUInt dst_offset, unsigned num_events_in_wait_list, Pcl_event event_wait_list, Pcl_event event);
extern PACKAGE int __stdcall (*clEnqueueCopyBufferToImage)(Tcl_command_queue command_queue, Tcl_mem src_buffer, Tcl_mem dst_image, NativeUInt src_offset, Gls_cl_platform::Psize_t dst_origin, Gls_cl_platform::Psize_t region, unsigned num_events_in_wait_list, Pcl_event event_wait_list, Pcl_event event);
extern PACKAGE void * __stdcall (*clEnqueueMapBuffer)(Tcl_command_queue command_queue, Tcl_mem buffer, unsigned blocking_map, unsigned __int64 map_flags, NativeUInt offset, NativeUInt cb, unsigned num_events_in_wait_list, Pcl_event event_wait_list, Pcl_event event, Gls_cl_platform::Pcl_int errcode_ret);
extern PACKAGE void * __stdcall (*clEnqueueMapImage)(Tcl_command_queue command_queue, Tcl_mem image, unsigned blocking_map, unsigned __int64 map_flags, Gls_cl_platform::Psize_t origin, Gls_cl_platform::Psize_t region, Gls_cl_platform::Psize_t image_row_pitch, Gls_cl_platform::Psize_t image_slice_pitch, unsigned num_events_in_wait_list, Pcl_event event_wait_list, Pcl_event event, Gls_cl_platform::Pcl_int errcode_ret);
extern PACKAGE int __stdcall (*clEnqueueUnmapMemObject)(Tcl_command_queue command_queue, Tcl_mem memobj, void * mapped_ptr, unsigned num_events_in_wait_list, Pcl_event event_wait_list, Pcl_event event);
extern PACKAGE int __stdcall (*clEnqueueNDRangeKernel)(Tcl_command_queue command_queue, Tcl_kernel kernel, unsigned work_dim, Gls_cl_platform::Psize_t global_work_offset, Gls_cl_platform::Psize_t global_work_size, Gls_cl_platform::Psize_t local_work_size, unsigned num_events_in_wait_list, Pcl_event event_wait_list, Pcl_event event);
extern PACKAGE int __stdcall (*clEnqueueTask)(Tcl_command_queue command_queue, Tcl_kernel kernel, unsigned num_events_in_wait_list, Pcl_event event_wait_list, Pcl_event event);
extern PACKAGE int __stdcall (*clEnqueueNativeKernel)(Tcl_command_queue command_queue, EnqueueNativeKernel_user_func user_func, void * args, NativeUInt cb_args, unsigned num_mem_objects, Pcl_mem mem_list, System::PPointer args_mem_loc, unsigned num_events_in_wait_list, Pcl_event event_wait_list, Pcl_event event);
extern PACKAGE int __stdcall (*clEnqueueMarker)(Tcl_command_queue command_queue, Pcl_event event);
extern PACKAGE int __stdcall (*clEnqueueWaitForEvents)(Tcl_command_queue command_queue, unsigned num_events, Pcl_event event_list);
extern PACKAGE int __stdcall (*clEnqueueBarrier)(Tcl_command_queue command_queue);
extern PACKAGE void * __stdcall (*clGetExtensionFunctionAddress)(char * func_name);
extern PACKAGE bool __fastcall InitOpenCL(void);
extern PACKAGE void __fastcall CloseOpenCL(void);
extern PACKAGE bool __fastcall InitOpenCLFromLibrary(const System::WideString CLName);
extern PACKAGE bool __fastcall IsOpenCLInitialized(void);
}	/* namespace Gls_cl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_CL)
using namespace Gls_cl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_clHPP
