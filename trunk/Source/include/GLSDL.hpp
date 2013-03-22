// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSDL.pas' rev: 24.00 (Win32)

#ifndef GlsdlHPP
#define GlsdlHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glsdl
{
//-- type declarations -------------------------------------------------------
enum TSDL_Bool : unsigned char { SDL_FALSE, SDL_TRUE };

typedef System::StaticArray<System::Byte, 1073741824> TUInt8Array;

typedef TUInt8Array *PUInt8Array;

typedef System::Byte *PUInt8;

typedef System::Byte UInt8;

typedef System::Word *PUInt16;

typedef System::Word UInt16;

typedef short SInt16;

typedef unsigned *PUInt32;

typedef unsigned UInt32;

typedef int SInt32;

typedef int *PInt;

struct UInt64;
typedef UInt64 *PUInt64;

struct DECLSPEC_DRECORD UInt64
{
public:
	unsigned hi;
	unsigned lo;
};


struct SInt64;
typedef SInt64 *PSInt64;

struct DECLSPEC_DRECORD SInt64
{
public:
	unsigned hi;
	unsigned lo;
};


enum TSDL_errorcode : unsigned char { SDL_ENOMEM, SDL_EFREAD, SDL_EFWRITE, SDL_EFSEEK, SDL_LASTERROR };

typedef TSDL_errorcode SDL_errorcode;

struct DECLSPEC_DRECORD TArg
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			System::StaticArray<System::Byte, 128> buf;
		};
		struct 
		{
			double value_f;
		};
		struct 
		{
			int value_i;
		};
		struct 
		{
			void *value_ptr;
		};
		
	};
	#pragma pack(pop)
};


struct TSDL_error;
typedef TSDL_error *PSDL_error;

struct DECLSPEC_DRECORD TSDL_error
{
public:
	int error;
	System::StaticArray<System::Byte, 128> key;
	int argc;
	System::StaticArray<TArg, 5> args;
};


struct DECLSPEC_DRECORD TStdio
{
public:
	int autoclose;
	void *fp;
};


struct DECLSPEC_DRECORD TMem
{
public:
	System::Byte *base;
	System::Byte *here;
	System::Byte *stop;
};


struct DECLSPEC_DRECORD TUnknown
{
public:
	void *data1;
};


struct TSDL_RWops;
typedef TSDL_RWops *PSDL_RWops;

typedef int __cdecl (*TSeek)(PSDL_RWops context, int offset, int whence);

typedef int __cdecl (*TRead)(PSDL_RWops context, void * Ptr, int size, int maxnum);

typedef int __cdecl (*TWrite)(PSDL_RWops context, void * Ptr, int size, int num);

typedef int __cdecl (*TClose)(PSDL_RWops context);

struct DECLSPEC_DRECORD TSDL_RWops
{
public:
	TSeek seek;
	TRead read;
	TWrite write;
	TClose close;
	unsigned type_;
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TUnknown unknown;
		};
		struct 
		{
			TMem mem;
		};
		struct 
		{
			TStdio stdio;
		};
		
	};
	#pragma pack(pop)
};


typedef TSDL_RWops SDL_RWops;

typedef unsigned __cdecl (*TSDL_TimerCallback)(unsigned interval);

typedef unsigned __cdecl (*TSDL_NewTimerCallback)(unsigned interval, void * param);

struct TSDL_TimerID;
typedef TSDL_TimerID *PSDL_TimerID;

struct DECLSPEC_DRECORD TSDL_TimerID
{
public:
	unsigned interval;
	TSDL_NewTimerCallback callback;
	void *param;
	unsigned last_alarm;
	TSDL_TimerID *next;
};


struct TSDL_AudioSpec;
typedef TSDL_AudioSpec *PSDL_AudioSpec;

struct DECLSPEC_DRECORD TSDL_AudioSpec
{
public:
	int freq;
	System::Word format;
	System::Byte channels;
	System::Byte silence;
	System::Word samples;
	System::Word padding;
	unsigned size;
	void __cdecl (*callback)(void * userdata, PUInt8 stream, int len);
	void *userdata;
};


struct TSDL_AudioCVT;
typedef TSDL_AudioCVT *PSDL_AudioCVT;

struct TSDL_AudioCVTFilter;
typedef TSDL_AudioCVTFilter *PSDL_AudioCVTFilter;

struct DECLSPEC_DRECORD TSDL_AudioCVTFilter
{
public:
	TSDL_AudioCVT *cvt;
	System::Word format;
};


typedef System::StaticArray<TSDL_AudioCVTFilter, 10> TSDL_AudioCVTFilterArray;

typedef TSDL_AudioCVTFilterArray *PSDL_AudioCVTFilterArray;

struct DECLSPEC_DRECORD TSDL_AudioCVT
{
public:
	int needed;
	System::Word src_format;
	System::Word dst_format;
	double rate_incr;
	System::Byte *buf;
	int len;
	int len_cvt;
	int len_mult;
	double len_ratio;
	TSDL_AudioCVTFilterArray *filters;
	int filter_index;
};


enum TSDL_Audiostatus : unsigned char { SDL_AUDIO_STOPPED, SDL_AUDIO_PLAYING, SDL_AUDIO_PAUSED };

enum TSDL_CDStatus : unsigned char { CD_ERROR, CD_TRAYEMPTY, CD_STOPPED, CD_PLAYING, CD_PAUSED };

struct TSDL_CDTrack;
typedef TSDL_CDTrack *PSDL_CDTrack;

struct DECLSPEC_DRECORD TSDL_CDTrack
{
public:
	int id;
	System::Byte type_;
	System::Word unused;
	unsigned length;
	unsigned offset;
};


struct TSDL_CD;
typedef TSDL_CD *PSDL_CD;

struct DECLSPEC_DRECORD TSDL_CD
{
public:
	int id;
	TSDL_CDStatus status;
	int numtracks;
	int cur_track;
	int cur_frame;
	System::StaticArray<TSDL_CDTrack, 100> track;
};


struct TTransAxis;
typedef TTransAxis *PTransAxis;

struct DECLSPEC_DRECORD TTransAxis
{
public:
	int offset;
	float scale;
};


struct TJoystick_hwdata;
typedef TJoystick_hwdata *PJoystick_hwdata;

struct DECLSPEC_DRECORD TJoystick_hwdata
{
public:
	int id;
	System::StaticArray<TTransAxis, 6> transaxis;
};


struct TBallDelta;
typedef TBallDelta *PBallDelta;

struct DECLSPEC_DRECORD TBallDelta
{
public:
	int dx;
	int dy;
};


struct TSDL_Joystick;
typedef TSDL_Joystick *PSDL_Joystick;

struct DECLSPEC_DRECORD TSDL_Joystick
{
public:
	System::Byte index;
	char *name;
	int naxes;
	System::Word *axes;
	int nhats;
	System::Byte *hats;
	int nballs;
	TBallDelta *balls;
	int nbuttons;
	System::Byte *buttons;
	TJoystick_hwdata *hwdata;
	int ref_count;
};


struct TSDL_version;
typedef TSDL_version *PSDL_version;

struct DECLSPEC_DRECORD TSDL_version
{
public:
	System::Byte major;
	System::Byte minor;
	System::Byte patch;
};


typedef unsigned TSDLKey;

typedef unsigned TSDLMod;

struct TSDL_KeySym;
typedef TSDL_KeySym *PSDL_KeySym;

struct DECLSPEC_DRECORD TSDL_KeySym
{
public:
	System::Byte scancode;
	unsigned sym;
	unsigned modifier;
	System::Word unicode;
};


enum TSDL_EventAction : unsigned char { SDL_ADDEVENT, SDL_PEEKEVENT, SDL_GETEVENT };

struct DECLSPEC_DRECORD TSDL_ActiveEvent
{
public:
	System::Byte type_;
	System::Byte gain;
	System::Byte state;
};


struct DECLSPEC_DRECORD TSDL_KeyboardEvent
{
public:
	System::Byte type_;
	System::Byte which;
	System::Byte state;
	TSDL_KeySym keysym;
};


struct DECLSPEC_DRECORD TSDL_MouseMotionEvent
{
public:
	System::Byte type_;
	System::Byte which;
	System::Byte state;
	System::Word x;
	System::Word y;
	short xrel;
	short yrel;
};


struct DECLSPEC_DRECORD TSDL_MouseButtonEvent
{
public:
	System::Byte type_;
	System::Byte which;
	System::Byte button;
	System::Byte state;
	System::Word x;
	System::Word y;
};


struct DECLSPEC_DRECORD TSDL_JoyAxisEvent
{
public:
	System::Byte type_;
	System::Byte which;
	System::Byte axis;
	short value;
};


struct DECLSPEC_DRECORD TSDL_JoyBallEvent
{
public:
	System::Byte type_;
	System::Byte which;
	System::Byte ball;
	short xrel;
	short yrel;
};


struct DECLSPEC_DRECORD TSDL_JoyHatEvent
{
public:
	System::Byte type_;
	System::Byte which;
	System::Byte hat;
	System::Byte value;
};


struct DECLSPEC_DRECORD TSDL_JoyButtonEvent
{
public:
	System::Byte type_;
	System::Byte which;
	System::Byte button;
	System::Byte state;
};


struct DECLSPEC_DRECORD TSDL_ResizeEvent
{
public:
	System::Byte type_;
	int w;
	int h;
};


struct TSDL_QuitEvent;
typedef TSDL_QuitEvent *PSDL_QuitEvent;

struct DECLSPEC_DRECORD TSDL_QuitEvent
{
public:
	System::Byte type_;
};


struct TSDL_UserEvent;
typedef TSDL_UserEvent *PSDL_UserEvent;

struct DECLSPEC_DRECORD TSDL_UserEvent
{
public:
	System::Byte type_;
	int code;
	void *data1;
	void *data2;
};


struct TSDL_ExposeEvent;
typedef TSDL_ExposeEvent *PSDL_ExposeEvent;

struct DECLSPEC_DRECORD TSDL_ExposeEvent
{
public:
	System::Byte type_;
};


struct TSDL_SysWMmsg;
typedef TSDL_SysWMmsg *PSDL_SysWMmsg;

struct DECLSPEC_DRECORD TSDL_SysWMmsg
{
public:
	TSDL_version version;
	HWND h_wnd;
	unsigned msg;
	NativeUInt w_Param;
	NativeInt lParam;
};


struct TSDL_SysWMEvent;
typedef TSDL_SysWMEvent *PSDL_SysWMEvent;

struct DECLSPEC_DRECORD TSDL_SysWMEvent
{
public:
	System::Byte type_;
	TSDL_SysWMmsg *msg;
};


struct TSDL_Event;
typedef TSDL_Event *PSDL_Event;

struct DECLSPEC_DRECORD TSDL_Event
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TSDL_SysWMEvent syswm;
		};
		struct 
		{
			TSDL_UserEvent user;
		};
		struct 
		{
			TSDL_QuitEvent quit;
		};
		struct 
		{
			TSDL_ResizeEvent resize;
		};
		struct 
		{
			TSDL_JoyButtonEvent jbutton;
		};
		struct 
		{
			TSDL_JoyHatEvent jhat;
		};
		struct 
		{
			TSDL_JoyBallEvent jball;
		};
		struct 
		{
			TSDL_JoyAxisEvent jaxis;
		};
		struct 
		{
			TSDL_MouseButtonEvent button;
		};
		struct 
		{
			TSDL_MouseMotionEvent motion;
		};
		struct 
		{
			TSDL_KeyboardEvent key;
		};
		struct 
		{
			TSDL_ActiveEvent active;
		};
		struct 
		{
			System::Byte type_;
		};
		
	};
	#pragma pack(pop)
};


typedef int __cdecl (*TSDL_EventFilter)(PSDL_Event event);

struct TSDL_Rect;
typedef TSDL_Rect *PSDL_Rect;

typedef PSDL_Rect *PPSDL_Rect;

struct DECLSPEC_DRECORD TSDL_Rect
{
public:
	short x;
	short y;
	System::Word w;
	System::Word h;
};


typedef TSDL_Rect SDL_Rect;

struct TSDL_Color;
typedef TSDL_Color *PSDL_Color;

struct DECLSPEC_DRECORD TSDL_Color
{
public:
	System::Byte r;
	System::Byte g;
	System::Byte b;
	System::Byte unused;
};


typedef System::StaticArray<TSDL_Color, 65001> TSDL_ColorArray;

typedef TSDL_ColorArray *PSDL_ColorArray;

struct TSDL_Palette;
typedef TSDL_Palette *PSDL_Palette;

struct DECLSPEC_DRECORD TSDL_Palette
{
public:
	int ncolors;
	TSDL_ColorArray *colors;
};


struct TSDL_PixelFormat;
typedef TSDL_PixelFormat *PSDL_PixelFormat;

struct DECLSPEC_DRECORD TSDL_PixelFormat
{
public:
	TSDL_Palette *palette;
	System::Byte BitsPerPixel;
	System::Byte BytesPerPixel;
	System::Byte Rloss;
	System::Byte Gloss;
	System::Byte Bloss;
	System::Byte Aloss;
	System::Byte Rshift;
	System::Byte Gshift;
	System::Byte Bshift;
	System::Byte Ashift;
	unsigned RMask;
	unsigned GMask;
	unsigned BMask;
	unsigned AMask;
	unsigned colorkey;
	System::Byte alpha;
};


struct TSDL_BlitInfo;
typedef TSDL_BlitInfo *PSDL_BlitInfo;

struct DECLSPEC_DRECORD TSDL_BlitInfo
{
public:
	System::Byte *s_pixels;
	int s_width;
	int s_height;
	int s_skip;
	System::Byte *d_pixels;
	int d_width;
	int d_height;
	int d_skip;
	void *aux_data;
	TSDL_PixelFormat *src;
	System::Byte *table;
	TSDL_PixelFormat *dst;
};


struct TSDL_Surface;
typedef TSDL_Surface *PSDL_Surface;

typedef int __cdecl (*TSDL_Blit)(PSDL_Surface src, PSDL_Rect srcrect, PSDL_Surface dst, PSDL_Rect dstrect);

struct DECLSPEC_DRECORD TSDL_Surface
{
public:
	unsigned flags;
	TSDL_PixelFormat *format;
	int w;
	int h;
	System::Word pitch;
	void *pixels;
	int offset;
	void *hwdata;
	TSDL_Rect clip_rect;
	unsigned unused1;
	unsigned locked;
	void *Blitmap;
	unsigned format_version;
	int refcount;
};


struct TSDL_VideoInfo;
typedef TSDL_VideoInfo *PSDL_VideoInfo;

struct DECLSPEC_DRECORD TSDL_VideoInfo
{
public:
	System::Byte hw_available;
	System::Byte blit_hw;
	System::Byte UnusedBits3;
	unsigned video_mem;
	TSDL_PixelFormat *vfmt;
};


struct TSDL_Overlay;
typedef TSDL_Overlay *PSDL_Overlay;

struct DECLSPEC_DRECORD TSDL_Overlay
{
public:
	unsigned format;
	int w;
	int h;
	int planes;
	System::Word *pitches;
	System::Byte *pixels;
	unsigned hw_overlay;
};


enum TSDL_GLAttr : unsigned char { SDL_GL_RED_SIZE, SDL_GL_GREEN_SIZE, SDL_GL_BLUE_SIZE, SDL_GL_ALPHA_SIZE, SDL_GL_BUFFER_SIZE, SDL_GL_DOUBLEBUFFER, SDL_GL_DEPTH_SIZE, SDL_GL_STENCIL_SIZE, SDL_GL_ACCUM_RED_SIZE, SDL_GL_ACCUM_GREEN_SIZE, SDL_GL_ACCUM_BLUE_SIZE, SDL_GL_ACCUM_ALPHA_SIZE };

enum TSDL_GrabMode : unsigned char { SDL_GRAB_QUERY, SDL_GRAB_OFF, SDL_GRAB_ON };

struct TSDL_Cursor;
typedef TSDL_Cursor *PSDL_Cursor;

struct DECLSPEC_DRECORD TSDL_Cursor
{
public:
	TSDL_Rect area;
	short hot_x;
	short hot_y;
	System::Byte *data;
	System::Byte *mask;
	System::StaticArray<PUInt8, 2> save;
	void *wm_cursor;
};


struct TSDL_Mutex;
typedef TSDL_Mutex *PSDL_Mutex;

struct DECLSPEC_DRECORD TSDL_Mutex
{
public:
	NativeUInt id;
};


struct TSDL_semaphore;
typedef TSDL_semaphore *PSDL_semaphore;

struct DECLSPEC_DRECORD TSDL_semaphore
{
public:
	NativeUInt id;
	unsigned count;
};


typedef TSDL_semaphore *PSDL_Sem;

typedef TSDL_semaphore TSDL_Sem;

struct TSDL_Cond;
typedef TSDL_Cond *PSDL_Cond;

struct DECLSPEC_DRECORD TSDL_Cond
{
public:
	TSDL_Mutex *lock;
	int waiting;
	int signals;
	TSDL_semaphore *wait_sem;
	TSDL_semaphore *wait_done;
};


typedef NativeUInt TSYS_ThreadHandle;

struct TSDL_Thread;
typedef TSDL_Thread *PSDL_Thread;

struct DECLSPEC_DRECORD TSDL_Thread
{
public:
	unsigned threadid;
	NativeUInt handle;
	int status;
	TSDL_error errbuf;
	void *data;
};


typedef System::StaticArray<System::Byte, 65001> TKeyStateArr;

typedef TKeyStateArr *PKeyStateArr;

typedef int *PInteger;

typedef System::Byte *PByte;

typedef System::Word *PWord;

typedef unsigned *PLongWord;

typedef System::StaticArray<System::Byte, 32768> TByteArray;

typedef TByteArray *PByteArray;

typedef System::StaticArray<System::Word, 16384> TWordArray;

typedef TWordArray *PWordArray;

struct TPoint;
typedef TPoint *PPoint;

struct DECLSPEC_DRECORD TPoint
{
public:
	int x;
	int y;
};


struct TRect;
typedef TRect *PRect;

struct DECLSPEC_DRECORD TRect
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TPoint TopLeft;
			TPoint BottomRight;
		};
		struct 
		{
			int Left;
			int Top;
			int Right;
			int Bottom;
		};
		
	};
	#pragma pack(pop)
};


//-- var, const, procedure ---------------------------------------------------
#define LibName L"SDL.dll"
static const System::Int8 SDL_INIT_TIMER = System::Int8(0x1);
static const System::Int8 SDL_INIT_AUDIO = System::Int8(0x10);
static const System::Int8 SDL_INIT_VIDEO = System::Int8(0x20);
static const System::Word SDL_INIT_CDROM = System::Word(0x100);
static const System::Word SDL_INIT_JOYSTICK = System::Word(0x200);
static const int SDL_INIT_NOPARACHUTE = int(0x100000);
static const int SDL_INIT_EVENTTHREAD = int(0x1000000);
static const System::Word SDL_INIT_EVERYTHING = System::Word(0xffff);
static const System::Byte ERR_MAX_STRLEN = System::Byte(0x80);
static const System::Int8 ERR_MAX_ARGS = System::Int8(0x5);
static const System::Int8 SDL_PRESSED = System::Int8(0x1);
static const System::Int8 SDL_RELEASED = System::Int8(0x0);
static const System::Int8 SDL_TIMESLICE = System::Int8(0xa);
static const System::Int8 TIMER_RESOLUTION = System::Int8(0xa);
static const System::Int8 AUDIO_U8 = System::Int8(0x8);
static const System::Word AUDIO_S8 = System::Word(0x8008);
static const System::Int8 AUDIO_U16LSB = System::Int8(0x10);
static const System::Word AUDIO_S16LSB = System::Word(0x8010);
static const System::Word AUDIO_U16MSB = System::Word(0x1010);
static const System::Word AUDIO_S16MSB = System::Word(0x9010);
static const System::Int8 AUDIO_U16 = System::Int8(0x10);
static const System::Word AUDIO_S16 = System::Word(0x8010);
static const System::Int8 SDL_MAX_TRACKS = System::Int8(0x63);
static const System::Int8 SDL_AUDIO_TRACK = System::Int8(0x0);
static const System::Int8 SDL_DATA_TRACK = System::Int8(0x4);
static const System::Int8 CD_FPS = System::Int8(0x4b);
static const System::Word SDL_LIL_ENDIAN = System::Word(0x4d2);
static const System::Word SDL_BIG_ENDIAN = System::Word(0x10e1);
static const System::Word SDL_BYTEORDER = System::Word(0x4d2);
static const System::Int8 AUDIO_U16SYS = System::Int8(0x10);
static const System::Word AUDIO_S16SYS = System::Word(0x8010);
static const System::Byte SDL_MIX_MAXVOLUME = System::Byte(0x80);
static const System::Int8 MAX_JOYSTICKS = System::Int8(0x2);
static const System::Int8 MAX_AXES = System::Int8(0x6);
static const System::Int8 MAX_BUTTONS = System::Int8(0x20);
static const short AXIS_MIN = short(-32768);
static const System::Word AXIS_MAX = System::Word(0x7fff);
static const System::Extended JOY_AXIS_THRESHOLD = 6.553500E+02;
static const System::Int8 SDL_HAT_CENTERED = System::Int8(0x0);
static const System::Int8 SDL_HAT_UP = System::Int8(0x1);
static const System::Int8 SDL_HAT_RIGHT = System::Int8(0x2);
static const System::Int8 SDL_HAT_DOWN = System::Int8(0x4);
static const System::Int8 SDL_HAT_LEFT = System::Int8(0x8);
static const System::Int8 SDL_HAT_RIGHTUP = System::Int8(0x3);
static const System::Int8 SDL_HAT_RIGHTDOWN = System::Int8(0x6);
static const System::Int8 SDL_HAT_LEFTUP = System::Int8(0x9);
static const System::Int8 SDL_HAT_LEFTDOWN = System::Int8(0xc);
static const System::Int8 SDL_MAJOR_VERSION = System::Int8(0x1);
static const System::Int8 SDL_MINOR_VERSION = System::Int8(0x2);
static const System::Int8 SDL_PATCHLEVEL = System::Int8(0x3);
static const System::Int8 SDL_NOEVENT = System::Int8(0x0);
static const System::Int8 SDL_ACTIVEEVENT = System::Int8(0x1);
static const System::Int8 SDL_KEYDOWN = System::Int8(0x2);
static const System::Int8 SDL_KEYUP = System::Int8(0x3);
static const System::Int8 SDL_MOUSEMOTION = System::Int8(0x4);
static const System::Int8 SDL_MOUSEBUTTONDOWN = System::Int8(0x5);
static const System::Int8 SDL_MOUSEBUTTONUP = System::Int8(0x6);
static const System::Int8 SDL_JOYAXISMOTION = System::Int8(0x7);
static const System::Int8 SDL_JOYBALLMOTION = System::Int8(0x8);
static const System::Int8 SDL_JOYHATMOTION = System::Int8(0x9);
static const System::Int8 SDL_JOYBUTTONDOWN = System::Int8(0xa);
static const System::Int8 SDL_JOYBUTTONUP = System::Int8(0xb);
static const System::Int8 SDL_QUITEV = System::Int8(0xc);
static const System::Int8 SDL_SYSWMEVENT = System::Int8(0xd);
static const System::Int8 SDL_EVENT_RESERVEDA = System::Int8(0xe);
static const System::Int8 SDL_EVENT_RESERVED = System::Int8(0xf);
static const System::Int8 SDL_VIDEORESIZE = System::Int8(0x10);
static const System::Int8 SDL_VIDEOEXPOSE = System::Int8(0x11);
static const System::Int8 SDL_EVENT_RESERVED2 = System::Int8(0x12);
static const System::Int8 SDL_EVENT_RESERVED3 = System::Int8(0x13);
static const System::Int8 SDL_EVENT_RESERVED4 = System::Int8(0x14);
static const System::Int8 SDL_EVENT_RESERVED5 = System::Int8(0x15);
static const System::Int8 SDL_EVENT_RESERVED6 = System::Int8(0x16);
static const System::Int8 SDL_EVENT_RESERVED7 = System::Int8(0x17);
static const System::Int8 SDL_USEREVENT = System::Int8(0x18);
static const System::Int8 SDL_NUMEVENTS = System::Int8(0x20);
static const unsigned SDL_ALLEVENTS = unsigned(0xffffffff);
static const System::Int8 SDL_ACTIVEEVENTMASK = System::Int8(0x2);
static const System::Int8 SDL_KEYDOWNMASK = System::Int8(0x4);
static const System::Int8 SDL_KEYUPMASK = System::Int8(0x8);
static const System::Int8 SDL_MOUSEMOTIONMASK = System::Int8(0x10);
static const System::Int8 SDL_MOUSEBUTTONDOWNMASK = System::Int8(0x20);
static const System::Int8 SDL_MOUSEBUTTONUPMASK = System::Int8(0x40);
static const System::Int8 SDL_MOUSEEVENTMASK = System::Int8(0x70);
static const System::Byte SDL_JOYAXISMOTIONMASK = System::Byte(0x80);
static const System::Word SDL_JOYBALLMOTIONMASK = System::Word(0x100);
static const System::Word SDL_JOYHATMOTIONMASK = System::Word(0x200);
static const System::Word SDL_JOYBUTTONDOWNMASK = System::Word(0x400);
static const System::Word SDL_JOYBUTTONUPMASK = System::Word(0x800);
static const System::Word SDL_JOYEVENTMASK = System::Word(0xf80);
static const int SDL_VIDEORESIZEMASK = int(0x10000);
static const System::Word SDL_QUITMASK = System::Word(0x1000);
static const System::Word SDL_SYSWMEVENTMASK = System::Word(0x2000);
static const System::Int8 SDL_QUERY = System::Int8(-1);
static const System::Int8 SDL_IGNORE = System::Int8(0x0);
static const System::Int8 SDL_DISABLE = System::Int8(0x0);
static const System::Int8 SDL_ENABLE = System::Int8(0x1);
static const unsigned SDL_ALL_HOTKEYS = unsigned(0xffffffff);
static const System::Word SDL_DEFAULT_REPEAT_DELAY = System::Word(0x1f4);
static const System::Int8 SDL_DEFAULT_REPEAT_INTERVAL = System::Int8(0x1e);
static const System::Int8 SDLK_UNKNOWN = System::Int8(0x0);
static const System::Int8 SDLK_FIRST = System::Int8(0x0);
static const System::Int8 SDLK_BACKSPACE = System::Int8(0x8);
static const System::Int8 SDLK_TAB = System::Int8(0x9);
static const System::Int8 SDLK_CLEAR = System::Int8(0xc);
static const System::Int8 SDLK_RETURN = System::Int8(0xd);
static const System::Int8 SDLK_PAUSE = System::Int8(0x13);
static const System::Int8 SDLK_ESCAPE = System::Int8(0x1b);
static const System::Int8 SDLK_SPACE = System::Int8(0x20);
static const System::Int8 SDLK_EXCLAIM = System::Int8(0x21);
static const System::Int8 SDLK_QUOTEDBL = System::Int8(0x22);
static const System::Int8 SDLK_HASH = System::Int8(0x23);
static const System::Int8 SDLK_DOLLAR = System::Int8(0x24);
static const System::Int8 SDLK_AMPERSAND = System::Int8(0x26);
static const System::Int8 SDLK_QUOTE = System::Int8(0x27);
static const System::Int8 SDLK_LEFTPAREN = System::Int8(0x28);
static const System::Int8 SDLK_RIGHTPAREN = System::Int8(0x29);
static const System::Int8 SDLK_ASTERISK = System::Int8(0x2a);
static const System::Int8 SDLK_PLUS = System::Int8(0x2b);
static const System::Int8 SDLK_COMMA = System::Int8(0x2c);
static const System::Int8 SDLK_MINUS = System::Int8(0x2d);
static const System::Int8 SDLK_PERIOD = System::Int8(0x2e);
static const System::Int8 SDLK_SLASH = System::Int8(0x2f);
static const System::Int8 SDLK_0 = System::Int8(0x30);
static const System::Int8 SDLK_1 = System::Int8(0x31);
static const System::Int8 SDLK_2 = System::Int8(0x32);
static const System::Int8 SDLK_3 = System::Int8(0x33);
static const System::Int8 SDLK_4 = System::Int8(0x34);
static const System::Int8 SDLK_5 = System::Int8(0x35);
static const System::Int8 SDLK_6 = System::Int8(0x36);
static const System::Int8 SDLK_7 = System::Int8(0x37);
static const System::Int8 SDLK_8 = System::Int8(0x38);
static const System::Int8 SDLK_9 = System::Int8(0x39);
static const System::Int8 SDLK_COLON = System::Int8(0x3a);
static const System::Int8 SDLK_SEMICOLON = System::Int8(0x3b);
static const System::Int8 SDLK_LESS = System::Int8(0x3c);
static const System::Int8 SDLK_EQUALS = System::Int8(0x3d);
static const System::Int8 SDLK_GREATER = System::Int8(0x3e);
static const System::Int8 SDLK_QUESTION = System::Int8(0x3f);
static const System::Int8 SDLK_AT = System::Int8(0x40);
static const System::Int8 SDLK_LEFTBRACKET = System::Int8(0x5b);
static const System::Int8 SDLK_BACKSLASH = System::Int8(0x5c);
static const System::Int8 SDLK_RIGHTBRACKET = System::Int8(0x5d);
static const System::Int8 SDLK_CARET = System::Int8(0x5e);
static const System::Int8 SDLK_UNDERSCORE = System::Int8(0x5f);
static const System::Int8 SDLK_BACKQUOTE = System::Int8(0x60);
static const System::Int8 SDLK_a = System::Int8(0x61);
static const System::Int8 SDLK_b = System::Int8(0x62);
static const System::Int8 SDLK_c = System::Int8(0x63);
static const System::Int8 SDLK_d = System::Int8(0x64);
static const System::Int8 SDLK_e = System::Int8(0x65);
static const System::Int8 SDLK_f = System::Int8(0x66);
static const System::Int8 SDLK_g = System::Int8(0x67);
static const System::Int8 SDLK_h = System::Int8(0x68);
static const System::Int8 SDLK_i = System::Int8(0x69);
static const System::Int8 SDLK_j = System::Int8(0x6a);
static const System::Int8 SDLK_k = System::Int8(0x6b);
static const System::Int8 SDLK_l = System::Int8(0x6c);
static const System::Int8 SDLK_m = System::Int8(0x6d);
static const System::Int8 SDLK_n = System::Int8(0x6e);
static const System::Int8 SDLK_o = System::Int8(0x6f);
static const System::Int8 SDLK_p = System::Int8(0x70);
static const System::Int8 SDLK_q = System::Int8(0x71);
static const System::Int8 SDLK_r = System::Int8(0x72);
static const System::Int8 SDLK_s = System::Int8(0x73);
static const System::Int8 SDLK_t = System::Int8(0x74);
static const System::Int8 SDLK_u = System::Int8(0x75);
static const System::Int8 SDLK_v = System::Int8(0x76);
static const System::Int8 SDLK_w = System::Int8(0x77);
static const System::Int8 SDLK_x = System::Int8(0x78);
static const System::Int8 SDLK_y = System::Int8(0x79);
static const System::Int8 SDLK_z = System::Int8(0x7a);
static const System::Int8 SDLK_DELETE = System::Int8(0x7f);
static const System::Byte SDLK_WORLD_0 = System::Byte(0xa0);
static const System::Byte SDLK_WORLD_1 = System::Byte(0xa1);
static const System::Byte SDLK_WORLD_2 = System::Byte(0xa2);
static const System::Byte SDLK_WORLD_3 = System::Byte(0xa3);
static const System::Byte SDLK_WORLD_4 = System::Byte(0xa4);
static const System::Byte SDLK_WORLD_5 = System::Byte(0xa5);
static const System::Byte SDLK_WORLD_6 = System::Byte(0xa6);
static const System::Byte SDLK_WORLD_7 = System::Byte(0xa7);
static const System::Byte SDLK_WORLD_8 = System::Byte(0xa8);
static const System::Byte SDLK_WORLD_9 = System::Byte(0xa9);
static const System::Byte SDLK_WORLD_10 = System::Byte(0xaa);
static const System::Byte SDLK_WORLD_11 = System::Byte(0xab);
static const System::Byte SDLK_WORLD_12 = System::Byte(0xac);
static const System::Byte SDLK_WORLD_13 = System::Byte(0xad);
static const System::Byte SDLK_WORLD_14 = System::Byte(0xae);
static const System::Byte SDLK_WORLD_15 = System::Byte(0xaf);
static const System::Byte SDLK_WORLD_16 = System::Byte(0xb0);
static const System::Byte SDLK_WORLD_17 = System::Byte(0xb1);
static const System::Byte SDLK_WORLD_18 = System::Byte(0xb2);
static const System::Byte SDLK_WORLD_19 = System::Byte(0xb3);
static const System::Byte SDLK_WORLD_20 = System::Byte(0xb4);
static const System::Byte SDLK_WORLD_21 = System::Byte(0xb5);
static const System::Byte SDLK_WORLD_22 = System::Byte(0xb6);
static const System::Byte SDLK_WORLD_23 = System::Byte(0xb7);
static const System::Byte SDLK_WORLD_24 = System::Byte(0xb8);
static const System::Byte SDLK_WORLD_25 = System::Byte(0xb9);
static const System::Byte SDLK_WORLD_26 = System::Byte(0xba);
static const System::Byte SDLK_WORLD_27 = System::Byte(0xbb);
static const System::Byte SDLK_WORLD_28 = System::Byte(0xbc);
static const System::Byte SDLK_WORLD_29 = System::Byte(0xbd);
static const System::Byte SDLK_WORLD_30 = System::Byte(0xbe);
static const System::Byte SDLK_WORLD_31 = System::Byte(0xbf);
static const System::Byte SDLK_WORLD_32 = System::Byte(0xc0);
static const System::Byte SDLK_WORLD_33 = System::Byte(0xc1);
static const System::Byte SDLK_WORLD_34 = System::Byte(0xc2);
static const System::Byte SDLK_WORLD_35 = System::Byte(0xc3);
static const System::Byte SDLK_WORLD_36 = System::Byte(0xc4);
static const System::Byte SDLK_WORLD_37 = System::Byte(0xc5);
static const System::Byte SDLK_WORLD_38 = System::Byte(0xc6);
static const System::Byte SDLK_WORLD_39 = System::Byte(0xc7);
static const System::Byte SDLK_WORLD_40 = System::Byte(0xc8);
static const System::Byte SDLK_WORLD_41 = System::Byte(0xc9);
static const System::Byte SDLK_WORLD_42 = System::Byte(0xca);
static const System::Byte SDLK_WORLD_43 = System::Byte(0xcb);
static const System::Byte SDLK_WORLD_44 = System::Byte(0xcc);
static const System::Byte SDLK_WORLD_45 = System::Byte(0xcd);
static const System::Byte SDLK_WORLD_46 = System::Byte(0xce);
static const System::Byte SDLK_WORLD_47 = System::Byte(0xcf);
static const System::Byte SDLK_WORLD_48 = System::Byte(0xd0);
static const System::Byte SDLK_WORLD_49 = System::Byte(0xd1);
static const System::Byte SDLK_WORLD_50 = System::Byte(0xd2);
static const System::Byte SDLK_WORLD_51 = System::Byte(0xd3);
static const System::Byte SDLK_WORLD_52 = System::Byte(0xd4);
static const System::Byte SDLK_WORLD_53 = System::Byte(0xd5);
static const System::Byte SDLK_WORLD_54 = System::Byte(0xd6);
static const System::Byte SDLK_WORLD_55 = System::Byte(0xd7);
static const System::Byte SDLK_WORLD_56 = System::Byte(0xd8);
static const System::Byte SDLK_WORLD_57 = System::Byte(0xd9);
static const System::Byte SDLK_WORLD_58 = System::Byte(0xda);
static const System::Byte SDLK_WORLD_59 = System::Byte(0xdb);
static const System::Byte SDLK_WORLD_60 = System::Byte(0xdc);
static const System::Byte SDLK_WORLD_61 = System::Byte(0xdd);
static const System::Byte SDLK_WORLD_62 = System::Byte(0xde);
static const System::Byte SDLK_WORLD_63 = System::Byte(0xdf);
static const System::Byte SDLK_WORLD_64 = System::Byte(0xe0);
static const System::Byte SDLK_WORLD_65 = System::Byte(0xe1);
static const System::Byte SDLK_WORLD_66 = System::Byte(0xe2);
static const System::Byte SDLK_WORLD_67 = System::Byte(0xe3);
static const System::Byte SDLK_WORLD_68 = System::Byte(0xe4);
static const System::Byte SDLK_WORLD_69 = System::Byte(0xe5);
static const System::Byte SDLK_WORLD_70 = System::Byte(0xe6);
static const System::Byte SDLK_WORLD_71 = System::Byte(0xe7);
static const System::Byte SDLK_WORLD_72 = System::Byte(0xe8);
static const System::Byte SDLK_WORLD_73 = System::Byte(0xe9);
static const System::Byte SDLK_WORLD_74 = System::Byte(0xea);
static const System::Byte SDLK_WORLD_75 = System::Byte(0xeb);
static const System::Byte SDLK_WORLD_76 = System::Byte(0xec);
static const System::Byte SDLK_WORLD_77 = System::Byte(0xed);
static const System::Byte SDLK_WORLD_78 = System::Byte(0xee);
static const System::Byte SDLK_WORLD_79 = System::Byte(0xef);
static const System::Byte SDLK_WORLD_80 = System::Byte(0xf0);
static const System::Byte SDLK_WORLD_81 = System::Byte(0xf1);
static const System::Byte SDLK_WORLD_82 = System::Byte(0xf2);
static const System::Byte SDLK_WORLD_83 = System::Byte(0xf3);
static const System::Byte SDLK_WORLD_84 = System::Byte(0xf4);
static const System::Byte SDLK_WORLD_85 = System::Byte(0xf5);
static const System::Byte SDLK_WORLD_86 = System::Byte(0xf6);
static const System::Byte SDLK_WORLD_87 = System::Byte(0xf7);
static const System::Byte SDLK_WORLD_88 = System::Byte(0xf8);
static const System::Byte SDLK_WORLD_89 = System::Byte(0xf9);
static const System::Byte SDLK_WORLD_90 = System::Byte(0xfa);
static const System::Byte SDLK_WORLD_91 = System::Byte(0xfb);
static const System::Byte SDLK_WORLD_92 = System::Byte(0xfc);
static const System::Byte SDLK_WORLD_93 = System::Byte(0xfd);
static const System::Byte SDLK_WORLD_94 = System::Byte(0xfe);
static const System::Byte SDLK_WORLD_95 = System::Byte(0xff);
static const System::Word SDLK_KP0 = System::Word(0x100);
static const System::Word SDLK_KP1 = System::Word(0x101);
static const System::Word SDLK_KP2 = System::Word(0x102);
static const System::Word SDLK_KP3 = System::Word(0x103);
static const System::Word SDLK_KP4 = System::Word(0x104);
static const System::Word SDLK_KP5 = System::Word(0x105);
static const System::Word SDLK_KP6 = System::Word(0x106);
static const System::Word SDLK_KP7 = System::Word(0x107);
static const System::Word SDLK_KP8 = System::Word(0x108);
static const System::Word SDLK_KP9 = System::Word(0x109);
static const System::Word SDLK_KP_PERIOD = System::Word(0x10a);
static const System::Word SDLK_KP_DIVIDE = System::Word(0x10b);
static const System::Word SDLK_KP_MULTIPLY = System::Word(0x10c);
static const System::Word SDLK_KP_MINUS = System::Word(0x10d);
static const System::Word SDLK_KP_PLUS = System::Word(0x10e);
static const System::Word SDLK_KP_ENTER = System::Word(0x10f);
static const System::Word SDLK_KP_EQUALS = System::Word(0x110);
static const System::Word SDLK_UP = System::Word(0x111);
static const System::Word SDLK_DOWN = System::Word(0x112);
static const System::Word SDLK_RIGHT = System::Word(0x113);
static const System::Word SDLK_LEFT = System::Word(0x114);
static const System::Word SDLK_INSERT = System::Word(0x115);
static const System::Word SDLK_HOME = System::Word(0x116);
static const System::Word SDLK_END = System::Word(0x117);
static const System::Word SDLK_PAGEUP = System::Word(0x118);
static const System::Word SDLK_PAGEDOWN = System::Word(0x119);
static const System::Word SDLK_F1 = System::Word(0x11a);
static const System::Word SDLK_F2 = System::Word(0x11b);
static const System::Word SDLK_F3 = System::Word(0x11c);
static const System::Word SDLK_F4 = System::Word(0x11d);
static const System::Word SDLK_F5 = System::Word(0x11e);
static const System::Word SDLK_F6 = System::Word(0x11f);
static const System::Word SDLK_F7 = System::Word(0x120);
static const System::Word SDLK_F8 = System::Word(0x121);
static const System::Word SDLK_F9 = System::Word(0x122);
static const System::Word SDLK_F10 = System::Word(0x123);
static const System::Word SDLK_F11 = System::Word(0x124);
static const System::Word SDLK_F12 = System::Word(0x125);
static const System::Word SDLK_F13 = System::Word(0x126);
static const System::Word SDLK_F14 = System::Word(0x127);
static const System::Word SDLK_F15 = System::Word(0x128);
static const System::Word SDLK_NUMLOCK = System::Word(0x12c);
static const System::Word SDLK_CAPSLOCK = System::Word(0x12d);
static const System::Word SDLK_SCROLLOCK = System::Word(0x12e);
static const System::Word SDLK_RSHIFT = System::Word(0x12f);
static const System::Word SDLK_LSHIFT = System::Word(0x130);
static const System::Word SDLK_RCTRL = System::Word(0x131);
static const System::Word SDLK_LCTRL = System::Word(0x132);
static const System::Word SDLK_RALT = System::Word(0x133);
static const System::Word SDLK_LALT = System::Word(0x134);
static const System::Word SDLK_RMETA = System::Word(0x135);
static const System::Word SDLK_LMETA = System::Word(0x136);
static const System::Word SDLK_LSUPER = System::Word(0x137);
static const System::Word SDLK_RSUPER = System::Word(0x138);
static const System::Word SDLK_MODE = System::Word(0x139);
static const System::Word SDLK_COMPOSE = System::Word(0x13a);
static const System::Word SDLK_HELP = System::Word(0x13b);
static const System::Word SDLK_PRINT = System::Word(0x13c);
static const System::Word SDLK_SYSREQ = System::Word(0x13d);
static const System::Word SDLK_BREAK = System::Word(0x13e);
static const System::Word SDLK_MENU = System::Word(0x13f);
static const System::Word SDLK_POWER = System::Word(0x140);
static const System::Word SDLK_EURO = System::Word(0x141);
static const System::Int8 KMOD_NONE = System::Int8(0x0);
static const System::Int8 KMOD_LSHIFT = System::Int8(0x1);
static const System::Int8 KMOD_RSHIFT = System::Int8(0x2);
static const System::Int8 KMOD_LCTRL = System::Int8(0x40);
static const System::Byte KMOD_RCTRL = System::Byte(0x80);
static const System::Word KMOD_LALT = System::Word(0x100);
static const System::Word KMOD_RALT = System::Word(0x200);
static const System::Word KMOD_LMETA = System::Word(0x400);
static const System::Word KMOD_RMETA = System::Word(0x800);
static const System::Word KMOD_NUM = System::Word(0x1000);
static const System::Word KMOD_CAPS = System::Word(0x2000);
static const System::Word KMOD_MODE = System::Word(0xabe0);
static const System::Word KMOD_RESERVED = System::Word(0x8000);
static const System::Byte KMOD_CTRL = System::Byte(0xc0);
static const System::Int8 KMOD_SHIFT = System::Int8(0x3);
static const System::Word KMOD_ALT = System::Word(0x300);
static const System::Word KMOD_META = System::Word(0xc00);
static const System::Byte SDL_ALPHA_OPAQUE = System::Byte(0xff);
static const System::Int8 SDL_ALPHA_TRANSPARENT = System::Int8(0x0);
static const System::Int8 SDL_SWSURFACE = System::Int8(0x0);
static const System::Int8 SDL_HWSURFACE = System::Int8(0x1);
static const System::Int8 SDL_ASYNCBLIT = System::Int8(0x4);
static const int SDL_ANYFORMAT = int(0x10000000);
static const int SDL_HWPALETTE = int(0x20000000);
static const int SDL_DOUBLEBUF = int(0x40000000);
static const unsigned SDL_FULLSCREEN = unsigned(0x80000000);
static const System::Int8 SDL_OPENGL = System::Int8(0x2);
static const System::Int8 SDL_OPENGLBLIT = System::Int8(0x2);
static const System::Int8 SDL_RESIZABLE = System::Int8(0x10);
static const System::Int8 SDL_NOFRAME = System::Int8(0x20);
static const System::Word SDL_HWACCEL = System::Word(0x100);
static const System::Word SDL_SRCCOLORKEY = System::Word(0x1000);
static const System::Word SDL_RLEACCELOK = System::Word(0x2000);
static const System::Word SDL_RLEACCEL = System::Word(0x4000);
static const int SDL_SRCALPHA = int(0x10000);
static const int SDL_SRCCLIPPING = int(0x100000);
static const int SDL_PREALLOC = int(0x1000000);
static const int SDL_YV12_OVERLAY = int(0x32315659);
static const int SDL_IYUV_OVERLAY = int(0x56555949);
static const int SDL_YUY2_OVERLAY = int(0x32595559);
static const int SDL_UYVY_OVERLAY = int(0x59565955);
static const int SDL_YVYU_OVERLAY = int(0x55595659);
static const System::Int8 SDL_LOGPAL = System::Int8(0x1);
static const System::Int8 SDL_PHYSPAL = System::Int8(0x2);
static const System::Int8 SDL_BUTTON_LEFT = System::Int8(0x1);
static const System::Int8 SDL_BUTTON_MIDDLE = System::Int8(0x2);
static const System::Int8 SDL_BUTTON_RIGHT = System::Int8(0x3);
static const System::Int8 SDL_BUTTON_LMASK = System::Int8(0x1);
static const System::Int8 SDL_BUTTON_MMASK = System::Int8(0x2);
static const System::Int8 SDL_BUTTON_RMask = System::Int8(0x4);
static const System::Int8 SDL_APPMOUSEFOCUS = System::Int8(0x1);
static const System::Int8 SDL_APPINPUTFOCUS = System::Int8(0x2);
static const System::Int8 SDL_APPACTIVE = System::Int8(0x4);
static const System::Int8 SDL_MUTEX_TIMEDOUT = System::Int8(0x1);
static const unsigned SDL_MUTEX_MAXWAIT = unsigned(0xffffffff);
extern "C" int __cdecl SDL_Init(unsigned flags);
extern "C" int __cdecl SDL_InitSubSystem(unsigned flags);
extern "C" void __cdecl SDL_QuitSubSystem(unsigned flags);
extern "C" unsigned __cdecl SDL_WasInit(unsigned flags);
extern "C" void __cdecl SDL_Quit(void);
extern "C" int __cdecl SDL_RegisterApp(char * name, unsigned style, void * h_Inst);
extern "C" char * __cdecl SDL_GetError(void);
extern "C" void __cdecl SDL_SetError(char * fmt);
extern "C" void __cdecl SDL_ClearError(void);
extern "C" PSDL_RWops __cdecl SDL_RWFromFile(char * filename, char * mode);
extern "C" void __cdecl SDL_FreeRW(PSDL_RWops area);
extern "C" PSDL_RWops __cdecl SDL_RWFromFP(void * fp, int autoclose);
extern "C" PSDL_RWops __cdecl SDL_RWFromMem(void * mem, int size);
extern "C" PSDL_RWops __cdecl SDL_AllocRW(void);
extern "C" unsigned __cdecl SDL_GetTicks(void);
extern "C" void __cdecl SDL_Delay(unsigned msec);
extern "C" PSDL_TimerID __cdecl SDL_AddTimer(unsigned interval, TSDL_NewTimerCallback callback, void * param);
extern "C" TSDL_Bool __cdecl SDL_RemoveTimer(PSDL_TimerID t);
extern "C" int __cdecl SDL_SetTimer(unsigned interval, TSDL_TimerCallback callback);
extern "C" int __cdecl SDL_AudioInit(char * driver_name);
extern "C" void __cdecl SDL_AudioQuit(void);
extern "C" char * __cdecl SDL_AudioDriverName(char * namebuf, int maxlen);
extern "C" int __cdecl SDL_OpenAudio(PSDL_AudioSpec desired, PSDL_AudioSpec obtained);
extern "C" TSDL_Audiostatus __cdecl SDL_GetAudioStatus(void);
extern "C" void __cdecl SDL_PauseAudio(int pause_on);
extern "C" PSDL_AudioSpec __cdecl SDL_LoadWAV_RW(PSDL_RWops src, int freesrc, PSDL_AudioSpec spec, PUInt8 audio_buf, PUInt32 audiolen);
extern "C" void __cdecl SDL_FreeWAV(PUInt8 audio_buf);
extern "C" int __cdecl SDL_BuildAudioCVT(PSDL_AudioCVT cvt, System::Word src_format, System::Byte src_channels, int src_rate, System::Word dst_format, System::Byte dst_channels, int dst_rate);
extern "C" int __cdecl SDL_ConvertAudio(PSDL_AudioCVT cvt);
extern "C" void __cdecl SDL_MixAudio(PUInt8 dst, PUInt8 src, unsigned len, int volume);
extern "C" void __cdecl SDL_LockAudio(void);
extern "C" void __cdecl SDL_UnlockAudio(void);
extern "C" void __cdecl SDL_CloseAudio(void);
extern "C" int __cdecl SDL_CDNumDrives(void);
extern "C" char * __cdecl SDL_CDName(int drive);
extern "C" PSDL_CD __cdecl SDL_CDOpen(int drive);
extern "C" TSDL_CDStatus __cdecl SDL_CDStatus(PSDL_CD cdrom);
extern "C" int __cdecl SDL_CDPlayTracks(PSDL_CD cdrom, int start_track, int start_frame, int ntracks, int nframes);
extern "C" int __cdecl SDL_CDPlay(PSDL_CD cdrom, int start, int length);
extern "C" int __cdecl SDL_CDPause(PSDL_CD cdrom);
extern "C" int __cdecl SDL_CDResume(PSDL_CD cdrom);
extern "C" int __cdecl SDL_CDStop(PSDL_CD cdrom);
extern "C" int __cdecl SDL_CDEject(PSDL_CD cdrom);
extern "C" void __cdecl SDL_CDClose(PSDL_CD cdrom);
extern "C" int __cdecl SDL_NumJoysticks(void);
extern "C" char * __cdecl SDL_JoystickName(int index);
extern "C" PSDL_Joystick __cdecl SDL_JoystickOpen(int index);
extern "C" int __cdecl SDL_JoystickOpened(int index);
extern "C" int __cdecl SDL_JoystickIndex(PSDL_Joystick joystick);
extern "C" int __cdecl SDL_JoystickNumAxes(PSDL_Joystick joystick);
extern "C" int __cdecl SDL_JoystickNumBalls(PSDL_Joystick joystick);
extern "C" int __cdecl SDL_JoystickNumHats(PSDL_Joystick joystick);
extern "C" int __cdecl SDL_JoystickNumButtons(PSDL_Joystick joystick);
extern "C" void __cdecl SDL_JoystickUpdate(void);
extern "C" int __cdecl SDL_JoystickEventState(int state);
extern "C" short __cdecl SDL_JoystickGetAxis(PSDL_Joystick joystick, int axis);
extern "C" System::Byte __cdecl SDL_JoystickGetHat(PSDL_Joystick joystick, int hat);
extern "C" int __cdecl SDL_JoystickGetBall(PSDL_Joystick joystick, int ball, int &dx, int &dy);
extern "C" System::Byte __cdecl SDL_JoystickGetButton(PSDL_Joystick joystick, int Button);
extern "C" void __cdecl SDL_JoystickClose(PSDL_Joystick joystick);
extern "C" void __cdecl SDL_PumpEvents(void);
extern "C" int __cdecl SDL_PeepEvents(PSDL_Event events, int numevents, TSDL_EventAction action, unsigned mask);
extern "C" int __cdecl SDL_PollEvent(PSDL_Event event);
extern "C" int __cdecl SDL_WaitEvent(PSDL_Event event);
extern "C" int __cdecl SDL_PushEvent(PSDL_Event event);
extern "C" void __cdecl SDL_SetEventFilter(TSDL_EventFilter filter);
extern "C" TSDL_EventFilter __cdecl SDL_GetEventFilter(void);
extern "C" System::Byte __cdecl SDL_EventState(System::Byte type_, int state);
extern "C" TSDL_version __cdecl SDL_Linked_Version(void);
extern "C" int __cdecl SDL_VideoInit(char * driver_name, unsigned flags);
extern "C" void __cdecl SDL_VideoQuit(void);
extern "C" char * __cdecl SDL_VideoDriverName(char * namebuf, int maxlen);
extern "C" PSDL_Surface __cdecl SDL_GetVideoSurface(void);
extern "C" PSDL_VideoInfo __cdecl SDL_GetVideoInfo(void);
extern "C" int __cdecl SDL_VideoModeOK(int width, int height, int bpp, unsigned flags);
extern "C" PPSDL_Rect __cdecl SDL_ListModes(PSDL_PixelFormat format, unsigned flags);
extern "C" PSDL_Surface __cdecl SDL_SetVideoMode(int width, int height, int bpp, unsigned flags);
extern "C" void __cdecl SDL_UpdateRects(PSDL_Surface screen, int numrects, PSDL_Rect rects);
extern "C" void __cdecl SDL_UpdateRect(PSDL_Surface screen, int x, int y, unsigned w, unsigned h);
extern "C" int __cdecl SDL_Flip(PSDL_Surface screen);
extern "C" int __cdecl SDL_SetGamma(float redgamma, float greengamma, float bluegamma);
extern "C" int __cdecl SDL_SetGammaRamp(PUInt16 redtable, PUInt16 greentable, PUInt16 bluetable);
extern "C" int __cdecl SDL_GetGammaRamp(PUInt16 redtable, PUInt16 greentable, PUInt16 bluetable);
extern "C" int __cdecl SDL_SetColors(PSDL_Surface surface, PSDL_Color colors, int firstcolor, int ncolors);
extern "C" int __cdecl SDL_SetPalette(PSDL_Surface surface, int flags, PSDL_Color colors, int firstcolor, int ncolors);
extern "C" unsigned __cdecl SDL_MapRGB(PSDL_PixelFormat format, System::Byte r, System::Byte g, System::Byte b);
extern "C" unsigned __cdecl SDL_MapRGBA(PSDL_PixelFormat format, System::Byte r, System::Byte g, System::Byte b, System::Byte a);
extern "C" void __cdecl SDL_GetRGB(unsigned pixel, PSDL_PixelFormat fmt, PUInt8 r, PUInt8 g, PUInt8 b);
extern "C" void __cdecl SDL_GetRGBA(unsigned pixel, PSDL_PixelFormat fmt, PUInt8 r, PUInt8 g, PUInt8 b, System::Byte a);
extern "C" PSDL_Surface __cdecl SDL_CreateRGBSurface(unsigned flags, int width, int height, int depth, unsigned RMask, unsigned GMask, unsigned BMask, unsigned AMask);
extern "C" PSDL_Surface __cdecl SDL_CreateRGBSurfaceFrom(void * pixels, int width, int height, int depth, int pitch, unsigned RMask, unsigned GMask, unsigned BMask, unsigned AMask);
extern "C" void __cdecl SDL_FreeSurface(PSDL_Surface surface);
extern "C" int __cdecl SDL_LockSurface(PSDL_Surface surface);
extern "C" void __cdecl SDL_UnlockSurface(PSDL_Surface surface);
extern "C" PSDL_Surface __cdecl SDL_LoadBMP_RW(PSDL_RWops src, int freesrc);
extern "C" int __cdecl SDL_SaveBMP_RW(PSDL_Surface surface, PSDL_RWops dst, int freedst);
extern "C" int __cdecl SDL_SetColorKey(PSDL_Surface surface, unsigned flag, unsigned key);
extern "C" int __cdecl SDL_SetAlpha(PSDL_Surface surface, unsigned flag, System::Byte alpha);
extern "C" void __cdecl SDL_SetClipRect(PSDL_Surface surface, PSDL_Rect rect);
extern "C" void __cdecl SDL_GetClipRect(PSDL_Surface surface, PSDL_Rect rect);
extern "C" PSDL_Surface __cdecl SDL_ConvertSurface(PSDL_Surface src, PSDL_PixelFormat fmt, unsigned flags);
extern "C" int __cdecl SDL_UpperBlit(PSDL_Surface src, PSDL_Rect srcrect, PSDL_Surface dst, PSDL_Rect dstrect);
extern "C" int __cdecl SDL_LowerBlit(PSDL_Surface src, PSDL_Rect srcrect, PSDL_Surface dst, PSDL_Rect dstrect);
extern "C" int __cdecl SDL_FillRect(PSDL_Surface dst, PSDL_Rect dstrect, unsigned color);
extern "C" PSDL_Surface __cdecl SDL_DisplayFormat(PSDL_Surface surface);
extern "C" PSDL_Surface __cdecl SDL_DisplayFormatAlpha(PSDL_Surface surface);
extern "C" PSDL_Overlay __cdecl SDL_CreateYUVOverlay(int width, int height, unsigned format, PSDL_Surface display);
extern "C" int __cdecl SDL_LockYUVOverlay(PSDL_Overlay Overlay);
extern "C" void __cdecl SDL_UnlockYUVOverlay(PSDL_Overlay Overlay);
extern "C" int __cdecl SDL_DisplayYUVOverlay(PSDL_Overlay Overlay, PSDL_Rect dstrect);
extern "C" void __cdecl SDL_FreeYUVOverlay(PSDL_Overlay Overlay);
extern "C" int __cdecl SDL_GL_LoadLibrary(System::WideChar * filename);
extern "C" void * __cdecl SDL_GL_GetProcAddress(System::WideChar * procname);
extern "C" int __cdecl SDL_GL_SetAttribute(TSDL_GLAttr attr, int value);
extern "C" int __cdecl SDL_GL_GetAttribute(TSDL_GLAttr attr, int &value);
extern "C" void __cdecl SDL_GL_SwapBuffers(void);
extern "C" void __cdecl SDL_GL_UpdateRects(int numrects, PSDL_Rect rects);
extern "C" void __cdecl SDL_GL_Lock(void);
extern "C" void __cdecl SDL_GL_Unlock(void);
extern "C" void __cdecl SDL_WM_GetCaption(char * &title, char * &icon);
extern "C" void __cdecl SDL_WM_SetCaption(char * title, char * icon);
extern "C" void __cdecl SDL_WM_SetIcon(PSDL_Surface icon, System::Byte mask);
extern "C" int __cdecl SDL_WM_IconifyWindow(void);
extern "C" int __cdecl SDL_WM_ToggleFullScreen(PSDL_Surface surface);
extern "C" TSDL_GrabMode __cdecl SDL_WM_GrabInput(TSDL_GrabMode mode);
extern "C" System::Byte __cdecl SDL_GetMouseState(int &x, int &y);
extern "C" System::Byte __cdecl SDL_GetRelativeMouseState(int &x, int &y);
extern "C" void __cdecl SDL_WarpMouse(System::Word x, System::Word y);
extern "C" PSDL_Cursor __cdecl SDL_CreateCursor(PUInt8 data, PUInt8 mask, int w, int h, int hot_x, int hot_y);
extern "C" void __cdecl SDL_SetCursor(PSDL_Cursor cursor);
extern "C" PSDL_Cursor __cdecl SDL_GetCursor(void);
extern "C" void __cdecl SDL_FreeCursor(PSDL_Cursor cursor);
extern "C" int __cdecl SDL_ShowCursor(int toggle);
extern "C" int __cdecl SDL_EnableUNICODE(int enable);
extern "C" int __cdecl SDL_EnableKeyRepeat(int delay, int interval);
extern "C" PUInt8 __cdecl SDL_GetKeyState(PInt numkeys);
extern "C" unsigned __cdecl SDL_GetModState(void);
extern "C" void __cdecl SDL_SetModState(unsigned modstate);
extern "C" char * __cdecl SDL_GetKeyName(unsigned key);
extern "C" System::Byte __cdecl SDL_GetAppState(void);
extern "C" PSDL_Mutex __cdecl SDL_CreateMutex(void);
extern "C" int __cdecl SDL_mutexP(PSDL_Mutex mutex);
extern "C" int __cdecl SDL_mutexV(PSDL_Mutex mutex);
extern "C" void __cdecl SDL_DestroyMutex(PSDL_Mutex mutex);
extern "C" PSDL_Sem __cdecl SDL_CreateSemaphore(unsigned initial_value);
extern "C" void __cdecl SDL_DestroySemaphore(PSDL_Sem sem);
extern "C" int __cdecl SDL_SemWait(PSDL_Sem sem);
extern "C" int __cdecl SDL_SemTryWait(PSDL_Sem sem);
extern "C" int __cdecl SDL_SemWaitTimeout(PSDL_Sem sem, unsigned ms);
extern "C" int __cdecl SDL_SemPost(PSDL_Sem sem);
extern "C" unsigned __cdecl SDL_SemValue(PSDL_Sem sem);
extern "C" PSDL_Cond __cdecl SDL_CreateCond(void);
extern "C" void __cdecl SDL_DestroyCond(PSDL_Cond cond);
extern "C" int __cdecl SDL_CondSignal(PSDL_Cond cond);
extern "C" int __cdecl SDL_CondBroadcast(PSDL_Cond cond);
extern "C" int __cdecl SDL_CondWait(PSDL_Cond cond, PSDL_Mutex mut);
extern "C" int __cdecl SDL_CondWaitTimeout(PSDL_Cond cond, PSDL_Mutex mut, unsigned ms);
extern "C" PSDL_Thread __cdecl SDL_CreateThread(PInt fn, void * data);
extern "C" unsigned __cdecl SDL_ThreadID(void);
extern "C" unsigned __cdecl SDL_GetThreadID(PSDL_Thread thread);
extern "C" void __cdecl SDL_WaitThread(PSDL_Thread thread, int &status);
extern "C" void __cdecl SDL_KillThread(PSDL_Thread thread);
extern PACKAGE int __fastcall SDL_TABLESIZE(char * table);
extern PACKAGE void __fastcall SDL_OutOfMemory(void);
extern PACKAGE int __fastcall SDL_RWSeek(PSDL_RWops context, int offset, int whence);
extern PACKAGE int __fastcall SDL_RWTell(PSDL_RWops context);
extern PACKAGE int __fastcall SDL_RWRead(PSDL_RWops context, void * ptr, int size, int n);
extern PACKAGE int __fastcall SDL_RWWrite(PSDL_RWops context, void * ptr, int size, int n);
extern PACKAGE int __fastcall SDL_RWClose(PSDL_RWops context);
extern PACKAGE PSDL_AudioSpec __fastcall SDL_LoadWAV(char * filename, PSDL_AudioSpec spec, PUInt8 audio_buf, PUInt32 audiolen);
extern PACKAGE BOOL __fastcall SDL_CDInDrive(TSDL_CDStatus status);
extern PACKAGE void __fastcall FRAMES_TO_MSF(int frames, int &M, int &S, int &F);
extern PACKAGE int __fastcall MSF_TO_FRAMES(int M, int S, int F);
extern PACKAGE void __fastcall SDL_VERSION(TSDL_version &X);
extern PACKAGE int __fastcall SDL_VERSIONNUM(int X, int Y, int Z);
extern PACKAGE int __fastcall SDL_COMPILEDVERSION(void);
extern PACKAGE BOOL __fastcall SDL_VERSION_ATLEAST(int X, int Y, int Z);
extern PACKAGE PSDL_Surface __fastcall SDL_LoadBMP(char * filename);
extern PACKAGE int __fastcall SDL_SaveBMP(PSDL_Surface surface, char * filename);
extern PACKAGE int __fastcall SDL_BlitSurface(PSDL_Surface src, PSDL_Rect srcrect, PSDL_Surface dst, PSDL_Rect dstrect);
extern PACKAGE PSDL_Surface __fastcall SDL_AllocSurface(unsigned flags, int width, int height, int depth, unsigned RMask, unsigned GMask, unsigned BMask, unsigned AMask);
extern PACKAGE bool __fastcall SDL_MustLock(PSDL_Surface Surface);
extern PACKAGE int __fastcall SDL_LockMutex(PSDL_Mutex mutex);
extern PACKAGE int __fastcall SDL_UnlockMutex(PSDL_Mutex mutex);
extern PACKAGE int __cdecl _putenv(const char * variable);
extern PACKAGE int __fastcall SDL_putenv(const char * variable);
extern "C" char * __cdecl getenv(const char * name);
extern PACKAGE char * __fastcall SDL_getenv(const char * name);
extern PACKAGE int __fastcall SDL_BUTTON(int Button);
extern PACKAGE unsigned __fastcall SDL_Swap32(unsigned D);
extern PACKAGE void __fastcall FreeAndNil(void *Obj);
}	/* namespace Glsdl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSDL)
using namespace Glsdl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlsdlHPP
