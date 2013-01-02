// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SDL.pas' rev: 24.00 (Win32)

#ifndef SdlHPP
#define SdlHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Sdl
{
//-- type declarations -------------------------------------------------------
enum TSDL_Bool : unsigned char { SDL_FALSE, SDL_TRUE };

typedef System::StaticArray<System::Byte, 1073741824> TUInt8Array;

typedef TUInt8Array *PUInt8Array;

typedef System::Byte *PUInt8;

typedef System::Word *PUInt16;

typedef unsigned *PUInt32;

typedef int *PInt;

typedef UInt64 *PUInt64;

typedef SInt64 *PSInt64;

enum TSDL_errorcode : unsigned char { SDL_ENOMEM, SDL_EFREAD, SDL_EFWRITE, SDL_EFSEEK, SDL_LASTERROR };

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

struct DECLSPEC_DRECORD TSDL_AudioCVTFilter
{
public:
	TSDL_AudioCVT *cvt;
	System::Word format;
};


typedef TSDL_AudioCVTFilter *PSDL_AudioCVTFilter;

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

struct DECLSPEC_DRECORD TSDL_CDTrack
{
public:
	int id;
	System::Byte type_;
	System::Word unused;
	unsigned length;
	unsigned offset;
};


typedef TSDL_CDTrack *PSDL_CDTrack;

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


struct DECLSPEC_DRECORD TBallDelta
{
public:
	int dx;
	int dy;
};


typedef TBallDelta *PBallDelta;

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


struct DECLSPEC_DRECORD TSDL_version
{
public:
	System::Byte major;
	System::Byte minor;
	System::Byte patch;
};


typedef TSDL_version *PSDL_version;

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


struct DECLSPEC_DRECORD TSDL_QuitEvent
{
public:
	System::Byte type_;
};


typedef TSDL_QuitEvent *PSDL_QuitEvent;

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


struct DECLSPEC_DRECORD TSDL_ExposeEvent
{
public:
	System::Byte type_;
};


typedef TSDL_ExposeEvent *PSDL_ExposeEvent;

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


struct DECLSPEC_DRECORD TSDL_SysWMEvent
{
public:
	System::Byte type_;
	TSDL_SysWMmsg *msg;
};


typedef TSDL_SysWMEvent *PSDL_SysWMEvent;

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


typedef TSDL_Event *PSDL_Event;

typedef int __cdecl (*TSDL_EventFilter)(PSDL_Event event);

struct DECLSPEC_DRECORD TSDL_Rect
{
public:
	short x;
	short y;
	System::Word w;
	System::Word h;
};


typedef TSDL_Rect *PSDL_Rect;

typedef PSDL_Rect *PPSDL_Rect;

struct DECLSPEC_DRECORD TSDL_Color
{
public:
	System::Byte r;
	System::Byte g;
	System::Byte b;
	System::Byte unused;
};


typedef TSDL_Color *PSDL_Color;

typedef System::StaticArray<TSDL_Color, 65001> TSDL_ColorArray;

typedef TSDL_ColorArray *PSDL_ColorArray;

struct DECLSPEC_DRECORD TSDL_Palette
{
public:
	int ncolors;
	TSDL_ColorArray *colors;
};


typedef TSDL_Palette *PSDL_Palette;

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


typedef TSDL_PixelFormat *PSDL_PixelFormat;

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


struct DECLSPEC_DRECORD TSDL_VideoInfo
{
public:
	System::Byte hw_available;
	System::Byte blit_hw;
	System::Byte UnusedBits3;
	unsigned video_mem;
	TSDL_PixelFormat *vfmt;
};


typedef TSDL_VideoInfo *PSDL_VideoInfo;

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


typedef TSDL_Overlay *PSDL_Overlay;

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

struct DECLSPEC_DRECORD TSDL_Cond
{
public:
	TSDL_Mutex *lock;
	int waiting;
	int signals;
	TSDL_semaphore *wait_sem;
	TSDL_semaphore *wait_done;
};


typedef TSDL_Cond *PSDL_Cond;

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


typedef TRect *PRect;

//-- var, const, procedure ---------------------------------------------------
#define LibName L"SDL.dll"
static const System::Int8 SDL_QUITEV = System::Int8(0xc);
extern "C" int __cdecl SDL_SemWaitTimeout(PSDL_Sem sem, unsigned ms);
extern "C" int __cdecl SDL_SemPost(PSDL_Sem sem);
extern PACKAGE void __fastcall SDL_OutOfMemory(void);
extern PACKAGE int __cdecl _putenv(const char * variable);
extern "C" char * __cdecl getenv(const char * name);
extern PACKAGE int __fastcall SDL_BUTTON(int Button);
extern PACKAGE void __fastcall FreeAndNil(void *Obj);
}	/* namespace Sdl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SDL)
using namespace Sdl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SdlHPP
