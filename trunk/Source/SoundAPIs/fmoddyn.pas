{ History:
     DaStr - 07/11/09 - Added $I GLScene.inc for Delhi 5 compatibility
                        Improved FPC compatibility (thanks Predator) (BugtrackerID = 2893580)
}                        
{============================================================================================ }
{ FMOD Main header file. Copyright (c), FireLight Technologies Pty, Ltd. 1999-2003.           }
{ =========================================================================================== }
{
  NOTE: For the demos to run you must have either fmod.dll (in Windows)
  or libfmod-3.75.so (in Linux) installed.

  In Windows, copy the fmod.dll file found in the api directory to either of
  the following locations (in order of preference)
  - your application directory
  - Windows\System (95/98) or WinNT\System32 (NT/2000/XP)

  In Linux, make sure you are signed in as root and copy the libfmod-3.75.so
  file from the api directory to your /usr/lib/ directory.
  Then via a command line, navigate to the /usr/lib/ directory and create
  a symbolic link between libfmod-3.75.so and libfmod.so. This is done with
  the following command (assuming you are in /usr/lib/)...
  ln -s libfmod-3.75.so libfmod.so.
}
{ =============================================================================================== }

unit fmoddyn;

{
  Disable assertions by changing the following compiler directive to OFF.
  Assertions are used to check the functions are correctly loaded when using
  dynamic loading.
}
{$ASSERTIONS ON}

interface

{$I GLScene.inc}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF FPC}
  LCLType,
{$ENDIF}
  fmodtypes;

{
  Disable warning for unsafe types in Delphi 7
}
{$IFDEF VER150}
{$WARN UNSAFE_TYPE OFF}
{$ENDIF}

//===============================================================================================
// FUNCTION PROTOTYPES
//===============================================================================================

{ ================================== }
{ Library load/unload functions.     }
{ ================================== }

{
  If no library name is passed to FMOD_Load, then the default library name
  used.
}

function FMOD_Load(LibName: PChar = nil): Boolean;
procedure FMOD_Unload;

{ ================================== }
{ Initialization / Global functions. }
{ ================================== }

{
  Pre FSOUND_Init functions. These can't be called after FSOUND_Init is
  called (they will fail). They set up FMOD system functionality.
}

var
  FSOUND_SetOutput: function (OutputType: TFSoundOutputTypes): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetDriver: function (Driver: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMixer: function (Mixer: TFSoundMixerTypes): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetBufferSize: function (LenMs: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetHWND: function (Hwnd: THandle): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMinHardwareChannels: function (Min: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMaxHardwareChannels: function (Max: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMemorySystem: function (Pool: Pointer;
        PoolLen: Integer;
        UserAlloc: TFSoundAllocCallback;
        UserRealloc: TFSoundReallocCallback;
        UserFree: TFSoundFreeCallback): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Main initialization / closedown functions
  Note : Use FSOUND_INIT_USEDEFAULTMIDISYNTH with FSOUND_Init for software override with MIDI playback.
       : Use FSOUND_INIT_GLOBALFOCUS with FSOUND_Init to make sound audible
         no matter which window is in focus. (FSOUND_OUTPUT_DSOUND only)
}

var
  FSOUND_Init: function (MixRate: Integer; MaxSoftwareChannels: Integer; Flags: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Close: procedure; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Runtime system level functions
}

var
  FSOUND_Update: procedure; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};  // This is called to update 3d sound / non-realtime output
  FSOUND_SetSpeakerMode: procedure (SpeakerMode: Cardinal); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetSFXMasterVolume: procedure (Volume: Integer); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetPanSeperation: procedure (PanSep: Single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_File_SetCallbacks: procedure (OpenCallback: TFSoundOpenCallback;
                                       CloseCallback: TFSoundCloseCallback;
                                       ReadCallback: TFSoundReadCallback;
                                       SeekCallback: TFSoundSeekCallback;
                                       TellCallback: TFSoundTellCallback); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  System information functions
}

var
  FSOUND_GetError: function: TFModErrors; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetVersion: function: Single; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetOutput: function: TFSoundOutputTypes; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetOutputHandle: function: Pointer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetDriver: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMixer: function: TFSoundMixerTypes; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetNumDrivers: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetDriverName: function (Id: Integer): PChar; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetDriverCaps: function (Id: Integer; var Caps: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

var
  FSOUND_GetOutputRate: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMaxChannels: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMaxSamples: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetSpeakerMode: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetSFXMasterVolume: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetNumHWChannels: function (var Num2D: Integer; var Num3D: Integer; var Total: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetChannelsPlaying: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetCPUUsage: function: Single; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMemoryStats: Procedure (var CurrentAlloced: Cardinal; var MaxAlloced: Cardinal); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ =================================== }
{ Sample management / load functions. }
{ =================================== }

{
  Sample creation and management functions
  Note : Use FSOUND_LOADMEMORY   flag with FSOUND_Sample_Load to load from memory.
         Use FSOUND_LOADRAW      flag with FSOUND_Sample_Load to treat as as raw pcm data.
}

var
  FSOUND_Sample_Load: function (Index: Integer; const NameOrData: PChar; Mode: Cardinal; Offset: Integer; Length: Integer): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Alloc: function (Index: Integer; Length: Integer; Mode: Cardinal; DefFreq: Integer; DefVol: Integer; DefPan: Integer; DefPri: Integer): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Free: procedure (Sptr: PFSoundSample); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Upload: function (Sptr: PFSoundSample; SrcData: Pointer; Mode: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Lock: function (Sptr: PFSoundSample; Offset: Integer; Length: Integer; var Ptr1: Pointer; var Ptr2: Pointer; var Len1: Cardinal; var Len2: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Unlock: function (Sptr: PFSoundSample; Ptr1: Pointer; Ptr2: Pointer; Len1: Cardinal; Len2: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Sample control functions
}

var
  FSOUND_Sample_SetMode: function (Sptr: PFSoundSample; Mode: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetLoopPoints: function (Sptr: PFSoundSample; LoopStart, LoopEnd: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetDefaults: function (Sptr: PFSoundSample; DefFreq, DefVol, DefPan, DefPri: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetDefaultsEx: function (Sptr: PFSoundSample; DefFreq, DefVol, DefPan, DefPri, VarFreq, VarVol, VarPan: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetMinMaxDistance: function (Sptr: PFSoundSample; Min, Max: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetMaxPlaybacks: function (Sptr: PFSoundSample; Max: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Sample information functions
}

var
  FSOUND_Sample_Get: function (SampNo: Integer): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetName: function (Sptr: PFSoundSample): PCHAR; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetLength: function (Sptr: PFSoundSample): Cardinal; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetLoopPoints: function (Sptr: PFSoundSample; var LoopStart: Integer; var LoopEnd: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetDefaults: function (Sptr: PFSoundSample; var DefFreq: Integer; var DefVol: Integer; var DefPan: Integer; var DefPri: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetDefaultsEx: function (Sptr: PFSoundSample; var DefFreq: Integer; var DefVol: Integer; var DefPan: Integer; var DefPri: Integer; var VarFreq: Integer; var VarVol: Integer; var VarPan): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetMode: function (Sptr: PFSoundSample): Cardinal; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetMinMaxDistance: function (Sptr: PFSoundSample; var Min: Single; var Max: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ ============================ }
{ Channel control functions.   }
{ ============================ }

{
  Playing and stopping sounds.
  Note : Use FSOUND_FREE as the 'channel' variable, to let FMOD pick a free channel for you.
         Use FSOUND_ALL as the 'channel' variable to control ALL channels with one function call!
}

var
  FSOUND_PlaySound: function (Channel: Integer; Sptr: PFSoundSample): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_PlaySoundEx: function (Channel: Integer; Sptr: PFSoundSample; Dsp: PFSoundDSPUnit; StartPaused: ByteBool): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_StopSound: function (Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Functions to control playback of a channel.
}

var
  FSOUND_SetFrequency: function (Channel: Integer; Freq: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetVolume: function (Channel: Integer; Vol: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetVolumeAbsolute: function (Channel: Integer; Vol: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetPan: function (Channel: Integer; Pan: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetSurround: function (Channel: Integer; Surround: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMute: function (Channel: Integer; Mute: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetPriority: function (Channel: Integer; Priority: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetReserved: function (Channel: Integer; Reserved: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetPaused: function (Channel: Integer; Paused: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetLoopMode: function (Channel: Integer; LoopMode: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetCurrentPosition: function (Channel: Integer; Offset: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_SetAttributes: function (Channel: Integer; Pos: PFSoundVector; Vel: PFSoundVector): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_SetMinMaxDistance: function (Channel: Integer; Min: Single; Max: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Channel information functions
}

var
  FSOUND_IsPlaying: function (Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetFrequency: function (Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetVolume: function (Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetAmplitude: function (Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetPan: function (Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetSurround: function (Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMute: function (Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetPriority: function (Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetReserved: function (Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetPaused: function (Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetLoopMode: function (Channel: Integer): Cardinal; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetCurrentPosition: function (Channel: Integer): Cardinal; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetCurrentSample: function (Channel: Integer): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetCurrentLevels: function (Channel: Integer; L, R: PSingle): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetNumSubChannels: function (Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetSubChannel: function (Channel: Integer; SubChannel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_GetAttributes: function (Channel: Integer; Pos: PFSoundVector; Vel: PFSoundVector): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_GetMinMaxDistance: function (Channel: Integer; var Min: Single; var Max: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ =================== }
{ 3D sound functions. }
{ =================== }

{
    See also 3d sample and channel based functions above.
    Call FSOUND_Update once a frame to process 3d information.
}

var
  FSOUND_3D_Listener_SetCurrent: procedure (current: Integer); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_Listener_SetAttributes: procedure (Pos: PFSoundVector; Vel: PFSoundVector;
                                               fx: Single; fy: Single; fz: Single;
                                               tx: Single; ty: Single; tz: Single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_Listener_GetAttributes: procedure (Pos: PFSoundVector; Vel: PFSoundVector;
                                               fx: PSingle; fy: PSingle; fz: PSingle;
                                               tx: PSingle; ty: PSingle; tz: PSingle); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_SetDopplerFactor: procedure (Scale: Single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_SetDistanceFactor: procedure (Scale: Single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_SetRolloffFactor: procedure (Scale: Single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ =================== }
{ FX functions.       }
{ =================== }

{
    Functions to control DX8 only effects processing.

    - FX enabled samples can only be played once at a time, not multiple times at once.
    - Sounds have to be created with FSOUND_HW2D or FSOUND_HW3D for this to work.
    - FSOUND_INIT_ENABLESYSTEMCHANNELFX can be used to apply hardware effect processing to the
      global mixed output of FMOD's software channels.
    - FSOUND_FX_Enable returns an FX handle that you can use to alter fx parameters.
    - FSOUND_FX_Enable can be called multiple times in a row, even on the same FX type,
      it will return a unique handle for each FX.
    - FSOUND_FX_Enable cannot be called if the sound is playing or locked.
    - Stopping or starting a sound resets all FX and they must be re-enabled each time
      if this happens.
}

var
  FSOUND_FX_Enable: function (Channel: Integer; Fx: TFSoundFXModes): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};    { Set bits to enable following fx }
  FSOUND_FX_Disable: function (Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_FX_SetChorus: function (FXId: Integer; WetDryMix, Depth, Feedback, Frequency: Single; Waveform: Integer; Delay: Single; Phase: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetCompressor: function (FXId: Integer; Gain, Attack, Release, Threshold, Ratio, Predelay: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetDistortion: function (FXId: Integer; Gain, Edge, PostEQCenterFrequency, PostEQBandwidth, PreLowpassCutoff: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetEcho: function (FXId: Integer; WetDryMix, Feedback, LeftDelay, RightDelay: Single; PanDelay: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetFlanger: function (FXId: Integer; WetDryMix, Depth, Feedback, Frequency: Single; Waveform: Integer; Delay: Single; Phase: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetGargle: function (FXId, RateHz, WaveShape: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetI3DL2Reverb: function (FXId, Room, RoomHF: Integer; RoomRolloffFactor, DecayTime, DecayHFRatio: Single; Reflections: Integer; ReflectionsDelay: Single; Reverb: Integer; ReverbDelay, Diffusion, Density, HFReference: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetParamEQ: function (FXId: Integer; Center, Bandwidth, Gain: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetWavesReverb: function (FXId: Integer; InGain, ReverbMix, ReverbTime, HighFreqRTRatio: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ ========================= }
{ File Streaming functions. }
{ ========================= }

{
    Note : Use FSOUND_LOADMEMORY   flag with FSOUND_Stream_Open to stream from memory.
           Use FSOUND_LOADRAW      flag with FSOUND_Stream_Open to treat stream as raw pcm data.
           Use FSOUND_MPEGACCURATE flag with FSOUND_Stream_Open to open mpegs in 'accurate mode' for settime/gettime/getlengthms.
           Use FSOUND_FREE as the 'channel' variable, to let FMOD pick a free channel for you.
}

var
  // call this before opening streams, not after
  FSOUND_Stream_SetBufferSize: function (Ms: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_Open: function(const name_or_data: PChar; Mode: Cardinal; Offset: Integer; Length: Integer): PFSoundStream; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Create: function (Callback: TFSoundStreamCallback; Length: Integer; Mode: Cardinal; SampleRate: Integer; UserData: Integer): PFSoundStream; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Close: function(Stream: PFSoundStream): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_Play: function(Channel: Integer; Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_PlayEx: function (Channel: Integer; Stream: PFSoundStream; Dsp: PFSoundDSPUnit; StartPaused: ByteBool): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Stop: function(Stream: PFSoundStream): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_SetPosition: function (Stream: PFSoundStream; Position: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetPosition: function (Stream: PFSoundStream): Cardinal; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetTime: function (Stream: PFSoundStream; Ms: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetTime: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetLength: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetLengthMs: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_SetMode: function (Stream: PFSoundStream; mode: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetMode: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetLoopPoints: function (Stream: PFSoundStream; LoopStartPCM, LoopEndPCM: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetLoopCount: function (Stream: PFSoundStream; Count: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetOpenState: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetSample: function (Stream: PFSoundStream): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF}; { Every stream contains a sample to play back on }
  FSOUND_Stream_CreateDSP: function (Stream: PFSoundStream; Callback: TFSoundDSPCallback; Priority: Integer; Param: Integer): PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_SetEndCallback: function (Stream: PFSoundStream; Callback: TFSoundStreamCallback; UserData: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetSyncCallback: function (Stream: PFSoundStream; Callback: TFSoundStreamCallback; UserData: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  
  FSOUND_Stream_AddSyncPoint: function (Stream: PFSoundStream; PCMOffset: Cardinal; Name: PChar): PFSyncPoint; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_DeleteSyncPoint: function (Point: PFSyncPoint): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetNumSyncPoints: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetSyncPoint: function (Stream: PFSoundStream; Index: Integer): PFSyncPoint; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetSyncPointInfo: function (Point: PFSyncPoint; var PCMOffset: Cardinal): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_SetSubStream: function (Stream: PFSoundStream; Index: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetNumSubStreams: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetSubStreamSentence: function (Stream: PFSoundStream; var sentencelist: Cardinal; numitems: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
                                                
  FSOUND_Stream_GetNumTagFields: function (Stream: PFSoundStream; var Num: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetTagField: function (Stream: PFSoundStream; Num: Integer; var _Type: TFSoundTagFieldType; var Name: PCHAR; var Value: Pointer; var Length: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_FindTagField: function (Stream: PFSoundStream; _Type: TFSoundTagFieldType; Name: PChar; var Value: Pointer; var Length: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_Net_SetProxy: function (Proxy: PChar): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Net_GetLastServerStatus: function: PChar; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Net_SetBufferProperties: function (BufferSize: Integer; PreBuffer_Percent: Integer; ReBuffer_Percent:  Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Net_GetBufferProperties: function (var Buffersize: Integer; var PreBuffer_Percent: Integer;  var ReBuffer_Percent: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Net_SetMetadataCallback: function (Stream: PFSoundStream; Callback: TFMetaDataCallback; UserData: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Net_GetStatus: function (Stream: PFSoundStream; var Status: TFSoundStreamNetStatus; var BufferPercentUsed: Integer; var BitRate: Integer; var Flags: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ =================== }
{ CD audio functions. }
{ =================== }

{
    Note : 0 = default cdrom.  Otherwise specify the drive letter, for example. 'D'. 
}

var
  FSOUND_CD_Play: function (Drive: Byte; Track: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_SetPlayMode: procedure (Drive: Byte; Mode: Integer); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_Stop: function (Drive: Byte): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_SetPaused: function (Drive: Byte; Paused: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_SetVolume: function (Drive: Byte; Volume: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_SetTrackTime: function (Drive: Byte; ms: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_OpenTray: function (Drive: Byte; Open: Byte): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

var
  FSOUND_CD_GetPaused: function (Drive: Byte): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetTrack: function (Drive: Byte): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetNumTracks: function (Drive: Byte): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetVolume: function (Drive: Byte): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetTrackLength: function (Drive: Byte; Track: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetTrackTime: function (Drive: Byte): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ ============== }
{ DSP functions. }
{ ============== }

{
  DSP Unit control and information functions.
}

var
  FSOUND_DSP_Create: function (Callback: TFSoundDSPCallback; Priority: Integer; Param: Integer): PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_Free: procedure (DSPUnit: PFSoundDSPUnit); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_SetPriority: procedure (DSPUnit: PFSoundDSPUnit; Priority: Integer); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetPriority: function (DSPUnit: PFSoundDSPUnit): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_SetActive: procedure (DSPUnit: PFSoundDSPUnit; Active: ByteBool); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetActive: function (DSPUnit: PFSoundDSPUnit): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Functions to get hold of FSOUND 'system DSP unit' handles.
}

var
  FSOUND_DSP_GetClearUnit: function: PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetSFXUnit: function: PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetMusicUnit: function: PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetClipAndCopyUnit: function: PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetFFTUnit: function: PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Miscellaneous DSP functions
  Note for the spectrum analysis function to work, you have to enable the FFT DSP unit with
  the following code FSOUND_DSP_SetActive(FSOUND_DSP_GetFFTUnit(), TRUE);
  It is off by default to save cpu usage.
}

var
  FSOUND_DSP_MixBuffers: function (DestBuffer: Pointer; SrcBuffer: Pointer; Len: Integer; Freq: Integer; Vol: Integer; Pan: Integer; Mode: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_ClearMixBuffer: procedure; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetBufferLength: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};      { Length of each DSP update }
  FSOUND_DSP_GetBufferLengthTotal: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF}; { Total buffer length due to FSOUND_SetBufferSize }
  FSOUND_DSP_GetSpectrum: function: PSingle; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};          { Array of 512 floats - call FSOUND_DSP_SetActive(FSOUND_DSP_GetFFTUnit(), TRUE)) for this to work. }

{ ========================================================================== }
{ Reverb functions. (eax2/eax3 reverb)  (NOT SUPPORTED IN LINUX/CE)               }
{ ========================================================================== }

{
  See structures above for definitions and information on the reverb parameters.
}

var
  FSOUND_Reverb_SetProperties: function (const Prop: TFSoundReverbProperties): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Reverb_GetProperties: function (var Prop: TFSoundReverbProperties): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Reverb_SetChannelProperties: function (Channel: Integer; var Prop: TFSoundReverbChannelProperties): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Reverb_GetChannelProperties: function (Channel: Integer; var Prop: TFSoundReverbChannelProperties): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ ================================================ }
{ Recording functions  (NOT SUPPORTED IN LINUX/MAC) }
{ ================================================ }

{
  Recording initialization functions
}

var
  FSOUND_Record_SetDriver: function (OutputType: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_GetNumDrivers: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_GetDriverName: function (Id: Integer): PChar; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_GetDriver: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Recording functionality. Only one recording session will work at a time.
}

var
  FSOUND_Record_StartSample: function (Sptr: PFSoundSample; Loop: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_Stop: function: ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_GetPosition: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ ============================================================================================= }
{ FMUSIC API (MOD,S3M,XM,IT,MIDI PLAYBACK)                                                      }
{ ============================================================================================= }

{
  Song management / playback functions.
}

var
  FMUSIC_LoadSong: function (const Name: PChar): PFMusicModule; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_LoadSongEx: function (Name_Or_Data: Pointer; Offset: Integer; Length: Integer; Mode: Cardinal; var SampleList: Integer; SampleListNum: Integer): PFMusicModule; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetOpenState: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_FreeSong: function (Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_PlaySong: function (Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_StopSong: function (Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_StopAllSongs: procedure; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

var
  FMUSIC_SetZxxCallback: function (Module: PFMusicModule; Callback: TFMusicCallback): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetRowCallback: function (Module: PFMusicModule; Callback: TFMusicCallback; RowStep: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetOrderCallback: function (Module: PFMusicModule; Callback: TFMusicCallback; OrderStep: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetInstCallback: function (Module: PFMusicModule; Callback: TFMusicCallback; Instrument: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

var
  FMUSIC_SetSample: function (Module: PFMusicModule; SampNo: Integer; Sptr: PFSoundSample): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetUserData: function (Module: PFMusicModule; userdata: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_OptimizeChannels: function (Module: PFMusicModule; MaxChannels: Integer; MinVolume: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Runtime song functions.
}

var
  FMUSIC_SetReverb: function (Reverb: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetLooping: function (Module: PFMusicModule; Looping: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetOrder: function (Module: PFMusicModule; Order: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetPaused: function (Module: PFMusicModule; Pause: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetMasterVolume: function (Module: PFMusicModule; Volume: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetMasterSpeed: function (Module: PFMusicModule; Speed: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetPanSeperation: function (Module: PFMusicModule; PanSep: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Static song information functions.
}

var
  FMUSIC_GetName: function (Module: PFMusicModule): PCHAR; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetType: function (Module: PFMusicModule): TFMusicTypes; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumOrders: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumPatterns: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumInstruments: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumSamples: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumChannels: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetSample: function (Module: PFMusicModule; SampNo: Integer): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetPatternLength: function (Module: PFMusicModule; OrderNo: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Runtime song information.
}

var
  FMUSIC_IsFinished: function (Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_IsPlaying: function (Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetMasterVolume: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetGlobalVolume: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetOrder: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetPattern: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetSpeed: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetBPM: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetRow: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetPaused: function (Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetTime: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetRealChannel: function (Module: PFMusicModule; ModChannel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetUserData: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

implementation

{$IFDEF UNIX}
uses
 {$IFDEF LINUX} Libc,{$ENDIF}
 dynlibs ;
{$ENDIF}

const
{$IFDEF LINUX}
  FMOD_DLL = 'libfmod.so';
{$ELSE}
{$IFDEF MSWINDOWS}
  FMOD_DLL = 'fmod.dll';
{$ENDIF}
{$IFDEF DARWIN}
  FMOD_DLL = 'fmod.dylib';
{$ENDIF}
{$ENDIF}

type
{$IFDEF UNIX}
  TFMODModuleHandle = TLibHandle;
{$ELSE}
  TFMODModuleHandle = HINST;
{$ENDIF}

const
  INVALID_MODULEHANDLE_VALUE = TFMODModuleHandle(0);


var
  FMODHandle: TFMODModuleHandle = 0;

function GetAddress(Handle: TFMODModuleHandle; FuncName: PChar): Pointer;
begin
  Result := GetProcAddress(Handle, FuncName);

//  Assert(Result <> nil, 'Failed to find ' + FuncName + ' in ' + FMOD_DLL);
end;

function FMOD_Load(LibName: PChar): Boolean;
begin
  Result := False;

  { Make sure the previous library is unloaded }
  FMOD_Unload;

  { If no library name given, use the default library names }
  if LibName = nil then
    LibName := FMOD_DLL;

  { Load the library }

  FMODHandle := LoadLibrary(LibName);

  if FMODHandle = INVALID_MODULEHANDLE_VALUE then
    Exit;

  { Get all the function addresses from the library }
  FSOUND_SetOutput                      := GetAddress(FMODHandle, 'FSOUND_SetOutput');
  FSOUND_SetDriver                      := GetAddress(FMODHandle, 'FSOUND_SetDriver');
  FSOUND_SetMixer                       := GetAddress(FMODHandle, 'FSOUND_SetMixer');
  FSOUND_SetBufferSize                  := GetAddress(FMODHandle, 'FSOUND_SetBufferSize');
  FSOUND_SetHWND                        := GetAddress(FMODHandle, 'FSOUND_SetHWND');
  FSOUND_SetMinHardwareChannels         := GetAddress(FMODHandle, 'FSOUND_SetMinHardwareChannels');
  FSOUND_SetMaxHardwareChannels         := GetAddress(FMODHandle, 'FSOUND_SetMaxHardwareChannels');
  FSOUND_SetMemorySystem                := GetAddress(FMODHandle, 'FSOUND_SetMemorySystem');
  FSOUND_Init                           := GetAddress(FMODHandle, 'FSOUND_Init');
  FSOUND_Close                          := GetAddress(FMODHandle, 'FSOUND_Close');
  FSOUND_Update                         := GetAddress(FMODHandle, 'FSOUND_Update');
  FSOUND_SetSpeakerMode                 := GetAddress(FMODHandle, 'FSOUND_SetSpeakerMode');
  FSOUND_SetSFXMasterVolume             := GetAddress(FMODHandle, 'FSOUND_SetSFXMasterVolume');
  FSOUND_SetPanSeperation               := GetAddress(FMODHandle, 'FSOUND_SetPanSeperation');
  FSOUND_GetError                       := GetAddress(FMODHandle, 'FSOUND_GetError');
  FSOUND_GetVersion                     := GetAddress(FMODHandle, 'FSOUND_GetVersion');
  FSOUND_GetOutput                      := GetAddress(FMODHandle, 'FSOUND_GetOutput');
  FSOUND_GetOutputHandle                := GetAddress(FMODHandle, 'FSOUND_GetOutputHandle');
  FSOUND_GetDriver                      := GetAddress(FMODHandle, 'FSOUND_GetDriver');
  FSOUND_GetMixer                       := GetAddress(FMODHandle, 'FSOUND_GetMixer');
  FSOUND_GetNumDrivers                  := GetAddress(FMODHandle, 'FSOUND_GetNumDrivers');
  FSOUND_GetDriverName                  := GetAddress(FMODHandle, 'FSOUND_GetDriverName');
  FSOUND_GetDriverCaps                  := GetAddress(FMODHandle, 'FSOUND_GetDriverCaps');
  FSOUND_GetOutputRate                  := GetAddress(FMODHandle, 'FSOUND_GetOutputRate');
  FSOUND_GetMaxChannels                 := GetAddress(FMODHandle, 'FSOUND_GetMaxChannels');
  FSOUND_GetMaxSamples                  := GetAddress(FMODHandle, 'FSOUND_GetMaxSamples');
  FSOUND_GetSpeakerMode                 := GetAddress(FMODHandle, 'FSOUND_GetSpeakerMode');
  FSOUND_GetSFXMasterVolume             := GetAddress(FMODHandle, 'FSOUND_GetSFXMasterVolume');
  FSOUND_GetNumHWChannels               := GetAddress(FMODHandle, 'FSOUND_GetNumHWChannels');
  FSOUND_GetChannelsPlaying             := GetAddress(FMODHandle, 'FSOUND_GetChannelsPlaying');
  FSOUND_GetCPUUsage                    := GetAddress(FMODHandle, 'FSOUND_GetCPUUsage');
  FSOUND_GetMemoryStats                 := GetAddress(FMODHandle, 'FSOUND_GetMemoryStats');
  FSOUND_Sample_Load                    := GetAddress(FMODHandle, 'FSOUND_Sample_Load');
  FSOUND_Sample_Alloc                   := GetAddress(FMODHandle, 'FSOUND_Sample_Alloc');
  FSOUND_Sample_Free                    := GetAddress(FMODHandle, 'FSOUND_Sample_Free');
  FSOUND_Sample_Upload                  := GetAddress(FMODHandle, 'FSOUND_Sample_Upload');
  FSOUND_Sample_Lock                    := GetAddress(FMODHandle, 'FSOUND_Sample_Lock');
  FSOUND_Sample_Unlock                  := GetAddress(FMODHandle, 'FSOUND_Sample_Unlock');
  FSOUND_Sample_SetMode                 := GetAddress(FMODHandle, 'FSOUND_Sample_SetMode');
  FSOUND_Sample_SetLoopPoints           := GetAddress(FMODHandle, 'FSOUND_Sample_SetLoopPoints');
  FSOUND_Sample_SetDefaults             := GetAddress(FMODHandle, 'FSOUND_Sample_SetDefaults');
  FSOUND_Sample_SetDefaultsEx           := GetAddress(FMODHandle, 'FSOUND_Sample_SetDefaultsEx');
  FSOUND_Sample_SetMinMaxDistance       := GetAddress(FMODHandle, 'FSOUND_Sample_SetMinMaxDistance');
  FSOUND_Sample_SetMaxPlaybacks         := GetAddress(FMODHandle, 'FSOUND_Sample_SetMaxPlaybacks');
  FSOUND_Sample_Get                     := GetAddress(FMODHandle, 'FSOUND_Sample_Get');
  FSOUND_Sample_GetName                 := GetAddress(FMODHandle, 'FSOUND_Sample_GetName');
  FSOUND_Sample_GetLength               := GetAddress(FMODHandle, 'FSOUND_Sample_GetLength');
  FSOUND_Sample_GetLoopPoints           := GetAddress(FMODHandle, 'FSOUND_Sample_GetLoopPoints');
  FSOUND_Sample_GetDefaults             := GetAddress(FMODHandle, 'FSOUND_Sample_GetDefaults');
  FSOUND_Sample_GetDefaultsEx           := GetAddress(FMODHandle, 'FSOUND_Sample_GetDefaultsEx');
  FSOUND_Sample_GetMode                 := GetAddress(FMODHandle, 'FSOUND_Sample_GetMode');
  FSOUND_Sample_GetMinMaxDistance       := GetAddress(FMODHandle, 'FSOUND_Sample_GetMinMaxDistance');
  FSOUND_PlaySound                      := GetAddress(FMODHandle, 'FSOUND_PlaySound');
  FSOUND_PlaySoundEx                    := GetAddress(FMODHandle, 'FSOUND_PlaySoundEx');
  FSOUND_StopSound                      := GetAddress(FMODHandle, 'FSOUND_StopSound');
  FSOUND_SetFrequency                   := GetAddress(FMODHandle, 'FSOUND_SetFrequency');
  FSOUND_SetVolume                      := GetAddress(FMODHandle, 'FSOUND_SetVolume');
  FSOUND_SetVolumeAbsolute              := GetAddress(FMODHandle, 'FSOUND_SetVolumeAbsolute');
  FSOUND_SetPan                         := GetAddress(FMODHandle, 'FSOUND_SetPan');
  FSOUND_SetSurround                    := GetAddress(FMODHandle, 'FSOUND_SetSurround');
  FSOUND_SetMute                        := GetAddress(FMODHandle, 'FSOUND_SetMute');
  FSOUND_SetPriority                    := GetAddress(FMODHandle, 'FSOUND_SetPriority');
  FSOUND_SetReserved                    := GetAddress(FMODHandle, 'FSOUND_SetReserved');
  FSOUND_SetPaused                      := GetAddress(FMODHandle, 'FSOUND_SetPaused');
  FSOUND_SetLoopMode                    := GetAddress(FMODHandle, 'FSOUND_SetLoopMode');
  FSOUND_SetCurrentPosition             := GetAddress(FMODHandle, 'FSOUND_SetCurrentPosition');
  FSOUND_3D_SetAttributes               := GetAddress(FMODHandle, 'FSOUND_3D_SetAttributes');
  FSOUND_3D_SetMinMaxDistance           := GetAddress(FMODHandle, 'FSOUND_3D_SetMinMaxDistance');
  FSOUND_IsPlaying                      := GetAddress(FMODHandle, 'FSOUND_IsPlaying');
  FSOUND_GetFrequency                   := GetAddress(FMODHandle, 'FSOUND_GetFrequency');
  FSOUND_GetVolume                      := GetAddress(FMODHandle, 'FSOUND_GetVolume');
  FSOUND_GetAmplitude                   := GetAddress(FMODHandle, 'FSOUND_GetAmplitude');
  FSOUND_GetPan                         := GetAddress(FMODHandle, 'FSOUND_GetPan');
  FSOUND_GetSurround                    := GetAddress(FMODHandle, 'FSOUND_GetSurround');
  FSOUND_GetMute                        := GetAddress(FMODHandle, 'FSOUND_GetMute');
  FSOUND_GetPriority                    := GetAddress(FMODHandle, 'FSOUND_GetPriority');
  FSOUND_GetReserved                    := GetAddress(FMODHandle, 'FSOUND_GetReserved');
  FSOUND_GetPaused                      := GetAddress(FMODHandle, 'FSOUND_GetPaused');
  FSOUND_GetLoopMode                    := GetAddress(FMODHandle, 'FSOUND_GetLoopMode');
  FSOUND_GetCurrentPosition             := GetAddress(FMODHandle, 'FSOUND_GetCurrentPosition');
  FSOUND_GetCurrentSample               := GetAddress(FMODHandle, 'FSOUND_GetCurrentSample');
  FSOUND_GetCurrentLevels               := GetAddress(FMODHandle, 'FSOUND_GetCurrentLevels');
  FSOUND_GetNumSubChannels              := GetAddress(FMODHandle, 'FSOUND_GetNumSubChannels');
  FSOUND_GetSubChannel                  := GetAddress(FMODHandle, 'FSOUND_GetSubChannel');
  FSOUND_3D_GetAttributes               := GetAddress(FMODHandle, 'FSOUND_3D_GetAttributes');
  FSOUND_3D_GetMinMaxDistance           := GetAddress(FMODHandle, 'FSOUND_3D_GetMinMaxDistance');
  FSOUND_3D_Listener_SetCurrent         := GetAddress(FMODHandle, 'FSOUND_3D_Listener_SetCurrent');
  FSOUND_3D_Listener_SetAttributes      := GetAddress(FMODHandle, 'FSOUND_3D_Listener_SetAttributes');
  FSOUND_3D_Listener_GetAttributes      := GetAddress(FMODHandle, 'FSOUND_3D_Listener_GetAttributes');
  FSOUND_3D_SetDopplerFactor            := GetAddress(FMODHandle, 'FSOUND_3D_SetDopplerFactor');
  FSOUND_3D_SetDistanceFactor           := GetAddress(FMODHandle, 'FSOUND_3D_SetDistanceFactor');
  FSOUND_3D_SetRolloffFactor            := GetAddress(FMODHandle, 'FSOUND_3D_SetRolloffFactor');
  FSOUND_FX_Enable                      := GetAddress(FMODHandle, 'FSOUND_FX_Enable');
  FSOUND_FX_SetChorus                   := GetAddress(FMODHandle, 'FSOUND_FX_SetChorus');
  FSOUND_FX_SetCompressor               := GetAddress(FMODHandle, 'FSOUND_FX_SetCompressor');
  FSOUND_FX_SetDistortion               := GetAddress(FMODHandle, 'FSOUND_FX_SetDistortion');
  FSOUND_FX_SetEcho                     := GetAddress(FMODHandle, 'FSOUND_FX_SetEcho');
  FSOUND_FX_SetFlanger                  := GetAddress(FMODHandle, 'FSOUND_FX_SetFlanger');
  FSOUND_FX_SetGargle                   := GetAddress(FMODHandle, 'FSOUND_FX_SetGargle');
  FSOUND_FX_SetI3DL2Reverb              := GetAddress(FMODHandle, 'FSOUND_FX_SetI3DL2Reverb');
  FSOUND_FX_SetParamEQ                  := GetAddress(FMODHandle, 'FSOUND_FX_SetParamEQ');
  FSOUND_FX_SetWavesReverb              := GetAddress(FMODHandle, 'FSOUND_FX_SetWavesReverb');
  FSOUND_Stream_Open                    := GetAddress(FMODHandle, 'FSOUND_Stream_Open');
  FSOUND_Stream_Create                  := GetAddress(FMODHandle, 'FSOUND_Stream_Create');
  FSOUND_Stream_Close                   := GetAddress(FMODHandle, 'FSOUND_Stream_Close');
  FSOUND_Stream_Play                    := GetAddress(FMODHandle, 'FSOUND_Stream_Play');
  FSOUND_Stream_PlayEx                  := GetAddress(FMODHandle, 'FSOUND_Stream_PlayEx');
  FSOUND_Stream_Stop                    := GetAddress(FMODHandle, 'FSOUND_Stream_Stop');
  FSOUND_Stream_SetEndCallback          := GetAddress(FMODHandle, 'FSOUND_Stream_SetEndCallback');
  FSOUND_Stream_SetSyncCallback         := GetAddress(FMODHandle, 'FSOUND_Stream_SetSyncCallback');
  FSOUND_Stream_GetSample               := GetAddress(FMODHandle, 'FSOUND_Stream_GetSample');
  FSOUND_Stream_CreateDSP               := GetAddress(FMODHandle, 'FSOUND_Stream_CreateDSP');
  FSOUND_Stream_SetBufferSize           := GetAddress(FMODHandle, 'FSOUND_Stream_SetBufferSize');
  FSOUND_Stream_SetPosition             := GetAddress(FMODHandle, 'FSOUND_Stream_SetPosition');
  FSOUND_Stream_GetPosition             := GetAddress(FMODHandle, 'FSOUND_Stream_GetPosition');
  FSOUND_Stream_SetTime                 := GetAddress(FMODHandle, 'FSOUND_Stream_SetTime');
  FSOUND_Stream_GetTime                 := GetAddress(FMODHandle, 'FSOUND_Stream_GetTime');
  FSOUND_Stream_GetLength               := GetAddress(FMODHandle, 'FSOUND_Stream_GetLength');
  FSOUND_Stream_GetLengthMs             := GetAddress(FMODHandle, 'FSOUND_Stream_GetLengthMs');
  FSOUND_Stream_SetMode                 := GetAddress(FMODHandle, 'FSOUND_Stream_SetMode');
  FSOUND_Stream_GetMode                 := GetAddress(FMODHandle, 'FSOUND_Stream_GetMode');
  FSOUND_Stream_SetLoopPoints           := GetAddress(FMODHandle, 'FSOUND_Stream_SetLoopPoints');
  FSOUND_Stream_SetLoopCount            := GetAddress(FMODHandle, 'FSOUND_Stream_SetLoopCount');
  FSOUND_Stream_GetOpenState            := GetAddress(FMODHandle, 'FSOUND_Stream_GetOpenState');
  FSOUND_Stream_AddSyncPoint            := GetAddress(FMODHandle, 'FSOUND_Stream_AddSyncPoint');
  FSOUND_Stream_DeleteSyncPoint         := GetAddress(FMODHandle, 'FSOUND_Stream_DeleteSyncPoint');
  FSOUND_Stream_GetNumSyncPoints        := GetAddress(FMODHandle, 'FSOUND_Stream_GetNumSyncPoints');
  FSOUND_Stream_GetSyncPoint            := GetAddress(FMODHandle, 'FSOUND_Stream_GetSyncPoint');
  FSOUND_Stream_GetSyncPointInfo        := GetAddress(FMODHandle, 'FSOUND_Stream_GetSyncPointInfo');
  FSOUND_Stream_SetSubStream            := GetAddress(FMODHandle, 'FSOUND_Stream_SetSubStream');
  FSOUND_Stream_GetNumSubStreams        := GetAddress(FMODHandle, 'FSOUND_Stream_GetNumSubStreams');
  FSOUND_Stream_SetSubStreamSentence    := GetAddress(FMODHandle, 'FSOUND_Stream_SetSubStreamSentence');
  FSOUND_Stream_GetNumTagFields         := GetAddress(FMODHandle, 'FSOUND_Stream_GetNumTagFields');
  FSOUND_Stream_GetTagField             := GetAddress(FMODHandle, 'FSOUND_Stream_GetTagField');
  FSOUND_Stream_FindTagField            := GetAddress(FMODHandle, 'FSOUND_Stream_FindTagField');
  FSOUND_Stream_Net_SetProxy            := GetAddress(FMODHandle, 'FSOUND_Stream_Net_SetProxy');
  FSOUND_Stream_Net_GetLastServerStatus := GetAddress(FMODHandle, 'FSOUND_Stream_Net_GetLastServerStatus');
  FSOUND_Stream_Net_SetBufferProperties := GetAddress(FMODHandle, 'FSOUND_Stream_Net_SetBufferProperties');
  FSOUND_Stream_Net_GetBufferProperties := GetAddress(FMODHandle, 'FSOUND_Stream_Net_GetBufferProperties');
  FSOUND_Stream_Net_SetMetadataCallback := GetAddress(FMODHandle, 'FSOUND_Stream_Net_SetMetadataCallback');
  FSOUND_Stream_Net_GetStatus           := GetAddress(FMODHandle, 'FSOUND_Stream_Net_GetStatus');
  FSOUND_CD_Play                        := GetAddress(FMODHandle, 'FSOUND_CD_Play');
  FSOUND_CD_SetPlayMode                 := GetAddress(FMODHandle, 'FSOUND_CD_SetPlayMode');
  FSOUND_CD_Stop                        := GetAddress(FMODHandle, 'FSOUND_CD_Stop');
  FSOUND_CD_SetPaused                   := GetAddress(FMODHandle, 'FSOUND_CD_SetPaused');
  FSOUND_CD_SetVolume                   := GetAddress(FMODHandle, 'FSOUND_CD_SetVolume');
  FSOUND_CD_SetTrackTime                := GetAddress(FMODHandle, 'FSOUND_CD_SetTrackTime');
  FSOUND_CD_OpenTray                    := GetAddress(FMODHandle, 'FSOUND_CD_OpenTray');
  FSOUND_CD_GetPaused                   := GetAddress(FMODHandle, 'FSOUND_CD_GetPaused');
  FSOUND_CD_GetTrack                    := GetAddress(FMODHandle, 'FSOUND_CD_GetTrack');
  FSOUND_CD_GetNumTracks                := GetAddress(FMODHandle, 'FSOUND_CD_GetNumTracks');
  FSOUND_CD_GetVolume                   := GetAddress(FMODHandle, 'FSOUND_CD_GetVolume');
  FSOUND_CD_GetTrackLength              := GetAddress(FMODHandle, 'FSOUND_CD_GetTrackLength');
  FSOUND_CD_GetTrackTime                := GetAddress(FMODHandle, 'FSOUND_CD_GetTrackTime');
  FSOUND_DSP_Create                     := GetAddress(FMODHandle, 'FSOUND_DSP_Create');
  FSOUND_DSP_Free                       := GetAddress(FMODHandle, 'FSOUND_DSP_Free');
  FSOUND_DSP_SetPriority                := GetAddress(FMODHandle, 'FSOUND_DSP_SetPriority');
  FSOUND_DSP_GetPriority                := GetAddress(FMODHandle, 'FSOUND_DSP_GetPriority');
  FSOUND_DSP_SetActive                  := GetAddress(FMODHandle, 'FSOUND_DSP_SetActive');
  FSOUND_DSP_GetActive                  := GetAddress(FMODHandle, 'FSOUND_DSP_GetActive');
  FSOUND_DSP_GetClearUnit               := GetAddress(FMODHandle, 'FSOUND_DSP_GetClearUnit');
  FSOUND_DSP_GetSFXUnit                 := GetAddress(FMODHandle, 'FSOUND_DSP_GetSFXUnit');
  FSOUND_DSP_GetMusicUnit               := GetAddress(FMODHandle, 'FSOUND_DSP_GetMusicUnit');
  FSOUND_DSP_GetClipAndCopyUnit         := GetAddress(FMODHandle, 'FSOUND_DSP_GetClipAndCopyUnit');
  FSOUND_DSP_GetFFTUnit                 := GetAddress(FMODHandle, 'FSOUND_DSP_GetFFTUnit');
  FSOUND_DSP_MixBuffers                 := GetAddress(FMODHandle, 'FSOUND_DSP_MixBuffers');
  FSOUND_DSP_ClearMixBuffer             := GetAddress(FMODHandle, 'FSOUND_DSP_ClearMixBuffer');
  FSOUND_DSP_GetBufferLength            := GetAddress(FMODHandle, 'FSOUND_DSP_GetBufferLength');
  FSOUND_DSP_GetBufferLengthTotal       := GetAddress(FMODHandle, 'FSOUND_DSP_GetBufferLengthTotal');
  FSOUND_DSP_GetSpectrum                := GetAddress(FMODHandle, 'FSOUND_DSP_GetSpectrum');
  FSOUND_Reverb_SetProperties           := GetAddress(FMODHandle, 'FSOUND_Reverb_SetProperties');
  FSOUND_Reverb_GetProperties           := GetAddress(FMODHandle, 'FSOUND_Reverb_GetProperties');
  FSOUND_Reverb_SetChannelProperties    := GetAddress(FMODHandle, 'FSOUND_Reverb_SetChannelProperties');
  FSOUND_Reverb_GetChannelProperties    := GetAddress(FMODHandle, 'FSOUND_Reverb_GetChannelProperties');
  FSOUND_Record_SetDriver               := GetAddress(FMODHandle, 'FSOUND_Record_SetDriver');
  FSOUND_Record_GetNumDrivers           := GetAddress(FMODHandle, 'FSOUND_Record_GetNumDrivers');
  FSOUND_Record_GetDriverName           := GetAddress(FMODHandle, 'FSOUND_Record_GetDriverName');
  FSOUND_Record_GetDriver               := GetAddress(FMODHandle, 'FSOUND_Record_GetDriver');
  FSOUND_Record_StartSample             := GetAddress(FMODHandle, 'FSOUND_Record_StartSample');
  FSOUND_Record_Stop                    := GetAddress(FMODHandle, 'FSOUND_Record_Stop');
  FSOUND_Record_GetPosition             := GetAddress(FMODHandle, 'FSOUND_Record_GetPosition');
  FSOUND_File_SetCallbacks              := GetAddress(FMODHandle, 'FSOUND_File_SetCallbacks');
  FMUSIC_LoadSong                       := GetAddress(FMODHandle, 'FMUSIC_LoadSong');
  FMUSIC_LoadSongEx                     := GetAddress(FMODHandle, 'FMUSIC_LoadSongEx');
  FMUSIC_GetOpenState                   := GetAddress(FMODHandle, 'FMUSIC_GetOpenState');
  FMUSIC_FreeSong                       := GetAddress(FMODHandle, 'FMUSIC_FreeSong');
  FMUSIC_PlaySong                       := GetAddress(FMODHandle, 'FMUSIC_PlaySong');
  FMUSIC_StopSong                       := GetAddress(FMODHandle, 'FMUSIC_StopSong');
  FMUSIC_StopAllSongs                   := GetAddress(FMODHandle, 'FMUSIC_StopAllSongs');
  FMUSIC_SetZxxCallback                 := GetAddress(FMODHandle, 'FMUSIC_SetZxxCallback');
  FMUSIC_SetRowCallback                 := GetAddress(FMODHandle, 'FMUSIC_SetRowCallback');
  FMUSIC_SetOrderCallback               := GetAddress(FMODHandle, 'FMUSIC_SetOrderCallback');
  FMUSIC_SetInstCallback                := GetAddress(FMODHandle, 'FMUSIC_SetInstCallback');
  FMUSIC_SetSample                      := GetAddress(FMODHandle, 'FMUSIC_SetSample');
  FMUSIC_SetUserData                    := GetAddress(FMODHandle, 'FMUSIC_SetUserData');
  FMUSIC_OptimizeChannels               := GetAddress(FMODHandle, 'FMUSIC_OptimizeChannels');
  FMUSIC_SetReverb                      := GetAddress(FMODHandle, 'FMUSIC_SetReverb');
  FMUSIC_SetLooping                     := GetAddress(FMODHandle, 'FMUSIC_SetLooping');
  FMUSIC_SetOrder                       := GetAddress(FMODHandle, 'FMUSIC_SetOrder');
  FMUSIC_SetPaused                      := GetAddress(FMODHandle, 'FMUSIC_SetPaused');
  FMUSIC_SetMasterVolume                := GetAddress(FMODHandle, 'FMUSIC_SetMasterVolume');
  FMUSIC_SetMasterSpeed                 := GetAddress(FMODHandle, 'FMUSIC_SetMasterSpeed');
  FMUSIC_SetPanSeperation               := GetAddress(FMODHandle, 'FMUSIC_SetPanSeperation');
  FMUSIC_GetName                        := GetAddress(FMODHandle, 'FMUSIC_GetName');
  FMUSIC_GetType                        := GetAddress(FMODHandle, 'FMUSIC_GetType');
  FMUSIC_GetNumOrders                   := GetAddress(FMODHandle, 'FMUSIC_GetNumOrders');
  FMUSIC_GetNumPatterns                 := GetAddress(FMODHandle, 'FMUSIC_GetNumPatterns');
  FMUSIC_GetNumInstruments              := GetAddress(FMODHandle, 'FMUSIC_GetNumInstruments');
  FMUSIC_GetNumSamples                  := GetAddress(FMODHandle, 'FMUSIC_GetNumSamples');
  FMUSIC_GetNumChannels                 := GetAddress(FMODHandle, 'FMUSIC_GetNumChannels');
  FMUSIC_GetSample                      := GetAddress(FMODHandle, 'FMUSIC_GetSample');
  FMUSIC_GetPatternLength               := GetAddress(FMODHandle, 'FMUSIC_GetPatternLength');
  FMUSIC_IsFinished                     := GetAddress(FMODHandle, 'FMUSIC_IsFinished');
  FMUSIC_IsPlaying                      := GetAddress(FMODHandle, 'FMUSIC_IsPlaying');
  FMUSIC_GetMasterVolume                := GetAddress(FMODHandle, 'FMUSIC_GetMasterVolume');
  FMUSIC_GetGlobalVolume                := GetAddress(FMODHandle, 'FMUSIC_GetGlobalVolume');
  FMUSIC_GetOrder                       := GetAddress(FMODHandle, 'FMUSIC_GetOrder');
  FMUSIC_GetPattern                     := GetAddress(FMODHandle, 'FMUSIC_GetPattern');
  FMUSIC_GetSpeed                       := GetAddress(FMODHandle, 'FMUSIC_GetSpeed');
  FMUSIC_GetBPM                         := GetAddress(FMODHandle, 'FMUSIC_GetBPM');
  FMUSIC_GetRow                         := GetAddress(FMODHandle, 'FMUSIC_GetRow');
  FMUSIC_GetPaused                      := GetAddress(FMODHandle, 'FMUSIC_GetPaused');
  FMUSIC_GetTime                        := GetAddress(FMODHandle, 'FMUSIC_GetTime');
  FMUSIC_GetRealChannel                 := GetAddress(FMODHandle, 'FMUSIC_GetRealChannel');
  FMUSIC_GetUserData                    := GetAddress(FMODHandle, 'FMUSIC_GetUserData');

  Result := True;
end;

procedure FMOD_Unload;
begin
  { Only free the library if it was already loaded }
  if FMODHandle <> INVALID_MODULEHANDLE_VALUE then
    FreeLibrary(FMODHandle);

  FMODHandle := INVALID_MODULEHANDLE_VALUE;
end;

{$ifndef FPC}
var
  Saved8087CW: Word;
{$endif}

initialization
  FMODHandle := INVALID_MODULEHANDLE_VALUE;

{$ifndef FPC}
  { Save the current FPU state and then disable FPU exceptions }
  Saved8087CW := Default8087CW;
  Set8087CW($133f); { Disable all fpu exceptions }
{$endif}
finalization
  { Make sure the library is unloaded }
  FMOD_Unload;
{$ifndef FPC}
  { Reset the FPU to the previous state }
  Set8087CW(Saved8087CW);
{$endif}
end.
