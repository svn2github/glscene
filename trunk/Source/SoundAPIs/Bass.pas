{
  BASS 1.6a Multimedia Library
  -----------------------------
  (c) 1999-2002 Ian Luck.
  Please report bugs/suggestions/etc... to bass@un4seen.com

  Questions, suggestions, etc. regarding the Delphi API
  can be sent to magicrt@hotmail.com

  See the BASS.CHM file for more complete documentation

  NOTE: This unit will only work with BASS.DLL version 1.6
  Check http://www.un4seen.com/music/ for any later
  versions of BASS.PAS


  How to install
  ----------------
  Copy BASS.PAS to the \LIB subdirectory of your Delphi path
  e.g. to C:\Program Files\Borland\Delphi 3\Lib
  Now you should be able to run the demo projects.
}
unit Bass;

interface

uses
  Windows;

const
  // Error codes returned by BASS_GetErrorCode()
  BASS_OK                 = 0;    // all is OK
  BASS_ERROR_MEM          = 1;    // memory error
  BASS_ERROR_FILEOPEN     = 2;    // can't open the file
  BASS_ERROR_DRIVER       = 3;    // can't find a free sound driver
  BASS_ERROR_BUFLOST      = 4;    // the sample buffer was lost - please report this!
  BASS_ERROR_HANDLE       = 5;    // invalid handle
  BASS_ERROR_FORMAT       = 6;    // unsupported format
  BASS_ERROR_POSITION     = 7;    // invalid playback position
  BASS_ERROR_INIT         = 8;    // BASS_Init has not been successfully called
  BASS_ERROR_START        = 9;    // BASS_Start has not been successfully called
  BASS_ERROR_INITCD       = 10;   // can't initialize CD
  BASS_ERROR_CDINIT       = 11;   // BASS_CDInit has not been successfully called
  BASS_ERROR_NOCD         = 12;   // no CD in drive
  BASS_ERROR_CDTRACK      = 13;   // can't play the selected CD track
  BASS_ERROR_ALREADY      = 14;   // already initialized
  BASS_ERROR_CDVOL        = 15;   // CD has no volume control
  BASS_ERROR_NOPAUSE      = 16;   // not paused
  BASS_ERROR_NOTAUDIO     = 17;   // not an audio track
  BASS_ERROR_NOCHAN       = 18;   // can't get a free channel
  BASS_ERROR_ILLTYPE      = 19;   // an illegal type was specified
  BASS_ERROR_ILLPARAM     = 20;   // an illegal parameter was specified
  BASS_ERROR_NO3D         = 21;   // no 3D support
  BASS_ERROR_NOEAX        = 22;   // no EAX support
  BASS_ERROR_DEVICE       = 23;   // illegal device number
  BASS_ERROR_NOPLAY       = 24;   // not playing
  BASS_ERROR_FREQ         = 25;   // illegal sample rate
  BASS_ERROR_NOA3D        = 26;   // A3D.DLL is not installed
  BASS_ERROR_NOTFILE      = 27;   // the stream is not a file stream (WAV/MP3/MP2/MP1/OGG)
  BASS_ERROR_NOHW         = 29;   // no hardware voices available
  BASS_ERROR_EMPTY        = 31;	  // the MOD music has no sequence data
  BASS_ERROR_NONET        = 32;	  // no internet connection could be opened
  BASS_ERROR_CREATE       = 33;   // couldn't create the file
  BASS_ERROR_NOFX         = 34;   // effects are not enabled
  BASS_ERROR_PLAYING      = 35;   // the channel is playing
  BASS_ERROR_NOTAVAIL     = 37;   // requested data is not available
  BASS_ERROR_DECODE       = 38;   // the channel is a "decoding channel"
  BASS_ERROR_DX           = 39;   // a sufficient DirectX version is not installed
  BASS_ERROR_TIMEOUT      = 40;   // connection timedout
  BASS_ERROR_UNKNOWN      = -1;   // some other mystery error

  // Device setup flags
  BASS_DEVICE_8BITS       = 1;    // use 8 bit resolution, else 16 bit
  BASS_DEVICE_MONO        = 2;    // use mono, else stereo
  BASS_DEVICE_3D          = 4;    // enable 3D functionality
  {
    If the BASS_DEVICE_3D flag is not specified when
    initilizing BASS, then the 3D flags (BASS_SAMPLE_3D
    and BASS_MUSIC_3D) are ignored when loading/creating
    a sample/stream/music.
  }
  BASS_DEVICE_LEAVEVOL	  = 32;	  // leave the volume as it is
  BASS_DEVICE_NOTHREAD    = 128;  // update buffers manually (using BASS_Update)
  BASS_DEVICE_LATENCY     = 256;  // calculate device latency (BASS_INFO struct)
  BASS_DEVICE_VOL1000     = 512;  // 0-1000 volume range (else 0-100)

  // DirectSound interfaces (for use with BASS_GetDSoundObject)
  BASS_OBJECT_DS          = 1;   // IDirectSound
  BASS_OBJECT_DS3DL       = 2;   // IDirectSound3DListener

  // BASS_INFO flags (from DSOUND.H)
  DSCAPS_CONTINUOUSRATE   = $00000010;
  { supports all sample rates between min/maxrate }
  DSCAPS_EMULDRIVER       = $00000020;
  { device does NOT have hardware DirectSound support }
  DSCAPS_CERTIFIED        = $00000040;
  { device driver has been certified by Microsoft }
  {
    The following flags tell what type of samples are
    supported by HARDWARE mixing, all these formats are
    supported by SOFTWARE mixing.
  }
  DSCAPS_SECONDARYMONO    = $00000100;     // mono
  DSCAPS_SECONDARYSTEREO  = $00000200;     // stereo
  DSCAPS_SECONDARY8BIT    = $00000400;     // 8 bit
  DSCAPS_SECONDARY16BIT   = $00000800;     // 16 bit

  // BASS_RECORDINFO flags (from DSOUND.H)
  DSCCAPS_EMULDRIVER = DSCAPS_EMULDRIVER;
  { device does NOT have hardware DirectSound recording support }
  DSCCAPS_CERTIFIED = DSCAPS_CERTIFIED;
  { device driver has been certified by Microsoft }

  // defines for formats field of BASS_RECORDINFO (from MMSYSTEM.H)
  WAVE_FORMAT_1M08       = $00000001;      // 11.025 kHz, Mono,   8-bit
  WAVE_FORMAT_1S08       = $00000002;      // 11.025 kHz, Stereo, 8-bit
  WAVE_FORMAT_1M16       = $00000004;      // 11.025 kHz, Mono,   16-bit
  WAVE_FORMAT_1S16       = $00000008;      // 11.025 kHz, Stereo, 16-bit
  WAVE_FORMAT_2M08       = $00000010;      // 22.05  kHz, Mono,   8-bit
  WAVE_FORMAT_2S08       = $00000020;      // 22.05  kHz, Stereo, 8-bit
  WAVE_FORMAT_2M16       = $00000040;      // 22.05  kHz, Mono,   16-bit
  WAVE_FORMAT_2S16       = $00000080;      // 22.05  kHz, Stereo, 16-bit
  WAVE_FORMAT_4M08       = $00000100;      // 44.1   kHz, Mono,   8-bit
  WAVE_FORMAT_4S08       = $00000200;      // 44.1   kHz, Stereo, 8-bit
  WAVE_FORMAT_4M16       = $00000400;      // 44.1   kHz, Mono,   16-bit
  WAVE_FORMAT_4S16       = $00000800;      // 44.1   kHz, Stereo, 16-bit

  // Music flags
  BASS_MUSIC_RAMP         = 1;   // normal ramping
  BASS_MUSIC_RAMPS        = 2;   // sensitive ramping
  {
    Ramping doesn't take a lot of extra processing and
    improves the sound quality by removing "clicks".
    Sensitive ramping will leave sharp attacked samples,
    unlike normal ramping.
  }
  BASS_MUSIC_LOOP         = 4;   // loop music
  BASS_MUSIC_FT2MOD       = 16;  // play .MOD as FastTracker 2 does
  BASS_MUSIC_PT1MOD       = 32;  // play .MOD as ProTracker 1 does
  BASS_MUSIC_MONO         = 64;  // force mono mixing (less CPU usage)
  BASS_MUSIC_3D           = 128; // enable 3D functionality
  BASS_MUSIC_POSRESET     = 256; // stop all notes when moving position
  BASS_MUSIC_SURROUND	  = 512; // surround sound
  BASS_MUSIC_SURROUND2	  = 1024;// surround sound (mode 2)
  BASS_MUSIC_STOPBACK	  = 2048;// stop the music on a backwards jump effect
  BASS_MUSIC_FX           = 4096;// enable DX8 effects
  BASS_MUSIC_CALCLEN      = 8192;// calculate playback length
  BASS_MUSIC_DECODE       = $200000;// don't play the music, only decode (BASS_ChannelGetData)
  BASS_MUSIC_NOSAMPLE     = $400000;// don't load the samples

  // Sample info flags
  BASS_SAMPLE_8BITS       = 1;   // 8 bit, else 16 bit
  BASS_SAMPLE_MONO        = 2;   // mono, else stereo
  BASS_SAMPLE_LOOP        = 4;   // looped
  BASS_SAMPLE_3D          = 8;   // 3D functionality enabled
  BASS_SAMPLE_SOFTWARE    = 16;  // it's NOT using hardware mixing
  BASS_SAMPLE_MUTEMAX     = 32;  // muted at max distance (3D only)
  BASS_SAMPLE_VAM         = 64;  // uses the DX7 voice allocation & management
  BASS_SAMPLE_FX          = 128;     // the DX8 effects are enabled
  BASS_SAMPLE_OVER_VOL    = $10000; // override lowest volume
  BASS_SAMPLE_OVER_POS    = $20000; // override longest playing
  BASS_SAMPLE_OVER_DIST   = $30000; // override furthest from listener (3D only)

  BASS_MP3_HALFRATE       = $10000; // reduced quality MP3/MP2/MP1 (half sample rate)
  BASS_MP3_SETPOS         = $20000; // enable pin-point seeking on the MP3/MP2/MP1/OGG

  BASS_STREAM_AUTOFREE	  = $40000;// automatically free the stream when it stop/ends
  BASS_STREAM_RESTRATE	  = $80000;// restrict the download rate of internet file streams
  BASS_STREAM_BLOCK       = $100000;// download & play internet
                                    // file stream (MPx/OGG) in small blocks
  BASS_STREAM_DECODE      = $200000;// don't play the stream, only decode (BASS_ChannelGetData)
  BASS_STREAM_META        = $400000;// request metadata from a Shoutcast stream

  // DX7 voice allocation flags
  BASS_VAM_HARDWARE       = 1;
  {
    Play the sample in hardware. If no hardware voices are available then
    the "play" call will fail
  }
  BASS_VAM_SOFTWARE       = 2;
  {
    Play the sample in software (ie. non-accelerated). No other VAM flags
    may be used together with this flag.
  }

  // DX7 voice management flags
  {
    These flags enable hardware resource stealing... if the hardware has no
    available voices, a currently playing buffer will be stopped to make room
    for the new buffer. NOTE: only samples loaded/created with the
    BASS_SAMPLE_VAM flag are considered for termination by the DX7 voice
    management.
  }
  BASS_VAM_TERM_TIME      = 4;
  {
    If there are no free hardware voices, the buffer to be terminated will be
    the one with the least time left to play.
  }
  BASS_VAM_TERM_DIST      = 8;
  {
    If there are no free hardware voices, the buffer to be terminated will be
    one that was loaded/created with the BASS_SAMPLE_MUTEMAX flag and is
    beyond
    it's max distance. If there are no buffers that match this criteria, then
    the "play" call will fail.
  }
  BASS_VAM_TERM_PRIO      = 16;
  {
    If there are no free hardware voices, the buffer to be terminated will be
    the one with the lowest priority.
  }

  // 3D channel modes
  BASS_3DMODE_NORMAL      = 0;
  { normal 3D processing }
  BASS_3DMODE_RELATIVE    = 1;
  {
    The channel's 3D position (position/velocity/
    orientation) are relative to the listener. When the
    listener's position/velocity/orientation is changed
    with BASS_Set3DPosition, the channel's position
    relative to the listener does not change.
  }
  BASS_3DMODE_OFF         = 2;
  {
    Turn off 3D processing on the channel, the sound will
    be played in the center.
  }

  // EAX environments, use with BASS_SetEAXParameters
  EAX_ENVIRONMENT_OFF               = -1;
  EAX_ENVIRONMENT_GENERIC           = 0;
  EAX_ENVIRONMENT_PADDEDCELL        = 1;
  EAX_ENVIRONMENT_ROOM              = 2;
  EAX_ENVIRONMENT_BATHROOM          = 3;
  EAX_ENVIRONMENT_LIVINGROOM        = 4;
  EAX_ENVIRONMENT_STONEROOM         = 5;
  EAX_ENVIRONMENT_AUDITORIUM        = 6;
  EAX_ENVIRONMENT_CONCERTHALL       = 7;
  EAX_ENVIRONMENT_CAVE              = 8;
  EAX_ENVIRONMENT_ARENA             = 9;
  EAX_ENVIRONMENT_HANGAR            = 10;
  EAX_ENVIRONMENT_CARPETEDHALLWAY   = 11;
  EAX_ENVIRONMENT_HALLWAY           = 12;
  EAX_ENVIRONMENT_STONECORRIDOR     = 13;
  EAX_ENVIRONMENT_ALLEY             = 14;
  EAX_ENVIRONMENT_FOREST            = 15;
  EAX_ENVIRONMENT_CITY              = 16;
  EAX_ENVIRONMENT_MOUNTAINS         = 17;
  EAX_ENVIRONMENT_QUARRY            = 18;
  EAX_ENVIRONMENT_PLAIN             = 19;
  EAX_ENVIRONMENT_PARKINGLOT        = 20;
  EAX_ENVIRONMENT_SEWERPIPE         = 21;
  EAX_ENVIRONMENT_UNDERWATER        = 22;
  EAX_ENVIRONMENT_DRUGGED           = 23;
  EAX_ENVIRONMENT_DIZZY             = 24;
  EAX_ENVIRONMENT_PSYCHOTIC         = 25;
  // total number of environments
  EAX_ENVIRONMENT_COUNT             = 26;

  // software 3D mixing algorithm modes (used with BASS_Set3DAlgorithm)
  BASS_3DALG_DEFAULT                = 0;
  {
    default algorithm (currently translates to BASS_3DALG_OFF)
  }
  BASS_3DALG_OFF                    = 1;
  {
    Uses normal left and right panning. The vertical axis is ignored except
    for scaling of volume due to distance. Doppler shift and volume scaling
    are still applied, but the 3D filtering is not performed. This is the
    most CPU efficient software implementation, but provides no virtual 3D
    audio effect. Head Related Transfer Function processing will not be done.
    Since only normal stereo panning is used, a channel using this algorithm
    may be accelerated by a 2D hardware voice if no free 3D hardware voices
    are available.
  }
  BASS_3DALG_FULL                   = 2;
  {
    This algorithm gives the highest quality 3D audio effect, but uses more
    CPU. Requires Windows 98 2nd Edition or Windows 2000 that uses WDM
    drivers, if this mode is not available then BASS_3DALG_OFF will be used
    instead.
  }
  BASS_3DALG_LIGHT                  = 3;
  {
    This algorithm gives a good 3D audio effect, and uses less CPU than the
    FULL mode. Requires Windows 98 2nd Edition or Windows 2000 that uses WDM
    drivers, if this mode is not available then BASS_3DALG_OFF will be used
    instead.
  }

  {
    Sync types (with BASS_ChannelSetSync() "param" and
    SYNCPROC "data" definitions) & flags.
  }
  BASS_SYNC_POS                     = 0;
  BASS_SYNC_MUSICPOS                = 0;
  {
    Sync when a music or stream reaches a position.
    if HMUSIC...
    param: LOWORD=order (0=first, -1=all) HIWORD=row (0=first, -1=all)
    data : LOWORD=order HIWORD=row
    if HSTREAM...
    param: position in bytes
    data : not used
  }
  BASS_SYNC_MUSICINST               = 1;
  {
    Sync when an instrument (sample for the non-instrument
    based formats) is played in a music (not including
    retrigs).
    param: LOWORD=instrument (1=first) HIWORD=note (0=c0...119=b9, -1=all)
    data : LOWORD=note HIWORD=volume (0-64)
  }
  BASS_SYNC_END                     = 2;
  {
    Sync when a music or file stream reaches the end.
    param: not used
    data : not used
  }
  BASS_SYNC_MUSICFX                 = 3;
  {
    Sync when the "sync" effect (XM/MTM/MOD: E8x/Wxx, IT/S3M: S2x) is used.
    param: 0:data=pos, 1:data="x" value
    data : param=0: LOWORD=order HIWORD=row, param=1: "x" value
  }
  BASS_SYNC_META                    = 4;
  {
    Sync when metadata is received in a Shoutcast stream.
    param: not used
    data : pointer to the metadata
  }
  BASS_SYNC_MESSAGE                 = $20000000;
  { FLAG: post a Windows message (instead of callback)
    When using a window message "callback", the message to post is given in the "proc"
    parameter of BASS_ChannelSetSync, and is posted to the window specified in the BASS_Init
    call. The message parameters are: WPARAM = data, LPARAM = user.
  }
  BASS_SYNC_MIXTIME                 = $40000000;
  { FLAG: sync at mixtime, else at playtime }
  BASS_SYNC_ONETIME                 = $80000000;
  { FLAG: sync only once, else continuously }

  CDCHANNEL           = 0; // CD channel, for use with BASS_Channel functions
  RECORDCHAN          = 1; // Recording channel, for use with BASS_Channel functions

  // BASS_ChannelIsActive return values
  BASS_ACTIVE_STOPPED = 0;
  BASS_ACTIVE_PLAYING = 1;
  BASS_ACTIVE_STALLED = 2;
  BASS_ACTIVE_PAUSED  = 3;

  // CD ID flags, use with BASS_CDGetID
  BASS_CDID_IDENTITY  = 0;
  BASS_CDID_UPC       = 1;
  BASS_CDID_CDDB      = 2;
  BASS_CDID_CDDB2     = 3;

  // BASS_ChannelGetData flags
  BASS_DATA_FFT512   = $80000000; // 512 sample FFT
  BASS_DATA_FFT1024  = $80000001; // 1024 FFT
  BASS_DATA_FFT2048  = $80000002; // 2048 FFT
  BASS_DATA_FFT512S  = $80000010; // stereo 512 sample FFT
  BASS_DATA_FFT1024S = $80000011; // stereo 1024 FFT
  BASS_DATA_FFT2048S = $80000012; // stereo 2048 FFT

  // BASS_StreamGetTags flags : what's returned
  BASS_TAG_ID3   = 0; // ID3v1 tags : 128 byte block
  BASS_TAG_ID3V2 = 1; // ID3v2 tags : variable length block
  BASS_TAG_OGG   = 2; // OGG comments : array of null-terminated strings
  BASS_TAG_HTTP  = 3; // HTTP headers : array of null-terminated strings
  BASS_TAG_ICY   = 4; // ICY headers : array of null-terminated strings
  BASS_TAG_META  = 5; // ICY metadata : null-terminated string

  BASS_FX_CHORUS      = 0;      // GUID_DSFX_STANDARD_CHORUS
  BASS_FX_COMPRESSOR  = 1;      // GUID_DSFX_STANDARD_COMPRESSOR
  BASS_FX_DISTORTION  = 2;      // GUID_DSFX_STANDARD_DISTORTION
  BASS_FX_ECHO        = 3;      // GUID_DSFX_STANDARD_ECHO
  BASS_FX_FLANGER     = 4;      // GUID_DSFX_STANDARD_FLANGER
  BASS_FX_GARGLE      = 5;      // GUID_DSFX_STANDARD_GARGLE
  BASS_FX_I3DL2REVERB = 6;      // GUID_DSFX_STANDARD_I3DL2REVERB
  BASS_FX_PARAMEQ     = 7;      // GUID_DSFX_STANDARD_PARAMEQ
  BASS_FX_REVERB      = 8;      // GUID_DSFX_WAVES_REVERB

  BASS_FX_PHASE_NEG_180 = 0;
  BASS_FX_PHASE_NEG_90  = 1;
  BASS_FX_PHASE_ZERO    = 2;
  BASS_FX_PHASE_90      = 3;
  BASS_FX_PHASE_180     = 4;

  // BASS_RecordSetInput flags
  BASS_INPUT_OFF    = $10000;
  BASS_INPUT_ON     = $20000;
  BASS_INPUT_LEVEL  = $40000;

type
  DWORD = cardinal;
  BOOL = LongBool;
  FLOAT = Single;
  QWORD = int64;

  HMUSIC = DWORD;       // MOD music handle
  HSAMPLE = DWORD;      // sample handle
  HCHANNEL = DWORD;     // playing sample's channel handle
  HSTREAM = DWORD;      // sample stream handle
  HSYNC = DWORD;        // synchronizer handle
  HDSP = DWORD;         // DSP handle
  HFX = DWORD;          // DX8 effect handle

  BASS_INFO = record
    size: DWORD;        // size of this struct (set this before calling the function)
    flags: DWORD;       // device capabilities (DSCAPS_xxx flags)
    {
      The following values are irrelevant if the device
      doesn't have hardware support
      (DSCAPS_EMULDRIVER is specified in flags)
    }
    hwsize: DWORD;      // size of total device hardware memory
    hwfree: DWORD;      // size of free device hardware memory
    freesam: DWORD;     // number of free sample slots in the hardware
    free3d: DWORD;      // number of free 3D sample slots in the hardware
    minrate: DWORD;     // min sample rate supported by the hardware
    maxrate: DWORD;     // max sample rate supported by the hardware
    eax: BOOL;          // device supports EAX? (always FALSE if BASS_DEVICE_3D was not used)
    a3d: DWORD;         // unused
    dsver: DWORD;       // DirectSound version (use to check for DX5/7 functions)
    latency: DWORD;     // delay (in ms) before start of playback (requires BASS_DEVICE_LATENCY)
  end;

  BASS_RECORDINFO = record
    size: DWORD;        // size of this struct (set this before calling the function)
    flags: DWORD;       // device capabilities (DSCCAPS_xxx flags)
    formats: DWORD;     // supported standard formats (WAVE_FORMAT_xxx flags)
    inputs: DWORD;      // number of inputs
    singlein: BOOL;     // only 1 input can be set at a time
  end;

  // Sample info structure
  BASS_SAMPLE = record
    freq: DWORD;        // default playback rate
    volume: DWORD;      // default volume (0-100)
    pan: Integer;       // default pan (-100=left, 0=middle, 100=right)
    flags: DWORD;       // BASS_SAMPLE_xxx flags
    length: DWORD;      // length (in samples, not bytes)
    max: DWORD;         // maximum simultaneous playbacks
    {
      The following are the sample's default 3D attributes
      (if the sample is 3D, BASS_SAMPLE_3D is in flags)
      see BASS_ChannelSet3DAttributes
    }
    mode3d: DWORD;      // BASS_3DMODE_xxx mode
    mindist: FLOAT;     // minimum distance
    maxdist: FLOAT;     // maximum distance
    iangle: DWORD;      // angle of inside projection cone
    oangle: DWORD;      // angle of outside projection cone
    outvol: DWORD;      // delta-volume outside the projection cone
    {
      The following are the defaults used if the sample uses the DirectX 7
      voice allocation/management features.
    }
    vam: DWORD;         // voice allocation/management flags (BASS_VAM_xxx)
    priority: DWORD;    // priority (0=lowest, $ffffffff=highest)
  end;

  // 3D vector (for 3D positions/velocities/orientations)
  BASS_3DVECTOR = record
    x: FLOAT;           // +=right, -=left
    y: FLOAT;           // +=up, -=down
    z: FLOAT;           // +=front, -=behind
  end;

  BASS_FXCHORUS = record
    fWetDryMix: FLOAT;
    fDepth: FLOAT;
    fFeedback: FLOAT;
    fFrequency: FLOAT;
    lWaveform: DWORD;   // 0=triangle, 1=sine
    fDelay: FLOAT;
    lPhase: DWORD;      // BASS_FX_PHASE_xxx
  end;

  BASS_FXCOMPRESSOR = record
    fGain: FLOAT;
    fAttack: FLOAT;
    fRelease: FLOAT;
    fThreshold: FLOAT;
    fRatio: FLOAT;
    fPredelay: FLOAT;
  end;

  BASS_FXDISTORTION = record
    fGain: FLOAT;
    fEdge: FLOAT;
    fPostEQCenterFrequency: FLOAT;
    fPostEQBandwidth: FLOAT;
    fPreLowpassCutoff: FLOAT;
  end;

  BASS_FXECHO = record
    fWetDryMix: FLOAT;
    fFeedback: FLOAT;
    fLeftDelay: FLOAT;
    fRightDelay: FLOAT;
    lPanDelay: BOOL;
  end;

  BASS_FXFLANGER = record
    fWetDryMix: FLOAT;
    fDepth: FLOAT;
    fFeedback: FLOAT;
    fFrequency: FLOAT;
    lWaveform: DWORD;   // 0=triangle, 1=sine
    fDelay: FLOAT;
    lPhase: DWORD;      // BASS_FX_PHASE_xxx
  end;

  BASS_FXGARGLE = record
    dwRateHz: DWORD;               // Rate of modulation in hz
    dwWaveShape: DWORD;            // 0=triangle, 1=square
  end;

  BASS_FXI3DL2REVERB = record
    lRoom: Longint;                // [-10000, 0]      default: -1000 mB
    lRoomHF: Longint;              // [-10000, 0]      default: 0 mB
    flRoomRolloffFactor: FLOAT;    // [0.0, 10.0]      default: 0.0
    flDecayTime: FLOAT;            // [0.1, 20.0]      default: 1.49s
    flDecayHFRatio: FLOAT;         // [0.1, 2.0]       default: 0.83
    lReflections: Longint;         // [-10000, 1000]   default: -2602 mB
    flReflectionsDelay: FLOAT;     // [0.0, 0.3]       default: 0.007 s
    lReverb: Longint;              // [-10000, 2000]   default: 200 mB
    flReverbDelay: FLOAT;          // [0.0, 0.1]       default: 0.011 s
    flDiffusion: FLOAT;            // [0.0, 100.0]     default: 100.0 %
    flDensity: FLOAT;              // [0.0, 100.0]     default: 100.0 %
    flHFReference: FLOAT;          // [20.0, 20000.0]  default: 5000.0 Hz
  end;

  BASS_FXPARAMEQ = record
    fCenter: FLOAT;
    fBandwidth: FLOAT;
    fGain: FLOAT;
  end;

  BASS_FXREVERB = record
    fInGain: FLOAT;                // [-96.0,0.0]            default: 0.0 dB
    fReverbMix: FLOAT;             // [-96.0,0.0]            default: 0.0 db
    fReverbTime: FLOAT;            // [0.001,3000.0]         default: 1000.0 ms
    fHighFreqRTRatio: FLOAT;       // [0.001,0.999]          default: 0.001
  end;

  // callback function types
  STREAMPROC = function(handle: HSTREAM; buffer: Pointer; length: DWORD; user: DWORD): DWORD; stdcall;
  {
    Stream callback function. NOTE: A stream function should obviously be as
    quick as possible, other streams (and MOD musics) can't be mixed until
    it's finished.
    handle : The stream that needs writing
    buffer : Buffer to write the samples in
    length : Number of bytes to write
    user   : The 'user' parameter value given when calling BASS_StreamCreate
    RETURN : Number of bytes written. If less than "length" then the
             stream is assumed to be at the end, and is stopped.
  }

  SYNCPROC = procedure(handle: HSYNC; channel, data: DWORD; user: DWORD); stdcall;
  {
    Sync callback function. NOTE: a sync callback function should be very
    quick as other syncs cannot be processed until it has finished. If the
    sync is a "mixtime" sync, then other streams and MOD musics can not be
    mixed until it's finished either.
    handle : The sync that has occured
    channel: Channel that the sync occured in
    data   : Additional data associated with the sync's occurance
    user   : The 'user' parameter given when calling BASS_ChannelSetSync
  }

  DSPPROC = procedure(handle: HDSP; channel: DWORD; buffer: Pointer; length: DWORD; user: DWORD); stdcall;
  {
    DSP callback function. NOTE: A DSP function should obviously be as quick
    as possible... other DSP functions, streams and MOD musics can not be
    processed until it's finished.
    handle : The DSP handle
    channel: Channel that the DSP is being applied to
    buffer : Buffer to apply the DSP to
    length : Number of bytes in the buffer
    user   : The 'user' parameter given when calling BASS_ChannelSetDSP
  }

  RECORDPROC = function(buffer: Pointer; length: DWORD; user: DWORD): BOOL; stdcall;
  {
    Recording callback function.
    buffer : Buffer containing the recorded sample data
    length : Number of bytes
    user   : The 'user' parameter value given when calling BASS_RecordStart
    RETURN : TRUE = continue recording, FALSE = stop
  }


// Functions
function BASS_GetVersion: DWORD; stdcall; external 'bass.dll' name 'BASS_GetVersion';
{
  Retrieve the version number of BASS that is loaded.
  RETURN : The BASS version (LOWORD.HIWORD)
}
function BASS_GetDeviceDescription(devnum: DWORD): PChar; stdcall; external 'bass.dll' name 'BASS_GetDeviceDescription';
{
  Get the text description of a device. This function can
  be used to enumerate the available devices.
  devnum : The device (0=first)
  RETURN : The text description of the device (NULL=error)
}
function BASS_SetBufferLength(length: FLOAT): FLOAT; stdcall; external 'bass.dll' name 'BASS_SetBufferLength';
{
  Set the amount that BASS mixes ahead new musics/streams.
  Changing this setting does not affect musics/streams
  that have already been loaded/created. Increasing the
  buffer length, decreases the chance of the sound
  possibly breaking-up on slower computers, but also
  requires more memory. The default length is 0.5 secs.
  length : The buffer length in seconds
  RETURN : The actual new buffer length
}
procedure BASS_SetGlobalVolumes(musvol, samvol, strvol: Integer); stdcall; external 'bass.dll' name 'BASS_SetGlobalVolumes';
{
  Set the global music/sample/stream volume levels.
  musvol : MOD music global volume level (0-100, -1=leave current)
  samvol : Sample global volume level (0-100, -1=leave current)
  strvol : Stream global volume level (0-100, -1=leave current)
}
procedure BASS_GetGlobalVolumes(var musvol, samvol, strvol: DWORD); stdcall; external 'bass.dll' name 'BASS_GetGlobalVolumes';
{
  Retrive the global music/sample/stream volume levels.
  musvol : MOD music global volume level (NULL=don't retrieve it)
  samvol : Sample global volume level (NULL=don't retrieve it)
  strvol : Stream global volume level (NULL=don't retrieve it)
}
procedure BASS_SetLogCurves(volume, pan: BOOL); stdcall; external 'bass.dll' name 'BASS_SetLogCurves';
{
  Make the volume/panning values translate to a logarithmic curve,
  or a linear "curve" (the default).
  volume : volume curve (FALSE=linear, TRUE=log)
  pan    : panning curve (FALSE=linear, TRUE=log)
}
procedure BASS_Set3DAlgorithm(algo: DWORD); stdcall; external 'bass.dll' name 'BASS_Set3DAlgorithm';
{
  Set the 3D algorithm for software mixed 3D channels (does not affect
  hardware mixed channels). Changing the mode only affects subsequently
  created or loaded samples/streams/musics, not those that already exist.
  Requires DirectX 7 or above.
  algo   : algorithm flag (BASS_3DALG_xxx)
}
function BASS_ErrorGetCode: DWORD; stdcall; external 'bass.dll' name 'BASS_ErrorGetCode';
{
  Get the BASS_ERROR_xxx error code. Use this function to
  get the reason for an error.
}
procedure BASS_SetCLSID(clsid: TGUID); stdcall; external 'bass.dll' name 'BASS_SetCLSID';
{
  Set the class identifier of the object to create, that will be used
  to initialize DirectSound.
  clsid  : Class identifier of the object to create (NULL=use default)
}
function BASS_Init(device: Integer; freq, flags: DWORD; win: HWND): BOOL; stdcall; external 'bass.dll' name 'BASS_Init';
{
  Initialize the digital output. This must be called
  before all following BASS functions (except CD functions).
  The volume is initially set to 100 (the maximum),
  use BASS_SetVolume() to adjust it.
  device : Device to use (0=first, -1=default, -2=no sound)
  freq   : Output sample rate
  flags  : BASS_DEVICE_xxx flags (optional HIWORD=update period)
  win    : Owner window (0=current foreground window)

  NOTE: The "no sound" device (device=-2), allows loading
  and "playing" of MOD musics only (all sample/stream
  functions and most other functions fail). This is so
  that you can still use the MOD musics as synchronizers
  when there is no soundcard present. When using device -2,
  you should still set the other arguments as you would do
  normally.
}
procedure BASS_Free; stdcall; external 'bass.dll' name 'BASS_Free';
{
  Free all resources used by the digital output, including
  all musics and samples.
}
function BASS_GetDSoundObject(obj: DWORD): Pointer; stdcall; external 'bass.dll' name 'BASS_GetDSoundObject';
{
  Retrieve a pointer to a DirectSound interface. This can be used by
  advanced users to "plugin" external functionality.
  object : The interface to retrieve (BASS_OBJECT_xxx)
  RETURN : A pointer to the requested interface (NULL=error)
}
procedure BASS_GetInfo(var info: BASS_INFO); stdcall; external 'bass.dll' name 'BASS_GetInfo';
{
  Retrieve some information on the device being used.
  info   : Pointer to store info at
}
function BASS_Update: BOOL; stdcall; external 'bass.dll' name 'BASS_Update';
{
  Update the HMUSIC/HSTREAM channel buffers.
}
function BASS_GetCPU: FLOAT; stdcall; external 'bass.dll' name 'BASS_GetCPU';
{
  Get the current CPU usage of BASS. This includes the
  time taken to mix the MOD musics and sample streams.
  It does not include plain sample mixing which is done
  by the output device (hardware accelerated) or
  DirectSound (emulated). Audio CD playback requires no
  CPU usage.
  RETURN : The CPU usage percentage
}
function BASS_Start: BOOL; stdcall; external 'bass.dll' name 'BASS_Start';
{
  Start the digital output.
}
function BASS_Stop: BOOL; stdcall; external 'bass.dll' name 'BASS_Stop';
{
  Stop the digital output, stopping all musics/samples/
  streams.
}
function BASS_Pause: BOOL; stdcall; external 'bass.dll' name 'BASS_Pause';
{
  Stop the digital output, pausing all musics/samples/
  streams. Use BASS_Start to resume the digital output.
}
function BASS_SetVolume(volume: DWORD): BOOL; stdcall; external 'bass.dll' name 'BASS_SetVolume';
{
  Set the digital output master volume.
  volume : Desired volume level (0-100)
}
function BASS_GetVolume: Integer; stdcall; external 'bass.dll' name 'BASS_GetVolume';
{
  Get the digital output master volume.
  RETURN : The volume level (0-100, -1=error)
}
function BASS_Set3DFactors(distf, rollf, doppf: FLOAT): BOOL; stdcall; external 'bass.dll' name 'BASS_Set3DFactors';
{
  Set the factors that affect the calculations of 3D sound.
  distf  : Distance factor (0.0-10.0, 1.0=use meters, 0.3=use feet, <0.0=leave current)
           By default BASS measures distances in meters, you can change this
           setting if you are using a different unit of measurement.
  roolf  : Rolloff factor, how fast the sound quietens with distance
           (0.0=no rolloff, 1.0=real world, 2.0=2x real... 10.0=max, <0.0=leave current)
  doppf  : Doppler factor (0.0=no doppler, 1.0=real world, 2.0=2x real... 10.0=max, <0.0=leave current)
           The doppler effect is the way a sound appears to change frequency when it is
           moving towards or away from you. The listener and sound velocity settings are
           used to calculate this effect, this "doppf" value can be used to lessen or
           exaggerate the effect.
}
function BASS_Get3DFactors(var distf, rollf, doppf: FLOAT): BOOL; stdcall; external 'bass.dll' name 'BASS_Get3DFactors';
{
  Get the factors that affect the calculations of 3D sound.
  distf  : Distance factor (NULL=don't get it)
  roolf  : Rolloff factor (NULL=don't get it)
  doppf  : Doppler factor (NULL=don't get it)
}
function BASS_Set3DPosition(var pos, vel, front, top: BASS_3DVECTOR): BOOL; stdcall; external 'bass.dll' name 'BASS_Set3DPosition';
{
  Set the position/velocity/orientation of the listener (ie. the player/viewer).
  pos    : Position of the listener (NULL=leave current)
  vel    : Listener's velocity, used to calculate doppler effect (NULL=leave current)
  front  : Direction that listener's front is pointing (NULL=leave current)
  top    : Direction that listener's top is pointing (NULL=leave current)
         NOTE: front & top must both be set in a single call
}
function BASS_Get3DPosition(var pos, vel, front, top: BASS_3DVECTOR): BOOL; stdcall; external 'bass.dll' name 'BASS_Get3DPosition';
{
  Get the position/velocity/orientation of the listener.
  pos    : Position of the listener (NULL=don't get it)
  vel    : Listener's velocity (NULL=don't get it)
  front  : Direction that listener's front is pointing (NULL=don't get it)
  top    : Direction that listener's top is pointing (NULL=don't get it)
         NOTE: front & top must both be retrieved in a single call
}
function BASS_Apply3D: BOOL; stdcall; external 'bass.dll' name 'BASS_Apply3D';
{
  Apply changes made to the 3D system. This must be called to apply any changes
  made with BASS_Set3DFactors, BASS_Set3DPosition, BASS_ChannelSet3DAttributes or
  BASS_ChannelSet3DPosition. It improves performance to have DirectSound do all the
  required recalculating at the same time like this, rather than recalculating after
  every little change is made. NOTE: This is automatically called when starting a 3D
  sample with BASS_SamplePlay3D/Ex.
}
function BASS_SetEAXParameters(env: Integer; vol, decay, damp: FLOAT): BOOL; stdcall; external 'bass.dll' name 'BASS_SetEAXParameters';
{
  Set the type of EAX environment and it's parameters. Obviously, EAX functions
  have no effect if no EAX supporting device (ie. SB Live) is used.
  env    : Reverb environment (EAX_ENVIRONMENT_xxx, -1=leave current)
  vol    : Volume of the reverb (0.0=off, 1.0=max, <0.0=leave current)
  decay  : Time in seconds it takes the reverb to diminish by 60dB (0.1-20.0, <0.0=leave current)
  damp   : The damping, high or low frequencies decay faster (0.0=high decays quickest,
           1.0=low/high decay equally, 2.0=low decays quickest, <0.0=leave current)
}
function BASS_GetEAXParameters(var env: DWORD; var vol, decay, damp: FLOAT): BOOL; stdcall; external 'bass.dll' name 'BASS_GetEAXParameters';
{
  Get the current EAX parameters.
  env    : Reverb environment (NULL=don't get it)
  vol    : Reverb volume (NULL=don't get it)
  decay  : Decay duration (NULL=don't get it)
  damp   : The damping (NULL=don't get it)
}
function BASS_MusicLoad(mem: BOOL; f: Pointer; offset, length, flags: DWORD): HMUSIC; stdcall; external 'bass.dll' name 'BASS_MusicLoad';
{
  Load a music (XM/MOD/S3M/IT/MTM). The amplification and pan
  seperation are initially set to 50, use BASS_MusicSetAmplify()
  and BASS_MusicSetPanSep() to adjust them.
  mem    : TRUE = Load music from memory
  f      : Filename (mem=FALSE) or memory location (mem=TRUE)
  offset : File offset to load the music from (only used if mem=FALSE)
  length : Data length (only used if mem=FALSE, 0=use to end of file)
  flags  : BASS_MUSIC_xxx flags
  RETURN : The loaded music's handle (NULL=error)
}
procedure BASS_MusicFree(handle: HMUSIC); stdcall; external 'bass.dll' name 'BASS_MusicFree';
{
  Free a music's resources.
  handle : Music handle
}
function BASS_MusicGetName(handle: HMUSIC): PChar; stdcall; external 'bass.dll' name 'BASS_MusicGetName';
{
  Retrieves a music's name.
  handle : Music handle
  RETURN : The music's name (NULL=error)
}
function BASS_MusicGetLength(handle: HMUSIC; playlen: BOOL): DWORD; stdcall; external 'bass.dll' name 'BASS_MusicGetLength';
{
  Retrieves the length of a music in patterns (how many "orders" there are)
  or in output bytes (requires BASS_MUSIC_CALCLEN was used with BASS_MusicLoad).
  handle : Music handle
  playlen: TRUE=get the playback length, FALSE=get the pattern length
  RETURN : The length of the music (-1=error)
}
function BASS_MusicPreBuf(handle: HMUSIC): BOOL; stdcall; external 'bass.dll' name 'BASS_MusicPreBuf';
{
  Pre-buffer initial sample data ready for playback.
  handle : Handle of music
}
function BASS_MusicPlay(handle: HMUSIC): BOOL; stdcall; external 'bass.dll' name 'BASS_MusicPlay';
{
  Play a music. Playback continues from where it was last stopped/paused.
  Multiple musics may be played simultaneously.
  handle : Handle of music to play
}
function BASS_MusicPlayEx(handle: HMUSIC; pos: DWORD; flags: Integer; reset: BOOL): BOOL; stdcall; external 'bass.dll' name 'BASS_MusicPlayEx';
{
  Play a music, specifying start position and playback flags.
  handle : Handle of music to play
  pos    : Position to start playback from, LOWORD=order HIWORD=row
  flags  : BASS_MUSIC_xxx flags. These flags overwrite the defaults
           specified when the music was loaded. (-1=use current flags)
  reset  : TRUE = Stop all current playing notes and reset bpm/etc...
}
function BASS_MusicSetAmplify(handle: HMUSIC; amp: DWORD): BOOL; stdcall; external 'bass.dll' name 'BASS_MusicSetAmplify';
{
  Set a music's amplification level.
  handle : Music handle
  amp    : Amplification level (0-100)
}
function BASS_MusicSetPanSep(handle: HMUSIC; pan: DWORD): BOOL; stdcall; external 'bass.dll' name 'BASS_MusicSetPanSep';
{
  Set a music's pan seperation.
  handle : Music handle
  pan    : Pan seperation (0-100, 50=linear)
}
function BASS_MusicSetPositionScaler(handle: HMUSIC; scale: DWORD): BOOL; stdcall; external 'bass.dll' name 'BASS_MusicSetPositionScaler';
{
  Set a music's "GetPosition" scaler
  When you call BASS_ChannelGetPosition, the "row" (HIWORD) will be
  scaled by this value. By using a higher scaler, you can get a more
  precise position indication.
  handle : Music handle
  scale  : The scaler (1-256)
}
function BASS_MusicSetChannelVol(handle: HMUSIC; channel,volume: DWORD): BOOL; stdcall; external 'bass.dll' name 'BASS_MusicSetChannelVol';
{
  Set the volume level of a channel in a music
  handle : Music handle
  channel: Channel number (0=first)
  volume : Volume level (0-100)
}
function BASS_MusicGetChannelVol(handle: HMUSIC; channel: DWORD): Integer; stdcall; external 'bass.dll' name 'BASS_MusicGetChannelVol';
{
  Get the volume level of a channel in a music
  handle : Music handle
  channel: Channel number (0=first)
  RETURN : The channel's volume (-1=error)
}
function BASS_SampleLoad(mem: BOOL; f: Pointer; offset, length, max, flags: DWORD): HSAMPLE; stdcall; external 'bass.dll' name 'BASS_SampleLoad';
{
  Load a WAV/MP3/MP2/MP1 sample. If you're loading a sample with 3D
  functionality, then you should use BASS_GetInfo and BASS_SetInfo to set
  the default 3D parameters. You can also use these two functions to set
  the sample's default frequency/volume/pan/looping.
  mem    : TRUE = Load sample from memory
  f      : Filename (mem=FALSE) or memory location (mem=TRUE)
  offset : File offset to load the sample from (only used if mem=FALSE)
  length : Data length (only used if mem=FALSE, 0=use to end of file)
  max    : Maximum number of simultaneous playbacks (1-65535)
  flags  : BASS_SAMPLE_xxx flags (only the LOOP/3D/SOFTWARE/OVER_xxx flags are used)
  RETURN : The loaded sample's handle (NULL=error)
}
function BASS_SampleCreate(length, freq, max, flags: DWORD): Pointer; stdcall; external 'bass.dll' name 'BASS_SampleCreate';
{
  Create a sample. This function allows you to generate custom samples, or
  load samples that are not in the WAV format. A pointer is returned to the
  memory location at which you should write the sample's data. After writing
  the data, call BASS_SampleCreateDone to get the new sample's handle.
  length : The sample's length (in samples, NOT bytes)
  freq   : default sample rate
  max    : Maximum number of simultaneous playbacks (1-65535)
  flags  : BASS_SAMPLE_xxx flags
  RETURN : Memory location to write the sample's data (NULL=error)
}
function BASS_SampleCreateDone: HSAMPLE; stdcall; external 'bass.dll' name 'BASS_SampleCreateDone';
{
  Finished creating a new sample.
  RETURN : The new sample's handle (NULL=error)
}
procedure BASS_SampleFree(handle: HSAMPLE); stdcall; external 'bass.dll' name 'BASS_SampleFree';
{
  Free a sample's resources.
  handle : Sample handle
}
function BASS_SampleGetInfo(handle: HSAMPLE; var info: BASS_SAMPLE): BOOL; stdcall; external 'bass.dll' name 'BASS_SampleGetInfo';
{
  Retrieve a sample's current default attributes.
  handle : Sample handle
  info   : Pointer to store sample info
}
function BASS_SampleSetInfo(handle: HSAMPLE; var info: BASS_SAMPLE): BOOL; stdcall; external 'bass.dll' name 'BASS_SampleSetInfo';
{
  Set a sample's default attributes.
  handle : Sample handle
  info   : Sample info, only the freq/volume/pan/3D attributes and
           looping/override method flags are used
}
function BASS_SamplePlay(handle: HSAMPLE): HCHANNEL; stdcall; external 'bass.dll' name 'BASS_SamplePlay';
{
  Play a sample, using the sample's default attributes.
  handle : Handle of sample to play
  RETURN : Handle of channel used to play the sample (NULL=error)
}
function BASS_SamplePlayEx(handle: HSAMPLE; start: DWORD; freq, volume, pan: Integer; loop: BOOL): HCHANNEL; stdcall; external 'bass.dll' name 'BASS_SamplePlayEx';
{
  Play a sample, using specified attributes.
  handle : Handle of sample to play
  start  : Playback start position (in samples, not bytes)
  freq   : Playback rate (-1=default)
  volume : Volume (-1=default, 0=silent, 100=max)
  pan    : Pan position (-101=default, -100=left, 0=middle, 100=right)
  loop   : TRUE = Loop sample (-1=default)
  RETURN : Handle of channel used to play the sample (NULL=error)
}
function BASS_SamplePlay3D(handle: HSAMPLE; var pos, orient, vel: BASS_3DVECTOR): HCHANNEL; stdcall; external 'bass.dll' name 'BASS_SamplePlay3D';
{
  Play a 3D sample, setting it's 3D position, orientation and velocity.
  handle : Handle of sample to play
  pos    : position of the sound (NULL = x/y/z=0.0)
  orient : orientation of the sound, this is irrelevant if it's an
           omnidirectional sound source (NULL = x/y/z=0.0)
  vel    : velocity of the sound (NULL = x/y/z=0.0)
  RETURN : Handle of channel used to play the sample (NULL=error)
}
function BASS_SamplePlay3DEx(handle: HSAMPLE; var pos, orient, vel: BASS_3DVECTOR; start: DWORD; freq, volume: Integer; loop: BOOL): HCHANNEL; stdcall; external 'bass.dll' name 'BASS_SamplePlay3DEx';
{
  Play a 3D sample, using specified attributes.
  handle : Handle of sample to play
  pos    : position of the sound (NULL = x/y/z=0.0)
  orient : orientation of the sound, this is irrelevant if it's an
           omnidirectional sound source (NULL = x/y/z=0.0)
  vel    : velocity of the sound (NULL = x/y/z=0.0)
  start  : Playback start position (in samples, not bytes)
  freq   : Playback rate (-1=default)
  volume : Volume (-1=default, 0=silent, 100=max)
  loop   : TRUE = Loop sample (-1=default)
  RETURN : Handle of channel used to play the sample (NULL=error)
}
function BASS_SampleStop(handle: HSAMPLE): BOOL; stdcall; external 'bass.dll' name 'BASS_SampleStop';
{
  Stops all instances of a sample. For example, if a sample is playing
  simultaneously 3 times, calling this function will stop all 3 of them,
  which is obviously simpler than calling BASS_ChannelStop() 3 times.
  handle : Handle of sample to stop
}

function BASS_StreamCreate(freq, flags: DWORD; proc: STREAMPROC; user: DWORD): HSTREAM; stdcall; external 'bass.dll' name 'BASS_StreamCreate';
{
  Create a user sample stream.
  freq   : Stream playback rate
  flags  : BASS_SAMPLE_xxx flags (only the 8BITS/MONO/3D flags are used)
  proc   : User defined stream writing function
  user   : The 'user' value passed to the callback function
  RETURN : The created stream's handle (NULL=error)
}
function BASS_StreamCreateFile(mem: BOOL; f: Pointer; offset, length, flags: DWORD): HSTREAM; stdcall; external 'bass.dll' name 'BASS_StreamCreateFile';
{
  Create a sample stream from an MP3/MP2/MP1/OGG or WAV file.
  mem    : TRUE = Stream file from memory
  f      : Filename (mem=FALSE) or memory location (mem=TRUE)
  offset : File offset of the stream data
  length : File length (0=use whole file if mem=FALSE)
  flags  : Flags
  RETURN : The created stream's handle (NULL=error)
}
function BASS_StreamCreateURL(URL: PChar; offset: DWORD; flags: DWORD; save: PChar):HSTREAM;stdcall;external 'bass.dll' name 'BASS_StreamCreateURL';
{
  Create a sample stream from an MP3/MP2/MP1/OGG or WAV file on the internet.
  url    : The URL (beginning with "http://" or "ftp://")
  offset : File offset of start streaming from
  flags  : Flags
  save   : Filename to save the streamed file as locally (NULL=don't save)
  RETURN : The created stream's handle (NULL=error)
}
procedure BASS_StreamFree(handle: HSTREAM); stdcall; external 'bass.dll' name 'BASS_StreamFree';
{
  Free a sample stream's resources.
  stream : Stream handle
}
function BASS_StreamGetLength(handle: HSTREAM): QWORD; stdcall; external 'bass.dll' name 'BASS_StreamGetLength';
{
  Retrieves the playback length (in bytes) of a file stream. It's not
  always possible to 100% accurately guess the length of a stream, so
  the length returned may be only an approximation when using some WAV
  codecs.
  handle : Stream handle
  RETURN : The length (-1=error)
}
function BASS_StreamGetTags(handle: HSTREAM; tags : DWORD): PChar; stdcall; external 'bass.dll' name 'BASS_StreamGetTags';
{
  Retrieves the requested tags/headers, if available.
  handle : Stream handle
  tags   : A BASS_TAG_xxx flag
  RETURN : Pointer to the tags (NULL=error)
}
function BASS_StreamPreBuf(handle: HMUSIC): BOOL; stdcall; external 'bass.dll' name 'BASS_StreamPreBuf';
{
  Pre-buffer initial sample data ready for playback.
  handle : Handle of stream
}
function BASS_StreamPlay(handle: HSTREAM; flush: BOOL; flags: DWORD): BOOL; stdcall; external 'bass.dll' name 'BASS_StreamPlay';
{
  Play a sample stream, optionally flushing the buffer first.
  handle : Handle of stream to play
  flush  : Flush buffer contents. If you stop a stream and then want to
           continue it from where it stopped, don't flush it. Flushing
           a file stream causes it to restart from the beginning.
  flags  : BASS_SAMPLE_xxx flags (only affects file streams, and only the
          LOOP flag is used)
}
Function BASS_StreamGetFilePosition(handle:HSTREAM; mode:DWORD) : DWORD;stdcall;external 'bass.dll' name 'BASS_StreamGetFilePosition';
{
  Retrieves the file position of the decoding, the download (if streaming from
  the internet), or the end (total length). Obviously only works with file streams.
  handle : Stream handle
  mode   : The position to retrieve (0=decoding, 1=download, 2=end)
  RETURN : The position (-1=error)
}

function BASS_CDInit(drive: PChar; flags: DWORD): BOOL; stdcall; external 'bass.dll' name 'BASS_CDInit';
{
  Initialize the CD functions, must be called before any other CD
  functions. The volume is initially set to 100 (the maximum), use
  BASS_ChannelSetAttributes() to adjust it.
  drive  : The CD drive, for example: "d:" (NULL=use default drive)
  flags  : BASS_DEVICE_LEAVEVOL = leave the volume as it is
}
procedure BASS_CDFree; stdcall; external 'bass.dll' name 'BASS_CDFree';
{
  Free resources used by the CD
}
function BASS_CDInDrive: BOOL; stdcall; external 'bass.dll' name 'BASS_CDInDrive';
{
  Check if there is a CD in the drive.
}
function BASS_CDDoor(open:BOOL): BOOL; stdcall; external 'bass.dll' name 'BASS_CDDoor';
{
  Opens or closes the CD door.
  open   : TRUE=open the door
}
function BASS_CDGetID(id: DWORD):PChar; stdcall; external 'bass.dll' name 'BASS_CDGetID';
{
  Retrieves identification info from the CD in the drive.
  id     : BASS_CDID_xxx flag
  RETURN : ID string (NULL=error)
}
Function BASS_CDGetTracks:DWORD;stdcall;external 'bass.dll' name 'BASS_CDGetTracks';
{
  Retrieves the number of tracks on the CD
  RETURN : The number of tracks (-1=error)
}
function BASS_CDPlay(track: DWORD; loop: BOOL; wait: BOOL): BOOL; stdcall; external 'bass.dll' name 'BASS_CDPlay';
{
  Play a CD track.
  track  : Track number to play (1=first)
  loop   : TRUE = Loop the track
  wait   : TRUE = don't return until playback has started (some drives
           will always wait anyway)
}
Function BASS_CDGetTrackLength(track:DWORD):DWORD;stdcall;external 'bass.dll' name 'BASS_CDGetTrackLength';
{
  Retrieves the playback length (in milliseconds) of a cd track.
  track  : The CD track (1=first)
  RETURN : The length (-1=error)
}

Function BASS_RecordGetDeviceDescription(devnum: DWORD):PChar;stdcall;external 'bass.dll' name 'BASS_RecordGetDeviceDescription';
{
  Get the text description of a recording device. This function can be
  used to enumerate the available devices.
  devnum : The device (0=first)
  RETURN : The text description of the device (NULL=error)
}
Function BASS_RecordInit(device: Integer):BOOL;stdcall;external 'bass.dll' name 'BASS_RecordInit';
{
  Initialize a recording device.
  device : Device to use (0=first, -1=default)
}
procedure BASS_RecordFree;stdcall;external 'bass.dll' name 'BASS_RecordFree';
{
  Free all resources used by the recording device.
}
procedure BASS_RecordGetInfo(var info:BASS_RECORDINFO);stdcall;external 'bass.dll' name 'BASS_RecordGetInfo';
{
  Retrieve some information on the recording device being used.
  info   : Pointer to store info at
}
Function BASS_RecordGetInputName(input:DWORD):PChar;stdcall;external 'bass.dll' name 'BASS_RecordGetInputName';
{
  Get the text description of a recording input.
  input  : Input number (0=first)
  RETURN : The text description (NULL=error)
}
Function BASS_RecordSetInput(input:DWORD; setting:DWORD):BOOL;stdcall;external 'bass.dll' name 'BASS_RecordSetInput';
{
  Adjust the setting of a recording input.
  input  : Input number (0=first)
  setting: BASS_INPUT flags (if BASS_INPUT_LEVEL used, LOWORD=volume)
}
Function BASS_RecordGetInput(input:DWORD):DWORD;stdcall;external 'bass.dll' name 'BASS_RecordGetInput';
{
  Retrieve the setting of a recording input.
  input  : Input number (0=first)
  RETURN : The setting (LOWORD=volume, with BASS_INPUT_OFF flag if off, -1=error)
}
Function BASS_RecordStart(freq,flags:DWORD; proc:RECORDPROC; user:DWORD):BOOL;stdcall;external 'bass.dll' name 'BASS_RecordStart';
{
  Start recording. Use BASS_ChannelStop to stop recording.
  freq   : Sampling rate
  flags  : BASS_SAMPLE_8BITS/MONO flags (optional HIWORD=update period)
  proc   : User defined function to receive the recorded data
  user   : The 'user' value passed to the callback function
}

{
  A "channel" can be a playing sample (HCHANNEL), a MOD music (HMUSIC), a
  sample stream (HSTREAM), the CD (CDCHANNEL), or recording (RECORDCHAN). The
  following functions can be used with one or more of these channel types.
}

Function BASS_ChannelBytes2Seconds(handle: DWORD; pos: QWORD): FLOAT; stdcall;external 'bass.dll' name 'BASS_ChannelBytes2Seconds';
{
  Translate a byte position into time (seconds)
  handle : Handle of channel (HCHANNEL/HMUSIC/HSTREAM, or RECORDCHAN, also HSAMPLE)
  pos    : The position to translate
  RETURN : The millisecond position (<0=error)
}
Function BASS_ChannelSeconds2Bytes(handle: DWORD; pos: FLOAT): QWORD; stdcall;external 'bass.dll' name 'BASS_ChannelSeconds2Bytes';
{
  Translate a time (seconds) position into bytes
  handle : Handle of channel (HCHANNEL/HMUSIC/HSTREAM, or RECORDCHAN, also HSAMPLE)
  pos    : The position to translate
  RETURN : The byte position (-1=error)
}
Function BASS_ChannelIsActive(handle: DWORD): DWORD; stdcall;external 'bass.dll' name 'BASS_ChannelIsActive';
{
  Check if a channel is active (playing) or stalled.
  handle : Channel handle (HCHANNEL/HMUSIC/HSTREAM, or CDCHANNEL/RECORDCHAN)
  RETURN : One of the BASS_ACTIVE_xxx values
}
function BASS_ChannelGetFlags(handle: DWORD): DWORD; stdcall; external 'bass.dll' name 'BASS_ChannelGetFlags';
{
  Get some info about a channel.
  handle : Channel handle (HCHANNEL/HMUSIC/HSTREAM)
  RETURN : BASS_SAMPLE_xxx flags (-1=error)
}
function BASS_ChannelStop(handle: DWORD): BOOL; stdcall; external 'bass.dll' name 'BASS_ChannelStop';
{
  Stop a channel.
  handle : Channel handle (HCHANNEL/HMUSIC/HSTREAM, or CDCHANNEL/RECORDCHAN)
}
function BASS_ChannelPause(handle: DWORD): BOOL; stdcall; external 'bass.dll' name 'BASS_ChannelPause';
{
  Pause a channel.
  handle : Channel handle (HCHANNEL/HMUSIC/HSTREAM, or CDCHANNEL/RECORDCHAN)
}
function BASS_ChannelResume(handle: DWORD): BOOL; stdcall; external 'bass.dll' name 'BASS_ChannelResume';
{
  Resume a paused channel.
  handle : Channel handle (HCHANNEL/HMUSIC/HSTREAM, or CDCHANNEL/RECORDCHAN)
}
function BASS_ChannelSetAttributes(handle: DWORD; freq, volume, pan: Integer): BOOL; stdcall; external 'bass.dll' name 'BASS_ChannelSetAttributes';
{
  Update a channel's attributes. The actual setting may not be exactly
  as specified, depending on the accuracy of the device and drivers.
  NOTE: Only the volume can be adjusted for the CD "channel", but not all
  soundcards allow controlling of the CD volume level.
  handle : Channel handle (HCHANNEL/HMUSIC/HSTREAM, or CDCHANNEL)
  freq   : Playback rate (-1=leave current)
  volume : Volume (-1=leave current, 0=silent, 100=max)
  pan    : Pan position (-101=current, -100=left, 0=middle, 100=right)
           panning has no effect on 3D channels
}
function BASS_ChannelGetAttributes(handle: DWORD; var freq, volume: DWORD; var pan: Integer): BOOL; stdcall; external 'bass.dll' name 'BASS_ChannelGetAttributes';
{
  Retrieve a channel's attributes. Only the volume is available for
  the CD "channel" (if allowed by the soundcard/drivers).
  handle : Channel handle (HCHANNEL/HMUSIC/HSTREAM, or CDCHANNEL)
  freq   : Pointer to store playback rate (NULL=don't retrieve it)
  volume : Pointer to store volume (NULL=don't retrieve it)
  pan    : Pointer to store pan position (NULL=don't retrieve it)
}
function BASS_ChannelSet3DAttributes(handle: DWORD; mode: Integer; min, max: FLOAT; iangle, oangle, outvol: Integer): BOOL; stdcall; external 'bass.dll' name 'BASS_ChannelSet3DAttributes';
{
  Set a channel's 3D attributes.
  handle : Channel handle (HCHANNEL/HSTREAM/HMUSIC)
  mode   : BASS_3DMODE_xxx mode (-1=leave current setting)
  min    : minimum distance, volume stops increasing within this distance (<0.0=leave current)
  max    : maximum distance, volume stops decreasing past this distance (<0.0=leave current)
  iangle : angle of inside projection cone in degrees (360=omnidirectional, -1=leave current)
  oangle : angle of outside projection cone in degrees (-1=leave current)
           NOTE: iangle & oangle must both be set in a single call
  outvol : delta-volume outside the projection cone (0=silent, 100=same as inside)
  The iangle/oangle angles decide how wide the sound is projected around the
  orientation angle. Within the inside angle the volume level is the channel
  level as set with BASS_ChannelSetAttributes, from the inside to the outside
  angles the volume gradually changes by the "outvol" setting.
}
function BASS_ChannelGet3DAttributes(handle: DWORD; var mode: DWORD; var min, max: FLOAT; var iangle, oangle, outvol: DWORD): BOOL; stdcall; external 'bass.dll' name 'BASS_ChannelGet3DAttributes';
{
  Retrieve a channel's 3D attributes.
  handle : Channel handle (HCHANNEL/HSTREAM/HMUSIC)
  mode   : BASS_3DMODE_xxx mode (NULL=don't retrieve it)
  min    : minumum distance (NULL=don't retrieve it)
  max    : maximum distance (NULL=don't retrieve it)
  iangle : angle of inside projection cone (NULL=don't retrieve it)
  oangle : angle of outside projection cone (NULL=don't retrieve it)
           NOTE: iangle & oangle must both be retrieved in a single call
  outvol : delta-volume outside the projection cone (NULL=don't retrieve it)
}
function BASS_ChannelSet3DPosition(handle: DWORD; var pos, orient, vel: BASS_3DVECTOR): BOOL; stdcall; external 'bass.dll' name 'BASS_ChannelSet3DPosition';
{
  Update a channel's 3D position, orientation and velocity. The velocity
  is only used to calculate the doppler effect.
  handle : Channel handle (HCHANNEL/HSTREAM/HMUSIC)
  pos    : position of the sound (NULL=leave current)
  orient : orientation of the sound, this is irrelevant if it's an
           omnidirectional sound source (NULL=leave current)
  vel    : velocity of the sound (NULL=leave current)
}
function BASS_ChannelGet3DPosition(handle: DWORD; var pos, orient, vel: BASS_3DVECTOR): BOOL; stdcall; external 'bass.dll' name 'BASS_ChannelGet3DPosition';
{
  Retrieve a channel's current 3D position, orientation and velocity.
  handle : Channel handle (HCHANNEL/HSTREAM/HMUSIC)
  pos    : position of the sound (NULL=don't retrieve it)
  orient : orientation of the sound, this is irrelevant if it's an
           omnidirectional sound source (NULL=don't retrieve it)
  vel    : velocity of the sound (NULL=don't retrieve it)
}
function BASS_ChannelSetPosition(handle: DWORD; pos: QWORD): BOOL; stdcall; external 'bass.dll' name 'BASS_ChannelSetPosition';
{
  Set the current playback position of a channel.
  handle : Channel handle (HCHANNEL/HMUSIC, or CDCHANNEL)
  pos    : the position
           if HCHANNEL: position in bytes
           if HMUSIC: LOWORD=order HIWORD=row ... use MAKELONG(order,row)
           if HSTREAM: position in bytes, file streams only
           if CDCHANNEL: position in milliseconds from start of track
}
function BASS_ChannelGetPosition(handle: DWORD): QWORD; stdcall; external 'bass.dll' name 'BASS_ChannelGetPosition';
{
  Get the current playback position of a channel.
  handle : Channel handle (HCHANNEL/HMUSIC/HSTREAM, or CDCHANNEL)
  RETURN : the position (-1=error)
           if HCHANNEL: position in bytes
           if HMUSIC: LOWORD=order HIWORD=row (see BASS_MusicSetPositionScaler)
           if HSTREAM: total bytes played since the stream was last flushed
           if CDCHANNEL: position in milliseconds from start of track
}
function BASS_ChannelGetLevel(handle: DWORD): DWORD; stdcall; external 'bass.dll' name 'BASS_ChannelGetLevel';
{
  Calculate a channel's current output level.
  handle : Channel handle (HMUSIC/HSTREAM, or RECORDCHAN)
  RETURN : LOWORD=left level (0-128) HIWORD=right level (0-128) (-1=error)
}
function BASS_ChannelGetData(handle: DWORD; buffer: Pointer; length: DWORD): DWORD; stdcall; external 'bass.dll' name 'BASS_ChannelGetData';
{
  Retrieves upto "length" bytes of the channel's current sample data.
  This is useful if you wish to "visualize" the sound.
  handle : Channel handle (HMUSIC/HSTREAM, or RECORDCHAN)
  buffer : Location to write the data
  length : Number of bytes wanted, or a BASS_DATA_xxx flag
  RETURN : Number of bytes actually written to the buffer (-1=error)
}
function BASS_ChannelSetSync(handle: DWORD; atype: DWORD; param: QWORD; proc: SYNCPROC; user: DWORD): HSYNC; stdcall; external 'bass.dll' name 'BASS_ChannelSetSync';
{
  Setup a sync on a channel. Multiple syncs may be used per channel.
  handle : Channel handle
  atype  : Sync type (BASS_SYNC_xxx type & flags)
  param  : Sync parameters (see the BASS_SYNC_xxx type description)
  proc   : User defined callback function
  user   : The 'user' value passed to the callback function
  RETURN : Sync handle (NULL=error)
}
function BASS_ChannelRemoveSync(handle: DWORD; sync: HSYNC): BOOL; stdcall; external 'bass.dll' name 'BASS_ChannelRemoveSync';
{
  Remove a sync from a channel
  handle : Channel handle (HMUSIC)
  sync   : Handle of sync to remove
}
function BASS_ChannelSetDSP(handle: DWORD; proc: DSPPROC; user: DWORD): HDSP; stdcall; external 'bass.dll' name 'BASS_ChannelSetDSP';
{
  Setup a user DSP function on a channel. When multiple DSP functions
  are used on a channel, they are called in the order that they were added.
  handle : Channel handle (HMUSIC/HSTREAM)
  proc   : User defined callback function
  user   : The 'user' value passed to the callback function
  RETURN : DSP handle (NULL=error)
}
function BASS_ChannelRemoveDSP(handle: DWORD; dsp: HDSP): BOOL; stdcall; external 'bass.dll' name 'BASS_ChannelRemoveDSP';
{
  Remove a DSP function from a channel
  handle : Channel handle (HMUSIC/HSTREAM)
  dsp    : Handle of DSP to remove
}
function BASS_ChannelSetEAXMix(handle: DWORD; mix: FLOAT): BOOL; stdcall; external 'bass.dll' name 'BASS_ChannelSetEAXMix';
{
  Set the wet(reverb)/dry(no reverb) mix ratio on the channel. By default
  the distance of the sound from the listener is used to calculate the mix.
  NOTE: The channel must have 3D functionality enabled for the EAX environment
  to have any affect on it.
  handle : Channel handle (HCHANNEL/HSTREAM/HMUSIC)
  mix    : The ratio (0.0=reverb off, 1.0=max reverb, -1.0=let EAX calculate
           the reverb mix based on the distance)
}
function BASS_ChannelGetEAXMix(handle: DWORD; var mix: FLOAT): BOOL; stdcall; external 'bass.dll' name 'BASS_ChannelGetEAXMix';
{
  Get the wet(reverb)/dry(no reverb) mix ratio on the channel.
  handle : Channel handle (HCHANNEL/HSTREAM/HMUSIC)
  mix    : Pointer to store the ratio at
}
function BASS_ChannelSetLink(handle, chan: DWORD): BOOL; stdcall; external 'bass.dll' name 'BASS_ChannelSetLink';
{
  Set a link between 2 channels. When the 1st is played/stopped/paused/resumed
  the 2nd channel is also played/stopped/paused/resumed.
  handle : Handle of channel to link "chan" to (HMUSIC/HSTREAM)
  chan   : Handle of channel to link to "handle" (HMUSIC/HSTREAM)
}
function BASS_ChannelRemoveLink(handle, chan: DWORD): BOOL; stdcall; external 'bass.dll' name 'BASS_ChannelRemoveLink';
{
  Remove a link from a channel.
  handle : Handle of channel to unlink with "chan" (HMUSIC/HSTREAM)
  chan   : Handle of channel to unlink from "handle" (HMUSIC/HSTREAM)
}
function BASS_ChannelSetFX(handle, etype: DWORD): HFX; stdcall; external 'bass.dll' name 'BASS_ChannelSetFX';
{
  Setup a DX8 effect on a channel. Can only be used when the channel
  is not playing. Use BASS_FXSetParameters to set the effect parameters.
  Obviously requires DX8.
  handle : Channel handle (HMUSIC/HSTREAM)
  etype  : Type of effect to setup (BASS_FX_xxx)
  RETURN : FX handle (NULL=error)
}
function BASS_ChannelRemoveFX(handle: DWORD; fx: HFX): BOOL; stdcall; external 'bass.dll' name 'BASS_ChannelRemoveFX';
{
  Remove a DX8 effect from a channel. Can only be used when the
  channel is not playing.
  handle : Channel handle (HMUSIC/HSTREAM)
  fx     : Handle of FX to remove
}

function BASS_FXSetParameters(handle: HFX; par: Pointer): BOOL; stdcall; external 'bass.dll' name 'BASS_FXSetParameters';
{
  Set the parameters of a DX8 effect.
  handle : FX handle
  par    : Pointer to the parameter structure
}
function BASS_FXGetParameters(handle: HFX; par: Pointer): BOOL; stdcall; external 'bass.dll' name 'BASS_FXGetParameters';
{
  Retrieve the parameters of a DX8 effect.
  handle : FX handle
  par    : Pointer to the parameter structure
}

procedure BASS_EAXPreset(env: Integer);
{
  This function is defined in the implementation part of this unit.
  It is not part of BASS.DLL but an extra function which makes it easier
  to set the predefined EAX environments.
  env    : a EAX_ENVIRONMENT_xxx constant
}


implementation

procedure BASS_EAXPreset(env: Integer);
begin
  case (env) of
    EAX_ENVIRONMENT_GENERIC:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_GENERIC, 0.5, 1.493, 0.5);
    EAX_ENVIRONMENT_PADDEDCELL:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_PADDEDCELL, 0.25, 0.1, 0);
    EAX_ENVIRONMENT_ROOM:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_ROOM, 0.417, 0.4, 0.666);
    EAX_ENVIRONMENT_BATHROOM:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_BATHROOM, 0.653, 1.499, 0.166);
    EAX_ENVIRONMENT_LIVINGROOM:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_LIVINGROOM, 0.208, 0.478, 0);
    EAX_ENVIRONMENT_STONEROOM:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_STONEROOM, 0.5, 2.309, 0.888);
    EAX_ENVIRONMENT_AUDITORIUM:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_AUDITORIUM, 0.403, 4.279, 0.5);
    EAX_ENVIRONMENT_CONCERTHALL:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_CONCERTHALL, 0.5, 3.961, 0.5);
    EAX_ENVIRONMENT_CAVE:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_CAVE, 0.5, 2.886, 1.304);
    EAX_ENVIRONMENT_ARENA:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_ARENA, 0.361, 7.284, 0.332);
    EAX_ENVIRONMENT_HANGAR:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_HANGAR, 0.5, 10.0, 0.3);
    EAX_ENVIRONMENT_CARPETEDHALLWAY:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_CARPETEDHALLWAY, 0.153, 0.259, 2.0);
    EAX_ENVIRONMENT_HALLWAY:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_HALLWAY, 0.361, 1.493, 0);
    EAX_ENVIRONMENT_STONECORRIDOR:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_STONECORRIDOR, 0.444, 2.697, 0.638);
    EAX_ENVIRONMENT_ALLEY:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_ALLEY, 0.25, 1.752, 0.776);
    EAX_ENVIRONMENT_FOREST:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_FOREST, 0.111, 3.145, 0.472);
    EAX_ENVIRONMENT_CITY:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_CITY, 0.111, 2.767, 0.224);
    EAX_ENVIRONMENT_MOUNTAINS:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_MOUNTAINS, 0.194, 7.841, 0.472);
    EAX_ENVIRONMENT_QUARRY:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_QUARRY, 1, 1.499, 0.5);
    EAX_ENVIRONMENT_PLAIN:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_PLAIN, 0.097, 2.767, 0.224);
    EAX_ENVIRONMENT_PARKINGLOT:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_PARKINGLOT, 0.208, 1.652, 1.5);
    EAX_ENVIRONMENT_SEWERPIPE:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_SEWERPIPE, 0.652, 2.886, 0.25);
    EAX_ENVIRONMENT_UNDERWATER:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_UNDERWATER, 1, 1.499, 0);
    EAX_ENVIRONMENT_DRUGGED:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_DRUGGED, 0.875, 8.392, 1.388);
    EAX_ENVIRONMENT_DIZZY:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_DIZZY, 0.139, 17.234, 0.666);
    EAX_ENVIRONMENT_PSYCHOTIC:
      BASS_SetEAXParameters(EAX_ENVIRONMENT_PSYCHOTIC, 0.486, 7.563, 0.806);
    else
      BASS_SetEAXParameters(-1, 0, -1, -1);
  end;
end;

end.
// END OF FILE /////////////////////////////////////////////////////////////////

