{ ========================================================================================== }
{ FMOD Main header file. Copyright (c), FireLight Multimedia 1999-2001.                     }
{ ========================================================================================== }

unit FMOD;

interface

uses
  Windows;

{ =============================================================================================== }
{ DEFINITIONS                                                                                     }
{ =============================================================================================== }

{
  Force four-byte enums
}
{$Z4}

const
  FMOD_VERSION: Single = 3.4;

{
  FMOD defined types
}

type
  PFSoundSample = Pointer;
  PFSoundStream = Pointer;
  PFSoundDSPUnit = Pointer;
  PFMusicModule = Pointer;
  PFSoundMaterial = Pointer;
  PFSoundGeomList = Pointer;

  PFSoundVector = ^TFSoundVector;
  TFSoundVector = record
    x: Single;
    y: Single;
    z: Single;
  end;

  {
    Callback types
  }

  TFSoundStreamCallback = function (Stream: PFSoundStream; Buff: Pointer; Length, Param: Integer): ByteBool; cdecl;
  TFSoundDSPCallback = function (OriginalBuffer: Pointer; NewBuffer: Pointer; Length, Param: Integer): Pointer; cdecl;
  TFMusicCallback = procedure (Module: PFMusicModule; Param: Byte); cdecl;

  TFSoundOpenCallback = function (Name: PChar): Cardinal; cdecl;
  TFSoundCloseCallback = procedure (Handle: Cardinal); cdecl;
  TFSoundReadCallback = function (Buffer: Pointer; Size: Cardinal; Handle: Cardinal): Cardinal; cdecl;
  TFSoundSeekCallback = procedure (Handle: Cardinal; Pos: Cardinal; Mode: ByteBool); cdecl;
  TFSoundTellCallback = function (Handle: Cardinal): Cardinal; cdecl;

  {
    To maintain compatability with existing Delphi code
  }

  PFSOUND_SAMPLE = PFSoundSample;
  PFSOUND_STREAM = PFSoundStream;
  PFSOUND_DSPUNIT = PFSoundDSPUnit;
  PFMUSIC_MODULE = PFMusicModule;
  PFSOUND_MATERIAL = PFSoundMaterial;
  PFSOUND_GEOMLIST = PFSoundGeomList;
  PFSOUND_VECTOR = PFSoundVector;

  FSOUND_STREAMCALLBACK = TFSoundStreamCallback;
  FSOUND_DSPCALLBACK = TFSoundDSPCallback;
  FMUSIC_CALLBACK = TFMusicCallback;

  FSOUND_OPENCALLBACK = TFSoundOpenCallback;
  FSOUND_CLOSECALLBACK = TFSoundCloseCallback;
  FSOUND_READCALLBACK = TFSoundReadCallback;
  FSOUND_SEEKCALLBACK = TFSoundSeekCallback;
  FSOUND_TELLCALLBACK = TFSoundTellCallback;

{
[ENUM]
[
  [DESCRIPTION]
  On failure of commands in FMOD, use FSOUND_GetError to attain what happened.

  [SEE_ALSO]
  FSOUND_GetError
]
}

type
  TFModErrors = (
    FMOD_ERR_NONE, // No errors
    FMOD_ERR_BUSY, // Cannot call this command after FSOUND_Init.  Call FSOUND_Close first.
    FMOD_ERR_UNINITIALIZED, // This command failed because FSOUND_Init was not called
    FMOD_ERR_INIT, // Error initializing output device.
    FMOD_ERR_ALLOCATED, // Error initializing output device, but more specifically, the output device is already in use and cannot be reused.
    FMOD_ERR_PLAY, // Playing the sound failed.
    FMOD_ERR_OUTPUT_FORMAT, // Soundcard does not support the features needed for this soundsystem (16bit stereo output)
    FMOD_ERR_COOPERATIVELEVEL, // Error setting cooperative level for hardware.
    FMOD_ERR_CREATEBUFFER, // Error creating hardware sound buffer.
    FMOD_ERR_FILE_NOTFOUND, // File not found
    FMOD_ERR_FILE_FORMAT, // Unknown file format
    FMOD_ERR_FILE_BAD, // Error loading file
    FMOD_ERR_MEMORY, // Not enough memory
    FMOD_ERR_VERSION, // The version number of this file format is not supported
    FMOD_ERR_INVALID_PARAM, // An invalid parameter was passed to this function
    FMOD_ERR_NO_EAX, // Tried to use an EAX command on a non EAX enabled channel or output.
    FMOD_ERR_NO_EAX2, // Tried to use an advanced EAX2 command on a non EAX2 enabled channel or output.
    FMOD_ERR_CHANNEL_ALLOC, // Failed to allocate a new channel
    FMOD_ERR_RECORD, // Recording is not supported on this machine
    FMOD_ERR_MEDIAPLAYER  // Required Mediaplayer codec is not installed
  );

  FMOD_ERRORS = TFModErrors;

{
[ENUM]
[
  [DESCRIPTION]
  These output types are used with FSOUND_SetOutput, to choose which output driver to use.

	FSOUND_OUTPUT_DSOUND will not support hardware 3d acceleration if the sound card driver
	does not support DirectX 6 Voice Manager Extensions.

  FSOUND_OUTPUT_WINMM is recommended for NT and CE.

  [SEE_ALSO]
  FSOUND_SetOutput
  FSOUND_GetOutput
]
}

type
  TFSoundOutputTypes = (
    FSOUND_OUTPUT_NOSOUND,  // NoSound driver, all calls to this succeed but do nothing.
    FSOUND_OUTPUT_WINMM,    // Windows Multimedia driver.
    FSOUND_OUTPUT_DSOUND,   // DirectSound driver.  You need this to get EAX or EAX2 support.
    FSOUND_OUTPUT_A3D,      // A3D driver.  You need this to get geometry support.

    FSOUND_OUTPUT_OSS,      // Linux/Unix OSS (Open Sound System) driver, i.e. the kernel sound drivers.
    FSOUND_OUTPUT_ESD,      // Linux/Unix ESD (Enlightment Sound Daemon) driver.
    FSOUND_OUTPUT_ALSA      // Linux Alsa driver.
  );

  FSOUND_OUTPUTTYPES = TFSoundOutputTypes;

{
[ENUM]
[
  [DESCRIPTION]
  These mixer types are used with FSOUND_SetMixer, to choose which mixer to use, or to act
  upon for other reasons using FSOUND_GetMixer.

  [SEE_ALSO]
  FSOUND_SetMixer
  FSOUND_GetMixer
]
}
type
  TFSoundMixerTypes = (
    FSOUND_MIXER_AUTODETECT,  // Enables autodetection of the fastest mixer based on your cpu.
    FSOUND_MIXER_BLENDMODE, // Enables the standard non mmx, blendmode mixer.
    FSOUND_MIXER_MMXP5, // Enables the mmx, pentium optimized blendmode mixer.
    FSOUND_MIXER_MMXP6, // Enables the mmx, ppro/p2/p3 optimized mixer.
    FSOUND_MIXER_QUALITY_AUTODETECT,  // Enables autodetection of the fastest quality mixer based on your cpu.
    FSOUND_MIXER_QUALITY_FPU, // Enables the interpolating FPU mixer.
    FSOUND_MIXER_QUALITY_MMXP5, // Enables the interpolating p5 MMX mixer.
    FSOUND_MIXER_QUALITY_MMXP6  // Enables the interpolating ppro/p2/p3 MMX mixer.
  );

  FSOUND_MIXERTYPES = TFSoundMixerTypes;

{
[ENUM]
[
  [DESCRIPTION]
  These definitions describe the type of song being played.

  [SEE_ALSO]
  FMUSIC_GetType
]
}
type
  TFMusicTypes = (
    FMUSIC_TYPE_NONE,
    FMUSIC_TYPE_MOD,  // Protracker / FastTracker
    FMUSIC_TYPE_S3M,  // ScreamTracker 3
    FMUSIC_TYPE_XM,   // FastTracker 2
    FMUSIC_TYPE_IT,   // Impulse Tracker
    FMUSIC_TYPE_MIDI  // MIDI file
  );
  FMUSIC_TYPES = TFMusicTypes;

{
[DEFINE_START]
[
  [NAME]
  FSOUND_DSP_PRIORITIES

  [DESCRIPTION]
  These default priorities are

  [SEE_ALSO]
  FSOUND_DSP_Create
  FSOUND_DSP_SetPriority
  FSOUND_DSP_GetSpectrum
]
}
const
  FSOUND_DSP_DEFAULTPRIORITY_CLEARUNIT = 0; // DSP CLEAR unit - done first
  FSOUND_DSP_DEFAULTPRIORITY_SFXUNIT = 100; // DSP SFX unit - done second
  FSOUND_DSP_DEFAULTPRIORITY_MUSICUNIT = 200; // DSP MUSIC unit - done third
  FSOUND_DSP_DEFAULTPRIORITY_USER = 300; // User priority, use this as reference
  FSOUND_DSP_DEFAULTPRIORITY_FFTUNIT = 900; // This reads data for FSOUND_DSP_GetSpectrum, so it comes after user units
  FSOUND_DSP_DEFAULTPRIORITY_CLIPANDCOPYUNIT = 1000; // DSP CLIP AND COPY unit - last
// [DEFINE_END]


{
[DEFINE_START]
[
  [NAME]
  FSOUND_CAPS

  [DESCRIPTION]
  Driver description bitfields. Use FSOUND_Driver_GetCaps to determine if a driver enumerated
  has the settings you are after. The enumerated driver depends on the output mode, see
  FSOUND_OUTPUTTYPES

  [SEE_ALSO]
  FSOUND_GetDriverCaps
  FSOUND_OUTPUTTYPES
]
}
const
  FSOUND_CAPS_HARDWARE = $1; // This driver supports hardware accelerated 3d sound.
  FSOUND_CAPS_EAX = $2; // This driver supports EAX reverb
  FSOUND_CAPS_GEOMETRY_OCCLUSIONS = $4; // This driver supports (A3D) geometry occlusions
  FSOUND_CAPS_GEOMETRY_REFLECTIONS = $8; // This driver supports (A3D) geometry reflections
  FSOUND_CAPS_EAX2 = $10; // This driver supports EAX2/A3D3 reverb
// [DEFINE_END]


{
[DEFINE_START]
[
  [NAME]
  FSOUND_MODES

  [DESCRIPTION]
  Sample description bitfields, OR them together for loading and describing samples.
    NOTE.  If the file format being loaded already has a defined format, such as WAV or MP3, then
    trying to override the pre-defined format with a new set of format flags will not work.  For
    example, an 8 bit WAV file will not load as 16bit if you specify FSOUND_16BITS.  It will just
    ignore the flag and go ahead loading it as 8bits.  For these type of formats the only flags
    you can specify that will really alter the behaviour of how it is loaded, are the following.

    FSOUND_LOOP_OFF
    FSOUND_LOOP_NORMAL
    FSOUND_LOOP_BIDI
    FSOUND_HW3D
    FSOUND_2D
    FSOUND_STREAMABLE
    FSOUND_LOADMEMORY
    FSOUND_LOADRAW
    FSOUND_MPEGACCURATE

    See flag descriptions for what these do.
]
}
const
  FSOUND_LOOP_OFF = $00000001; // For non looping samples.
  FSOUND_LOOP_NORMAL = $00000002; // For forward looping samples.
  FSOUND_LOOP_BIDI = $00000004; // For bidirectional looping samples. (no effect if in hardware).
  FSOUND_8BITS = $00000008; // For 8 bit samples.
  FSOUND_16BITS = $00000010; // For 16 bit samples.
  FSOUND_MONO = $00000020; // For mono samples.
  FSOUND_STEREO = $00000040; // For stereo samples.
  FSOUND_UNSIGNED = $00000080; // For source data containing unsigned samples.
  FSOUND_SIGNED = $00000100; // For source data containing signed data.
  FSOUND_DELTA = $00000200; // For source data stored as delta values.
  FSOUND_IT214 = $00000400; // For source data stored using IT214 compression.
  FSOUND_IT215 = $00000800; // For source data stored using IT215 compression.
  FSOUND_HW3D = $00001000; // Attempts to make samples use 3d hardware acceleration. (if the card supports it)
  FSOUND_2D = $00002000; // Ignores any 3d processing. overrides FSOUND_HW3D. Located in software.
  FSOUND_STREAMABLE = $00004000; // For realtime streamable samples. If you dont supply this sound may come out corrupted.
  FSOUND_LOADMEMORY = $00008000; // For FSOUND_Sample_Load - 'name' will be interpreted as a pointer to data
  FSOUND_LOADRAW = $00010000; // For FSOUND_Sample_Load/FSOUND_Stream_Open - will ignore file format and treat as raw pcm.
  FSOUND_MPEGACCURATE = $00020000; // For FSOUND_Stream_Open - scans MP2/MP3 (VBR also) for accurate FSOUND_Stream_GetLengthMs/FSOUND_Stream_SetTime.
  FSOUND_FORCEMONO = $00040000; // For forcing stereo streams and samples to be mono - needed with FSOUND_HW3D - incurs speed hit
  FSOUND_HW2D = $00080000; // 2d hardware sounds.  allows hardware specific effects
  FSOUND_ENABLEFX = $00100000; // Allows DX8 FX to be played back on a sound.  Requires DirectX 8 - Note these sounds cant be played more than once, or have a changing frequency

{
    FSOUND_NORMAL is a default sample type.  Loop off, 8bit mono, signed, not hardware
    accelerated.  Some API functions ignore 8bits and mono, as it may be an mpeg/wav/etc which
    has its format predetermined.
}
const
  FSOUND_NORMAL = (FSOUND_LOOP_OFF or FSOUND_8BITS or FSOUND_MONO);
// [DEFINE_END]


{
[DEFINE_START]
[
  [NAME]
  FSOUND_CDPLAYMODES

  [DESCRIPTION]
  Playback method for a CD Audio track, using FSOUND_CD_Play

  [SEE_ALSO]
  FSOUND_CD_Play
]
}
const
  FSOUND_CD_PLAYCONTINUOUS = 0;   // Starts from the current track and plays to end of CD.
  FSOUND_CD_PLAYONCE = 1;         // Plays the specified track then stops.
  FSOUND_CD_PLAYLOOPED = 2;       // Plays the specified track looped, forever until stopped manually.
  FSOUND_CD_PLAYRANDOM = 3;       // Plays tracks in random order
// [DEFINE_END]


{
[DEFINE_START]
[
  [NAME]
  FSOUND_CHANNELSAMPLEMODE

  [DESCRIPTION]
  Miscellaneous values for FMOD functions.

  [SEE_ALSO]
  FSOUND_PlaySound
  FSOUND_PlaySound3DAttrib
  FSOUND_Sample_Alloc
  FSOUND_Sample_Load
  FSOUND_SetPan
]
}
const
  FSOUND_FREE          = -1;    // value to play on any free channel, or to allocate a sample in a free sample slot.
  FSOUND_UNMANAGED     = -2;    // value to allocate a sample that is NOT managed by FSOUND or placed in a sample slot.
  FSOUND_ALL           = -3;    // for a channel index , this flag will affect ALL channels available! Not supported by every function.
  FSOUND_STEREOPAN     = -1;    // value for FSOUND_SetPan so that stereo sounds are not played at half volume. See FSOUND_SetPan for more on this.
  FSOUND_SYSTEMCHANNEL = -1000; // special channel ID for channel based functions that want to alter the global FSOUND software mixing output channel.
// [DEFINE_END]


{
[ENUM]
[
  [DESCRIPTION]
  These are environment types defined for use with the FSOUND_Reverb API.

  [SEE_ALSO]
  FSOUND_Reverb_SetEnvironment
  FSOUND_Reverb_SetEnvironmentAdvanced
]
}
type
  TFSoundReverbEnvironments = (
    FSOUND_ENVIRONMENT_GENERIC,
    FSOUND_ENVIRONMENT_PADDEDCELL,
    FSOUND_ENVIRONMENT_ROOM,
    FSOUND_ENVIRONMENT_BATHROOM,
    FSOUND_ENVIRONMENT_LIVINGROOM,
    FSOUND_ENVIRONMENT_STONEROOM,
    FSOUND_ENVIRONMENT_AUDITORIUM,
    FSOUND_ENVIRONMENT_CONCERTHALL,
    FSOUND_ENVIRONMENT_CAVE,
    FSOUND_ENVIRONMENT_ARENA,
    FSOUND_ENVIRONMENT_HANGAR,
    FSOUND_ENVIRONMENT_CARPETEDHALLWAY,
    FSOUND_ENVIRONMENT_HALLWAY,
    FSOUND_ENVIRONMENT_STONECORRIDOR,
    FSOUND_ENVIRONMENT_ALLEY,
    FSOUND_ENVIRONMENT_FOREST,
    FSOUND_ENVIRONMENT_CITY,
    FSOUND_ENVIRONMENT_MOUNTAINS,
    FSOUND_ENVIRONMENT_QUARRY,
    FSOUND_ENVIRONMENT_PLAIN,
    FSOUND_ENVIRONMENT_PARKINGLOT,
    FSOUND_ENVIRONMENT_SEWERPIPE,
    FSOUND_ENVIRONMENT_UNDERWATER,
    FSOUND_ENVIRONMENT_DRUGGED,
    FSOUND_ENVIRONMENT_DIZZY,
    FSOUND_ENVIRONMENT_PSYCHOTIC,

    FSOUND_ENVIRONMENT_COUNT
  );

  FSOUND_REVERB_ENVIRONMENTS = TFSoundReverbEnvironments;

{
[DEFINE_START]
[
  [NAME]
  FSOUND_REVERBMIX_USEDISTANCE

  [DESCRIPTION]
  Used with FSOUND_Reverb_SetMix, this setting allows reverb to attenuate based on distance from the listener.
  Instead of hard coding a value with FSOUND_Reverb_SetMix, this value can be used instead, for a more natural
  reverb dropoff.

  [SEE_ALSO]
  FSOUND_Reverb_SetMix
]
}
const
  FSOUND_REVERBMIX_USEDISTANCE = -1.0; // used with FSOUND_Reverb_SetMix to scale reverb by distance
// [DEFINE_END]


{
[DEFINE_START]
[
  [NAME]
  FSOUND_REVERB_IGNOREPARAM

  [DESCRIPTION]
  Used with FSOUND_Reverb_SetEnvironment and FSOUND_Reverb_SetEnvironmentAdvanced, this can
  be placed in the place of a specific parameter for the reverb setting. It allows you to
  not set any parameters except the ones you are interested in .. and example would be this.
  FSOUND_Reverb_SetEnvironment(FSOUND_REVERB_IGNOREPARAM,
  FSOUND_REVERB_IGNOREPARAM,
  FSOUND_REVERB_IGNOREPARAM,
  0.0f);
  This means env, vol and decay are left alone, but 'damp' is set to 0.

  [SEE_ALSO]
  FSOUND_Reverb_SetEnvironment
  FSOUND_Reverb_SetEnvironmentAdvanced
]
}
const
  FSOUND_REVERB_IGNOREPARAM = -9999999; // used with FSOUND_Reverb_SetEnvironmentAdvanced to ignore certain parameters by choice.
// [DEFINE_END]


{
[DEFINE_START]
[
  [NAME]
  FSOUND_REVERB_PRESETS

  [DESCRIPTION]
  A set of predefined environment PARAMETERS, created by Creative Labs
  These can be placed directly into the FSOUND_Reverb_SetEnvironment call

  [SEE_ALSO]
  FSOUND_Reverb_SetEnvironment
]
}
{
const
  FSOUND_PRESET_OFF = FSOUND_ENVIRONMENT_GENERIC, 0.0, 0.0 f, 0.0 f;
  FSOUND_PRESET_GENERIC = FSOUND_ENVIRONMENT_GENERIC, 0.5, 1.493 f, 0.5 f;
  FSOUND_PRESET_PADDEDCELL = FSOUND_ENVIRONMENT_PADDEDCELL, 0.25, 0.1 f, 0.0 f;
  FSOUND_PRESET_ROOM = FSOUND_ENVIRONMENT_ROOM, 0.417, 0.4 f, 0.666 f;
  FSOUND_PRESET_BATHROOM = FSOUND_ENVIRONMENT_BATHROOM, 0.653, 1.499 f, 0.166 f;
  FSOUND_PRESET_LIVINGROOM = FSOUND_ENVIRONMENT_LIVINGROOM, 0.208, 0.478 f, 0.0 f;
  FSOUND_PRESET_STONEROOM = FSOUND_ENVIRONMENT_STONEROOM, 0.5, 2.309 f, 0.888 f;
  FSOUND_PRESET_AUDITORIUM = FSOUND_ENVIRONMENT_AUDITORIUM, 0.403, 4.279 f, 0.5 f;
  FSOUND_PRESET_CONCERTHALL = FSOUND_ENVIRONMENT_CONCERTHALL, 0.5, 3.961 f, 0.5 f;
  FSOUND_PRESET_CAVE = FSOUND_ENVIRONMENT_CAVE, 0.5, 2.886 f, 1.304 f;
  FSOUND_PRESET_ARENA = FSOUND_ENVIRONMENT_ARENA, 0.361, 7.284 f, 0.332 f;
  FSOUND_PRESET_HANGAR = FSOUND_ENVIRONMENT_HANGAR, 0.5, 10.0 f, 0.3 f;
  FSOUND_PRESET_CARPETEDHALLWAY = FSOUND_ENVIRONMENT_CARPETEDHALLWAY, 0.153, 0.259 f, 2.0 f;
  FSOUND_PRESET_HALLWAY = FSOUND_ENVIRONMENT_HALLWAY, 0.361, 1.493 f, 0.0 f;
  FSOUND_PRESET_STONECORRIDOR = FSOUND_ENVIRONMENT_STONECORRIDOR, 0.444, 2.697 f, 0.638 f;
  FSOUND_PRESET_ALLEY = FSOUND_ENVIRONMENT_ALLEY, 0.25, 1.752 f, 0.776 f;
  FSOUND_PRESET_FOREST = FSOUND_ENVIRONMENT_FOREST, 0.111, 3.145 f, 0.472 f;
  FSOUND_PRESET_CITY = FSOUND_ENVIRONMENT_CITY, 0.111, 2.767 f, 0.224 f;
  FSOUND_PRESET_MOUNTAINS = FSOUND_ENVIRONMENT_MOUNTAINS, 0.194, 7.841 f, 0.472 f;
  FSOUND_PRESET_QUARRY = FSOUND_ENVIRONMENT_QUARRY, 1.0, 1.499 f, 0.5 f;
  FSOUND_PRESET_PLAIN = FSOUND_ENVIRONMENT_PLAIN, 0.097, 2.767 f, 0.224 f;
  FSOUND_PRESET_PARKINGLOT = FSOUND_ENVIRONMENT_PARKINGLOT, 0.208, 1.652 f, 1.5 f;
  FSOUND_PRESET_SEWERPIPE = FSOUND_ENVIRONMENT_SEWERPIPE, 0.652, 2.886 f, 0.25 f;
  FSOUND_PRESET_UNDERWATER = FSOUND_ENVIRONMENT_UNDERWATER, 1.0, 1.499 f, 0.0 f;
  FSOUND_PRESET_DRUGGED = FSOUND_ENVIRONMENT_DRUGGED, 0.875, 8.392 f, 1.388 f;
  FSOUND_PRESET_DIZZY = FSOUND_ENVIRONMENT_DIZZY, 0.139, 17.234 f, 0.666 f;
  FSOUND_PRESET_PSYCHOTIC = FSOUND_ENVIRONMENT_PSYCHOTIC, 0.486, 7.563 f, 0.806 f;
}
// [DEFINE_END]


{
[DEFINE_START]
[
  [NAME]
  FSOUND_GEOMETRY_MODES

  [DESCRIPTION]
  Geometry flags, used as the mode flag in FSOUND_Geometry_AddPolygon

  [SEE_ALSO]
  FSOUND_Geometry_AddPolygon
]
}
const
  FSOUND_GEOMETRY_NORMAL = $0; // Default geometry type. Occluding polygon
  FSOUND_GEOMETRY_REFLECTIVE = $01; // This polygon is reflective
  FSOUND_GEOMETRY_OPENING = $02; // Overlays a transparency over the previous polygon. The 'openingfactor' value supplied is copied internally.
  FSOUND_GEOMETRY_OPENING_REFERENCE = $04; // Overlays a transparency over the previous polygon. The 'openingfactor' supplied is pointed to (for access when building a list)
// [DEFINE_END]

{
[DEFINE_START]
[
 	[NAME]
	FSOUND_FX_MODES

	[DESCRIPTION]
    These flags are used with FSOUND_FX_Enable to enable or disable DirectX 8 FX for a channel.

	[SEE_ALSO]
    FSOUND_FX_Enable
    FSOUND_FX_SetChorus
    FSOUND_FX_SetCompressor
    FSOUND_FX_SetDistortion
    FSOUND_FX_SetEcho
    FSOUND_FX_SetFlanger
    FSOUND_FX_SetGargle
    FSOUND_FX_SetI3DL2Reverb
    FSOUND_FX_SetParamEQ
    FSOUND_FX_SetWavesReverb
]
}
const
  FSOUND_FX_CHORUS                  = $001;
  FSOUND_FX_COMPRESSOR              = $002;
  FSOUND_FX_DISTORTION              = $004;
  FSOUND_FX_ECHO                    = $008;
  FSOUND_FX_FLANGER                 = $010;
  FSOUND_FX_GARGLE                  = $020;
  FSOUND_FX_I3DL2REVERB             = $040;
  FSOUND_FX_PARAMEQ                 = $080;
  FSOUND_FX_WAVES_REVERB            = $100;
// [DEFINE_END]


{
[ENUM]
[
	[DESCRIPTION]
	These are speaker types defined for use with the FSOUND_SetSpeakerMode command.

	[SEE_ALSO]
    FSOUND_SetSpeakerMode

    [REMARKS]
    Only works with FSOUND_OUTPUT_DSOUND output mode.
]
}
type
  TFSoundSpeakerModes =
  (
    FSOUND_SPEAKERMODE_5POINT1,       // The audio is played through a speaker arrangement of surround speakers with a subwoofer.
    FSOUND_SPEAKERMODE_HEADPHONE,     // The speakers are headphones.
    FSOUND_SPEAKERMODE_MONO,          // The speakers are monaural.
    FSOUND_SPEAKERMODE_QUAD,          // The speakers are quadraphonic.
    FSOUND_SPEAKERMODE_STEREO,        // The speakers are stereo (default value).
    FSOUND_SPEAKERMODE_SURROUND       // The speakers are surround sound.
  );
  FSOUND_SPEAKERMODES = TFSoundSpeakerModes;


{
[DEFINE_START]
[
  [NAME]
  FSOUND_INIT_FLAGS

  [DESCRIPTION]
  Initialization flags. Use them with FSOUND_Init in the flags parameter to change various behaviour.

  [SEE_ALSO]
  FSOUND_Init
]
}
const
  FSOUND_INIT_USEDEFAULTMIDISYNTH = $01; // Causes MIDI playback to force software decoding.
  FSOUND_INIT_GLOBALFOCUS = $02;         // For DirectSound output - sound is not muted when window is out of focus.
  FSOUND_INIT_ENABLEOUTPUTFX = $04;      // For DirectSound output - Allows FSOUND_FX api to be used on global software mixer output!
// [DEFINE_END]

//===============================================================================================
// FUNCTION PROTOTYPES
//===============================================================================================

{ ================================== }
{ Initialization / Global functions. }
{ ================================== }

{
  Pre FSOUND_Init functions. These can't be called after FSOUND_Init is
  called (they will fail). They set up FMOD system functionality.
}

function FSOUND_SetOutput(OutputType: TFSoundOutputTypes): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetDriver(Driver: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetMixer(Mixer: TFSoundMixerTypes): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetBufferSize(LenMs: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetHWND(Hwnd: THandle): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetMinHardwareChannels(Min: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetMaxHardwareChannels(Max: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Main initialization / closedown functions
  Note : Use FSOUND_INIT_USEDEFAULTMIDISYNTH with FSOUND_Init for software override with MIDI playback.
       : Use FSOUND_INIT_GLOBALFOCUS with FSOUND_Init to make sound audible no matter which window is in focus.
}

function FSOUND_Init(MixRate: Integer; MaxSoftwareChannels: Integer; Flags: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_Close; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Runtime system level functions
}

procedure FSOUND_SetSpeakerMode(SpeakerMode: Cardinal); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_SetSFXMasterVolume(Volume: Integer); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_SetPanSeperation(PanSep: Single); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_File_SetCallbacks(
        OpenCallback: TFSoundOpenCallback;
        CloseCallback: TFSoundCloseCallback;
        ReadCallback: TFSoundReadCallback;
        SeekCallback: TFSoundSeekCallback;
        TellCallback: TFSoundTellCallback); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{
  System information functions
}

function FSOUND_GetError: TFModErrors; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetVersion: Single; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetOutput: TFSoundOutputTypes; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetOutputHandle: Pointer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetDriver: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetMixer: TFSoundMixerTypes; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetNumDrivers: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetDriverName(Id: Integer): PChar; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetDriverCaps(Id: Integer; var Caps: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

function FSOUND_GetOutputRate: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetMaxChannels: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetMaxSamples: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetSFXMasterVolume: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetNumHardwareChannels: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetChannelsPlaying: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetCPUUsage: Single; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{ =================================== }
{ Sample management / load functions. }
{ =================================== }

{
  Sample creation and management functions
  Note : Use FSOUND_LOADMEMORY   flag with FSOUND_Sample_Load to load from memory.
         Use FSOUND_LOADRAW      flag with FSOUND_Sample_Load to treat as as raw pcm data.
}

function FSOUND_Sample_Load(Index: Integer; const Name: PChar; Mode: Cardinal; MemLength: Integer): PFSoundSample; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_Alloc(Index: Integer;
  Length: Integer;
  Mode: Cardinal;
  DefFreq: Integer;
  DefVol: Integer;
  DefPan: Integer;
  DefPri: Integer): PFSoundSample; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_Sample_Free(Sptr: PFSoundSample); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_Upload(Sptr: PFSoundSample; SrcData: Pointer; Mode: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_Lock(Sptr: PFSoundSample;
  Offset: Integer;
  Length: Integer;
  var Ptr1: Pointer;
  var Ptr2: Pointer;
  var Len1: Cardinal;
  var Len2: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_Unlock(Sptr: PFSoundSample;
  Ptr1: Pointer;
  Ptr2: Pointer;
  Len1: Cardinal;
  Len2: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Sample control functions
}

function FSOUND_Sample_SetLoopMode(Sptr: PFSoundSample; LoopMode: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_SetLoopPoints(Sptr: PFSoundSample; LoopStart, LoopEnd: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_SetDefaults(Sptr: PFSoundSample; DefFreq, DefVol, DefPan, DefPri: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_SetMinMaxDistance(Sptr: PFSoundSample; Min, Max: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Sample information functions
}

function FSOUND_Sample_Get(SampNo: Integer): PFSoundSample; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_GetName(Sptr: PFSoundSample): PCHAR; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_GetLength(Sptr: PFSoundSample): Cardinal; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_GetLoopPoints(Sptr: PFSoundSample;
  var LoopStart: Integer; var LoopEnd: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_GetDefaults(Sptr: PFSoundSample;
  var DefFreq: Integer;
  var DefVol: Integer;
  var DefPan: Integer;
  var DefPri: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_GetMode(Sptr: PFSoundSample): Cardinal; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{ ============================ }
{ Channel control functions.   }
{ ============================ }

{
  Playing and stopping sounds.
}

function FSOUND_PlaySound(Channel: Integer; Sptr: PFSoundSample): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_PlaySoundEx(Channel: Integer; Sptr: PFSoundSample; Dsp: PFSoundDSPUnit; StartPaused: ByteBool): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_StopSound(Channel: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Functions to control playback of a channel.
}

function FSOUND_SetFrequency(Channel: Integer; Freq: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetVolume(Channel: Integer; Vol: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetVolumeAbsolute(Channel: Integer; Vol: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetPan(Channel: Integer; Pan: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetSurround(Channel: Integer; Surround: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetMute(Channel: Integer; Mute: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetPriority(Channel: Integer; Priority: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetReserved(Channel: Integer; Reserved: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetPaused(Channel: Integer; Paused: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetLoopMode(Channel: Integer; LoopMode: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetCurrentPosition(Channel: Integer; Offset: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{
    Functions to control DX8 only effects processing.
    Note that FX enabled samples can only be played once at a time.
}

function FSOUND_FX_Enable(Channel: integer; Fx: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};    { Set bits to enable following fx }
function FSOUND_FX_SetChorus(Channel: Integer; WetDryMix, Depth, Feedback, Frequency: Single; Waveform: Integer; Delay: Single; Phase: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetCompressor(Channel: Integer; Gain, Attack, Release, Threshold, Ratio, Predelay: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetDistortion(Channel: Integer; Gain, Edge, PostEQCenterFrequency, PostEQBandwidth, PreLowpassCutoff: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetEcho(Channel: Integer; WetDryMix, Feedback, LeftDelay, RightDelay: Single; PanDelay: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetFlanger(Channel: Integer; WetDryMix, Depth, Feedback, Frequency: Single; Waveform: Integer; Delay: Single; Phase: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetGargle(Channel, RateHz, WaveShape: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetI3DL2Reverb(Channel, Room, RoomHF: Integer; RoomRolloffFactor, DecayTime, DecayHFRatio: Single; Reflections: Integer; ReflectionsDelay: Single; Reverb: Integer; ReverbDelay, Diffusion, Density, HFReference: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetParamEQ(Channel: Integer; Center, Bandwidth, Gain: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetWavesReverb(Channel: Integer; InGain, ReverbMix, ReverbTime, HighFreqRTRatio: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Channel information functions
}

function FSOUND_IsPlaying(Channel: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetFrequency(Channel: Integer): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetVolume(Channel: Integer): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetPan(Channel: Integer): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetSurround(Channel: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetMute(Channel: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetPriority(Channel: Integer): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetReserved(Channel: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetPaused(Channel: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetCurrentPosition(Channel: Integer): Cardinal; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetCurrentSample(Channel: Integer): PFSoundSample; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetCurrentVU(Channel: Integer): Single; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{ =================== }
{ 3D sound functions. }
{ =================== }

{
  See also FSOUND_Sample_SetMinMaxDistance (above)
}

procedure FSOUND_3D_Update; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_3D_SetAttributes(Channel: Integer;
  Pos: PFSoundVector;
  Vel: PFSoundVector): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_3D_GetAttributes(Channel: Integer;
  Pos: PFSoundVector;
  Vel: PFSoundVector): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_3D_Listener_SetAttributes(Pos: PFSoundVector;
  Vel: PFSoundVector;
  fx: Single;
  fy: Single;
  fz: Single;
  tx: Single;
  ty: Single;
  tz: Single); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_3D_Listener_GetAttributes(Pos: PFSoundVector;
  Vel: PFSoundVector;
  fx: PSingle;
  fy: PSingle;
  fz: PSingle;
  tx: PSingle;
  ty: PSingle;
  tz: PSingle); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_3D_Listener_SetDopplerFactor(Scale: Single); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_3D_Listener_SetDistanceFactor(Scale: Single); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_3D_Listener_SetRolloffFactor(Scale: Single); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{ ========================= }
{ File Streaming functions. }
{ ========================= }

{
    Note : Use FSOUND_LOADMEMORY   flag with FSOUND_Stream_OpenFile to stream from memory.
           Use FSOUND_LOADRAW      flag with FSOUND_Stream_OpenFile to treat stream as raw pcm data.
           Use FSOUND_MPEGACCURATE flag with FSOUND_Stream_OpenFile to open mpegs in 'accurate mode' for settime/gettime/getlengthms.
}

function FSOUND_Stream_OpenFile(const Filename: PChar; Mode: Cardinal;
  MemLength: Integer): PFSoundStream; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_Create(Callback: TFSoundStreamCallback;
  Length: Integer;
  Mode: Cardinal;
  SampleRate: Integer;
  UserData: Integer): PFSoundStream; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_Play(Channel: Integer; Stream: PFSoundStream): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_PlayEx(Channel: Integer; Stream: PFSoundStream; Dsp: PFSoundDSPUnit; StartPaused: ByteBool): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_Stop(Stream: PFSoundStream): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_Close(Stream: PFSoundStream): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_SetEndCallback(Stream: PFSoundStream;
  Callback: TFSoundStreamCallback; UserData: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_SetSynchCallback(Stream: PFSoundStream;
  Callback: TFSoundStreamCallback; UserData: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetSample(Stream: PFSoundStream): PFSOUND_SAMPLE; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF}; { Every stream contains a sample to play back on }
function FSOUND_Stream_CreateDSP(Stream: PFSoundStream; Callback: TFSoundDSPCallback;
  Priority: Integer; Param: Integer): PFSoundDSPUnit; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

function FSOUND_Stream_SetPosition(Stream: PFSoundStream; Position: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetPosition(Stream: PFSoundStream): Cardinal; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_SetTime(Stream: PFSoundStream; Ms: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetTime(Stream: PFSoundStream): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetLength(Stream: PFSoundStream): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetLengthMs(Stream: PFSoundStream): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{ =================== }
{ CD audio functions. }
{ =================== }

function FSOUND_CD_Play(Track: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_CD_SetPlayMode(Mode: Integer); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_Stop: ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

function FSOUND_CD_SetPaused(Paused: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_SetVolume(Volume: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_Eject: ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

function FSOUND_CD_GetPaused: ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_GetTrack: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_GetNumTracks: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_GetVolume: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_GetTrackLength(Track: Integer): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_GetTrackTime: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{ ============== }
{ DSP functions. }
{ ============== }

{
  DSP Unit control and information functions.
}

function FSOUND_DSP_Create(Callback: TFSoundDSPCallback;
  Priority: Integer; Param: Integer): PFSoundDSPUnit; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_DSP_Free(DSPUnit: PFSoundDSPUnit); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_DSP_SetPriority(DSPUnit: PFSoundDSPUnit; Priority: Integer); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetPriority(DSPUnit: PFSoundDSPUnit): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_DSP_SetActive(DSPUnit: PFSoundDSPUnit; Active: ByteBool); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetActive(DSPUnit: PFSoundDSPUnit): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Functions to get hold of FSOUND 'system DSP unit' handles.
}

function FSOUND_DSP_GetClearUnit: PFSoundDSPUnit; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetSFXUnit: PFSoundDSPUnit; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetMusicUnit: PFSoundDSPUnit; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetClipAndCopyUnit: PFSoundDSPUnit; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetFFTUnit: PFSoundDSPUnit;

{
  Miscellaneous DSP functions
}

function FSOUND_DSP_MixBuffers(DestBuffer: Pointer;
  SrcBuffer: Pointer;
  Len: Integer;
  Freq: Integer;
  Vol: Integer;
  Pan: Integer;
  Mode: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_DSP_ClearMixBuffer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetBufferLength: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};      { Length of each DSP update }
function FSOUND_DSP_GetBufferLengthTotal: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF}; { Total buffer length due to FSOUND_SetBufferSize }
function FSOUND_DSP_GetSpectrum: PSingle; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};          { Array of 512 floats }

{ ================================================ }
{ Geometry functions.  (NOT SUPPORTED IN LINUX/CE) }
{ ================================================ }

{
  Scene/polygon functions
}

function FSOUND_Geometry_AddPolygon(P1: PFSoundVector;
  P2: PFSoundVector;
  P3: PFSoundVector;
  P4: PFSoundVector;
  Normal: PFSoundVector;
  Mode: Cardinal;
  OpeningFactor: PSingle): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Geometry_AddList(GeomList: PFSoundGeomList): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Polygon list functions
}

function FSOUND_Geometry_List_Create(BoundingVolume: ByteBool): PFSoundGeomList; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Geometry_List_Free(GeomList: PFSoundGeomList): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Geometry_List_Begin(GeomList: PFSoundGeomList): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Geometry_List_End(GeomList: PFSoundGeomList): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Geometry_List_Add(GeomList: PFSoundGeomList): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Material functions
}

function FSOUND_Geometry_Material_Create: PFSoundMaterial; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Geometry_Material_Free(Material: PFSoundMaterial): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Geometry_Material_SetAttributes(Material: PFSoundMaterial;
  ReflectanceGain: Single;
  ReflectanceFreq: Single;
  TransmittanceGain: Single;
  TransmittanceFreq: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Geometry_Material_GetAttributes(Material: PFSoundMaterial;
  var ReflectanceGain: Single;
  var ReflectanceFreq: Single;
  var TransmittanceGain: Single;
  var TransmittanceFreq: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Geometry_Material_Set(Material: PFSoundMaterial): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{ ========================================================================== }
{ Reverb functions. (eax, eax2, a3d 3.0 reverb)  (NOT SUPPORTED IN LINUX/CE) }
{ ========================================================================== }

{
    Supporing EAX1, EAX2, A3D 3.0 (use FSOUND_REVERB_PRESETS if you like),
    (EAX2 support through emulation/parameter conversion)
}

function FSOUND_Reverb_SetEnvironment(Env: TFSoundReverbEnvironments; Vol: Single; Decay: Single; Damp: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{
    Supporting EAX2, A3D 3.0 only.  Has no effect on EAX1
}

function FSOUND_Reverb_SetEnvironmentAdvanced(Env: TFSoundReverbEnvironments;
  Room: Integer;
  RoomHF: Integer;
  RoomRolloffFactor: Single;
  DecayTime: Single;
  DecayHFRatio: Single;
  Reflections: Integer;
  ReflectionsDelay: Single;
  Reverb: Integer;
  ReverbDelay: Single;
  Environment: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Reverb_SetMix(Channel: Integer; Mix: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Reverb information functions
}

function FSOUND_Reverb_GetEnvironment(var Env: TFSoundReverbEnvironments;
  var Vol: Single; var Decay: Single; var Damp: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Reverb_GetEnvironmentAdvanced(var Env: TFSoundReverbEnvironments;
  var Room: Integer;
  var RoomHF: Integer;
  var RoomRolloffFactor: Single;
  var DecayTime: Single;
  var DecayHFRatio: Single;
  var Reflections: Integer;
  var ReflectionsDelay: Single;
  var Reverb: Integer;
  var ReverbDelay: Single;
  var EnvironmentSize: Single;
  var EnvironmentDiffusion: Single;
  var AirAbsorptionHF: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Reverb_GetMix(Channel: Integer; var Mix: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{ ================================================ }
{ Recording functions  (NOT SUPPORTED IN LINUX/CE) }
{ ================================================ }

{
  Recording initialization functions
}

function FSOUND_Record_SetDriver(OutputType: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Record_GetNumDrivers: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Record_GetDriverName(Id: Integer): PChar; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Record_GetDriver: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Recording functionality. Only one recording session will work at a time.
}

function FSOUND_Record_StartSample(Sptr: PFSoundSample; Loop: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Record_Stop: ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Record_GetPosition: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{ ============================================================================================= }
{ FMUSIC API (MOD,S3M,XM,IT,MIDI PLAYBACK)                                                      }
{ ============================================================================================= }

{
  Song management / playback functions.
}

function FMUSIC_LoadSong(const Name: PChar): PFMUSIC_MODULE; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_LoadSongMemory(Data: Pointer; Length: Integer): PFMusicModule; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_FreeSong(Module: PFMusicModule): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_PlaySong(Module: PFMusicModule): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_StopSong(Module: PFMusicModule): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FMUSIC_StopAllSongs; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

function FMUSIC_SetZxxCallback(Module: PFMusicModule; Callback: TFMusicCallback): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetRowCallback(Module: PFMusicModule; Callback: TFMusicCallback; RowStep: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetOrderCallback(Module: PFMusicModule; Callback: TFMusicCallback; OrderStep: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetInstCallback(Module: PFMusicModule; Callback: TFMusicCallback; Instrument: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

function FMUSIC_SetSample(Module: PFMusicModule; SampNo: Integer; Sptr: PFSoundSample): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_OptimizeChannels(Module: PFMusicModule; MaxChannels: Integer; MinVolume: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Runtime song functions.
}

function FMUSIC_SetReverb(Reverb: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetOrder(Module: PFMusicModule; Order: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetPaused(Module: PFMusicModule; Pause: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetMasterVolume(Module: PFMusicModule; Volume: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetPanSeperation(Module: PFMusicModule; PanSep: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Static song information functions.
}

function FMUSIC_GetName(Module: PFMusicModule): PCHAR; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetType(Module: PFMusicModule): TFMusicTypes; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetNumOrders(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetNumPatterns(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetNumInstruments(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetNumSamples(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetNumChannels(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetSample(Module: PFMusicModule; SampNo: Integer): PFSoundSample; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetPatternLength(Module: PFMusicModule; OrderNo: Integer): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Runtime song information.
}

function FMUSIC_IsFinished(Module: PFMusicModule): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_IsPlaying(Module: PFMusicModule): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetMasterVolume(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetGlobalVolume(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetOrder(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetPattern(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetSpeed(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetBPM(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetRow(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetPaused(Module: PFMusicModule): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetTime(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

implementation

const
{$IFDEF LINUX}
  FMOD_DLL = 'libfmod-3.33.so';
{$ELSE}
  FMOD_DLL = 'fmod.dll';
{$ENDIF}

function FSOUND_SetOutput; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetOutput@4' {$ENDIF};
function FSOUND_SetDriver; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetDriver@4' {$ENDIF};
function FSOUND_SetMixer; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetMixer@4' {$ENDIF};
function FSOUND_SetBufferSize; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetBufferSize@4' {$ENDIF};
function FSOUND_SetHWND; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetHWND@4' {$ENDIF};
function FSOUND_SetMinHardwareChannels; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetMinHardwareChannels@4' {$ENDIF};
function FSOUND_SetMaxHardwareChannels; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetMaxHardwareChannels@4' {$ENDIF};
function FSOUND_Init; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Init@12' {$ENDIF};
procedure FSOUND_Close; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Close@0' {$ENDIF};
procedure FSOUND_SetSFXMasterVolume; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetSFXMasterVolume@4' {$ENDIF};
procedure FSOUND_SetPanSeperation; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetPanSeperation@4' {$ENDIF};
procedure FSOUND_SetSpeakerMode; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetSpeakerMode@4' {$ENDIF};
function FSOUND_GetError; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetError@0' {$ENDIF};
function FSOUND_GetVersion; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetVersion@0' {$ENDIF};
function FSOUND_GetOutput; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetOutput@0' {$ENDIF};
function FSOUND_GetOutputHandle; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetOutputHandle@0' {$ENDIF};
function FSOUND_GetDriver; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetDriver@0' {$ENDIF};
function FSOUND_GetMixer; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetMixer@0' {$ENDIF};
function FSOUND_GetNumDrivers; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetNumDrivers@0' {$ENDIF};
function FSOUND_GetDriverName; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetDriverName@4' {$ENDIF};
function FSOUND_GetDriverCaps; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetDriverCaps@8' {$ENDIF};
function FSOUND_GetOutputRate; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetOutputRate@0' {$ENDIF};
function FSOUND_GetMaxChannels; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetMaxChannels@0' {$ENDIF};
function FSOUND_GetMaxSamples; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetMaxSamples@0' {$ENDIF};
function FSOUND_GetSFXMasterVolume; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetSFXMasterVolume@0' {$ENDIF};
function FSOUND_GetNumHardwareChannels; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetNumHardwareChannels@0' {$ENDIF};
function FSOUND_GetChannelsPlaying; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetChannelsPlaying@0' {$ENDIF};
function FSOUND_GetCPUUsage; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetCPUUsage@0' {$ENDIF};
function FSOUND_Sample_Load; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_Load@16' {$ENDIF};
function FSOUND_Sample_Alloc; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_Alloc@28' {$ENDIF};
procedure FSOUND_Sample_Free; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_Free@4' {$ENDIF};
function FSOUND_Sample_Upload; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_Upload@12' {$ENDIF};
function FSOUND_Sample_Lock; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_Lock@28' {$ENDIF};
function FSOUND_Sample_Unlock; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_Unlock@20' {$ENDIF};
function FSOUND_Sample_SetLoopMode; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_SetLoopMode@8' {$ENDIF};
function FSOUND_Sample_SetLoopPoints; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_SetLoopPoints@12' {$ENDIF};
function FSOUND_Sample_SetDefaults; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_SetDefaults@20' {$ENDIF};
function FSOUND_Sample_SetMinMaxDistance; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_SetMinMaxDistance@12' {$ENDIF};
function FSOUND_Sample_Get; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_Get@4' {$ENDIF};
function FSOUND_Sample_GetName; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_GetName@4' {$ENDIF};
function FSOUND_Sample_GetLength; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_GetLength@4' {$ENDIF};
function FSOUND_Sample_GetLoopPoints; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_GetLoopPoints@12' {$ENDIF};
function FSOUND_Sample_GetDefaults; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_GetDefaults@20' {$ENDIF};
function FSOUND_Sample_GetMode; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_GetMode@4' {$ENDIF};
function FSOUND_PlaySound; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_PlaySound@8' {$ENDIF};
function FSOUND_PlaySoundEx; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_PlaySoundEx@16' {$ENDIF};
function FSOUND_StopSound; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_StopSound@4' {$ENDIF};
function FSOUND_SetFrequency; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetFrequency@8' {$ENDIF};
function FSOUND_SetVolume; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetVolume@8' {$ENDIF};
function FSOUND_SetVolumeAbsolute; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetVolumeAbsolute@8' {$ENDIF};
function FSOUND_SetPan; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetPan@8' {$ENDIF};
function FSOUND_SetSurround; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetSurround@8' {$ENDIF};
function FSOUND_SetMute; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetMute@8' {$ENDIF};
function FSOUND_SetPriority; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetPriority@8' {$ENDIF};
function FSOUND_SetReserved; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetReserved@8' {$ENDIF};
function FSOUND_SetPaused; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetPaused@8' {$ENDIF};
function FSOUND_SetLoopMode; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetLoopMode@8' {$ENDIF};
function FSOUND_IsPlaying; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_IsPlaying@4' {$ENDIF};
function FSOUND_GetFrequency; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetFrequency@4' {$ENDIF};
function FSOUND_GetVolume; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetVolume@4' {$ENDIF};
function FSOUND_GetPan; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetPan@4' {$ENDIF};
function FSOUND_GetSurround; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetSurround@4' {$ENDIF};
function FSOUND_GetMute; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetMute@4' {$ENDIF};
function FSOUND_GetPriority; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetPriority@4' {$ENDIF};
function FSOUND_GetReserved; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetReserved@4' {$ENDIF};
function FSOUND_GetPaused; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetPaused@4' {$ENDIF};
function FSOUND_GetCurrentPosition; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetCurrentPosition@4' {$ENDIF};
function FSOUND_SetCurrentPosition; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetCurrentPosition@8' {$ENDIF};
function FSOUND_GetCurrentSample; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetCurrentSample@4' {$ENDIF};
function FSOUND_GetCurrentVU; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetCurrentVU@4' {$ENDIF};
function FSOUND_FX_Enable; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_FX_Enable@8' {$ENDIF};
function FSOUND_FX_SetChorus; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_FX_SetChorus@32' {$ENDIF};
function FSOUND_FX_SetCompressor; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_FX_SetCompressor@28' {$ENDIF};
function FSOUND_FX_SetDistortion; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_FX_SetDistortion@24' {$ENDIF};
function FSOUND_FX_SetEcho; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_FX_SetEcho@24' {$ENDIF};
function FSOUND_FX_SetFlanger; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_FX_SetFlanger@32' {$ENDIF};
function FSOUND_FX_SetGargle; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_FX_SetGargle@12' {$ENDIF};
function FSOUND_FX_SetI3DL2Reverb; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_FX_SetI3DL2Reverb@52' {$ENDIF};
function FSOUND_FX_SetParamEq; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_FX_SetParamEq@16' {$ENDIF};
function FSOUND_FX_SetWavesReverb; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_FX_SetWavesReverb@20' {$ENDIF};
procedure FSOUND_3D_Update; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_3D_Update@0' {$ENDIF};
function FSOUND_3D_SetAttributes; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_3D_SetAttributes@12' {$ENDIF};
function FSOUND_3D_GetAttributes; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_3D_GetAttributes@12' {$ENDIF};
procedure FSOUND_3D_Listener_SetAttributes; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_3D_Listener_SetAttributes@32' {$ENDIF};
procedure FSOUND_3D_Listener_GetAttributes; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_3D_Listener_GetAttributes@32' {$ENDIF};
procedure FSOUND_3D_Listener_SetDopplerFactor; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_3D_Listener_SetDopplerFactor@4' {$ENDIF};
procedure FSOUND_3D_Listener_SetDistanceFactor; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_3D_Listener_SetDistanceFactor@4' {$ENDIF};
procedure FSOUND_3D_Listener_SetRolloffFactor; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_3D_Listener_SetRolloffFactor@4' {$ENDIF};
function FSOUND_Stream_OpenFile; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_OpenFile@12' {$ENDIF};
function FSOUND_Stream_Create; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_Create@20' {$ENDIF};
function FSOUND_Stream_Play; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_Play@8' {$ENDIF};
function FSOUND_Stream_PlayEx; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_PlayEx@16' {$ENDIF};
function FSOUND_Stream_Stop; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_Stop@4' {$ENDIF};
function FSOUND_Stream_Close; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_Close@4' {$ENDIF};
function FSOUND_Stream_SetEndCallback; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_SetEndCallback@12' {$ENDIF};
function FSOUND_Stream_SetSynchCallback; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_SetSynchCallback@12' {$ENDIF};
function FSOUND_Stream_GetSample; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_GetSample@4' {$ENDIF};
function FSOUND_Stream_CreateDSP; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_CreateDSP@16' {$ENDIF};
function FSOUND_Stream_SetPosition; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_SetPosition@8' {$ENDIF};
function FSOUND_Stream_GetPosition; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_GetPosition@4' {$ENDIF};
function FSOUND_Stream_SetTime; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_SetTime@8' {$ENDIF};
function FSOUND_Stream_GetTime; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_GetTime@4' {$ENDIF};
function FSOUND_Stream_GetLength; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_GetLength@4' {$ENDIF};
function FSOUND_Stream_GetLengthMs; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_GetLengthMs@4' {$ENDIF};
function FSOUND_CD_Play; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_Play@4' {$ENDIF};
procedure FSOUND_CD_SetPlayMode; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_SetPlayMode@4' {$ENDIF};
function FSOUND_CD_Stop; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_Stop@0' {$ENDIF};
function FSOUND_CD_SetPaused; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_SetPaused@4' {$ENDIF};
function FSOUND_CD_SetVolume; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_SetVolume@4' {$ENDIF};
function FSOUND_CD_Eject; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_Eject@0' {$ENDIF};
function FSOUND_CD_GetPaused; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_GetPaused@0' {$ENDIF};
function FSOUND_CD_GetTrack; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_GetTrack@0' {$ENDIF};
function FSOUND_CD_GetNumTracks; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_GetNumTracks@0' {$ENDIF};
function FSOUND_CD_GetVolume; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_GetVolume@0' {$ENDIF};
function FSOUND_CD_GetTrackLength; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_GetTrackLength@4' {$ENDIF};
function FSOUND_CD_GetTrackTime; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_GetTrackTime@0' {$ENDIF};
function FSOUND_DSP_Create; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_Create@12' {$ENDIF};
procedure FSOUND_DSP_Free; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_Free@4' {$ENDIF};
procedure FSOUND_DSP_SetPriority; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_SetPriority@8' {$ENDIF};
function FSOUND_DSP_GetPriority; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_GetPriority@4' {$ENDIF};
procedure FSOUND_DSP_SetActive; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_SetActive@8' {$ENDIF};
function FSOUND_DSP_GetActive; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_GetActive@4' {$ENDIF};
function FSOUND_DSP_GetClearUnit; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_GetClearUnit@0' {$ENDIF};
function FSOUND_DSP_GetSFXUnit; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_GetSFXUnit@0' {$ENDIF};
function FSOUND_DSP_GetMusicUnit; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_GetMusicUnit@0' {$ENDIF};
function FSOUND_DSP_GetClipAndCopyUnit; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_GetClipAndCopyUnit@0' {$ENDIF};
function FSOUND_DSP_GetFFTUnit; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_GetFFTUnit@0' {$ENDIF};
function FSOUND_DSP_MixBuffers; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_MixBuffers@28' {$ENDIF};
procedure FSOUND_DSP_ClearMixBuffer; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_ClearMixBuffer@0' {$ENDIF};
function FSOUND_DSP_GetBufferLength; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_GetBufferLength@0' {$ENDIF};
function FSOUND_DSP_GetBufferLengthTotal; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_GetBufferLengthTotal@0' {$ENDIF};
function FSOUND_DSP_GetSpectrum; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_GetSpectrum@0' {$ENDIF};
function FSOUND_Geometry_AddPolygon; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_AddPolygon@28' {$ENDIF};
function FSOUND_Geometry_AddList; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_AddList@4' {$ENDIF};
function FSOUND_Geometry_List_Create; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_List_Create@4' {$ENDIF};
function FSOUND_Geometry_List_Free; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_List_Free@4' {$ENDIF};
function FSOUND_Geometry_List_Begin; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_List_Begin@4' {$ENDIF};
function FSOUND_Geometry_List_End; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_List_End@4' {$ENDIF};
function FSOUND_Geometry_List_Add; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_List_Add@4' {$ENDIF};
function FSOUND_Geometry_Material_Create; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_Material_Create@0' {$ENDIF};
function FSOUND_Geometry_Material_Free; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_Material_Free@4' {$ENDIF};
function FSOUND_Geometry_Material_SetAttributes; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_Material_SetAttributes@20' {$ENDIF};
function FSOUND_Geometry_Material_GetAttributes; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_Material_GetAttributes@20' {$ENDIF};
function FSOUND_Geometry_Material_Set; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_Material_Set@4' {$ENDIF};
function FSOUND_Reverb_SetEnvironment; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Reverb_SetEnvironment@16' {$ENDIF};
function FSOUND_Reverb_SetEnvironmentAdvanced; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Reverb_SetEnvironmentAdvanced@52' {$ENDIF};
function FSOUND_Reverb_SetMix; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Reverb_SetMix@8' {$ENDIF};
function FSOUND_Reverb_GetEnvironment; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Reverb_GetEnvironment@16' {$ENDIF};
function FSOUND_Reverb_GetEnvironmentAdvanced; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Reverb_GetEnvironmentAdvanced@52' {$ENDIF};
function FSOUND_Reverb_GetMix; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Reverb_GetMix@8' {$ENDIF};
function FSOUND_Record_SetDriver; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Record_SetDriver@4' {$ENDIF};
function FSOUND_Record_GetNumDrivers; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Record_GetNumDrivers@0' {$ENDIF};
function FSOUND_Record_GetDriverName; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Record_GetDriverName@4' {$ENDIF};
function FSOUND_Record_GetDriver; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Record_GetDriver@0' {$ENDIF};
function FSOUND_Record_StartSample; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Record_StartSample@8' {$ENDIF};
function FSOUND_Record_Stop; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Record_Stop@0' {$ENDIF};
function FSOUND_Record_GetPosition; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Record_GetPosition@0' {$ENDIF};
procedure FSOUND_File_SetCallbacks; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_File_SetCallbacks@20' {$ENDIF};
function FMUSIC_LoadSong; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_LoadSong@4' {$ENDIF};
function FMUSIC_LoadSongMemory; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_LoadSongMemory@8' {$ENDIF};
function FMUSIC_FreeSong; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_FreeSong@4' {$ENDIF};
function FMUSIC_PlaySong; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_PlaySong@4' {$ENDIF};
function FMUSIC_StopSong; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_StopSong@4' {$ENDIF};
procedure FMUSIC_StopAllSongs; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_StopAllSongs@0' {$ENDIF};
function FMUSIC_SetZxxCallback; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_SetZxxCallback@8' {$ENDIF};
function FMUSIC_SetRowCallback; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_SetRowCallback@12' {$ENDIF};
function FMUSIC_SetOrderCallback; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_SetOrderCallback@12' {$ENDIF};
function FMUSIC_SetInstCallback; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_SetInstCallback@12' {$ENDIF};
function FMUSIC_SetSample; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_SetSample@12' {$ENDIF};
function FMUSIC_OptimizeChannels; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_OptimizeChannels@12' {$ENDIF};
function FMUSIC_SetReverb; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_SetReverb@4' {$ENDIF};
function FMUSIC_SetOrder; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_SetOrder@8' {$ENDIF};
function FMUSIC_SetPaused; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_SetPaused@8' {$ENDIF};
function FMUSIC_SetMasterVolume; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_SetMasterVolume@8' {$ENDIF};
function FMUSIC_SetPanSeperation; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_SetPanSeperation@8' {$ENDIF};
function FMUSIC_GetName; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetName@4' {$ENDIF};
function FMUSIC_GetType; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetType@4' {$ENDIF};
function FMUSIC_GetNumOrders; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetNumOrders@4' {$ENDIF};
function FMUSIC_GetNumPatterns; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetNumPatterns@4' {$ENDIF};
function FMUSIC_GetNumInstruments; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetNumInstruments@4' {$ENDIF};
function FMUSIC_GetNumSamples; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetNumSamples@4' {$ENDIF};
function FMUSIC_GetNumChannels; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetNumChannels@4' {$ENDIF};
function FMUSIC_GetSample; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetSample@8' {$ENDIF};
function FMUSIC_GetPatternLength; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetPatternLength@8' {$ENDIF};
function FMUSIC_IsFinished; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_IsFinished@4' {$ENDIF};
function FMUSIC_IsPlaying; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_IsPlaying@4' {$ENDIF};
function FMUSIC_GetMasterVolume; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetMasterVolume@4' {$ENDIF};
function FMUSIC_GetGlobalVolume; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetGlobalVolume@4' {$ENDIF};
function FMUSIC_GetOrder; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetOrder@4' {$ENDIF};
function FMUSIC_GetPattern; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetPattern@4' {$ENDIF};
function FMUSIC_GetSpeed; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetSpeed@4' {$ENDIF};
function FMUSIC_GetBPM; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetBPM@4' {$ENDIF};
function FMUSIC_GetRow; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetRow@4' {$ENDIF};
function FMUSIC_GetPaused; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetPaused@4' {$ENDIF};
function FMUSIC_GetTime; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetTime@4' {$ENDIF};

var
  Saved8087CW: Word;

initialization
  Saved8087CW := Default8087CW;
  Set8087CW($133f); { Disable all fpu exceptions }

finalization
  Set8087CW(Saved8087CW);

end.

