// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'fmoddyn.pas' rev: 24.00 (Win32)

#ifndef FmoddynHPP
#define FmoddynHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <fmodtypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fmoddyn
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::ByteBool __stdcall (*FSOUND_SetOutput)(Fmodtypes::TFSoundOutputTypes OutputType);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_SetDriver)(int Driver);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_SetMixer)(Fmodtypes::TFSoundMixerTypes Mixer);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_SetBufferSize)(int LenMs);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_SetHWND)(NativeUInt Hwnd);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_SetMinHardwareChannels)(int Min);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_SetMaxHardwareChannels)(int Max);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_SetMemorySystem)(void * Pool, int PoolLen, Fmodtypes::TFSoundAllocCallback UserAlloc, Fmodtypes::TFSoundReallocCallback UserRealloc, Fmodtypes::TFSoundFreeCallback UserFree);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Init)(int MixRate, int MaxSoftwareChannels, unsigned Flags);
extern PACKAGE void __stdcall (*FSOUND_Close)(void);
extern PACKAGE void __stdcall (*FSOUND_Update)(void);
extern PACKAGE void __stdcall (*FSOUND_SetSpeakerMode)(unsigned SpeakerMode);
extern PACKAGE void __stdcall (*FSOUND_SetSFXMasterVolume)(int Volume);
extern PACKAGE void __stdcall (*FSOUND_SetPanSeperation)(float PanSep);
extern PACKAGE void __stdcall (*FSOUND_File_SetCallbacks)(Fmodtypes::TFSoundOpenCallback OpenCallback, Fmodtypes::TFSoundCloseCallback CloseCallback, Fmodtypes::TFSoundReadCallback ReadCallback, Fmodtypes::TFSoundSeekCallback SeekCallback, Fmodtypes::TFSoundTellCallback TellCallback);
extern PACKAGE Fmodtypes::TFModErrors __stdcall (*FSOUND_GetError)(void);
extern PACKAGE float __stdcall (*FSOUND_GetVersion)(void);
extern PACKAGE Fmodtypes::TFSoundOutputTypes __stdcall (*FSOUND_GetOutput)(void);
extern PACKAGE void * __stdcall (*FSOUND_GetOutputHandle)(void);
extern PACKAGE int __stdcall (*FSOUND_GetDriver)(void);
extern PACKAGE Fmodtypes::TFSoundMixerTypes __stdcall (*FSOUND_GetMixer)(void);
extern PACKAGE int __stdcall (*FSOUND_GetNumDrivers)(void);
extern PACKAGE System::WideChar * __stdcall (*FSOUND_GetDriverName)(int Id);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_GetDriverCaps)(int Id, unsigned &Caps);
extern PACKAGE int __stdcall (*FSOUND_GetOutputRate)(void);
extern PACKAGE int __stdcall (*FSOUND_GetMaxChannels)(void);
extern PACKAGE int __stdcall (*FSOUND_GetMaxSamples)(void);
extern PACKAGE int __stdcall (*FSOUND_GetSpeakerMode)(void);
extern PACKAGE int __stdcall (*FSOUND_GetSFXMasterVolume)(void);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_GetNumHWChannels)(int &Num2D, int &Num3D, int &Total);
extern PACKAGE int __stdcall (*FSOUND_GetChannelsPlaying)(void);
extern PACKAGE float __stdcall (*FSOUND_GetCPUUsage)(void);
extern PACKAGE void __stdcall (*FSOUND_GetMemoryStats)(unsigned &CurrentAlloced, unsigned &MaxAlloced);
extern PACKAGE void * __stdcall (*FSOUND_Sample_Load)(int Index, const System::WideChar * NameOrData, unsigned Mode, int Offset, int Length);
extern PACKAGE void * __stdcall (*FSOUND_Sample_Alloc)(int Index, int Length, unsigned Mode, int DefFreq, int DefVol, int DefPan, int DefPri);
extern PACKAGE void __stdcall (*FSOUND_Sample_Free)(void * Sptr);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Sample_Upload)(void * Sptr, void * SrcData, unsigned Mode);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Sample_Lock)(void * Sptr, int Offset, int Length, void * &Ptr1, void * &Ptr2, unsigned &Len1, unsigned &Len2);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Sample_Unlock)(void * Sptr, void * Ptr1, void * Ptr2, unsigned Len1, unsigned Len2);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Sample_SetMode)(void * Sptr, unsigned Mode);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Sample_SetLoopPoints)(void * Sptr, int LoopStart, int LoopEnd);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Sample_SetDefaults)(void * Sptr, int DefFreq, int DefVol, int DefPan, int DefPri);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Sample_SetDefaultsEx)(void * Sptr, int DefFreq, int DefVol, int DefPan, int DefPri, int VarFreq, int VarVol, int VarPan);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Sample_SetMinMaxDistance)(void * Sptr, float Min, float Max);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Sample_SetMaxPlaybacks)(void * Sptr, int Max);
extern PACKAGE void * __stdcall (*FSOUND_Sample_Get)(int SampNo);
extern PACKAGE System::WideChar * __stdcall (*FSOUND_Sample_GetName)(void * Sptr);
extern PACKAGE unsigned __stdcall (*FSOUND_Sample_GetLength)(void * Sptr);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Sample_GetLoopPoints)(void * Sptr, int &LoopStart, int &LoopEnd);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Sample_GetDefaults)(void * Sptr, int &DefFreq, int &DefVol, int &DefPan, int &DefPri);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Sample_GetDefaultsEx)(void * Sptr, int &DefFreq, int &DefVol, int &DefPan, int &DefPri, int &VarFreq, int &VarVol, void *VarPan);
extern PACKAGE unsigned __stdcall (*FSOUND_Sample_GetMode)(void * Sptr);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Sample_GetMinMaxDistance)(void * Sptr, float &Min, float &Max);
extern PACKAGE int __stdcall (*FSOUND_PlaySound)(int Channel, void * Sptr);
extern PACKAGE int __stdcall (*FSOUND_PlaySoundEx)(int Channel, void * Sptr, void * Dsp, System::ByteBool StartPaused);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_StopSound)(int Channel);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_SetFrequency)(int Channel, int Freq);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_SetVolume)(int Channel, int Vol);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_SetVolumeAbsolute)(int Channel, int Vol);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_SetPan)(int Channel, int Pan);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_SetSurround)(int Channel, System::ByteBool Surround);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_SetMute)(int Channel, System::ByteBool Mute);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_SetPriority)(int Channel, int Priority);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_SetReserved)(int Channel, System::ByteBool Reserved);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_SetPaused)(int Channel, System::ByteBool Paused);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_SetLoopMode)(int Channel, unsigned LoopMode);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_SetCurrentPosition)(int Channel, unsigned Offset);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_3D_SetAttributes)(int Channel, Fmodtypes::PFSoundVector Pos, Fmodtypes::PFSoundVector Vel);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_3D_SetMinMaxDistance)(int Channel, float Min, float Max);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_IsPlaying)(int Channel);
extern PACKAGE int __stdcall (*FSOUND_GetFrequency)(int Channel);
extern PACKAGE int __stdcall (*FSOUND_GetVolume)(int Channel);
extern PACKAGE int __stdcall (*FSOUND_GetAmplitude)(int Channel);
extern PACKAGE int __stdcall (*FSOUND_GetPan)(int Channel);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_GetSurround)(int Channel);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_GetMute)(int Channel);
extern PACKAGE int __stdcall (*FSOUND_GetPriority)(int Channel);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_GetReserved)(int Channel);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_GetPaused)(int Channel);
extern PACKAGE unsigned __stdcall (*FSOUND_GetLoopMode)(int Channel);
extern PACKAGE unsigned __stdcall (*FSOUND_GetCurrentPosition)(int Channel);
extern PACKAGE void * __stdcall (*FSOUND_GetCurrentSample)(int Channel);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_GetCurrentLevels)(int Channel, Winapi::Windows::PSingle L, Winapi::Windows::PSingle R);
extern PACKAGE int __stdcall (*FSOUND_GetNumSubChannels)(int Channel);
extern PACKAGE int __stdcall (*FSOUND_GetSubChannel)(int Channel, int SubChannel);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_3D_GetAttributes)(int Channel, Fmodtypes::PFSoundVector Pos, Fmodtypes::PFSoundVector Vel);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_3D_GetMinMaxDistance)(int Channel, float &Min, float &Max);
extern PACKAGE void __stdcall (*FSOUND_3D_Listener_SetCurrent)(int current);
extern PACKAGE void __stdcall (*FSOUND_3D_Listener_SetAttributes)(Fmodtypes::PFSoundVector Pos, Fmodtypes::PFSoundVector Vel, float fx, float fy, float fz, float tx, float ty, float tz);
extern PACKAGE void __stdcall (*FSOUND_3D_Listener_GetAttributes)(Fmodtypes::PFSoundVector Pos, Fmodtypes::PFSoundVector Vel, Winapi::Windows::PSingle fx, Winapi::Windows::PSingle fy, Winapi::Windows::PSingle fz, Winapi::Windows::PSingle tx, Winapi::Windows::PSingle ty, Winapi::Windows::PSingle tz);
extern PACKAGE void __stdcall (*FSOUND_3D_SetDopplerFactor)(float Scale);
extern PACKAGE void __stdcall (*FSOUND_3D_SetDistanceFactor)(float Scale);
extern PACKAGE void __stdcall (*FSOUND_3D_SetRolloffFactor)(float Scale);
extern PACKAGE int __stdcall (*FSOUND_FX_Enable)(int Channel, Fmodtypes::TFSoundFXModes Fx);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_FX_Disable)(int Channel);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_FX_SetChorus)(int FXId, float WetDryMix, float Depth, float Feedback, float Frequency, int Waveform, float Delay, int Phase);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_FX_SetCompressor)(int FXId, float Gain, float Attack, float Release, float Threshold, float Ratio, float Predelay);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_FX_SetDistortion)(int FXId, float Gain, float Edge, float PostEQCenterFrequency, float PostEQBandwidth, float PreLowpassCutoff);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_FX_SetEcho)(int FXId, float WetDryMix, float Feedback, float LeftDelay, float RightDelay, int PanDelay);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_FX_SetFlanger)(int FXId, float WetDryMix, float Depth, float Feedback, float Frequency, int Waveform, float Delay, int Phase);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_FX_SetGargle)(int FXId, int RateHz, int WaveShape);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_FX_SetI3DL2Reverb)(int FXId, int Room, int RoomHF, float RoomRolloffFactor, float DecayTime, float DecayHFRatio, int Reflections, float ReflectionsDelay, int Reverb, float ReverbDelay, float Diffusion, float Density, float HFReference);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_FX_SetParamEQ)(int FXId, float Center, float Bandwidth, float Gain);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_FX_SetWavesReverb)(int FXId, float InGain, float ReverbMix, float ReverbTime, float HighFreqRTRatio);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_SetBufferSize)(int Ms);
extern PACKAGE void * __stdcall (*FSOUND_Stream_Open)(const System::WideChar * name_or_data, unsigned Mode, int Offset, int Length);
extern PACKAGE void * __stdcall (*FSOUND_Stream_Create)(Fmodtypes::TFSoundStreamCallback Callback, int Length, unsigned Mode, int SampleRate, int UserData);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_Close)(void * Stream);
extern PACKAGE int __stdcall (*FSOUND_Stream_Play)(int Channel, void * Stream);
extern PACKAGE int __stdcall (*FSOUND_Stream_PlayEx)(int Channel, void * Stream, void * Dsp, System::ByteBool StartPaused);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_Stop)(void * Stream);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_SetPosition)(void * Stream, unsigned Position);
extern PACKAGE unsigned __stdcall (*FSOUND_Stream_GetPosition)(void * Stream);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_SetTime)(void * Stream, int Ms);
extern PACKAGE int __stdcall (*FSOUND_Stream_GetTime)(void * Stream);
extern PACKAGE int __stdcall (*FSOUND_Stream_GetLength)(void * Stream);
extern PACKAGE int __stdcall (*FSOUND_Stream_GetLengthMs)(void * Stream);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_SetMode)(void * Stream, int mode);
extern PACKAGE int __stdcall (*FSOUND_Stream_GetMode)(void * Stream);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_SetLoopPoints)(void * Stream, int LoopStartPCM, int LoopEndPCM);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_SetLoopCount)(void * Stream, int Count);
extern PACKAGE int __stdcall (*FSOUND_Stream_GetOpenState)(void * Stream);
extern PACKAGE void * __stdcall (*FSOUND_Stream_GetSample)(void * Stream);
extern PACKAGE void * __stdcall (*FSOUND_Stream_CreateDSP)(void * Stream, Fmodtypes::TFSoundDSPCallback Callback, int Priority, int Param);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_SetEndCallback)(void * Stream, Fmodtypes::TFSoundStreamCallback Callback, int UserData);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_SetSyncCallback)(void * Stream, Fmodtypes::TFSoundStreamCallback Callback, int UserData);
extern PACKAGE void * __stdcall (*FSOUND_Stream_AddSyncPoint)(void * Stream, unsigned PCMOffset, System::WideChar * Name);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_DeleteSyncPoint)(void * Point);
extern PACKAGE int __stdcall (*FSOUND_Stream_GetNumSyncPoints)(void * Stream);
extern PACKAGE void * __stdcall (*FSOUND_Stream_GetSyncPoint)(void * Stream, int Index);
extern PACKAGE int __stdcall (*FSOUND_Stream_GetSyncPointInfo)(void * Point, unsigned &PCMOffset);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_SetSubStream)(void * Stream, int Index);
extern PACKAGE int __stdcall (*FSOUND_Stream_GetNumSubStreams)(void * Stream);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_SetSubStreamSentence)(void * Stream, unsigned &sentencelist, int numitems);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_GetNumTagFields)(void * Stream, int &Num);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_GetTagField)(void * Stream, int Num, Fmodtypes::TFSoundTagFieldType &_Type, System::WideChar * &Name, void * &Value, int &Length);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_FindTagField)(void * Stream, Fmodtypes::TFSoundTagFieldType _Type, System::WideChar * Name, void * &Value, int &Length);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_Net_SetProxy)(System::WideChar * Proxy);
extern PACKAGE System::WideChar * __stdcall (*FSOUND_Stream_Net_GetLastServerStatus)(void);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_Net_SetBufferProperties)(int BufferSize, int PreBuffer_Percent, int ReBuffer_Percent);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_Net_GetBufferProperties)(int &Buffersize, int &PreBuffer_Percent, int &ReBuffer_Percent);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_Net_SetMetadataCallback)(void * Stream, Fmodtypes::TFMetaDataCallback Callback, int UserData);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Stream_Net_GetStatus)(void * Stream, Fmodtypes::TFSoundStreamNetStatus &Status, int &BufferPercentUsed, int &BitRate, unsigned &Flags);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_CD_Play)(System::Byte Drive, int Track);
extern PACKAGE void __stdcall (*FSOUND_CD_SetPlayMode)(System::Byte Drive, int Mode);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_CD_Stop)(System::Byte Drive);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_CD_SetPaused)(System::Byte Drive, System::ByteBool Paused);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_CD_SetVolume)(System::Byte Drive, int Volume);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_CD_SetTrackTime)(System::Byte Drive, int ms);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_CD_OpenTray)(System::Byte Drive, System::Byte Open);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_CD_GetPaused)(System::Byte Drive);
extern PACKAGE int __stdcall (*FSOUND_CD_GetTrack)(System::Byte Drive);
extern PACKAGE int __stdcall (*FSOUND_CD_GetNumTracks)(System::Byte Drive);
extern PACKAGE int __stdcall (*FSOUND_CD_GetVolume)(System::Byte Drive);
extern PACKAGE int __stdcall (*FSOUND_CD_GetTrackLength)(System::Byte Drive, int Track);
extern PACKAGE int __stdcall (*FSOUND_CD_GetTrackTime)(System::Byte Drive);
extern PACKAGE void * __stdcall (*FSOUND_DSP_Create)(Fmodtypes::TFSoundDSPCallback Callback, int Priority, int Param);
extern PACKAGE void __stdcall (*FSOUND_DSP_Free)(void * DSPUnit);
extern PACKAGE void __stdcall (*FSOUND_DSP_SetPriority)(void * DSPUnit, int Priority);
extern PACKAGE int __stdcall (*FSOUND_DSP_GetPriority)(void * DSPUnit);
extern PACKAGE void __stdcall (*FSOUND_DSP_SetActive)(void * DSPUnit, System::ByteBool Active);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_DSP_GetActive)(void * DSPUnit);
extern PACKAGE void * __stdcall (*FSOUND_DSP_GetClearUnit)(void);
extern PACKAGE void * __stdcall (*FSOUND_DSP_GetSFXUnit)(void);
extern PACKAGE void * __stdcall (*FSOUND_DSP_GetMusicUnit)(void);
extern PACKAGE void * __stdcall (*FSOUND_DSP_GetClipAndCopyUnit)(void);
extern PACKAGE void * __stdcall (*FSOUND_DSP_GetFFTUnit)(void);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_DSP_MixBuffers)(void * DestBuffer, void * SrcBuffer, int Len, int Freq, int Vol, int Pan, unsigned Mode);
extern PACKAGE void __stdcall (*FSOUND_DSP_ClearMixBuffer)(void);
extern PACKAGE int __stdcall (*FSOUND_DSP_GetBufferLength)(void);
extern PACKAGE int __stdcall (*FSOUND_DSP_GetBufferLengthTotal)(void);
extern PACKAGE Winapi::Windows::PSingle __stdcall (*FSOUND_DSP_GetSpectrum)(void);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Reverb_SetProperties)(const Fmodtypes::TFSoundReverbProperties &Prop);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Reverb_GetProperties)(Fmodtypes::TFSoundReverbProperties &Prop);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Reverb_SetChannelProperties)(int Channel, Fmodtypes::TFSoundReverbChannelProperties &Prop);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Reverb_GetChannelProperties)(int Channel, Fmodtypes::TFSoundReverbChannelProperties &Prop);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Record_SetDriver)(int OutputType);
extern PACKAGE int __stdcall (*FSOUND_Record_GetNumDrivers)(void);
extern PACKAGE System::WideChar * __stdcall (*FSOUND_Record_GetDriverName)(int Id);
extern PACKAGE int __stdcall (*FSOUND_Record_GetDriver)(void);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Record_StartSample)(void * Sptr, System::ByteBool Loop);
extern PACKAGE System::ByteBool __stdcall (*FSOUND_Record_Stop)(void);
extern PACKAGE int __stdcall (*FSOUND_Record_GetPosition)(void);
extern PACKAGE void * __stdcall (*FMUSIC_LoadSong)(const System::WideChar * Name);
extern PACKAGE void * __stdcall (*FMUSIC_LoadSongEx)(void * Name_Or_Data, int Offset, int Length, unsigned Mode, int &SampleList, int SampleListNum);
extern PACKAGE int __stdcall (*FMUSIC_GetOpenState)(void * Module);
extern PACKAGE System::ByteBool __stdcall (*FMUSIC_FreeSong)(void * Module);
extern PACKAGE System::ByteBool __stdcall (*FMUSIC_PlaySong)(void * Module);
extern PACKAGE System::ByteBool __stdcall (*FMUSIC_StopSong)(void * Module);
extern PACKAGE void __stdcall (*FMUSIC_StopAllSongs)(void);
extern PACKAGE System::ByteBool __stdcall (*FMUSIC_SetZxxCallback)(void * Module, Fmodtypes::TFMusicCallback Callback);
extern PACKAGE System::ByteBool __stdcall (*FMUSIC_SetRowCallback)(void * Module, Fmodtypes::TFMusicCallback Callback, int RowStep);
extern PACKAGE System::ByteBool __stdcall (*FMUSIC_SetOrderCallback)(void * Module, Fmodtypes::TFMusicCallback Callback, int OrderStep);
extern PACKAGE System::ByteBool __stdcall (*FMUSIC_SetInstCallback)(void * Module, Fmodtypes::TFMusicCallback Callback, int Instrument);
extern PACKAGE System::ByteBool __stdcall (*FMUSIC_SetSample)(void * Module, int SampNo, void * Sptr);
extern PACKAGE System::ByteBool __stdcall (*FMUSIC_SetUserData)(void * Module, int userdata);
extern PACKAGE System::ByteBool __stdcall (*FMUSIC_OptimizeChannels)(void * Module, int MaxChannels, int MinVolume);
extern PACKAGE System::ByteBool __stdcall (*FMUSIC_SetReverb)(System::ByteBool Reverb);
extern PACKAGE System::ByteBool __stdcall (*FMUSIC_SetLooping)(void * Module, System::ByteBool Looping);
extern PACKAGE System::ByteBool __stdcall (*FMUSIC_SetOrder)(void * Module, int Order);
extern PACKAGE System::ByteBool __stdcall (*FMUSIC_SetPaused)(void * Module, System::ByteBool Pause);
extern PACKAGE System::ByteBool __stdcall (*FMUSIC_SetMasterVolume)(void * Module, int Volume);
extern PACKAGE System::ByteBool __stdcall (*FMUSIC_SetMasterSpeed)(void * Module, float Speed);
extern PACKAGE System::ByteBool __stdcall (*FMUSIC_SetPanSeperation)(void * Module, float PanSep);
extern PACKAGE System::WideChar * __stdcall (*FMUSIC_GetName)(void * Module);
extern PACKAGE Fmodtypes::TFMusicTypes __stdcall (*FMUSIC_GetType)(void * Module);
extern PACKAGE int __stdcall (*FMUSIC_GetNumOrders)(void * Module);
extern PACKAGE int __stdcall (*FMUSIC_GetNumPatterns)(void * Module);
extern PACKAGE int __stdcall (*FMUSIC_GetNumInstruments)(void * Module);
extern PACKAGE int __stdcall (*FMUSIC_GetNumSamples)(void * Module);
extern PACKAGE int __stdcall (*FMUSIC_GetNumChannels)(void * Module);
extern PACKAGE void * __stdcall (*FMUSIC_GetSample)(void * Module, int SampNo);
extern PACKAGE int __stdcall (*FMUSIC_GetPatternLength)(void * Module, int OrderNo);
extern PACKAGE System::ByteBool __stdcall (*FMUSIC_IsFinished)(void * Module);
extern PACKAGE System::ByteBool __stdcall (*FMUSIC_IsPlaying)(void * Module);
extern PACKAGE int __stdcall (*FMUSIC_GetMasterVolume)(void * Module);
extern PACKAGE int __stdcall (*FMUSIC_GetGlobalVolume)(void * Module);
extern PACKAGE int __stdcall (*FMUSIC_GetOrder)(void * Module);
extern PACKAGE int __stdcall (*FMUSIC_GetPattern)(void * Module);
extern PACKAGE int __stdcall (*FMUSIC_GetSpeed)(void * Module);
extern PACKAGE int __stdcall (*FMUSIC_GetBPM)(void * Module);
extern PACKAGE int __stdcall (*FMUSIC_GetRow)(void * Module);
extern PACKAGE System::ByteBool __stdcall (*FMUSIC_GetPaused)(void * Module);
extern PACKAGE int __stdcall (*FMUSIC_GetTime)(void * Module);
extern PACKAGE int __stdcall (*FMUSIC_GetRealChannel)(void * Module, int ModChannel);
extern PACKAGE int __stdcall (*FMUSIC_GetUserData)(void * Module);
extern PACKAGE bool __fastcall FMOD_Load(System::WideChar * LibName = (System::WideChar *)(0x0));
extern PACKAGE void __fastcall FMOD_Unload(void);
}	/* namespace Fmoddyn */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMODDYN)
using namespace Fmoddyn;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FmoddynHPP
