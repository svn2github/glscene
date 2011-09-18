{*
 * This unit is primarily based upon
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/media/UAudioPlaybackBase.pas $
 *}

unit UAudioPlaybackBase;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMusic,
  UTime,
  Classes,
  SysUtils;

type
  TAudioPlaybackBase = class(TInterfacedObject)
    protected
      OutputDeviceList: TAudioOutputDeviceList;
      MusicStream: TAudioPlaybackStream;
      function CreatePlaybackStream(): TAudioPlaybackStream; virtual; abstract;
      procedure ClearOutputDeviceList();
      function GetLatency(): double; virtual; abstract;

      // open sound or music stream (used by Open() and OpenSound())
      function OpenStream(const Filename: UTF8String): TAudioPlaybackStream;
      function OpenStreamBuffer(Buffer: TStream; Format: TAudioFormatInfo): TAudioPlaybackStream;
      function OpenDecodeStream(const Filename: UTF8String): TAudioDecodeStream;
    public
      function GetName: string; virtual; abstract;

      function Open(const Filename: UTF8String): boolean; // true if succeed
      procedure Close;

      procedure Play;
      procedure Pause;
      procedure Stop;
      procedure FadeIn(Time: real; TargetVolume: single);

      procedure SetSyncSource(SyncSource: TSyncSource);

      procedure SetPosition(Time: real);
      function  GetPosition: real;

      function InitializePlayback: boolean; virtual; abstract;
      function FinalizePlayback: boolean; virtual;

      //function SetOutputDevice(Device: TAudioOutputDevice): boolean;
      function GetOutputDeviceList(): TAudioOutputDeviceList;

      procedure SetAppVolume(Volume: single); virtual; abstract;
      procedure SetVolume(Volume: single);
      procedure SetLoop(Enabled: boolean);

      procedure Rewind;
      function  Finished: boolean;
      function  Length: real;

      // Sounds
      function OpenSound(const Filename: UTF8String): TAudioPlaybackStream;
      function OpenSoundBuffer(Buffer: TStream; Format: TAudioFormatInfo): TAudioPlaybackStream;
      procedure PlaySound(Stream: TAudioPlaybackStream);
      procedure StopSound(Stream: TAudioPlaybackStream);

      // Equalizer
      procedure GetFFTData(var Data: TFFTData);

      // Interface for Visualizer
      function GetPCMData(var Data: TPCMData): Cardinal;

      function CreateVoiceStream(Channel: integer; FormatInfo: TAudioFormatInfo): TAudioVoiceStream; virtual; abstract;
  end;

  TAudioBufferSourceStream = class(TAudioSourceStream)
    private
      fLoop: boolean;
      fStream: TStream;
      fFormat: TAudioFormatInfo;
    protected
      function IsEOF(): boolean; override;
      function IsError(): boolean; override;
      function GetLength(): real; override;
      function GetPosition(): real; override;
      procedure SetPosition(Time: real); override;
      function GetLoop(): boolean; override;
      procedure SetLoop(Enabled: boolean); override;
    public
      constructor Create(Buffer: TStream; Format: TAudioFormatInfo);
      function ReadData(Buffer: PByteArray; BufferSize: integer): integer; override;
      function GetAudioFormatInfo(): TAudioFormatInfo; override;
      procedure Close(); override;
  end;

implementation

uses
  GLSLog;

{ TAudioPlaybackBase }

function TAudioPlaybackBase.FinalizePlayback: boolean;
begin
  FreeAndNil(MusicStream);
  ClearOutputDeviceList();
  Result := true;
end;

function TAudioPlaybackBase.Open(const Filename: UTF8String): boolean;
begin
  // free old MusicStream
  MusicStream.Free;

  MusicStream := OpenStream(Filename);
  if not assigned(MusicStream) then
  begin
    Result := false;
    Exit;
  end;

  //странно, эта часть кода приглушивает звук
  MusicStream.AddSoundEffect(TVoiceRemoval.Create());

  Result := true;
end;

procedure TAudioPlaybackBase.Close;
begin
  FreeAndNil(MusicStream);
end;

function TAudioPlaybackBase.OpenDecodeStream(const Filename: UTF8String): TAudioDecodeStream;
var
  i: integer;
begin
  for i := 0 to AudioDecoders.Count-1 do
  begin
    Result := IAudioDecoder(AudioDecoders[i]).Open(Filename);
    if (assigned(Result)) then
    begin
      GLSLogger.LogInfo('Using decoder ' + IAudioDecoder(AudioDecoders[i]).GetName() +
        ' for "' + Filename + '"' +'TAudioPlaybackBase.OpenDecodeStream');
      Exit;
    end;
  end;
  Result := nil;
end;

procedure OnClosePlaybackStream(Stream: TAudioProcessingStream);
var
  PlaybackStream: TAudioPlaybackStream;
  SourceStream: TAudioSourceStream;
begin
  PlaybackStream := TAudioPlaybackStream(Stream);
  SourceStream := PlaybackStream.GetSourceStream();
  SourceStream.Free;
end;

function TAudioPlaybackBase.OpenStream(const Filename: UTF8String): TAudioPlaybackStream;
var
  PlaybackStream: TAudioPlaybackStream;
  DecodeStream: TAudioDecodeStream;
begin
  Result := nil;

  //Log.LogStatus('Loading Sound: "' + Filename + '"', 'TAudioPlayback_Bass.OpenStream');

  DecodeStream := OpenDecodeStream(Filename);
  if (not assigned(DecodeStream)) then
  begin
    GLSLogger.LogInfo('Could not open "' + Filename + '"'+ 'TAudioPlayback_Base.OpenStream');
    Exit;
  end;

  // create a matching playback-stream for the decoder
  PlaybackStream := CreatePlaybackStream();
  if (not PlaybackStream.Open(DecodeStream)) then
  begin
    FreeAndNil(PlaybackStream);
    FreeAndNil(DecodeStream);
    Exit;
  end;

  PlaybackStream.AddOnCloseHandler(OnClosePlaybackStream);

  Result := PlaybackStream;
end;

function TAudioPlaybackBase.OpenStreamBuffer(Buffer: TStream; Format: TAudioFormatInfo): TAudioPlaybackStream;
var
  PlaybackStream: TAudioPlaybackStream;
  SourceStream: TAudioSourceStream;
begin
  Result := nil;

  // create a matching playback-stream for the decoder
  PlaybackStream := CreatePlaybackStream();
  SourceStream := TAudioBufferSourceStream.Create(Buffer, Format);
  if (not PlaybackStream.Open(SourceStream)) then
  begin
    FreeAndNil(PlaybackStream);
    FreeAndNil(SourceStream);
    Exit;
  end;

  PlaybackStream.AddOnCloseHandler(OnClosePlaybackStream);

  Result := PlaybackStream;
end;

procedure TAudioPlaybackBase.Play;
begin
  if assigned(MusicStream) then
    MusicStream.Play();
end;

procedure TAudioPlaybackBase.Pause;
begin
  if assigned(MusicStream) then
    MusicStream.Pause();
end;

procedure TAudioPlaybackBase.Stop;
begin
  if assigned(MusicStream) then
    MusicStream.Stop();
end;

function TAudioPlaybackBase.Length: real;
begin
  if assigned(MusicStream) then
    Result := MusicStream.Length
  else
    Result := 0;
end;

function TAudioPlaybackBase.GetPosition: real;
begin
  if assigned(MusicStream) then
    Result := MusicStream.Position
  else
    Result := 0;
end;

procedure TAudioPlaybackBase.SetPosition(Time: real);
begin
  if assigned(MusicStream) then
    MusicStream.Position := Time;
end;

procedure TAudioPlaybackBase.SetSyncSource(SyncSource: TSyncSource);
begin
  if assigned(MusicStream) then
    MusicStream.SetSyncSource(SyncSource);
end;

procedure TAudioPlaybackBase.Rewind;
begin
  SetPosition(0);
end;

function TAudioPlaybackBase.Finished: boolean;
begin
  if assigned(MusicStream) then
    Result := (MusicStream.Status = ssStopped)
  else
    Result := true;
end;

procedure TAudioPlaybackBase.SetVolume(Volume: single);
begin
  if assigned(MusicStream) then
    MusicStream.Volume := Volume;
end;

procedure TAudioPlaybackBase.FadeIn(Time: real; TargetVolume: single);
begin
  if assigned(MusicStream) then
    MusicStream.FadeIn(Time, TargetVolume);
end;

procedure TAudioPlaybackBase.SetLoop(Enabled: boolean);
begin
  if assigned(MusicStream) then
    MusicStream.Loop := Enabled;
end;

// Equalizer
procedure TAudioPlaybackBase.GetFFTData(var data: TFFTData);
begin
  if assigned(MusicStream) then
    MusicStream.GetFFTData(data);
end;

{*
 * Copies interleaved PCM SInt16 stereo samples into data.
 * Returns the number of frames
 *}
function TAudioPlaybackBase.GetPCMData(var data: TPCMData): Cardinal;
begin
  if assigned(MusicStream) then
    Result := MusicStream.GetPCMData(data)
  else
    Result := 0;
end;

function TAudioPlaybackBase.OpenSound(const Filename: UTF8String): TAudioPlaybackStream;
begin
  Result := OpenStream(Filename);
end;

function TAudioPlaybackBase.OpenSoundBuffer(Buffer: TStream; Format: TAudioFormatInfo): TAudioPlaybackStream;
begin
  Result := OpenStreamBuffer(Buffer, Format);
end;

procedure TAudioPlaybackBase.PlaySound(stream: TAudioPlaybackStream);
begin
  if assigned(stream) then
    stream.Play();
end;

procedure TAudioPlaybackBase.StopSound(stream: TAudioPlaybackStream);
begin
  if assigned(stream) then
    stream.Stop();
end;

procedure TAudioPlaybackBase.ClearOutputDeviceList();
var
  DeviceIndex: integer;
begin
  for DeviceIndex := 0 to High(OutputDeviceList) do
    OutputDeviceList[DeviceIndex].Free();
  SetLength(OutputDeviceList, 0);
end;

function TAudioPlaybackBase.GetOutputDeviceList(): TAudioOutputDeviceList;
begin
  Result := OutputDeviceList;
end;

{ TAudioBufferSourceStream }

constructor TAudioBufferSourceStream.Create(Buffer: TStream; Format: TAudioFormatInfo);
begin
  fStream := Buffer;
  fFormat := Format.Copy;
end;

function TAudioBufferSourceStream.IsEOF(): boolean;
begin
  Result := (not fLoop and (fStream.Position >= fStream.Size));
end;

function TAudioBufferSourceStream.IsError(): boolean;
begin
  Result := false;
end;

function TAudioBufferSourceStream.GetLength(): real;
begin
  Result := fStream.Size / fFormat.BytesPerSec;
end;

function TAudioBufferSourceStream.GetPosition(): real;
begin
  Result := fStream.Position / fFormat.BytesPerSec;
end;

procedure TAudioBufferSourceStream.SetPosition(Time: real);
begin
  fStream.Position := Trunc(Time * fFormat.BytesPerSec);
end;

function TAudioBufferSourceStream.GetLoop(): boolean;
begin
  Result := fLoop;
end;

procedure TAudioBufferSourceStream.SetLoop(Enabled: boolean);
begin
  fLoop := Enabled;
end;

function TAudioBufferSourceStream.ReadData(Buffer: PByteArray; BufferSize: integer): integer;
var
  BufSizeLeft: integer;
  NumRead: integer;
begin
  Result := fStream.Read(Buffer^, BufferSize);
  if (fLoop) then
  begin
    BufSizeLeft := BufferSize - Result;
    while (BufSizeLeft > 0) do
    begin
      fStream.Position := 0;
      NumRead := fStream.Read(Buffer^, BufSizeLeft);
      BufSizeLeft := BufSizeLeft - NumRead;
    end;
    Result := BufferSize;
  end;
end;

function TAudioBufferSourceStream.GetAudioFormatInfo(): TAudioFormatInfo;
begin
  Result := fFormat;
end;

procedure TAudioBufferSourceStream.Close();
begin
  FreeAndNil(fFormat);
  fStream := nil;
end;

end.
