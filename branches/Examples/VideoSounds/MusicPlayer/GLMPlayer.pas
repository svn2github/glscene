{
  ---------------------ќ јвторе ---------------------------
  ƒанный проигрыватель €вл€етс€ разработкой –устама јсманди€рова
  ќ всех проделанных изменени€ в модуле GLMplayer сообщайте на сайт
  GLScene.org, или Predator_rust@mail.ru.


  ------------------------Ћог –азработки--------------------
  ћодуль музыкального сопровождени€
  ќснован на различных библиотеках
  —оздан 10.05.09
  14.05.09 подключена библиотека Bass
  15.05.09 автоматизировано воспроизведение
  16.05.09 подключена библиотека fmod
  требуетс€ свойство определение длинны трека
  18.05.09 подключена библиотека openAl
  19.05.09 openal вопросизводит токо низкокач. wav
  не удаетс€ воспроизвести звук
  не удаетс€ определить длину трека позицию
  и установку позиции
  19.05.09 библиотека OpenAL дл€ воспроизведени€ не используетс€!
  19.05.09 подключил stream воспроизведение дл€ bass
  20.05.09 подключил stream воспроизведение дл€ fmod
  20.05.09 провел испытани€ добавил событие onstartmusic
}

unit GLMPlayer;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Dialogs,

  Bass,
  Fmod,
  FmodTypes,
  FmodPresets,
  OpenAL,
  GLSound,
  GLSMBASS,
  GLSMFMOD,
  GLSMOpenAL,
  GLBaseClasses,

  GLCadencer,
  GLCrossplatform,
  GLSoundFileObjects;

type
  TGLMusicPlayer = class;
  TPlayingMode = (pmFromLib, pmStream);
  TGLMusicStopEvent = procedure(sender: TGLMusicPlayer) of Object;
  TGLMusicStartEvent = procedure(sender: TGLMusicPlayer) of Object;

  TGLMusicPlayer = class(TGLCadenceAbleComponent)
  private
    fPlaying: boolean;
    fItemIsSelected: boolean;
    fOnMusicStop: TGLMusicStopEvent;
    fOnMusicStart: TGLMusicStartEvent;
    fAutoPlay: boolean;
    fRepeatPlaying: boolean;
    fIndexMusic: integer;
    fPaused: boolean;
    fSoundLib: TGLSoundLibrary;
    fCadencer: TGLCadencer;
    fManager: TGLSoundManager;
    fActiveMgr: integer;
    fPlayList: TStrings;

    fMode: TPlayingMode;

    procedure SetCadencer(aValue: TGLCadencer);
    procedure SetManager(aValue: TGLSoundManager);
    procedure SetMusicStop(sender: TGLMusicPlayer);
    procedure SetSoundLib(aValue: TGLSoundLibrary);
    procedure SetIndexMusic(aValue: integer);
    procedure SetMode(aValue: TPlayingMode);
    function GetALFormat(sampling: TGLSoundSampling): integer;
    procedure FreeSound;
    function GetPosition: int64;
    procedure SetPosition(TimeSec: int64);
  public
    procedure DoProgress(const progressTime: TProgressTimes); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Play;
    procedure Stop;
    procedure Pause;
    function GetLength: int64;
    procedure Add(aValue: string);
    procedure ClearLib;
    procedure MoveUp(aValue: integer);
    procedure MoveDown(aValue: integer);
    procedure Delete(aValue: integer);
    property Position: int64 read GetPosition write SetPosition;
    property IndexMusic: integer read fIndexMusic write SetIndexMusic;
  published
    property OnMusicStop: TGLMusicStopEvent read fOnMusicStop
      write fOnMusicStop;
    property OnMusicStart: TGLMusicStartEvent read fOnMusicStart
      write fOnMusicStart;
    property SoundLibrary: TGLSoundLibrary read fSoundLib write SetSoundLib;
    property RepeatPlaying: boolean read fRepeatPlaying write fRepeatPlaying;
    property Cadencer: TGLCadencer read fCadencer write SetCadencer;
    property Manager: TGLSoundManager read fManager write SetManager;
    property PlayingMode: TPlayingMode read fMode write SetMode;
    property PlayList: TStrings read fPlayList;
    property MusicIsPlaying: boolean read fPlaying;
    property AutoPlay: boolean read fAutoPlay write fAutoPlay;
  end;

implementation

type
  TBASSInfo = record
    channel: HCHANNEL;
    sample: HSAMPLE;
  end;

  PBASSInfo = ^TBASSInfo;

  TFMODInfo = record
    channel: integer;
    pfs: PFSoundSample;
    pfstr: PFSoundStream;
  end;

  PFMODInfo = ^TFMODInfo;

  TOpenALInfo = record
    buffer: TALuint;
    source: TALuint;
  end;

  POpenALInfo = ^TOpenALInfo;

var
  b: PBASSInfo;
  fm: PFMODInfo;
  a: POpenALInfo;

function TGLMusicPlayer.GetALFormat(sampling: TGLSoundSampling): integer;
begin
  result := 0;
  // mono
  if sampling.NbChannels = 1 then
    case sampling.BitsPerSample of
      8:
        result := AL_FORMAT_MONO8;
      16:
        result := AL_FORMAT_MONO16;
    end
  else
    case sampling.BitsPerSample of // stereo
      8:
        result := AL_FORMAT_STEREO8;
      16:
        result := AL_FORMAT_STEREO16;
    end;
end;

procedure TGLMusicPlayer.SetMode(aValue: TPlayingMode);
begin
  if fMode <> aValue then
    fMode := aValue;
end;

procedure TGLMusicPlayer.SetManager(aValue: TGLSoundManager);
begin
  if fManager <> aValue then
  begin
    if MusicIsPlaying then
      Stop;
    FreeSound;
    fManager := aValue;
    if fManager = nil then
      fActiveMgr := -1;
    if fManager is TGLSMBass then
      fActiveMgr := 0;
    if fManager is TGLSMFMOD then
      fActiveMgr := 1;
    if fManager is TGLSMOpenAl then
    begin
      fActiveMgr := -1;
      ShowMessage
        ('¬нимание!!! Ѕиблиотека OpenAl не используетс€ дл€ воспроизведени€!!!');
    end;
  end;
end;

procedure TGLMusicPlayer.SetCadencer(aValue: TGLCadencer);
begin
  if aValue <> fCadencer then
  begin
    if Assigned(fCadencer) then
      fCadencer.UnSubscribe(Self);
    fCadencer := aValue;
    if Assigned(fCadencer) then
      fCadencer.Subscribe(Self);
  end;

end;

procedure TGLMusicPlayer.SetMusicStop(sender: TGLMusicPlayer);
begin
  if fSoundLib <> nil then
  begin
    if IndexMusic < fSoundLib.Samples.Count - 1 then
      IndexMusic := IndexMusic + 1
    else if IndexMusic = fSoundLib.Samples.Count - 1 then
      if fRepeatPlaying then
        IndexMusic := 0;
  end
  else if IndexMusic < fPlayList.Count - 1 then
    IndexMusic := IndexMusic + 1
  else if IndexMusic = fPlayList.Count - 1 then
    if fRepeatPlaying then
      IndexMusic := 0;

end;

procedure TGLMusicPlayer.SetSoundLib(aValue: TGLSoundLibrary);
begin
  if fSoundLib <> aValue then
    fSoundLib := aValue;
end;

procedure TGLMusicPlayer.DoProgress(const progressTime: TProgressTimes);
var
  satate: TALint;
begin
  if fPlaying then
    case fActiveMgr of
      0:
        if BASS_ChannelIsActive(b.channel) = 0 then
          If Assigned(fOnMusicStop) then
            OnMusicStop(Self);
      1:
        if not FSOUND_IsPlaying(fm.channel) then
          If Assigned(fOnMusicStop) then
            OnMusicStop(Self);
    end;
end;

procedure TGLMusicPlayer.SetIndexMusic(aValue: integer);
begin
  fIndexMusic := -1;
  fItemIsSelected := false;
  if aValue <> -1 then
    if fPlaying then
      Stop;
  if fIndexMusic <> aValue then
  begin
    fIndexMusic := aValue;
    if fMode = pmFromLib then
    begin
      if (aValue >= 0) and (aValue <= fSoundLib.Samples.Count - 1) then
        if (fSoundLib <> nil) and (fSoundLib.Samples.Items[aValue] <> nil) then
          with fSoundLib.Samples.Items[aValue] do
            case fActiveMgr of
              0:
                begin
                  b := AllocMem(SizeOf(TBASSInfo));
                  b.channel := 0;
                  b.sample := BASS_SampleLoad(True, Data.WAVData, 0,
                    Data.WAVDataSize, 32, 0);

                end;
              1:
                begin
                  fm := AllocMem(SizeOf(TFMODInfo));
                  fm.channel := 0;
                  fm.pfs := FSOUND_Sample_Load(FSOUND_FREE, Data.WAVData,
                    FSOUND_LOOP_OFF + FSOUND_LOADMEMORY, 0, Data.WAVDataSize);
                end;
            end;
    end
    else if (aValue >= 0) and (aValue <= fPlayList.Count - 1) then
      case fActiveMgr of
        0:
          begin
            b := AllocMem(SizeOf(TBASSInfo));
            b.channel := BASS_StreamCreateFile(false,
              pchar(fPlayList[aValue]), 0, 0, 0);
          end;
        1:
          begin
            fm := AllocMem(SizeOf(TFMODInfo));
            fm.pfstr := FSOUND_Stream_Open(PAnsiChar(fPlayList[aValue]),
              FSOUND_LOOP_OFF, 0, 0);
          end;
      end;
    fItemIsSelected := True;
    if fAutoPlay then
      Play;
  end;
end;

constructor TGLMusicPlayer.Create(AOwner: TComponent);
begin
  fAutoPlay := True;
  fOnMusicStop := SetMusicStop;
  fRepeatPlaying := false;
  fActiveMgr := -1;
  fPlayList := TStringList.Create;
  fMode := pmStream;
  inherited;
end;

destructor TGLMusicPlayer.Destroy;
begin
  FreeAndNil(fPlayList);
  inherited;
end;

procedure TGLMusicPlayer.FreeSound;
begin
  if fPlaying then
  begin
    case fActiveMgr of
      0:
        begin
          if fMode = pmFromLib then
            Bass_SampleFree(b.sample)
          else
            BASS_StreamFree(b.channel);
          FreeMem(b);
        end;
      1:
        if fMode = pmFromLib then
        begin
          FSOUND_Sample_Free(fm.pfs);
          FreeMem(fm);
        end
        else
        begin
          FSOUND_Stream_Close(fm.pfstr);
          FreeMem(fm);
        end;
    end;
  end;

end;

procedure TGLMusicPlayer.Play;
begin
  if fItemIsSelected then
    if not fPaused then
      if not fPlaying then
      begin
        case fActiveMgr of
          0:
            begin
              if fMode = pmFromLib then
                b.channel := BASS_SampleGetChannel(b.sample, false);
              BASS_ChannelPlay(b.channel, false);
              If Assigned(fOnMusicStart) then
                OnMusicStart(Self);
            end;
          1:
            begin
              if fMode = pmFromLib then
                fm.channel := FSOUND_PlaySound(FSOUND_FREE, fm.pfs)
              else
                fm.channel := FSOUND_Stream_Play(FSOUND_FREE, fm.pfstr);
              If Assigned(fOnMusicStart) then
                OnMusicStart(Self);
            end;

        end;
        fPaused := false;
        fPlaying := True;
      end;
  if fPaused then
    case fActiveMgr of
      0:
        fPaused := not BASS_ChannelPlay(b.channel, false);
      1:
        begin
          fPaused := fPaused;
          FSOUND_SetPaused(fm.channel, fPaused);
        end;
    end;
end;

procedure TGLMusicPlayer.Stop;
begin
  case fActiveMgr of
    0:
      if fMode = pmFromLib then
        BASS_ChannelStop(b.channel)
      else
      begin
        BASS_ChannelStop(b.channel);
        SetPosition(0);
      end;
    1:
      if fMode = pmFromLib then
        Fsound_StopSound(fm.channel)
      else
        FSOUND_Stream_Stop(fm.pfstr);

  end;
  fPlaying := false;
  fPaused := false;
end;

procedure TGLMusicPlayer.Pause;
begin
  if fItemIsSelected then
    case fActiveMgr of
      0:
        begin
          if not fPaused then
            fPaused := BASS_ChannelPause(b.channel)
          else
            fPaused := not BASS_ChannelPlay(b.channel, false);
        end;
      1:
        begin
          fPaused := not fPaused;
          FSOUND_SetPaused(fm.channel, fPaused);
        end;
    end;
end;

function TGLMusicPlayer.GetLength: int64;
begin
  result := 0;
  if fPlaying then
    case fActiveMgr of
      0:
        result := BASS_ChannelGetLength(b.channel, 0);
      1:
        if fMode = pmFromLib then
          result := FSOUND_Sample_GetLength(fm.pfs)
        else
          result := FSOUND_Stream_GetLength(fm.pfstr)
    end;
end;

function TGLMusicPlayer.GetPosition: int64;
begin
  result := 0;
  if fPlaying then
    case fActiveMgr of
      0:
        result := BASS_ChannelGetPosition(b.channel, 0);
      1:
        if fMode = pmFromLib then
          result := FSOUND_GetCurrentPosition(fm.channel)
        else
          result := FSOUND_Stream_GetPosition(fm.pfstr);
    end;
end;

procedure TGLMusicPlayer.SetPosition(TimeSec: int64);
begin
  if fItemIsSelected then
    if fPlaying then
      case fActiveMgr of
        0:
          BASS_ChannelSetPosition(b.channel, TimeSec, 0);
        1:
          if fMode = pmFromLib then
            FSOUND_SetCurrentPosition(fm.channel, TimeSec)
          else
            FSOUND_Stream_SetPosition(fm.pfstr, TimeSec);
      end;
end;

procedure TGLMusicPlayer.Add(aValue: string);
begin
  if aValue <> '' then
  begin
    if fSoundLib <> nil then
      with fSoundLib.Samples.Add do
      begin
        LoadFromFile(aValue);
        Name := extractfileName(aValue);
      end;
    fPlayList.Add(aValue);
  end;
end;

procedure TGLMusicPlayer.ClearLib;
begin
  if fPlaying then
  begin
    Stop;
    FreeSound;
  end;
  if fSoundLib <> nil then
    fSoundLib.Samples.Clear;
  fPlayList.Clear;
end;

procedure TGLMusicPlayer.MoveUp(aValue: integer);
begin
  if aValue <> -1 then
  begin
    if fSoundLib <> nil then
      if (aValue >= 0) and (aValue <= fSoundLib.Samples.Count - 1) then
        fSoundLib.Samples.Items[aValue].Index := aValue - 1;
    if (aValue > 0) and (aValue <= fPlayList.Count - 1) then
      fPlayList.Move(aValue, aValue - 1);
  end;
end;

procedure TGLMusicPlayer.MoveDown(aValue: integer);
begin
  if aValue <> -1 then
  begin
    if fSoundLib <> nil then
      if (aValue >= 0) and (aValue <= fSoundLib.Samples.Count - 1) then
        fSoundLib.Samples.Items[aValue].Index := aValue + 1;

    if (aValue >= 0) and (aValue < fPlayList.Count - 1) then
      fPlayList.Move(aValue, aValue + 1);
  end;
end;

procedure TGLMusicPlayer.Delete(aValue: integer);
begin
  if fIndexMusic = aValue then
  begin
    Stop;
    FreeSound;
  end;
  if aValue <> -1 then
  begin
    if fSoundLib <> nil then
      if (aValue >= 0) and (aValue <= fSoundLib.Samples.Count - 1) then
        fSoundLib.Samples.Delete(aValue);

    if (aValue >= 0) and (aValue < fPlayList.Count - 1) then
      fPlayList.Delete(aValue);
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

registerClass(TGLMusicPlayer);

end.
