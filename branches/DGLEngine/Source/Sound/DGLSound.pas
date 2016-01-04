//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ : DGLSound<p>

  @HTML (
  <p>
  Base classes and interface for DGLEngine Sound System</p>
  <b>History: </b><font size=-1><ul>
  <li>01/01/16 - JD - Imported from GLScene
  </ul></font></p>
  )
}
unit DGLSound;

interface

{$I DGLEngine.inc}

uses
  System.Classes, System.SysUtils, System.Types,
  // GLS
  DGLSoundFileObjects, DGLScene, DGLXCollection, DGLVectorMaths,
  DGLCadencer, DGLBaseClasses, DGLCrossPlatform, DGLUtils;

type
  // ****************************************************************************************
  // TDGLSoundSample
  //
  { : Stores a single PCM coded sound sample. }
  TDGLSoundSample = class(TCollectionItem)
  private
    { Private Declarations }
    FName: string;
    FData: TDGLSoundFile;
    FTag:  Integer;

  protected
    { Protected Declarations }
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream); virtual;
    procedure WriteData(Stream: TStream); virtual;
    function GetDisplayName: string; override;
    procedure SetData(const val: TDGLSoundFile);

  public
    { Public Declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure LoadFromFile(const fileName: string);

    procedure PlayOnWaveOut;

    function Sampling: TDGLSoundSampling;
    function LengthInBytes: Integer;
    function LengthInSamples: Integer;
    function LengthInSec: Single;

    // : This Tag is reserved for sound manager use only
    property ManagerTag: Integer read FTag write FTag;

  published
    { Published Declarations }
    property Name: string read FName write FName;
    property Data: TDGLSoundFile read FData write SetData stored False;
  end;

  // ****************************************************************************************
  // TDGLSoundSamples
  //
  TDGLSoundSamples = class(TCollection)
  protected
    { Protected Declarations }
    owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TDGLSoundSample);
    function GetItems(index: Integer): TDGLSoundSample;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);
    function Add: TDGLSoundSample;
    function FindItemID(ID: Integer): TDGLSoundSample;
    property Items[index: Integer]: TDGLSoundSample read GetItems write SetItems; default;
    function GetByName(const aName: string): TDGLSoundSample;

    function AddFile(const fileName: string; const sampleName: string = ''): TDGLSoundSample;
  end;

  // ****************************************************************************************
  // TDGLSoundLibrary
  //
  TDGLSoundLibrary = class(TComponent)
  private
    { Private Declarations }
    FSamples: TDGLSoundSamples;

  protected
    { Protected Declarations }
    procedure SetSamples(const val: TDGLSoundSamples);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Published Declarations }
    property Samples: TDGLSoundSamples read FSamples write SetSamples;
  end;

  // ****************************************************************************************
  // TDGLSoundSource
  //
  TDGLSoundSourceChange  = (sscTransformation, sscSample, sscStatus);
  TDGLSoundSourceChanges = set of TDGLSoundSourceChange;

  TDGLBSoundEmitter = class;

  // ****************************************************************************************
  // TDGLBaseSoundSource
  //
  { : Base class for origin of sound playback. }
  TDGLBaseSoundSource = class(TCollectionItem)
  private
    { Private Declarations }
    FBehaviourToNotify: TDGLBSoundEmitter;
    // private only, NOT persistent, not assigned
    FPriority:                           Integer;
    FOrigin:                             TDGLBaseSceneObject; // NOT persistent
    FVolume:                             Single;
    FMinDistance, FMaxDistance:          Single;
    FInsideConeAngle, FOutsideConeAngle: Single;
    FConeOutsideVolume:                  Single;
    FSoundLibraryName:                   string; // used for persistence
    FSoundLibrary:                       TDGLSoundLibrary; // persistence via name
    FSoundName:                          string;
    FMute:                               Boolean;
    FPause:                              Boolean;
    FChanges:                            TDGLSoundSourceChanges; // NOT persistent, not assigned
    FNbLoops:                            Integer;
    FTag:                                PtrUInt; // NOT persistent, not assigned
    FFrequency:                          Integer;

  protected
    { Protected Declarations }
    procedure WriteToFiler(writer: TWriter);
    procedure ReadFromFiler(reader: TReader);

    function GetDisplayName: string; override;
    procedure SetPriority(const val: Integer);
    procedure SetOrigin(const val: TDGLBaseSceneObject);
    procedure SetVolume(const val: Single);
    procedure SetMinDistance(const val: Single);
    procedure SetMaxDistance(const val: Single);
    procedure SetInsideConeAngle(const val: Single);
    procedure SetOutsideConeAngle(const val: Single);
    procedure SetConeOutsideVolume(const val: Single);
    function GetSoundLibrary: TDGLSoundLibrary;
    procedure SetSoundLibrary(const val: TDGLSoundLibrary);
    procedure SetSoundName(const val: string);
    procedure SetMute(const val: Boolean);
    procedure SetPause(const val: Boolean);
    procedure SetNbLoops(const val: Integer);
    procedure SetFrequency(const val: Integer);

  public
    { Public Declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Changes: TDGLSoundSourceChanges read FChanges;

    function Sample: TDGLSoundSample;

    // : This Tag is reserved for sound manager use only
    property ManagerTag: PtrUInt read FTag write FTag;

    { : Origin object for the sound sources.<p>
      Absolute object position/orientation are taken into account, the
      object's TGLBInertia is considered if any.<p>
      If origin is nil, the source is assumed to be static at the origin.<p>
      <b>Note :</b> since TCollectionItem do not support the "Notification"
      scheme, it is up to the Origin object to take care of updating this
      property prior to release/destruction. }
    property Origin: TDGLBaseSceneObject read FOrigin write SetOrigin;

  published
    { Published Declarations }
    property SoundLibrary: TDGLSoundLibrary read GetSoundLibrary write SetSoundLibrary;
    property SoundName:    string read FSoundName write SetSoundName;

    { : Volume of the source, [0.0; 1.0] range }
    property Volume: Single read FVolume write SetVolume;
    { : Nb of playing loops. }
    property NbLoops: Integer read FNbLoops write SetNbLoops default 1;

    property Mute:  Boolean read FMute write SetMute default False;
    property Pause: Boolean read FPause write SetPause default False;

    { : Sound source priority, the higher the better.<p>
      When maximum number of sound sources is reached, only the sources
      with the highest priority will continue to play, however, even
      non-playing sources should be tracked by the manager, thus allowing
      an "unlimited" amount of sources from the application point of view. }
    property Priority: Integer read FPriority write SetPriority default 0;

    { : Min distance before spatial attenuation occurs.<p>
      1.0 by default }
    property MinDistance: Single read FMinDistance write SetMinDistance;
    { : Max distance, if source is further away, it will not be heard.<p>
      100.0 by default }
    property MaxDistance: Single read FMaxDistance write SetMaxDistance;

    { : Inside cone angle, [0°; 360°].<p>
      Sound volume is maximal within this cone.<p>
      See DirectX SDK for details. }
    property InsideConeAngle: Single read FInsideConeAngle write SetInsideConeAngle;
    { : Outside cone angle, [0°; 360°].<p>
      Between inside and outside cone, sound volume decreases between max
      and cone outside volume.<p>
      See DirectX SDK for details. }
    property OutsideConeAngle: Single read FOutsideConeAngle write SetOutsideConeAngle;
    { : Cone outside volume, [0.0; 1.0] range.<p>
      See DirectX SDK for details. }
    property ConeOutsideVolume: Single read FConeOutsideVolume write SetConeOutsideVolume;
    { : Sample custom playback frequency.<p>
      Values null or negative are interpreted as 'default frequency'. }
    property Frequency: Integer read FFrequency write SetFrequency default -1;
  end;

  // ****************************************************************************************
  // TDGLSoundSource
  //
  { : Origin of sound playback.<p>
    Just publishes the 'Origin' property.<p>
    Note that the "orientation" is the the source's Direction, ie. the "Z"
    vector. }
  TDGLSoundSource = class(TDGLBaseSoundSource)
  public
    { Public Declarations }
    destructor Destroy; override;

  published
    { Published Declarations }
    property Origin;
  end;

  // ****************************************************************************************
  // TDGLSoundSources
  //
  TDGLSoundSources = class(TCollection)
  protected
    { Protected Declarations }
    owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TDGLSoundSource);
    function GetItems(index: Integer): TDGLSoundSource;

    function Add: TDGLSoundSource;
    function FindItemID(ID: Integer): TDGLSoundSource;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);

    property Items[index: Integer]: TDGLSoundSource read GetItems write SetItems; default;
  end;

  // ****************************************************************************************
  // TDGLSoundEnvironment
  //
  { : EAX standard sound environments. }
  TDGLSoundEnvironment = (seDefault, sePaddedCell, seRoom, seBathroom, seLivingRoom, seStoneroom, seAuditorium, seConcertHall, seCave, seArena, seHangar, seCarpetedHallway, seHallway, seStoneCorridor, seAlley, seForest, seCity, seMountains, seQuarry,
    sePlain, seParkingLot, seSewerPipe, seUnderWater, seDrugged, seDizzy, sePsychotic);

  // ****************************************************************************************
  // TDGLSoundManager
  //
  { : Base class for sound manager components.<p>
    The sound manager component is the interface to a low-level audio API
    (like DirectSound), there can only be one active manager at any time
    (this class takes care of this).<p>
    Subclass should override the DoActivate and DoDeActivate protected methods
    to "initialize/unitialize" their sound layer, actual data releases should
    occur in destructor however. }
  TDGLSoundManager = class(TDGLCadenceAbleComponent)
  private
    { Private Declarations }
    FActive:                         Boolean;
    FMute:                           Boolean;
    FPause:                          Boolean;
    FMasterVolume:                   Single;
    FListener:                       TDGLBaseSceneObject;
    FLastListenerPosition:           TVector;
    FSources:                        TDGLSoundSources;
    FMaxChannels:                    Integer;
    FOutputFrequency:                Integer;
    FUpdateFrequency:                Single;
    FDistanceFactor:                 Single;
    FRollOffFactor:                  Single;
    FDopplerFactor:                  Single;
    FSoundEnvironment:               TDGLSoundEnvironment;
    FLastUpdateTime, FLastDeltaTime: Single;
    // last time UpdateSources was fired, not persistent
    FCadencer: TDGLCadencer;
    procedure SetActive(const val: Boolean);
    procedure SetMute(const val: Boolean);
    procedure SetPause(const val: Boolean);
    procedure WriteDoppler(writer: TWriter);
    procedure ReadDoppler(reader: TReader);

  protected
    { Protected Declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetSources(const val: TDGLSoundSources);
    procedure SetMasterVolume(const val: Single);
    procedure SetListener(const val: TDGLBaseSceneObject);
    procedure SetMaxChannels(const val: Integer);
    procedure SetOutputFrequency(const val: Integer);
    procedure SetUpdateFrequency(const val: Single);
    function StoreUpdateFrequency: Boolean;
    procedure SetCadencer(const val: TDGLCadencer);
    procedure SetDistanceFactor(const val: Single);
    function StoreDistanceFactor: Boolean;
    procedure SetRollOffFactor(const val: Single);
    function StoreRollOffFactor: Boolean;
    procedure SetDopplerFactor(const val: Single);
    procedure SetSoundEnvironment(const val: TDGLSoundEnvironment);

    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;

    procedure ListenerCoordinates(var position, velocity, direction, up: TVector);

    function DoActivate: Boolean; dynamic;
    // : Invoked AFTER all sources have been stopped
    procedure DoDeActivate; dynamic;
    { : Effect mute of all sounds.<p>
      Default implementation call MuteSource for all non-muted sources
      with "True" as parameter. }
    function DoMute: Boolean; dynamic;
    { : Effect un-mute of all sounds.<p>
      Default implementation call MuteSource for all non-muted sources
      with "False" as parameter. }
    procedure DoUnMute; dynamic;
    { : Effect pause of all sounds.<p>
      Default implementation call PauseSource for all non-paused sources
      with "True" as parameter. }
    function DoPause: Boolean; dynamic;
    { : Effect un-pause of all sounds.<p>
      Default implementation call PauseSource for all non-paused sources
      with "True" as parameter. }
    procedure DoUnPause; dynamic;

    procedure NotifyMasterVolumeChange; dynamic;
    procedure Notify3DFactorsChanged; dynamic;
    procedure NotifyEnvironmentChanged; dynamic;

    // : Called when a source will be freed
    procedure KillSource(aSource: TDGLBaseSoundSource); virtual;
    { : Request to update source's data in low-level sound API.<p>
      Default implementation just clears the "Changes" flags. }
    procedure UpdateSource(aSource: TDGLBaseSoundSource); virtual;
    procedure MuteSource(aSource: TDGLBaseSoundSource; muted: Boolean); virtual;
    procedure PauseSource(aSource: TDGLBaseSoundSource; paused: Boolean); virtual;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { : Manual request to update all sources to reflect changes.<p>
      Default implementation invokes UpdateSource for all known sources. }
    procedure UpdateSources; virtual;
    { : Stop and free all sources. }
    procedure StopAllSources;

    { : Progress notification for time synchronization.<p>
      This method will call UpdateSources depending on the last time
      it was performed and the value of the UpdateFrequency property. }
    procedure DoProgress(const progressTime: TProgressTimes); override;

    { : Sound manager API reported CPU Usage.<p>
      Returns -1 when unsupported. }
    function CPUUsagePercent: Single; virtual;
    { : True if EAX is supported. }
    function EAXSupported: Boolean; dynamic;

  published
    { Published Declarations }
    { : Activation/deactivation of the low-level sound API }
    property Active: Boolean read FActive write SetActive default False;

    { : Maximum number of sound output channels.<p>
      While some drivers will just ignore this value, others cannot
      dynamically adjust the maximum number of channels (you need to
      de-activate and re-activate the manager for this property to be
      taken into account). }
    property MaxChannels: Integer read FMaxChannels write SetMaxChannels default 8;
    { : Sound output mixing frequency.<p>
      Commonly used values ar 11025, 22050 and 44100.<p>
      Note that most driver cannot dynamically adjust the output frequency
      (you need to de-ativate and re-activate the manager for this property
      to be taken into account). }
    property OutputFrequency: Integer read FOutputFrequency write SetOutputFrequency default 44100;

    { : Request to mute all sounds.<p>
      All sound requests should be handled as if sound is unmuted though,
      however drivers should try to take a CPU advantage of mute over
      MasterVolume=0 }
    property Mute: Boolean read FMute write SetMute default False;
    { : Request to pause all sound, sound output should be muted too.<p>
      When unpausing, all sound should resume at the point they were paused. }
    property Pause: Boolean read FPause write SetPause default False;
    { : Master Volume adjustement in the [0.0; 1.0] range.<p>
      Driver should take care of properly clamping the master volume. }
    property MasterVolume: Single read FMasterVolume write SetMasterVolume;

    { : Scene object that materializes the listener.<p>
      The sceneobject's AbsolutePosition and orientation are used to define
      the listener coordinates, velocity is automatically calculated
      (if you're using DoProgress or connected the manager to a cadencer).<p>
      If this property is nil, the listener is assumed to be static at
      the NullPoint coordinate, facing Z axis, with up being Y (ie. the
      default GLScene orientation). }
    property Listener: TDGLBaseSceneObject read FListener write SetListener;
    { : Currently active and playing sound sources. }
    property Sources: TDGLSoundSources read FSources write SetSources;

    { : Update frequency for time-based control (DoProgress).<p>
      Default value is 10 Hz (frequency is clamped in the 1Hz-60Hz range). }
    property UpdateFrequency: Single read FUpdateFrequency write SetUpdateFrequency stored StoreUpdateFrequency;
    { : Cadencer for time-based control.<p> }
    property Cadencer: TDGLCadencer read FCadencer write SetCadencer;
    { : Engine relative distance factor, compared to 1.0 meters.<p>
      Equates to 'how many units per meter' your engine has. }
    property DistanceFactor: Single read FDistanceFactor write SetDistanceFactor stored StoreDistanceFactor;
    { : Sets the global attenuation rolloff factor.<p>
      Normally volume for a sample will scale at 1 / distance.
      This gives a logarithmic attenuation of volume as the source gets
      further away (or closer).<br>
      Setting this value makes the sound drop off faster or slower.
      The higher the value, the faster volume will fall off. }
    property RollOffFactor: Single read FRollOffFactor write SetRollOffFactor stored StoreRollOffFactor;
    { : Engine relative Doppler factor, compared to 1.0 meters.<p>
      Equates to 'how many units per meter' your engine has. }
    property DopplerFactor: Single read FDopplerFactor write SetDopplerFactor stored False;
    { : Sound environment (requires EAX compatible soundboard). }
    property Environment: TDGLSoundEnvironment read FSoundEnvironment write SetSoundEnvironment default seDefault;
  end;

  // ****************************************************************************************
  // TDGLBSoundEmitter
  //
  { : A sound emitter behaviour, plug it on any object to make it noisy.<p>
    This behaviour is just an interface to a TDGLSoundSource, for editing
    convenience. }
  TDGLBSoundEmitter = class(TDGLBehaviour)
  private
    { Private Declarations }
    FPlaying:       Boolean; // used at design-time ONLY
    FSource:        TDGLBaseSoundSource;
    FPlayingSource: TDGLSoundSource;

  protected
    { Protected Declarations }
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;

    procedure SetSource(const val: TDGLBaseSoundSource);
    procedure SetPlaying(const val: Boolean);
    function GetPlaying: Boolean;

    procedure NotifySourceDestruction(aSource: TDGLSoundSource);

  public
    { Public Declarations }
    constructor Create(AOwner: TDGLXCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    class function UniqueItem: Boolean; override;

    procedure DoProgress(const progressTime: TProgressTimes); override;

    property PlayingSource: TDGLSoundSource read FPlayingSource;

  published
    { Published Declarations }
    property Source:  TDGLBaseSoundSource read FSource write SetSource;
    property Playing: Boolean read GetPlaying write SetPlaying default False;

  end;

function ActiveSoundManager: TDGLSoundManager;
function GetSoundLibraryByName(const aName: string): TDGLSoundLibrary;

function GetOrCreateSoundEmitter(behaviours: TDGLBehaviours): TDGLBSoundEmitter; overload;
function GetOrCreateSoundEmitter(obj: TDGLBaseSceneObject): TDGLBSoundEmitter; overload;

var
  // If this variable is true, errors in GLSM may be displayed to the user
  vVerboseGLSMErrors: Boolean = True;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
  vActiveSoundManager: TDGLSoundManager;
  vSoundLibraries:     TList;

// ------------------
{ Helpers Functions }
{$IFDEF GLS_REGION}{$REGION 'Helpers Functions'}{$ENDIF}

function ActiveSoundManager: TDGLSoundManager;
begin
  Result := vActiveSoundManager;
end;

function GetSoundLibraryByName(const aName: string): TDGLSoundLibrary;
var
  i: Integer;
begin
  Result := nil;
  if Assigned(vSoundLibraries) then
    for i := 0 to vSoundLibraries.Count - 1 do
      if TDGLSoundLibrary(vSoundLibraries[i]).Name = aName then
      begin
        Result := TDGLSoundLibrary(vSoundLibraries[i]);
        Break;
      end;
end;

function GetOrCreateSoundEmitter(behaviours: TDGLBehaviours): TDGLBSoundEmitter;
var
  i: Integer;
begin
  i := behaviours.IndexOfClass(TDGLBSoundEmitter);
  if i >= 0 then
    Result := TDGLBSoundEmitter(behaviours[i])
  else
    Result := TDGLBSoundEmitter.Create(behaviours);
end;


function GetOrCreateSoundEmitter(obj: TDGLBaseSceneObject): TDGLBSoundEmitter;
begin
  Result := GetOrCreateSoundEmitter(obj.behaviours);
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLSoundSample }
{$IFDEF GLS_REGION}{$REGION 'TDGLSoundSample'}{$ENDIF}

constructor TDGLSoundSample.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TDGLSoundSample.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TDGLSoundSample.Assign(Source: TPersistent);
begin
  if Source is TDGLSoundSample then
  begin
    FName := TDGLSoundSample(Source).Name;
    FData.Free;
    FData := TDGLSoundFile(TDGLSoundSample(Source).Data.CreateCopy(Self));
  end
  else
    inherited Assign(Source); // Assign error
end;

procedure TDGLSoundSample.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('BinData', ReadData, WriteData, Assigned(FData));
end;

procedure TDGLSoundSample.ReadData(Stream: TStream);
var
  n:      Integer;
  clName: AnsiString;
begin
  with Stream do
  begin
    Read(n, SizeOf(Integer));
    SetLength(clName, n);
    if n > 0 then
      Read(clName[1], n);
    FData := TDGLSoundFileClass(FindClass(string(clName))).Create(Self);
    FData.LoadFromStream(Stream);
  end;
end;

procedure TDGLSoundSample.WriteData(Stream: TStream);
var
  n:   Integer;
  buf: AnsiString;
begin
  with Stream do
  begin
    n := Length(FData.ClassName);
    Write(n, SizeOf(Integer));
    buf := AnsiString(FData.ClassName);
    if n > 0 then
      Write(buf[1], n);
    FData.SaveToStream(Stream);
  end;
end;

function TDGLSoundSample.GetDisplayName: string;
var
  s: string;
begin
  if Assigned(FData) then
  begin
    if Data.Sampling.NbChannels > 1 then
      s := 's'
    else
      s    := '';
    Result := Format('%s (%d Hz, %d bits, %d channel%s, %.2f sec)', [Name, Data.Sampling.Frequency, Data.Sampling.BitsPerSample, Data.Sampling.NbChannels, s, LengthInSec])
  end
  else
    Result := Format('%s (empty)', [Name]);
end;

procedure TDGLSoundSample.LoadFromFile(const fileName: string);
var
  sfc: TDGLSoundFileClass;
begin
  FData.Free;
  sfc := GeTDGLSoundFileFormats.FindExt(ExtractFileExt(fileName));
  if Assigned(sfc) then
  begin
    FData := sfc.Create(Self);
    FData.LoadFromFile(fileName);
  end
  else
    FData := nil;
  Assert(Data <> nil, 'Could not load ' + fileName + ', make sure you include the unit required to load this format in your uses clause.');
  Name := ExtractFileName(fileName);
end;

procedure TDGLSoundSample.PlayOnWaveOut;
begin
  if Assigned(FData) then
    FData.PlayOnWaveOut;
end;

function TDGLSoundSample.Sampling: TDGLSoundSampling;
begin
  if Assigned(FData) then
    Result := FData.Sampling
  else
    Result := nil;
end;

function TDGLSoundSample.LengthInBytes: Integer;
begin
  if Assigned(FData) then
    Result := FData.LengthInBytes
  else
    Result := 0;
end;

function TDGLSoundSample.LengthInSamples: Integer;
begin
  if Assigned(FData) then
    Result := FData.LengthInSamples
  else
    Result := 0;
end;

function TDGLSoundSample.LengthInSec: Single;
begin
  if Assigned(FData) then
    Result := FData.LengthInSec
  else
    Result := 0;
end;

procedure TDGLSoundSample.SetData(const val: TDGLSoundFile);
begin
  FData.Free;
  if Assigned(val) then
    FData := TDGLSoundFile(val.CreateCopy(Self))
  else
    FData := nil;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLSoundSamples }
{$IFDEF GLS_REGION}{$REGION 'TDGLSoundSamples'}{$ENDIF}

constructor TDGLSoundSamples.Create(AOwner: TComponent);
begin
  owner := AOwner;
  inherited Create(TDGLSoundSample);
end;

function TDGLSoundSamples.GetOwner: TPersistent;
begin
  Result := owner;
end;

procedure TDGLSoundSamples.SetItems(index: Integer; const val: TDGLSoundSample);
begin
  inherited Items[index] := val;
end;

function TDGLSoundSamples.GetItems(index: Integer): TDGLSoundSample;
begin
  Result := TDGLSoundSample(inherited Items[index]);
end;

function TDGLSoundSamples.Add: TDGLSoundSample;
begin
  Result := (inherited Add) as TDGLSoundSample;
end;

function TDGLSoundSamples.FindItemID(ID: Integer): TDGLSoundSample;
begin
  Result := (inherited FindItemID(ID)) as TDGLSoundSample;
end;

function TDGLSoundSamples.GetByName(const aName: string): TDGLSoundSample;
var
  i: Integer;
begin
  Result := nil;
  for i  := 0 to Count - 1 do
    if CompareText(Items[i].Name, aName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TDGLSoundSamples.AddFile(const fileName: string; const sampleName: string = ''): TDGLSoundSample;
begin
  Result := Add;
  Result.LoadFromFile(fileName);
  if sampleName <> '' then
    Result.Name := sampleName;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLSoundLibrary }
{$IFDEF GLS_REGION}{$REGION 'TDGLSoundLibrary'}{$ENDIF}

constructor TDGLSoundLibrary.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSamples := TDGLSoundSamples.Create(Self);
  vSoundLibraries.Add(Self);
end;

destructor TDGLSoundLibrary.Destroy;
begin
  vSoundLibraries.Remove(Self);
  FSamples.Free;
  inherited Destroy;
end;

procedure TDGLSoundLibrary.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
end;

procedure TDGLSoundLibrary.SetSamples(const val: TDGLSoundSamples);
begin
  FSamples.Assign(val);
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLBaseSoundSource }
{$IFDEF GLS_REGION}{$REGION 'TDGLBaseSoundSource'}{$ENDIF}

constructor TDGLBaseSoundSource.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FChanges           := [sscTransformation, sscSample, sscStatus];
  FVolume            := 1.0;
  FMinDistance       := 1.0;
  FMaxDistance       := 100.0;
  FInsideConeAngle   := 360;
  FOutsideConeAngle  := 360;
  FConeOutsideVolume := 0.0;
  FNbLoops           := 1;
  FFrequency         := -1;
end;

destructor TDGLBaseSoundSource.Destroy;
begin
  inherited Destroy;
end;

function TDGLBaseSoundSource.GetDisplayName: string;
begin
  Result := Format('%s', [FSoundName]);
end;

procedure TDGLBaseSoundSource.Assign(Source: TPersistent);
begin
  if Source is TDGLBaseSoundSource then
  begin
    FPriority          := TDGLBaseSoundSource(Source).FPriority;
    FOrigin            := TDGLBaseSoundSource(Source).FOrigin;
    FVolume            := TDGLBaseSoundSource(Source).FVolume;
    FMinDistance       := TDGLBaseSoundSource(Source).FMinDistance;
    FMaxDistance       := TDGLBaseSoundSource(Source).FMaxDistance;
    FInsideConeAngle   := TDGLBaseSoundSource(Source).FInsideConeAngle;
    FOutsideConeAngle  := TDGLBaseSoundSource(Source).FOutsideConeAngle;
    FConeOutsideVolume := TDGLBaseSoundSource(Source).FConeOutsideVolume;
    FSoundLibraryName  := TDGLBaseSoundSource(Source).FSoundLibraryName;
    FSoundLibrary      := TDGLBaseSoundSource(Source).FSoundLibrary;
    FSoundName         := TDGLBaseSoundSource(Source).FSoundName;
    FMute              := TDGLBaseSoundSource(Source).FMute;
    FPause             := TDGLBaseSoundSource(Source).FPause;
    FChanges           := [sscTransformation, sscSample, sscStatus];
    FNbLoops           := TDGLBaseSoundSource(Source).FNbLoops;
    FFrequency         := TDGLBaseSoundSource(Source).FFrequency;
  end
  else
    inherited Assign(Source);
end;

procedure TDGLBaseSoundSource.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteInteger(FPriority);
    WriteFloat(FVolume);
    WriteFloat(FMinDistance);
    WriteFloat(FMaxDistance);
    WriteFloat(FInsideConeAngle);
    WriteFloat(FOutsideConeAngle);
    WriteFloat(FConeOutsideVolume);
    if Assigned(FSoundLibrary) then
      WriteString(FSoundLibrary.Name)
    else
      WriteString(FSoundLibraryName);
    WriteString(FSoundName);
    WriteBoolean(FMute);
    WriteBoolean(FPause);
    WriteInteger(FNbLoops);
    // WriteInteger(FFrequency);
  end;
end;

procedure TDGLBaseSoundSource.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    ReadInteger; // ignore archiveVersion
    FPriority          := ReadInteger;
    FVolume            := ReadFloat;
    FMinDistance       := ReadFloat;
    FMaxDistance       := ReadFloat;
    FInsideConeAngle   := ReadFloat;
    FOutsideConeAngle  := ReadFloat;
    FConeOutsideVolume := ReadFloat;
    FSoundLibraryName  := ReadString;
    FSoundLibrary      := nil;
    FSoundName         := ReadString;
    FMute              := ReadBoolean;
    FPause             := ReadBoolean;
    FChanges           := [sscTransformation, sscSample, sscStatus];
    FNbLoops           := ReadInteger;
    // FFrequency:=ReadInteger;
  end;
end;

function TDGLBaseSoundSource.Sample: TDGLSoundSample;
begin
  if SoundLibrary <> nil then
    Result := FSoundLibrary.Samples.GetByName(FSoundName)
  else
    Result := nil;
end;

procedure TDGLBaseSoundSource.SetPriority(const val: Integer);
begin
  if val <> FPriority then
  begin
    FPriority := val;
    Include(FChanges, sscStatus);
  end;
end;

procedure TDGLBaseSoundSource.SetOrigin(const val: TDGLBaseSceneObject);
begin
  if val <> FOrigin then
  begin
    FOrigin := val;
    Include(FChanges, sscTransformation);
  end;
end;

procedure TDGLBaseSoundSource.SetVolume(const val: Single);
begin
  if val <> FVolume then
  begin
    FVolume := ClampValue(val, 0, 1);
    Include(FChanges, sscStatus);
  end;
end;

procedure TDGLBaseSoundSource.SetMinDistance(const val: Single);
begin
  if val <> FMinDistance then
  begin
    FMinDistance := ClampValue(val, 0);
    Include(FChanges, sscStatus);
  end;
end;

procedure TDGLBaseSoundSource.SetMaxDistance(const val: Single);
begin
  if val <> FMaxDistance then
  begin
    FMaxDistance := ClampValue(val, 0);
    Include(FChanges, sscStatus);
  end;
end;

procedure TDGLBaseSoundSource.SetInsideConeAngle(const val: Single);
begin
  if val <> FInsideConeAngle then
  begin
    FInsideConeAngle := ClampValue(val, 0, 360);
    Include(FChanges, sscStatus);
  end;
end;

procedure TDGLBaseSoundSource.SetOutsideConeAngle(const val: Single);
begin
  if val <> FOutsideConeAngle then
  begin
    FOutsideConeAngle := ClampValue(val, 0, 360);
    Include(FChanges, sscStatus);
  end;
end;

procedure TDGLBaseSoundSource.SetConeOutsideVolume(const val: Single);
begin
  if val <> FConeOutsideVolume then
  begin
    FConeOutsideVolume := ClampValue(val, 0, 1);
    Include(FChanges, sscStatus);
  end;
end;

function TDGLBaseSoundSource.GetSoundLibrary: TDGLSoundLibrary;
begin
  if (FSoundLibrary = nil) and (FSoundLibraryName <> '') then
    FSoundLibrary := GetSoundLibraryByName(FSoundLibraryName);
  Result          := FSoundLibrary;
end;

procedure TDGLBaseSoundSource.SetSoundLibrary(const val: TDGLSoundLibrary);
begin
  if val <> FSoundLibrary then
  begin
    FSoundLibrary := val;
    if Assigned(FSoundLibrary) then
      FSoundLibraryName := FSoundLibrary.Name
    else
      FSoundLibraryName := '';
    Include(FChanges, sscSample);
  end;
end;

procedure TDGLBaseSoundSource.SetSoundName(const val: string);
begin
  if val <> FSoundName then
  begin
    FSoundName := val;
    Include(FChanges, sscSample);
  end;
end;

procedure TDGLBaseSoundSource.SetPause(const val: Boolean);
begin
  if val <> FPause then
  begin
    FPause := val;
    if Collection <> nil then
      TDGLSoundManager(TDGLSoundSources(Collection).owner).PauseSource(Self, FPause);
  end;
end;

procedure TDGLBaseSoundSource.SetNbLoops(const val: Integer);
begin
  if val <> FNbLoops then
  begin
    FNbLoops := val;
    Include(FChanges, sscSample);
  end;
end;

procedure TDGLBaseSoundSource.SetFrequency(const val: Integer);
begin
  if val <> FFrequency then
  begin
    FFrequency := val;
    Include(FChanges, sscStatus);
  end;
end;

procedure TDGLBaseSoundSource.SetMute(const val: Boolean);
begin
  if val <> FMute then
  begin
    FMute := val;
    if Collection <> nil then
      TDGLSoundManager(TDGLSoundSources(Collection).owner).MuteSource(Self, FMute);
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLSoundSource }
{$IFDEF GLS_REGION}{$REGION 'TDGLSoundSource'}{$ENDIF}

destructor TDGLSoundSource.Destroy;
begin
  if Assigned(FBehaviourToNotify) then
    FBehaviourToNotify.NotifySourceDestruction(Self);
  if Collection <> nil then
    ((Collection as TDGLSoundSources).owner as TDGLSoundManager).KillSource(Self);
  inherited;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLSoundSources }
{$IFDEF GLS_REGION}{$REGION 'TDGLSoundSources'}{$ENDIF}

constructor TDGLSoundSources.Create(AOwner: TComponent);
begin
  owner := AOwner;
  inherited Create(TDGLSoundSource);
end;

function TDGLSoundSources.GetOwner: TPersistent;
begin
  Result := owner;
end;

procedure TDGLSoundSources.SetItems(index: Integer; const val: TDGLSoundSource);
begin
  inherited Items[index] := val;
end;

function TDGLSoundSources.GetItems(index: Integer): TDGLSoundSource;
begin
  Result := TDGLSoundSource(inherited Items[index]);
end;

function TDGLSoundSources.Add: TDGLSoundSource;
begin
  Result := (inherited Add) as TDGLSoundSource;
end;

function TDGLSoundSources.FindItemID(ID: Integer): TDGLSoundSource;
begin
  Result := (inherited FindItemID(ID)) as TDGLSoundSource;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLSoundManager }
{$IFDEF GLS_REGION}{$REGION 'TDGLSoundManager'}{$ENDIF}

constructor TDGLSoundManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSources         := TDGLSoundSources.Create(Self);
  FMasterVolume    := 1.0;
  FOutputFrequency := 44100;
  FMaxChannels     := 8;
  FUpdateFrequency := 10;
  FLastUpdateTime  := -1E30;
  FDistanceFactor  := 1.0;
  FRollOffFactor   := 1.0;
  FDopplerFactor   := 1.0;
end;

destructor TDGLSoundManager.Destroy;
begin
  Active   := False;
  Listener := nil;
  FSources.Free;
  inherited Destroy;
end;

procedure TDGLSoundManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FListener then
      Listener := nil;
    if AComponent = FCadencer then
      Cadencer := nil;
  end;
  inherited;
end;

procedure TDGLSoundManager.SetActive(const val: Boolean);
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    FActive := val
  else if val <> FActive then
  begin
    if val then
    begin
      if Assigned(vActiveSoundManager) then
        vActiveSoundManager.Active := False;
      if DoActivate then
      begin
        FActive             := True;
        vActiveSoundManager := Self;
      end;
    end
    else
    begin
      try
        StopAllSources;
        DoDeActivate;
      finally
        FActive             := val;
        vActiveSoundManager := nil;
      end;
    end;
  end;
end;

function TDGLSoundManager.DoActivate: Boolean;
begin
  Result := True;
end;

procedure TDGLSoundManager.DoDeActivate;
begin
  StopAllSources;
end;

procedure TDGLSoundManager.SetMute(const val: Boolean);
begin
  if val <> FMute then
  begin
    if val then
    begin
      if DoMute then
        FMute := True
    end
    else
    begin
      DoUnMute;
      FMute := False;
    end;
  end;
end;

function TDGLSoundManager.DoMute: Boolean;
var
  i: Integer;
begin
  for i := 0 to Sources.Count - 1 do
    if not Sources[i].Mute then
      MuteSource(Sources[i], True);
  Result := True;
end;

procedure TDGLSoundManager.DoUnMute;
var
  i: Integer;
begin
  for i := 0 to Sources.Count - 1 do
    if not Sources[i].Mute then
      MuteSource(Sources[i], False);
end;

procedure TDGLSoundManager.SetPause(const val: Boolean);
begin
  if val <> FPause then
  begin
    if val then
    begin
      if DoPause then
        FPause := True
    end
    else
    begin
      DoUnPause;
      FPause := False;
    end;
  end;
end;

procedure TDGLSoundManager.Loaded;
begin
  inherited;
  if Active and (not(csDesigning in ComponentState)) then
  begin
    FActive := False;
    Active  := True;
  end;
end;

procedure TDGLSoundManager.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Doppler', ReadDoppler, WriteDoppler, (DopplerFactor <> 1));
end;

procedure TDGLSoundManager.WriteDoppler(writer: TWriter);
begin
  writer.WriteFloat(DopplerFactor);
end;

procedure TDGLSoundManager.ReadDoppler(reader: TReader);
begin
  FDopplerFactor := reader.ReadFloat;
end;

function TDGLSoundManager.DoPause: Boolean;
var
  i: Integer;
begin
  for i := 0 to Sources.Count - 1 do
    if not Sources[i].Pause then
      PauseSource(Sources[i], True);
  Result := True;
end;

procedure TDGLSoundManager.DoUnPause;
var
  i: Integer;
begin
  for i := 0 to Sources.Count - 1 do
    if not Sources[i].Pause then
      PauseSource(Sources[i], False);
end;

procedure TDGLSoundManager.SetMasterVolume(const val: Single);
begin
  if val < 0 then
    FMasterVolume := 0
  else if val > 1 then
    FMasterVolume := 1
  else
    FMasterVolume := val;
  NotifyMasterVolumeChange;
end;

procedure TDGLSoundManager.SetMaxChannels(const val: Integer);
begin
  if val <> FMaxChannels then
  begin
    if val < 1 then
      FMaxChannels := 1
    else
      FMaxChannels := val;
  end;
end;

procedure TDGLSoundManager.SetOutputFrequency(const val: Integer);
begin
  if val <> FOutputFrequency then
  begin
    if val < 11025 then
      FOutputFrequency := 11025
    else
      FOutputFrequency := val;
  end;
end;

procedure TDGLSoundManager.SetUpdateFrequency(const val: Single);
begin
  FUpdateFrequency := ClampValue(val, 1, 60);
end;

function TDGLSoundManager.StoreUpdateFrequency: Boolean;
begin
  Result := (FUpdateFrequency <> 10);
end;

procedure TDGLSoundManager.SetCadencer(const val: TDGLCadencer);
begin
  if val <> FCadencer then
  begin
    if Assigned(FCadencer) then
      FCadencer.UnSubscribe(Self);
    FCadencer := val;
    if Assigned(FCadencer) then
      FCadencer.Subscribe(Self);
  end;
end;

procedure TDGLSoundManager.SetDistanceFactor(const val: Single);
begin
  if val <= 0 then
    FDistanceFactor := 1
  else
    FDistanceFactor := val;
  Notify3DFactorsChanged;
end;

function TDGLSoundManager.StoreDistanceFactor: Boolean;
begin
  Result := (FDistanceFactor <> 1);
end;

procedure TDGLSoundManager.SetRollOffFactor(const val: Single);
begin
  if val <= 0 then
    FRollOffFactor := 1
  else
    FRollOffFactor := val;
  Notify3DFactorsChanged;
end;

function TDGLSoundManager.StoreRollOffFactor: Boolean;
begin
  Result := (FRollOffFactor <> 1);
end;

procedure TDGLSoundManager.SetDopplerFactor(const val: Single);
begin
  if val < 0 then
    FDopplerFactor := 0
  else if val > 10 then
    FDopplerFactor := 10
  else
    FDopplerFactor := val;
  Notify3DFactorsChanged;
end;

procedure TDGLSoundManager.SetSoundEnvironment(const val: TDGLSoundEnvironment);
begin
  if val <> FSoundEnvironment then
  begin
    FSoundEnvironment := val;
    NotifyEnvironmentChanged;
  end;
end;

procedure TDGLSoundManager.ListenerCoordinates(var position, velocity, direction, up: TVector);
var
  right: TVector;
begin
  if Listener <> nil then
  begin
    position := Listener.AbsolutePosition;
    if FLastDeltaTime <> 0 then
    begin
      velocity := VectorSubtract(position, FLastListenerPosition);
      ScaleVector(velocity, 1 / FLastDeltaTime);
    end;
    FLastListenerPosition := position;
    if (Listener is TDGLCamera) and (TDGLCamera(Listener).TargetObject <> nil) then
    begin
      // special case of the camera targeting something
      direction := TDGLCamera(Listener).AbsoluteVectorToTarget;
      NormalizeVector(direction);
      up    := Listener.AbsoluteYVector;
      right := VectorCrossProduct(direction, up);
      up    := VectorCrossProduct(right, direction);
    end
    else
    begin
      direction := Listener.AbsoluteZVector;
      up        := Listener.AbsoluteYVector;
    end;
  end
  else
  begin
    position  := NullHmgPoint;
    velocity  := NullHmgVector;
    direction := ZHmgVector;
    up        := YHmgVector;
  end;
end;

procedure TDGLSoundManager.NotifyMasterVolumeChange;
begin
  // nothing
end;

procedure TDGLSoundManager.Notify3DFactorsChanged;
begin
  // nothing
end;

procedure TDGLSoundManager.NotifyEnvironmentChanged;
begin
  // nothing
end;

procedure TDGLSoundManager.SetListener(const val: TDGLBaseSceneObject);
begin
  if Assigned(FListener) then
    FListener.RemoveFreeNotification(Self);
  FListener := val;
  if Assigned(FListener) then
    FListener.FreeNotification(Self);
end;

procedure TDGLSoundManager.SetSources(const val: TDGLSoundSources);
begin
  FSources.Assign(val);
end;

procedure TDGLSoundManager.KillSource(aSource: TDGLBaseSoundSource);
begin
  // nothing
end;

procedure TDGLSoundManager.UpdateSource(aSource: TDGLBaseSoundSource);
begin
  aSource.FChanges := [];
end;

procedure TDGLSoundManager.MuteSource(aSource: TDGLBaseSoundSource; muted: Boolean);
begin
  // nothing
end;

procedure TDGLSoundManager.PauseSource(aSource: TDGLBaseSoundSource; paused: Boolean);
begin
  // nothing
end;

procedure TDGLSoundManager.UpdateSources;
var
  i: Integer;
begin
  for i := Sources.Count - 1 downto 0 do
    UpdateSource(Sources[i]);
end;

procedure TDGLSoundManager.StopAllSources;
var
  i: Integer;
begin
  for i := Sources.Count - 1 downto 0 do
    Sources.Delete(i);
end;

procedure TDGLSoundManager.DoProgress(const progressTime: TProgressTimes);
begin
  if not Active then
    Exit;
  with progressTime do
    if newTime - FLastUpdateTime > 1 / FUpdateFrequency then
    begin
      FLastDeltaTime  := newTime - FLastUpdateTime;
      FLastUpdateTime := newTime;
      UpdateSources;
    end;
end;

function TDGLSoundManager.CPUUsagePercent: Single;
begin
  Result := -1;
end;

function TDGLSoundManager.EAXSupported: Boolean;
begin
  Result := False;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLSoundEmitter }
{$IFDEF GLS_REGION}{$REGION 'TDGLSoundEmitter'}{$ENDIF}

constructor TDGLBSoundEmitter.Create(AOwner: TDGLXCollection);
begin
  inherited Create(AOwner);
  FSource := TDGLSoundSource.Create(nil);
end;

destructor TDGLBSoundEmitter.Destroy;
begin
  if Assigned(FPlayingSource) then
    FPlayingSource.Free;
  FSource.Free;
  inherited Destroy;
end;

procedure TDGLBSoundEmitter.Assign(Source: TPersistent);
begin
  if Source is TDGLBSoundEmitter then
  begin
    FSource.Assign(TDGLBSoundEmitter(Source).FSource);
  end;
  inherited Assign(Source);
end;

procedure TDGLBSoundEmitter.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FSource.WriteToFiler(writer);
    WriteBoolean(FPlaying);
  end;
end;

procedure TDGLBSoundEmitter.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    ReadInteger; // ignore archiveVersion
    FSource.ReadFromFiler(reader);
    FPlaying := ReadBoolean;
  end;
end;

procedure TDGLBSoundEmitter.Loaded;
begin
  inherited;
  if not(csDesigning in OwnerBaseSceneObject.ComponentState) then
    SetPlaying(FPlaying);
end;

class function TDGLBSoundEmitter.FriendlyName: string;
begin
  Result := 'Sound Emitter';
end;

class function TDGLBSoundEmitter.FriendlyDescription: string;
begin
  Result := 'A simple sound emitter behaviour';
end;

class function TDGLBSoundEmitter.UniqueItem: Boolean;
begin
  Result := False;
end;

procedure TDGLBSoundEmitter.DoProgress(const progressTime: TProgressTimes);
begin
  // nothing, yet
end;

procedure TDGLBSoundEmitter.SetSource(const val: TDGLBaseSoundSource);
begin
  FSource.Assign(val);
end;

procedure TDGLBSoundEmitter.SetPlaying(const val: Boolean);
begin
  if csDesigning in OwnerBaseSceneObject.ComponentState then
    FPlaying := val
  else if ActiveSoundManager <> nil then
  begin
    if val <> Playing then
    begin
      if val then
      begin
        FPlayingSource                    := ActiveSoundManager.Sources.Add;
        FPlayingSource.FBehaviourToNotify := Self;
        FPlayingSource.Assign(FSource);
        FPlayingSource.Origin := OwnerBaseSceneObject;
      end
      else
        FPlayingSource.Free;
    end;
  end
  else if vVerboseGLSMErrors then
    InformationDlg('No Active Sound Manager.'#13#10'Make sure manager is created before emitter');
end;

function TDGLBSoundEmitter.GetPlaying: Boolean;
begin
  if csDesigning in OwnerBaseSceneObject.ComponentState then
    Result := FPlaying
  else
    Result := Assigned(FPlayingSource);
end;

procedure TDGLBSoundEmitter.NotifySourceDestruction(aSource: TDGLSoundSource);
begin
  Assert(FPlayingSource = aSource);
  FPlayingSource := nil;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

initialization

  // class registrations
  RegisterClasses([TDGLSoundLibrary]);
  RegisterXCollectionItemClass(TDGLBSoundEmitter);
  vSoundLibraries := TList.Create;

finalization

  if Assigned(vActiveSoundManager) then
    vActiveSoundManager.Active := False;

  vSoundLibraries.Free;
  vSoundLibraries := nil;

  UnregisterXCollectionItemClass(TDGLBSoundEmitter);

end.
