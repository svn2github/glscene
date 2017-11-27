//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  Base classes and interface for GLScene Sound System 
}

unit VXS.Sound;

interface

uses
  System.Classes, System.SysUtils, System.Types,

  VXS.SoundFileObjects, VXS.Scene, VXS.XCollection, VXS.VectorGeometry,
  VXS.Cadencer, VXS.BaseClasses, VXS.CrossPlatform, VXS.Utils;

{$I VXScene.inc}

type

  // TVXSoundSample
  //
    { Stores a single PCM coded sound sample. }
  TVXSoundSample = class(TCollectionItem)
  private
    
    FName: string;
    FData: TVXSoundFile;
    FTag: Integer;

  protected
    
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream); virtual;
    procedure WriteData(Stream: TStream); virtual;
    function GetDisplayName: string; override;
    procedure SetData(const val: TVXSoundFile);

  public
    
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure LoadFromFile(const fileName: string);

    procedure PlayOnWaveOut;

    function Sampling: TVXSoundSampling;
    function LengthInBytes: Integer;
    function LengthInSamples: Integer;
    function LengthInSec: Single;

    // This Tag is reserved for sound manager use only
    property ManagerTag: Integer read FTag write FTag;

  published
    
    property Name: string read FName write FName;
    property Data: TVXSoundFile read FData write SetData stored False;
  end;

  // TVXSoundSamples
  //
  TVXSoundSamples = class(TCollection)
  protected
    
    owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TVXSoundSample);
    function GetItems(index: Integer): TVXSoundSample;

  public
    
    constructor Create(AOwner: TComponent);
    function Add: TVXSoundSample;
    function FindItemID(ID: Integer): TVXSoundSample;
    property Items[index: Integer]: TVXSoundSample read GetItems write SetItems;
      default;
    function GetByName(const aName: string): TVXSoundSample;

    function AddFile(const fileName: string; const sampleName: string = ''):
      TVXSoundSample;
  end;

  // TVXSoundLibrary
  //
  TVXSoundLibrary = class(TComponent)
  private
    
    FSamples: TVXSoundSamples;

  protected
    
    procedure SetSamples(const val: TVXSoundSamples);

    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

  public
    
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    
    property Samples: TVXSoundSamples read FSamples write SetSamples;
  end;

  // TVXSoundSource
  //
  TVXSoundSourceChange = (sscTransformation, sscSample, sscStatus);
  TVXSoundSourceChanges = set of TVXSoundSourceChange;

  TVXBSoundEmitter = class;

  // TVXBaseSoundSource
  //
    { Base class for origin of sound playback. }
  TVXBaseSoundSource = class(TCollectionItem)
  private
    
    FBehaviourToNotify: TVXBSoundEmitter;
      // private only, NOT persistent, not assigned
    FPriority: Integer;
    FOrigin: TVXBaseSceneObject; // NOT persistent
    FVolume: Single;
    FMinDistance, FMaxDistance: Single;
    FInsideConeAngle, FOutsideConeAngle: Single;
    FConeOutsideVolume: Single;
    FSoundLibraryName: string; // used for persistence
    FSoundLibrary: TVXSoundLibrary; // persistence via name
    FSoundName: string;
    FMute: Boolean;
    FPause: Boolean;
    FChanges: TVXSoundSourceChanges; // NOT persistent, not assigned
    FNbLoops: Integer;
    FTag: PtrUInt; // NOT persistent, not assigned
    FFrequency: Integer;

  protected
    
    procedure WriteToFiler(writer: TWriter);
    procedure ReadFromFiler(reader: TReader);

    function GetDisplayName: string; override;
    procedure SetPriority(const val: Integer);
    procedure SetOrigin(const val: TVXBaseSceneObject);
    procedure SetVolume(const val: Single);
    procedure SetMinDistance(const val: Single);
    procedure SetMaxDistance(const val: Single);
    procedure SetInsideConeAngle(const val: Single);
    procedure SetOutsideConeAngle(const val: Single);
    procedure SetConeOutsideVolume(const val: Single);
    function GetSoundLibrary: TVXSoundLibrary;
    procedure SetSoundLibrary(const val: TVXSoundLibrary);
    procedure SetSoundName(const val: string);
    procedure SetMute(const val: Boolean);
    procedure SetPause(const val: Boolean);
    procedure SetNbLoops(const val: Integer);
    procedure SetFrequency(const val: Integer);

  public
    
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Changes: TVXSoundSourceChanges read FChanges;

    function Sample: TVXSoundSample;

    // This Tag is reserved for sound manager use only
    property ManagerTag: PtrUInt read FTag write FTag;

    { Origin object for the sound sources. 
       Absolute object position/orientation are taken into account, the
       object's TVXBInertia is considered if any. 
       If origin is nil, the source is assumed to be static at the origin. 
        Note :  since TCollectionItem do not support the "Notification"
       scheme, it is up to the Origin object to take care of updating this
       property prior to release/destruction. }
    property Origin: TVXBaseSceneObject read FOrigin write SetOrigin;

  published
    
    property SoundLibrary: TVXSoundLibrary read GetSoundLibrary write
      SetSoundLibrary;
    property SoundName: string read FSoundName write SetSoundName;

    { Volume of the source, [0.0; 1.0] range }
    property Volume: Single read FVolume write SetVolume;
    { Nb of playing loops. }
    property NbLoops: Integer read FNbLoops write SetNbLoops default 1;

    property Mute: Boolean read FMute write SetMute default False;
    property Pause: Boolean read FPause write SetPause default False;

    { Sound source priority, the higher the better. 
       When maximum number of sound sources is reached, only the sources
       with the highest priority will continue to play, however, even
       non-playing sources should be tracked by the manager, thus allowing
       an "unlimited" amount of sources from the application point of view. }
    property Priority: Integer read FPriority write SetPriority default 0;

    { Min distance before spatial attenuation occurs. 
       1.0 by default }
    property MinDistance: Single read FMinDistance write SetMinDistance;
    { Max distance, if source is further away, it will not be heard. 
       100.0 by default }
    property MaxDistance: Single read FMaxDistance write SetMaxDistance;

    { Inside cone angle, [0°; 360°]. 
       Sound volume is maximal within this cone. 
       See DirectX SDK for details. }
    property InsideConeAngle: Single read FInsideConeAngle write
      SetInsideConeAngle;
    { Outside cone angle, [0°; 360°]. 
       Between inside and outside cone, sound volume decreases between max
       and cone outside volume. 
       See DirectX SDK for details. }
    property OutsideConeAngle: Single read FOutsideConeAngle write
      SetOutsideConeAngle;
    { Cone outside volume, [0.0; 1.0] range. 
       See DirectX SDK for details. }
    property ConeOutsideVolume: Single read FConeOutsideVolume write
      SetConeOutsideVolume;
    { Sample custom playback frequency. 
       Values null or negative are interpreted as 'default frequency'. }
    property Frequency: Integer read FFrequency write SetFrequency default -1;
  end;

  // TVXSoundSource
  //
    { Origin of sound playback. 
       Just publishes the 'Origin' property. 
       Note that the "orientation" is the the source's Direction, ie. the "Z"
       vector. }
  TVXSoundSource = class(TVXBaseSoundSource)
  public
    
    destructor Destroy; override;

  published
    
    property Origin;
  end;

  // TVXSoundSources
  //
  TVXSoundSources = class(TCollection)
  protected
    
    owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TVXSoundSource);
    function GetItems(index: Integer): TVXSoundSource;

    function Add: TVXSoundSource;
    function FindItemID(ID: Integer): TVXSoundSource;

  public
    
    constructor Create(AOwner: TComponent);

    property Items[index: Integer]: TVXSoundSource read GetItems write SetItems;
      default;
  end;

  // TVXSoundEnvironment
  //
  { EAX standard sound environments. }
  TVXSoundEnvironment = (seDefault, sePaddedCell, seRoom, seBathroom,
    seLivingRoom, seStoneroom, seAuditorium,
    seConcertHall, seCave, seArena, seHangar,
    seCarpetedHallway, seHallway, seStoneCorridor,
    seAlley, seForest, seCity, seMountains, seQuarry,
    sePlain, seParkingLot, seSewerPipe, seUnderWater,
    seDrugged, seDizzy, sePsychotic);

  // TVXSoundManager
  //
    { Base class for sound manager components. 
       The sound manager component is the interface to a low-level audio API
       (like DirectSound), there can only be one active manager at any time
       (this class takes care of this). 
       Subclass should override the DoActivate and DoDeActivate protected methods
       to "initialize/unitialize" their sound layer, actual data releases should
       occur in destructor however. }
  TVXSoundManager = class(TVXCadenceAbleComponent)
  private
    
    FActive: Boolean;
    FMute: Boolean;
    FPause: Boolean;
    FMasterVolume: Single;
    FListener: TVXBaseSceneObject;
    FLastListenerPosition: TVector;
    FSources: TVXSoundSources;
    FMaxChannels: Integer;
    FOutputFrequency: Integer;
    FUpdateFrequency: Single;
    FDistanceFactor: Single;
    FRollOffFactor: Single;
    FDopplerFactor: Single;
    FSoundEnvironment: TVXSoundEnvironment;
    FLastUpdateTime, FLastDeltaTime: Single;
      // last time UpdateSources was fired, not persistent
    FCadencer: TVXCadencer;
    procedure SetActive(const val: Boolean);
    procedure SetMute(const val: Boolean);
    procedure SetPause(const val: Boolean);
    procedure WriteDoppler(writer: TWriter);
    procedure ReadDoppler(reader: TReader);

  protected
    
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure SetSources(const val: TVXSoundSources);
    procedure SetMasterVolume(const val: Single);
    procedure SetListener(const val: TVXBaseSceneObject);
    procedure SetMaxChannels(const val: Integer);
    procedure SetOutputFrequency(const val: Integer);
    procedure SetUpdateFrequency(const val: Single);
    function StoreUpdateFrequency: Boolean;
    procedure SetCadencer(const val: TVXCadencer);
    procedure SetDistanceFactor(const val: Single);
    function StoreDistanceFactor: Boolean;
    procedure SetRollOffFactor(const val: Single);
    function StoreRollOffFactor: Boolean;
    procedure SetDopplerFactor(const val: Single);
    procedure SetSoundEnvironment(const val: TVXSoundEnvironment);

    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;

    procedure ListenerCoordinates(var position, velocity, direction, up:
      TVector);

    function DoActivate: Boolean; virtual;
    // Invoked AFTER all sources have been stopped
    procedure DoDeActivate; virtual;
    { Effect mute of all sounds. 
       Default implementation call MuteSource for all non-muted sources
       with "True" as parameter. }
    function DoMute: Boolean; virtual;
    { Effect un-mute of all sounds. 
       Default implementation call MuteSource for all non-muted sources
       with "False" as parameter. }
    procedure DoUnMute; virtual;
    { Effect pause of all sounds. 
       Default implementation call PauseSource for all non-paused sources
       with "True" as parameter. }
    function DoPause: Boolean; virtual;
    { Effect un-pause of all sounds. 
       Default implementation call PauseSource for all non-paused sources
       with "True" as parameter. }
    procedure DoUnPause; virtual;

    procedure NotifyMasterVolumeChange; virtual;
    procedure Notify3DFactorsChanged; virtual;
    procedure NotifyEnvironmentChanged; virtual;

    // Called when a source will be freed
    procedure KillSource(aSource: TVXBaseSoundSource); virtual;
    { Request to update source's data in low-level sound API. 
       Default implementation just clears the "Changes" flags. }
    procedure UpdateSource(aSource: TVXBaseSoundSource); virtual;
    procedure MuteSource(aSource: TVXBaseSoundSource; muted: Boolean); virtual;
    procedure PauseSource(aSource: TVXBaseSoundSource; paused: Boolean);
      virtual;

  public
    
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Manual request to update all sources to reflect changes. 
       Default implementation invokes UpdateSource for all known sources. }
    procedure UpdateSources; virtual;
    { Stop and free all sources. }
    procedure StopAllSources;

    { Progress notification for time synchronization. 
       This method will call UpdateSources depending on the last time
       it was performed and the value of the UpdateFrequency property. }
    procedure DoProgress(const progressTime: TProgressTimes); override;

    { Sound manager API reported CPU Usage. 
       Returns -1 when unsupported. }
    function CPUUsagePercent: Single; virtual;
    { True if EAX is supported. }
    function EAXSupported: Boolean; virtual;

  published
    
      { Activation/deactivation of the low-level sound API }
    property Active: Boolean read FActive write SetActive default False;

    { Maximum number of sound output channels. 
       While some drivers will just ignore this value, others cannot
       dynamically adjust the maximum number of channels (you need to
       de-activate and re-activate the manager for this property to be
       taken into account). }
    property MaxChannels: Integer read FMaxChannels write SetMaxChannels default
      8;
    { Sound output mixing frequency. 
       Commonly used values ar 11025, 22050 and 44100. 
       Note that most driver cannot dynamically adjust the output frequency
       (you need to de-ativate and re-activate the manager for this property
       to be taken into account). }
    property OutputFrequency: Integer read FOutputFrequency write
      SetOutputFrequency default 44100;

    { Request to mute all sounds. 
       All sound requests should be handled as if sound is unmuted though,
       however drivers should try to take a CPU advantage of mute over
       MasterVolume=0 }
    property Mute: Boolean read FMute write SetMute default False;
    { Request to pause all sound, sound output should be muted too. 
       When unpausing, all sound should resume at the point they were paused. }
    property Pause: Boolean read FPause write SetPause default False;
    { Master Volume adjustement in the [0.0; 1.0] range. 
       Driver should take care of properly clamping the master volume. }
    property MasterVolume: Single read FMasterVolume write SetMasterVolume;

    { Scene object that materializes the listener. 
       The sceneobject's AbsolutePosition and orientation are used to define
       the listener coordinates, velocity is automatically calculated
       (if you're using DoProgress or connected the manager to a cadencer). 
       If this property is nil, the listener is assumed to be static at
       the NullPoint coordinate, facing Z axis, with up being Y (ie. the
       default orientation). }
    property Listener: TVXBaseSceneObject read FListener write SetListener;
    { Currently active and playing sound sources. }
    property Sources: TVXSoundSources read FSources write SetSources;

    { Update frequency for time-based control (DoProgress). 
       Default value is 10 Hz (frequency is clamped in the 1Hz-60Hz range). }
    property UpdateFrequency: Single read FUpdateFrequency write
      SetUpdateFrequency stored StoreUpdateFrequency;
    { Cadencer for time-based control.  }
    property Cadencer: TVXCadencer read FCadencer write SetCadencer;
    { Engine relative distance factor, compared to 1.0 meters. 
       Equates to 'how many units per meter' your engine has. }
    property DistanceFactor: Single read FDistanceFactor write SetDistanceFactor
      stored StoreDistanceFactor;
    { Sets the global attenuation rolloff factor. 
       Normally volume for a sample will scale at 1 / distance.
       This gives a logarithmic attenuation of volume as the source gets
       further away (or closer). 
       Setting this value makes the sound drop off faster or slower.
       The higher the value, the faster volume will fall off. }
    property RollOffFactor: Single read FRollOffFactor write SetRollOffFactor
      stored StoreRollOffFactor;
    { Engine relative Doppler factor, compared to 1.0 meters. 
       Equates to 'how many units per meter' your engine has. }
    property DopplerFactor: Single read FDopplerFactor write SetDopplerFactor
      stored False;
    { Sound environment (requires EAX compatible soundboard). }
    property Environment: TVXSoundEnvironment read FSoundEnvironment write
      SetSoundEnvironment default seDefault;
  end;

  // TVXBSoundEmitter
  //
  { A sound emitter behaviour, plug it on any object to make it noisy. 
       This behaviour is just an interface to a TVXSoundSource, for editing
       convenience. }
  TVXBSoundEmitter = class(TVXBehaviour)
  private
    
    FPlaying: Boolean; // used at design-time ONLY
    FSource: TVXBaseSoundSource;
    FPlayingSource: TVXSoundSource;

  protected
    
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;

    procedure SetSource(const val: TVXBaseSoundSource);
    procedure SetPlaying(const val: Boolean);
    function GetPlaying: Boolean;

    procedure NotifySourceDestruction(aSource: TVXSoundSource);

  public
    
    constructor Create(aOwner: TVXXCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    class function UniqueItem: Boolean; override;

    procedure DoProgress(const progressTime: TProgressTimes); override;

    property PlayingSource: TVXSoundSource read FPlayingSource;

  published
    
    property Source: TVXBaseSoundSource read FSource write SetSource;
    property Playing: Boolean read GetPlaying write SetPlaying default False;

  end;

function ActiveSoundManager: TVXSoundManager;
function GetSoundLibraryByName(const aName: string): TVXSoundLibrary;

function GetOrCreateSoundEmitter(behaviours: TVXBehaviours): TVXBSoundEmitter;
  overload;
function GetOrCreateSoundEmitter(obj: TVXBaseSceneObject): TVXBSoundEmitter;
  overload;

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
  vActiveSoundManager: TVXSoundManager;
  vSoundLibraries: TList;

  // ActiveSoundManager
  //

function ActiveSoundManager: TVXSoundManager;
begin
  Result := vActiveSoundManager;
end;

// GetSoundLibraryByName
//

function GetSoundLibraryByName(const aName: string): TVXSoundLibrary;
var
  i: Integer;
begin
  Result := nil;
  if Assigned(vSoundLibraries) then
    for i := 0 to vSoundLibraries.Count - 1 do
      if TVXSoundLibrary(vSoundLibraries[i]).Name = aName then
      begin
        Result := TVXSoundLibrary(vSoundLibraries[i]);
        Break;
      end;
end;

// GetOrCreateSoundEmitter (TVXBehaviours)
//

function GetOrCreateSoundEmitter(behaviours: TVXBehaviours): TVXBSoundEmitter;
var
  i: Integer;
begin
  i := behaviours.IndexOfClass(TVXBSoundEmitter);
  if i >= 0 then
    Result := TVXBSoundEmitter(behaviours[i])
  else
    Result := TVXBSoundEmitter.Create(behaviours);
end;

// GetOrCreateSoundEmitter (TVXBaseSceneObject)
//

function GetOrCreateSoundEmitter(obj: TVXBaseSceneObject): TVXBSoundEmitter;
begin
  Result := GetOrCreateSoundEmitter(obj.Behaviours);
end;

// ------------------
// ------------------ TVXSoundSample ------------------
// ------------------

// Create
//

constructor TVXSoundSample.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

// Destroy
//

destructor TVXSoundSample.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

// Assign
//

procedure TVXSoundSample.Assign(Source: TPersistent);
begin
  if Source is TVXSoundSample then
  begin
    FName := TVXSoundSample(Source).Name;
    FData.Free;
    FData := TVXSoundFile(TVXSoundSample(Source).Data.CreateCopy(Self));
  end
  else
    inherited Assign(Source); // Assign error
end;

// DefineProperties
//

procedure TVXSoundSample.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('BinData', ReadData, WriteData, Assigned(FData));
end;

// ReadData
//

procedure TVXSoundSample.ReadData(Stream: TStream);
var
  n: Integer;
  clName: AnsiString;
begin
  with Stream do
  begin
    Read(n, SizeOf(Integer));
    SetLength(clName, n);
    if n > 0 then
      Read(clName[1], n);
    FData := TVXSoundFileClass(FindClass(string(clName))).Create(Self);
    FData.LoadFromStream(Stream);
  end;
end;

// WriteData
//

procedure TVXSoundSample.WriteData(Stream: TStream);
var
  n: Integer;
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

// GetDisplayName
//

function TVXSoundSample.GetDisplayName: string;
var
  s: string;
begin
  if Assigned(FData) then
  begin
    if Data.Sampling.NbChannels > 1 then
      s := 's'
    else
      s := '';
    Result := Format('%s (%d Hz, %d bits, %d channel%s, %.2f sec)',
      [Name, Data.Sampling.Frequency,
      Data.Sampling.BitsPerSample,
        Data.Sampling.NbChannels, s, LengthInSec])
  end
  else
    Result := Format('%s (empty)', [Name]);
end;

// LoadFromFile
//

procedure TVXSoundSample.LoadFromFile(const fileName: string);
var
  sfc: TVXSoundFileClass;
begin
  FData.Free;
  sfc := GetGLSoundFileFormats.FindExt(ExtractFileExt(fileName));
  if Assigned(sfc) then
  begin
    FData := sfc.Create(Self);
    FData.LoadFromFile(fileName);
  end
  else
    FData := nil;
  Assert(Data <> nil, 'Could not load ' + fileName +
    ', make sure you include the unit required to load this format in your uses clause.');
  Name := ExtractFileName(fileName);
end;

// PlayOnWaveOut
//

procedure TVXSoundSample.PlayOnWaveOut;
begin
  if Assigned(FData) then
    FData.PlayOnWaveOut;
end;

// TVXSoundSample
//

function TVXSoundSample.Sampling: TVXSoundSampling;
begin
  if Assigned(FData) then
    Result := FData.Sampling
  else
    Result := nil;
end;

// LengthInBytes
//

function TVXSoundSample.LengthInBytes: Integer;
begin
  if Assigned(FData) then
    Result := FData.LengthInBytes
  else
    Result := 0;
end;

// LengthInSamples
//

function TVXSoundSample.LengthInSamples: Integer;
begin
  if Assigned(FData) then
    Result := FData.LengthInSamples
  else
    Result := 0;
end;

// LengthInSec
//

function TVXSoundSample.LengthInSec: Single;
begin
  if Assigned(FData) then
    Result := FData.LengthInSec
  else
    Result := 0;
end;

// SetData
//

procedure TVXSoundSample.SetData(const val: TVXSoundFile);
begin
  FData.Free;
  if Assigned(val) then
    FData := TVXSoundFile(val.CreateCopy(Self))
  else
    FData := nil;
end;

// ------------------
// ------------------ TVXSoundSamples ------------------
// ------------------

constructor TVXSoundSamples.Create(AOwner: TComponent);
begin
  Owner := AOwner;
  inherited Create(TVXSoundSample);
end;

function TVXSoundSamples.GetOwner: TPersistent;
begin
  Result := Owner;
end;

procedure TVXSoundSamples.SetItems(index: Integer; const val: TVXSoundSample);
begin
  inherited Items[index] := val;
end;

function TVXSoundSamples.GetItems(index: Integer): TVXSoundSample;
begin
  Result := TVXSoundSample(inherited Items[index]);
end;

function TVXSoundSamples.Add: TVXSoundSample;
begin
  Result := (inherited Add) as TVXSoundSample;
end;

function TVXSoundSamples.FindItemID(ID: Integer): TVXSoundSample;
begin
  Result := (inherited FindItemID(ID)) as TVXSoundSample;
end;

// GetByName
//

function TVXSoundSamples.GetByName(const aName: string): TVXSoundSample;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if CompareText(Items[i].Name, aName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

// AddFile
//

function TVXSoundSamples.AddFile(const fileName: string; const sampleName: string
  = ''): TVXSoundSample;
begin
  Result := Add;
  Result.LoadFromFile(fileName);
  if sampleName <> '' then
    Result.Name := sampleName;
end;

// ------------------
// ------------------ TVXSoundLibrary ------------------
// ------------------

constructor TVXSoundLibrary.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSamples := TVXSoundSamples.Create(Self);
  vSoundLibraries.Add(Self);
end;

destructor TVXSoundLibrary.Destroy;
begin
  vSoundLibraries.Remove(Self);
  FSamples.Free;
  inherited Destroy;
end;

// Notification
//

procedure TVXSoundLibrary.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited;
end;

// SetSamples
//

procedure TVXSoundLibrary.SetSamples(const val: TVXSoundSamples);
begin
  FSamples.Assign(val);
end;

// ------------------
// ------------------ TVXBaseSoundSource ------------------
// ------------------

// Create
//

constructor TVXBaseSoundSource.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FChanges := [sscTransformation, sscSample, sscStatus];
  FVolume := 1.0;
  FMinDistance := 1.0;
  FMaxDistance := 100.0;
  FInsideConeAngle := 360;
  FOutsideConeAngle := 360;
  FConeOutsideVolume := 0.0;
  FNbLoops := 1;
  FFrequency := -1;
end;

// Destroy
//

destructor TVXBaseSoundSource.Destroy;
begin
  inherited Destroy;
end;

// GetDisplayName
//

function TVXBaseSoundSource.GetDisplayName: string;
begin
  Result := Format('%s', [FSoundName]);
end;

// Assign
//

procedure TVXBaseSoundSource.Assign(Source: TPersistent);
begin
  if Source is TVXBaseSoundSource then
  begin
    FPriority := TVXBaseSoundSource(Source).FPriority;
    FOrigin := TVXBaseSoundSource(Source).FOrigin;
    FVolume := TVXBaseSoundSource(Source).FVolume;
    FMinDistance := TVXBaseSoundSource(Source).FMinDistance;
    FMaxDistance := TVXBaseSoundSource(Source).FMaxDistance;
    FInsideConeAngle := TVXBaseSoundSource(Source).FInsideConeAngle;
    FOutsideConeAngle := TVXBaseSoundSource(Source).FOutsideConeAngle;
    FConeOutsideVolume := TVXBaseSoundSource(Source).FConeOutsideVolume;
    FSoundLibraryName := TVXBaseSoundSource(Source).FSoundLibraryName;
    FSoundLibrary := TVXBaseSoundSource(Source).FSoundLibrary;
    FSoundName := TVXBaseSoundSource(Source).FSoundName;
    FMute := TVXBaseSoundSource(Source).FMute;
    FPause := TVXBaseSoundSource(Source).FPause;
    FChanges := [sscTransformation, sscSample, sscStatus];
    FNbLoops := TVXBaseSoundSource(Source).FNbLoops;
    FFrequency := TVXBaseSoundSource(Source).FFrequency;
  end
  else
    inherited Assign(Source);
end;

// WriteToFiler
//

procedure TVXBaseSoundSource.WriteToFiler(writer: TWriter);
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
    //      WriteInteger(FFrequency);
  end;
end;

// ReadFromFiler
//

procedure TVXBaseSoundSource.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    ReadInteger; // ignore archiveVersion
    FPriority := ReadInteger;
    FVolume := ReadFloat;
    FMinDistance := ReadFloat;
    FMaxDistance := ReadFloat;
    FInsideConeAngle := ReadFloat;
    FOutsideConeAngle := ReadFloat;
    FConeOutsideVolume := ReadFloat;
    FSoundLibraryName := ReadString;
    FSoundLibrary := nil;
    FSoundName := ReadString;
    FMute := ReadBoolean;
    FPause := ReadBoolean;
    FChanges := [sscTransformation, sscSample, sscStatus];
    FNbLoops := ReadInteger;
    //      FFrequency:=ReadInteger;
  end;
end;

// Sample
//

function TVXBaseSoundSource.Sample: TVXSoundSample;
begin
  if SoundLibrary <> nil then
    Result := FSoundLibrary.Samples.GetByName(FSoundName)
  else
    Result := nil;
end;

// SetPriority
//

procedure TVXBaseSoundSource.SetPriority(const val: Integer);
begin
  if val <> FPriority then
  begin
    FPriority := val;
    Include(FChanges, sscStatus);
  end;
end;

// SetOrigin
//

procedure TVXBaseSoundSource.SetOrigin(const val: TVXBaseSceneObject);
begin
  if val <> FOrigin then
  begin
    FOrigin := val;
    Include(FChanges, sscTransformation);
  end;
end;

// SetVolume
//

procedure TVXBaseSoundSource.SetVolume(const val: Single);
begin
  if val <> FVolume then
  begin
    FVolume := ClampValue(val, 0, 1);
    Include(FChanges, sscStatus);
  end;
end;

// SetMinDistance
//

procedure TVXBaseSoundSource.SetMinDistance(const val: Single);
begin
  if val <> FMinDistance then
  begin
    FMinDistance := ClampValue(val, 0);
    Include(FChanges, sscStatus);
  end;
end;

// SetMaxDistance
//

procedure TVXBaseSoundSource.SetMaxDistance(const val: Single);
begin
  if val <> FMaxDistance then
  begin
    FMaxDistance := ClampValue(val, 0);
    Include(FChanges, sscStatus);
  end;
end;

// SetInsideConeAngle
//

procedure TVXBaseSoundSource.SetInsideConeAngle(const val: Single);
begin
  if val <> FInsideConeAngle then
  begin
    FInsideConeAngle := ClampValue(val, 0, 360);
    Include(FChanges, sscStatus);
  end;
end;

// SetOutsideConeAngle
//

procedure TVXBaseSoundSource.SetOutsideConeAngle(const val: Single);
begin
  if val <> FOutsideConeAngle then
  begin
    FOutsideConeAngle := ClampValue(val, 0, 360);
    Include(FChanges, sscStatus);
  end;
end;

// SetConeOutsideVolume
//

procedure TVXBaseSoundSource.SetConeOutsideVolume(const val: Single);
begin
  if val <> FConeOutsideVolume then
  begin
    FConeOutsideVolume := ClampValue(val, 0, 1);
    Include(FChanges, sscStatus);
  end;
end;

// GetSoundLibrary
//

function TVXBaseSoundSource.GetSoundLibrary: TVXSoundLibrary;
begin
  if (FSoundLibrary = nil) and (FSoundLibraryName <> '') then
    FSoundLibrary := GetSoundLibraryByName(FSoundLibraryName);
  Result := FSoundLibrary;
end;

// SetSoundLibrary
//

procedure TVXBaseSoundSource.SetSoundLibrary(const val: TVXSoundLibrary);
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

// SetSoundName
//

procedure TVXBaseSoundSource.SetSoundName(const val: string);
begin
  if val <> FSoundName then
  begin
    FSoundName := val;
    Include(FChanges, sscSample);
  end;
end;

// SetPause
//

procedure TVXBaseSoundSource.SetPause(const val: Boolean);
begin
  if val <> FPause then
  begin
    FPause := val;
    if Collection <> nil then
      TVXSoundManager(TVXSoundSources(Collection).owner).PauseSource(Self,
        FPause);
  end;
end;

// SetNbLoops
//

procedure TVXBaseSoundSource.SetNbLoops(const val: Integer);
begin
  if val <> FNbLoops then
  begin
    FNbLoops := val;
    Include(FChanges, sscSample);
  end;
end;

// SetFrequency
//

procedure TVXBaseSoundSource.SetFrequency(const val: integer);
begin
  if val <> FFrequency then
  begin
    FFrequency := val;
    Include(FChanges, sscStatus);
  end;
end;

// SetMute
//

procedure TVXBaseSoundSource.SetMute(const val: Boolean);
begin
  if val <> FMute then
  begin
    FMute := val;
    if Collection <> nil then
      TVXSoundManager(TVXSoundSources(Collection).owner).MuteSource(Self,
        FMute);
  end;
end;

// ------------------
// ------------------ TVXSoundSource ------------------
// ------------------

// Destroy
//

destructor TVXSoundSource.Destroy;
begin
  if Assigned(FBehaviourToNotify) then
    FBehaviourToNotify.NotifySourceDestruction(Self);
  if Collection <> nil then
    ((Collection as TVXSoundSources).Owner as TVXSoundManager).KillSource(Self);
  inherited;
end;

// ------------------
// ------------------ TVXSoundSources ------------------
// ------------------

constructor TVXSoundSources.Create(AOwner: TComponent);
begin
  Owner := AOwner;
  inherited Create(TVXSoundSource);
end;

function TVXSoundSources.GetOwner: TPersistent;
begin
  Result := Owner;
end;

procedure TVXSoundSources.SetItems(index: Integer; const val: TVXSoundSource);
begin
  inherited Items[index] := val;
end;

function TVXSoundSources.GetItems(index: Integer): TVXSoundSource;
begin
  Result := TVXSoundSource(inherited Items[index]);
end;

function TVXSoundSources.Add: TVXSoundSource;
begin
  Result := (inherited Add) as TVXSoundSource;
end;

function TVXSoundSources.FindItemID(ID: Integer): TVXSoundSource;
begin
  Result := (inherited FindItemID(ID)) as TVXSoundSource;
end;

// ------------------
// ------------------ TVXSoundManager ------------------
// ------------------

// Create
//

constructor TVXSoundManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSources := TVXSoundSources.Create(Self);
  FMasterVolume := 1.0;
  FOutputFrequency := 44100;
  FMaxChannels := 8;
  FUpdateFrequency := 10;
  FLastUpdateTime := -1e30;
  FDistanceFactor := 1.0;
  FRollOffFactor := 1.0;
  FDopplerFactor := 1.0;
end;

// Destroy
//

destructor TVXSoundManager.Destroy;
begin
  Active := False;
  Listener := nil;
  FSources.Free;
  inherited Destroy;
end;

// Notification
//

procedure TVXSoundManager.Notification(AComponent: TComponent; Operation:
  TOperation);
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

// SetActive
//

procedure TVXSoundManager.SetActive(const val: Boolean);
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
        FActive := True;
        vActiveSoundManager := Self;
      end;
    end
    else
    begin
      try
        StopAllSources;
        DoDeActivate;
      finally
        FActive := val;
        vActiveSoundManager := nil;
      end;
    end;
  end;
end;

// Activate
//

function TVXSoundManager.DoActivate: Boolean;
begin
  Result := True;
end;

// DeActivate
//

procedure TVXSoundManager.DoDeActivate;
begin
  StopAllSources;
end;

// SetMute
//

procedure TVXSoundManager.SetMute(const val: Boolean);
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

// DoMute
//

function TVXSoundManager.DoMute: Boolean;
var
  i: Integer;
begin
  for i := 0 to Sources.Count - 1 do
    if not Sources[i].Mute then
      MuteSource(Sources[i], True);
  Result := True;
end;

// DoUnMute
//

procedure TVXSoundManager.DoUnMute;
var
  i: Integer;
begin
  for i := 0 to Sources.Count - 1 do
    if not Sources[i].Mute then
      MuteSource(Sources[i], False);
end;

// SetPause
//

procedure TVXSoundManager.SetPause(const val: Boolean);
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

// Loaded
//

procedure TVXSoundManager.Loaded;
begin
  inherited;
  if Active and (not (csDesigning in ComponentState)) then
  begin
    FActive := False;
    Active := True;
  end;
end;

// DefineProperties
//

procedure TVXSoundManager.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Doppler', ReadDoppler, WriteDoppler, (DopplerFactor <>
    1));
end;

// WriteDoppler
//

procedure TVXSoundManager.WriteDoppler(writer: TWriter);
begin
  writer.WriteFloat(DopplerFactor);
end;

// ReadDoppler
//

procedure TVXSoundManager.ReadDoppler(reader: TReader);
begin
  FDopplerFactor := reader.ReadFloat;
end;

// DoPause
//

function TVXSoundManager.DoPause: Boolean;
var
  i: Integer;
begin
  for i := 0 to Sources.Count - 1 do
    if not Sources[i].Pause then
      PauseSource(Sources[i], True);
  Result := True;
end;

// DoUnPause
//

procedure TVXSoundManager.DoUnPause;
var
  i: Integer;
begin
  for i := 0 to Sources.Count - 1 do
    if not Sources[i].Pause then
      PauseSource(Sources[i], False);
end;

// SetMasterVolume
//

procedure TVXSoundManager.SetMasterVolume(const val: Single);
begin
  if val < 0 then
    FMasterVolume := 0
  else if val > 1 then
    FMasterVolume := 1
  else
    FMasterVolume := val;
  NotifyMasterVolumeChange;
end;

// SetMaxChannels
//

procedure TVXSoundManager.SetMaxChannels(const val: Integer);
begin
  if val <> FMaxChannels then
  begin
    if val < 1 then
      FMaxChannels := 1
    else
      FMaxChannels := val;
  end;
end;

// SetOutputFrequency
//

procedure TVXSoundManager.SetOutputFrequency(const val: Integer);
begin
  if val <> FOutputFrequency then
  begin
    if val < 11025 then
      FOutputFrequency := 11025
    else
      FOutputFrequency := val;
  end;
end;

// SetUpdateFrequency
//

procedure TVXSoundManager.SetUpdateFrequency(const val: Single);
begin
  FUpdateFrequency := ClampValue(val, 1, 60);
end;

// StoreUpdateFrequency
//

function TVXSoundManager.StoreUpdateFrequency: Boolean;
begin
  Result := (FUpdateFrequency <> 10);
end;

// SetCadencer
//

procedure TVXSoundManager.SetCadencer(const val: TVXCadencer);
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

// SetDistanceFactor
//

procedure TVXSoundManager.SetDistanceFactor(const val: Single);
begin
  if val <= 0 then
    FDistanceFactor := 1
  else
    FDistanceFactor := val;
  Notify3DFactorsChanged;
end;

// StoreDistanceFactor
//

function TVXSoundManager.StoreDistanceFactor: Boolean;
begin
  Result := (FDistanceFactor <> 1);
end;

// SetRollOffFactor
//

procedure TVXSoundManager.SetRollOffFactor(const val: Single);
begin
  if val <= 0 then
    FRollOffFactor := 1
  else
    FRollOffFactor := val;
  Notify3DFactorsChanged;
end;

// StoreRollOffFactor
//

function TVXSoundManager.StoreRollOffFactor: Boolean;
begin
  Result := (FRollOffFactor <> 1);
end;

// SetDopplerFactor
//

procedure TVXSoundManager.SetDopplerFactor(const val: Single);
begin
  if val < 0 then
    FDopplerFactor := 0
  else if val > 10 then
    FDopplerFactor := 10
  else
    FDopplerFactor := val;
  Notify3DFactorsChanged;
end;

// SetSoundEnvironment
//

procedure TVXSoundManager.SetSoundEnvironment(const val: TVXSoundEnvironment);
begin
  if val <> FSoundEnvironment then
  begin
    FSoundEnvironment := val;
    NotifyEnvironmentChanged;
  end;
end;

// ListenerCoordinates
//

procedure TVXSoundManager.ListenerCoordinates(var position, velocity, direction,
  up: TVector);
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
    if (Listener is TVXCamera) and (TVXCamera(Listener).TargetObject <> nil)
      then
    begin
      // special case of the camera targeting something
      direction := TVXCamera(Listener).AbsoluteVectorToTarget;
      NormalizeVector(direction);
      up := Listener.AbsoluteYVector;
      right := VectorCrossProduct(direction, up);
      up := VectorCrossProduct(right, direction);
    end
    else
    begin
      direction := Listener.AbsoluteZVector;
      up := Listener.AbsoluteYVector;
    end;
  end
  else
  begin
    position := NullHmgPoint;
    velocity := NullHmgVector;
    direction := ZHmgVector;
    up := YHmgVector;
  end;
end;

// NotifyMasterVolumeChange
//

procedure TVXSoundManager.NotifyMasterVolumeChange;
begin
  // nothing
end;

// Notify3DFactorsChanged
//

procedure TVXSoundManager.Notify3DFactorsChanged;
begin
  // nothing
end;

// NotifyEnvironmentChanged
//

procedure TVXSoundManager.NotifyEnvironmentChanged;
begin
  // nothing
end;

// SetListener
//

procedure TVXSoundManager.SetListener(const val: TVXBaseSceneObject);
begin
  if Assigned(FListener) then
    FListener.RemoveFreeNotification(Self);
  FListener := val;
  if Assigned(FListener) then
    FListener.FreeNotification(Self);
end;

// SetSources
//

procedure TVXSoundManager.SetSources(const val: TVXSoundSources);
begin
  FSources.Assign(val);
end;

// KillSource
//

procedure TVXSoundManager.KillSource(aSource: TVXBaseSoundSource);
begin
  // nothing
end;

// UpdateSource
//

procedure TVXSoundManager.UpdateSource(aSource: TVXBaseSoundSource);
begin
  aSource.FChanges := [];
end;

// MuteSource
//

procedure TVXSoundManager.MuteSource(aSource: TVXBaseSoundSource; muted:
  Boolean);
begin
  // nothing
end;

// PauseSource
//

procedure TVXSoundManager.PauseSource(aSource: TVXBaseSoundSource; paused:
  Boolean);
begin
  // nothing
end;

// UpdateSources
//

procedure TVXSoundManager.UpdateSources;
var
  i: Integer;
begin
  for i := Sources.Count - 1 downto 0 do
    UpdateSource(Sources[i]);
end;

// StopAllSources
//

procedure TVXSoundManager.StopAllSources;
var
  i: Integer;
begin
  for i := Sources.Count - 1 downto 0 do
    Sources.Delete(i);
end;

// DoProgress
//

procedure TVXSoundManager.DoProgress(const progressTime: TProgressTimes);
begin
  if not Active then
    Exit;
  with progressTime do
    if newTime - FLastUpdateTime > 1 / FUpdateFrequency then
    begin
      FLastDeltaTime := newTime - FLastUpdateTime;
      FLastUpdateTime := newTime;
      UpdateSources;
    end;
end;

// CPUUsagePercent
//

function TVXSoundManager.CPUUsagePercent: Single;
begin
  Result := -1;
end;

// EAXSupported
//

function TVXSoundManager.EAXSupported: Boolean;
begin
  Result := False;
end;

// ------------------
// ------------------ TVXBSoundEmitter ------------------
// ------------------

// Create
//

constructor TVXBSoundEmitter.Create(aOwner: TVXXCollection);
begin
  inherited Create(aOwner);
  FSource := TVXSoundSource.Create(nil);
end;

// Destroy
//

destructor TVXBSoundEmitter.Destroy;
begin
  if Assigned(FPlayingSource) then
    FPlayingSource.Free;
  FSource.Free;
  inherited Destroy;
end;

// Assign
//

procedure TVXBSoundEmitter.Assign(Source: TPersistent);
begin
  if Source is TVXBSoundEmitter then
  begin
    FSource.Assign(TVXBSoundEmitter(Source).FSource);
  end;
  inherited Assign(Source);
end;

// WriteToFiler
//

procedure TVXBSoundEmitter.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FSource.WriteToFiler(writer);
    WriteBoolean(FPlaying);
  end;
end;

// ReadFromFiler
//

procedure TVXBSoundEmitter.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    ReadInteger; // ignore archiveVersion
    FSource.ReadFromFiler(reader);
    FPlaying := ReadBoolean;
  end;
end;

// Loaded
//

procedure TVXBSoundEmitter.Loaded;
begin
  inherited;
  if not (csDesigning in OwnerBaseSceneObject.ComponentState) then
    SetPlaying(FPlaying);
end;

// FriendlyName
//

class function TVXBSoundEmitter.FriendlyName: string;
begin
  Result := 'Sound Emitter';
end;

// FriendlyDescription
//

class function TVXBSoundEmitter.FriendlyDescription: string;
begin
  Result := 'A simple sound emitter behaviour';
end;

// UniqueBehaviour
//

class function TVXBSoundEmitter.UniqueItem: Boolean;
begin
  Result := False;
end;

// DoProgress
//

procedure TVXBSoundEmitter.DoProgress(const progressTime: TProgressTimes);
begin
  // nothing, yet
end;

// SetSource
//

procedure TVXBSoundEmitter.SetSource(const val: TVXBaseSoundSource);
begin
  FSource.Assign(val);
end;

// SetPlaying
//

procedure TVXBSoundEmitter.SetPlaying(const val: Boolean);
begin
  if csDesigning in OwnerBaseSceneObject.ComponentState then
    FPlaying := val
  else if ActiveSoundManager <> nil then
  begin
    if val <> Playing then
    begin
      if val then
      begin
        FPlayingSource := ActiveSoundManager.Sources.Add;
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

// GetPlaying
//

function TVXBSoundEmitter.GetPlaying: Boolean;
begin
  if csDesigning in OwnerBaseSceneObject.ComponentState then
    Result := FPlaying
  else
    Result := Assigned(FPlayingSource);
end;

// NotifySourceDestruction
//

procedure TVXBSoundEmitter.NotifySourceDestruction(aSource: TVXSoundSource);
begin
  Assert(FPlayingSource = aSource);
  FPlayingSource := nil;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

   // class registrations
  RegisterClasses([TVXSoundLibrary]);
  RegisterXCollectionItemClass(TVXBSoundEmitter);
  vSoundLibraries := TList.Create;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

finalization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  if Assigned(vActiveSoundManager) then
    vActiveSoundManager.Active := False;

  vSoundLibraries.Free;
  vSoundLibraries := nil;

  UnregisterXCollectionItemClass(TVXBSoundEmitter);

end.

