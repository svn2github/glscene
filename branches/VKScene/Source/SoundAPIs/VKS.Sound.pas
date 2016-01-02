//
// This unit is part of the GLScene Project   
//
{: VKS.Sound<p>

 Base classes and interface for GLScene Sound System<p>

 <b>History : </b><font size=-1><ul>
      <li>24/04/11 - Yar - Bugfixed TVKSoundSample.Assign (thanks to Anonymous)
      <li>06/06/10 - Yar - Fixed warnings
      <li>06/05/09 - DanB - Split TVKSMWaveOut to GLSMWaveOut.pas, to remove windows dependancy
      <li>16/10/08 - UweR - Compatibility fix for Delphi 2009
      <li>22/07/02 - EG - SetMute/SetPause fix (Sternas Stefanos)
      <li>02/07/02 - EG - Persistence fix (MP3 / Sternas Stefanos)
      <li>05/03/02 - EG - TVKBSoundEmitter.Loaded
      <li>27/02/02 - EG - Added 3D Factors, special listener-is-camera support
      <li>13/01/01 - EG - Added CPUUsagePercent
      <li>09/06/00 - EG - Various enhancements
    <li>04/06/00 - EG - Creation
 </ul></font>
}
unit VKS.Sound;

interface

uses
  System.Classes, System.SysUtils, System.Types,

  VKS.SoundFileObjects, VKS.Scene, VKS.XCollection, VKS.VectorGeometry,
  VKS.Cadencer, VKS.BaseClasses, VKS.CrossPlatform, VKS.Utils;

{$I VKScene.inc}

type

  // TVKSoundSample
  //
    {: Stores a single PCM coded sound sample. }
  TVKSoundSample = class(TCollectionItem)
  private
    { Private Declarations }
    FName: string;
    FData: TVKSoundFile;
    FTag: Integer;

  protected
    { Protected Declarations }
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream); virtual;
    procedure WriteData(Stream: TStream); virtual;
    function GetDisplayName: string; override;
    procedure SetData(const val: TVKSoundFile);

  public
    { Public Declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure LoadFromFile(const fileName: string);

    procedure PlayOnWaveOut;

    function Sampling: TVKSoundSampling;
    function LengthInBytes: Integer;
    function LengthInSamples: Integer;
    function LengthInSec: Single;

    //: This Tag is reserved for sound manager use only
    property ManagerTag: Integer read FTag write FTag;

  published
    { Published Declarations }
    property Name: string read FName write FName;
    property Data: TVKSoundFile read FData write SetData stored False;
  end;

  // TVKSoundSamples
  //
  TVKSoundSamples = class(TCollection)
  protected
    { Protected Declarations }
    owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TVKSoundSample);
    function GetItems(index: Integer): TVKSoundSample;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);
    function Add: TVKSoundSample;
    function FindItemID(ID: Integer): TVKSoundSample;
    property Items[index: Integer]: TVKSoundSample read GetItems write SetItems;
      default;
    function GetByName(const aName: string): TVKSoundSample;

    function AddFile(const fileName: string; const sampleName: string = ''):
      TVKSoundSample;
  end;

  // TVKSoundLibrary
  //
  TVKSoundLibrary = class(TComponent)
  private
    { Private Declarations }
    FSamples: TVKSoundSamples;

  protected
    { Protected Declarations }
    procedure SetSamples(const val: TVKSoundSamples);

    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Published Declarations }
    property Samples: TVKSoundSamples read FSamples write SetSamples;
  end;

  // TVKSoundSource
  //
  TVKSoundSourceChange = (sscTransformation, sscSample, sscStatus);
  TVKSoundSourceChanges = set of TVKSoundSourceChange;

  TVKBSoundEmitter = class;

  // TVKBaseSoundSource
  //
    {: Base class for origin of sound playback. }
  TVKBaseSoundSource = class(TCollectionItem)
  private
    { Private Declarations }
    FBehaviourToNotify: TVKBSoundEmitter;
      // private only, NOT persistent, not assigned
    FPriority: Integer;
    FOrigin: TVKBaseSceneObject; // NOT persistent
    FVolume: Single;
    FMinDistance, FMaxDistance: Single;
    FInsideConeAngle, FOutsideConeAngle: Single;
    FConeOutsideVolume: Single;
    FSoundLibraryName: string; // used for persistence
    FSoundLibrary: TVKSoundLibrary; // persistence via name
    FSoundName: string;
    FMute: Boolean;
    FPause: Boolean;
    FChanges: TVKSoundSourceChanges; // NOT persistent, not assigned
    FNbLoops: Integer;
    FTag: PtrUInt; // NOT persistent, not assigned
    FFrequency: Integer;

  protected
    { Protected Declarations }
    procedure WriteToFiler(writer: TWriter);
    procedure ReadFromFiler(reader: TReader);

    function GetDisplayName: string; override;
    procedure SetPriority(const val: Integer);
    procedure SetOrigin(const val: TVKBaseSceneObject);
    procedure SetVolume(const val: Single);
    procedure SetMinDistance(const val: Single);
    procedure SetMaxDistance(const val: Single);
    procedure SetInsideConeAngle(const val: Single);
    procedure SetOutsideConeAngle(const val: Single);
    procedure SetConeOutsideVolume(const val: Single);
    function GetSoundLibrary: TVKSoundLibrary;
    procedure SetSoundLibrary(const val: TVKSoundLibrary);
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

    property Changes: TVKSoundSourceChanges read FChanges;

    function Sample: TVKSoundSample;

    //: This Tag is reserved for sound manager use only
    property ManagerTag: PtrUInt read FTag write FTag;

    {: Origin object for the sound sources.<p>
       Absolute object position/orientation are taken into account, the
       object's TVKBInertia is considered if any.<p>
       If origin is nil, the source is assumed to be static at the origin.<p>
       <b>Note :</b> since TCollectionItem do not support the "Notification"
       scheme, it is up to the Origin object to take care of updating this
       property prior to release/destruction. }
    property Origin: TVKBaseSceneObject read FOrigin write SetOrigin;

  published
    { Published Declarations }
    property SoundLibrary: TVKSoundLibrary read GetSoundLibrary write
      SetSoundLibrary;
    property SoundName: string read FSoundName write SetSoundName;

    {: Volume of the source, [0.0; 1.0] range }
    property Volume: Single read FVolume write SetVolume;
    {: Nb of playing loops. }
    property NbLoops: Integer read FNbLoops write SetNbLoops default 1;

    property Mute: Boolean read FMute write SetMute default False;
    property Pause: Boolean read FPause write SetPause default False;

    {: Sound source priority, the higher the better.<p>
       When maximum number of sound sources is reached, only the sources
       with the highest priority will continue to play, however, even
       non-playing sources should be tracked by the manager, thus allowing
       an "unlimited" amount of sources from the application point of view. }
    property Priority: Integer read FPriority write SetPriority default 0;

    {: Min distance before spatial attenuation occurs.<p>
       1.0 by default }
    property MinDistance: Single read FMinDistance write SetMinDistance;
    {: Max distance, if source is further away, it will not be heard.<p>
       100.0 by default }
    property MaxDistance: Single read FMaxDistance write SetMaxDistance;

    {: Inside cone angle, [0°; 360°].<p>
       Sound volume is maximal within this cone.<p>
       See DirectX SDK for details. }
    property InsideConeAngle: Single read FInsideConeAngle write
      SetInsideConeAngle;
    {: Outside cone angle, [0°; 360°].<p>
       Between inside and outside cone, sound volume decreases between max
       and cone outside volume.<p>
       See DirectX SDK for details. }
    property OutsideConeAngle: Single read FOutsideConeAngle write
      SetOutsideConeAngle;
    {: Cone outside volume, [0.0; 1.0] range.<p>
       See DirectX SDK for details. }
    property ConeOutsideVolume: Single read FConeOutsideVolume write
      SetConeOutsideVolume;
    {: Sample custom playback frequency.<p>
       Values null or negative are interpreted as 'default frequency'. }
    property Frequency: Integer read FFrequency write SetFrequency default -1;
  end;

  // TVKSoundSource
  //
    {: Origin of sound playback.<p>
       Just publishes the 'Origin' property.<p>
       Note that the "orientation" is the the source's Direction, ie. the "Z"
       vector. }
  TVKSoundSource = class(TVKBaseSoundSource)
  public
    { Public Declarations }
    destructor Destroy; override;

  published
    { Published Declarations }
    property Origin;
  end;

  // TVKSoundSources
  //
  TVKSoundSources = class(TCollection)
  protected
    { Protected Declarations }
    owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TVKSoundSource);
    function GetItems(index: Integer): TVKSoundSource;

    function Add: TVKSoundSource;
    function FindItemID(ID: Integer): TVKSoundSource;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);

    property Items[index: Integer]: TVKSoundSource read GetItems write SetItems;
      default;
  end;

  // TVKSoundEnvironment
  //
  {: EAX standard sound environments. }
  TVKSoundEnvironment = (seDefault, sePaddedCell, seRoom, seBathroom,
    seLivingRoom, seStoneroom, seAuditorium,
    seConcertHall, seCave, seArena, seHangar,
    seCarpetedHallway, seHallway, seStoneCorridor,
    seAlley, seForest, seCity, seMountains, seQuarry,
    sePlain, seParkingLot, seSewerPipe, seUnderWater,
    seDrugged, seDizzy, sePsychotic);

  // TVKSoundManager
  //
    {: Base class for sound manager components.<p>
       The sound manager component is the interface to a low-level audio API
       (like DirectSound), there can only be one active manager at any time
       (this class takes care of this).<p>
       Subclass should override the DoActivate and DoDeActivate protected methods
       to "initialize/unitialize" their sound layer, actual data releases should
       occur in destructor however. }
  TVKSoundManager = class(TVKCadenceAbleComponent)
  private
    { Private Declarations }
    FActive: Boolean;
    FMute: Boolean;
    FPause: Boolean;
    FMasterVolume: Single;
    FListener: TVKBaseSceneObject;
    FLastListenerPosition: TVector;
    FSources: TVKSoundSources;
    FMaxChannels: Integer;
    FOutputFrequency: Integer;
    FUpdateFrequency: Single;
    FDistanceFactor: Single;
    FRollOffFactor: Single;
    FDopplerFactor: Single;
    FSoundEnvironment: TVKSoundEnvironment;
    FLastUpdateTime, FLastDeltaTime: Single;
      // last time UpdateSources was fired, not persistent
    FCadencer: TVKCadencer;
    procedure SetActive(const val: Boolean);
    procedure SetMute(const val: Boolean);
    procedure SetPause(const val: Boolean);
    procedure WriteDoppler(writer: TWriter);
    procedure ReadDoppler(reader: TReader);

  protected
    { Protected Declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure SetSources(const val: TVKSoundSources);
    procedure SetMasterVolume(const val: Single);
    procedure SetListener(const val: TVKBaseSceneObject);
    procedure SetMaxChannels(const val: Integer);
    procedure SetOutputFrequency(const val: Integer);
    procedure SetUpdateFrequency(const val: Single);
    function StoreUpdateFrequency: Boolean;
    procedure SetCadencer(const val: TVKCadencer);
    procedure SetDistanceFactor(const val: Single);
    function StoreDistanceFactor: Boolean;
    procedure SetRollOffFactor(const val: Single);
    function StoreRollOffFactor: Boolean;
    procedure SetDopplerFactor(const val: Single);
    procedure SetSoundEnvironment(const val: TVKSoundEnvironment);

    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;

    procedure ListenerCoordinates(var position, velocity, direction, up:
      TVector);

    function DoActivate: Boolean; dynamic;
    //: Invoked AFTER all sources have been stopped
    procedure DoDeActivate; dynamic;
    {: Effect mute of all sounds.<p>
       Default implementation call MuteSource for all non-muted sources
       with "True" as parameter. }
    function DoMute: Boolean; dynamic;
    {: Effect un-mute of all sounds.<p>
       Default implementation call MuteSource for all non-muted sources
       with "False" as parameter. }
    procedure DoUnMute; dynamic;
    {: Effect pause of all sounds.<p>
       Default implementation call PauseSource for all non-paused sources
       with "True" as parameter. }
    function DoPause: Boolean; dynamic;
    {: Effect un-pause of all sounds.<p>
       Default implementation call PauseSource for all non-paused sources
       with "True" as parameter. }
    procedure DoUnPause; dynamic;

    procedure NotifyMasterVolumeChange; dynamic;
    procedure Notify3DFactorsChanged; dynamic;
    procedure NotifyEnvironmentChanged; dynamic;

    //: Called when a source will be freed
    procedure KillSource(aSource: TVKBaseSoundSource); virtual;
    {: Request to update source's data in low-level sound API.<p>
       Default implementation just clears the "Changes" flags. }
    procedure UpdateSource(aSource: TVKBaseSoundSource); virtual;
    procedure MuteSource(aSource: TVKBaseSoundSource; muted: Boolean); virtual;
    procedure PauseSource(aSource: TVKBaseSoundSource; paused: Boolean);
      virtual;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {: Manual request to update all sources to reflect changes.<p>
       Default implementation invokes UpdateSource for all known sources. }
    procedure UpdateSources; virtual;
    {: Stop and free all sources. }
    procedure StopAllSources;

    {: Progress notification for time synchronization.<p>
       This method will call UpdateSources depending on the last time
       it was performed and the value of the UpdateFrequency property. }
    procedure DoProgress(const progressTime: TProgressTimes); override;

    {: Sound manager API reported CPU Usage.<p>
       Returns -1 when unsupported. }
    function CPUUsagePercent: Single; virtual;
    {: True if EAX is supported. }
    function EAXSupported: Boolean; dynamic;

  published
    { Published Declarations }
      {: Activation/deactivation of the low-level sound API }
    property Active: Boolean read FActive write SetActive default False;

    {: Maximum number of sound output channels.<p>
       While some drivers will just ignore this value, others cannot
       dynamically adjust the maximum number of channels (you need to
       de-activate and re-activate the manager for this property to be
       taken into account). }
    property MaxChannels: Integer read FMaxChannels write SetMaxChannels default
      8;
    {: Sound output mixing frequency.<p>
       Commonly used values ar 11025, 22050 and 44100.<p>
       Note that most driver cannot dynamically adjust the output frequency
       (you need to de-ativate and re-activate the manager for this property
       to be taken into account). }
    property OutputFrequency: Integer read FOutputFrequency write
      SetOutputFrequency default 44100;

    {: Request to mute all sounds.<p>
       All sound requests should be handled as if sound is unmuted though,
       however drivers should try to take a CPU advantage of mute over
       MasterVolume=0 }
    property Mute: Boolean read FMute write SetMute default False;
    {: Request to pause all sound, sound output should be muted too.<p>
       When unpausing, all sound should resume at the point they were paused. }
    property Pause: Boolean read FPause write SetPause default False;
    {: Master Volume adjustement in the [0.0; 1.0] range.<p>
       Driver should take care of properly clamping the master volume. }
    property MasterVolume: Single read FMasterVolume write SetMasterVolume;

    {: Scene object that materializes the listener.<p>
       The sceneobject's AbsolutePosition and orientation are used to define
       the listener coordinates, velocity is automatically calculated
       (if you're using DoProgress or connected the manager to a cadencer).<p>
       If this property is nil, the listener is assumed to be static at
       the NullPoint coordinate, facing Z axis, with up being Y (ie. the
       default GLScene orientation). }
    property Listener: TVKBaseSceneObject read FListener write SetListener;
    {: Currently active and playing sound sources. }
    property Sources: TVKSoundSources read FSources write SetSources;

    {: Update frequency for time-based control (DoProgress).<p>
       Default value is 10 Hz (frequency is clamped in the 1Hz-60Hz range). }
    property UpdateFrequency: Single read FUpdateFrequency write
      SetUpdateFrequency stored StoreUpdateFrequency;
    {: Cadencer for time-based control.<p> }
    property Cadencer: TVKCadencer read FCadencer write SetCadencer;
    {: Engine relative distance factor, compared to 1.0 meters.<p>
       Equates to 'how many units per meter' your engine has. }
    property DistanceFactor: Single read FDistanceFactor write SetDistanceFactor
      stored StoreDistanceFactor;
    {: Sets the global attenuation rolloff factor.<p>
       Normally volume for a sample will scale at 1 / distance.
       This gives a logarithmic attenuation of volume as the source gets
       further away (or closer).<br>
       Setting this value makes the sound drop off faster or slower.
       The higher the value, the faster volume will fall off. }
    property RollOffFactor: Single read FRollOffFactor write SetRollOffFactor
      stored StoreRollOffFactor;
    {: Engine relative Doppler factor, compared to 1.0 meters.<p>
       Equates to 'how many units per meter' your engine has. }
    property DopplerFactor: Single read FDopplerFactor write SetDopplerFactor
      stored False;
    {: Sound environment (requires EAX compatible soundboard). }
    property Environment: TVKSoundEnvironment read FSoundEnvironment write
      SetSoundEnvironment default seDefault;
  end;

  // TVKBSoundEmitter
  //
  {: A sound emitter behaviour, plug it on any object to make it noisy.<p>
       This behaviour is just an interface to a TVKSoundSource, for editing
       convenience. }
  TVKBSoundEmitter = class(TVKBehaviour)
  private
    { Private Declarations }
    FPlaying: Boolean; // used at design-time ONLY
    FSource: TVKBaseSoundSource;
    FPlayingSource: TVKSoundSource;

  protected
    { Protected Declarations }
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;

    procedure SetSource(const val: TVKBaseSoundSource);
    procedure SetPlaying(const val: Boolean);
    function GetPlaying: Boolean;

    procedure NotifySourceDestruction(aSource: TVKSoundSource);

  public
    { Public Declarations }
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    class function UniqueItem: Boolean; override;

    procedure DoProgress(const progressTime: TProgressTimes); override;

    property PlayingSource: TVKSoundSource read FPlayingSource;

  published
    { Published Declarations }
    property Source: TVKBaseSoundSource read FSource write SetSource;
    property Playing: Boolean read GetPlaying write SetPlaying default False;

  end;

function ActiveSoundManager: TVKSoundManager;
function GetSoundLibraryByName(const aName: string): TVKSoundLibrary;

function GetOrCreateSoundEmitter(behaviours: TVKBehaviours): TVKBSoundEmitter;
  overload;
function GetOrCreateSoundEmitter(obj: TVKBaseSceneObject): TVKBSoundEmitter;
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
  vActiveSoundManager: TVKSoundManager;
  vSoundLibraries: TList;

  // ActiveSoundManager
  //

function ActiveSoundManager: TVKSoundManager;
begin
  Result := vActiveSoundManager;
end;

// GetSoundLibraryByName
//

function GetSoundLibraryByName(const aName: string): TVKSoundLibrary;
var
  i: Integer;
begin
  Result := nil;
  if Assigned(vSoundLibraries) then
    for i := 0 to vSoundLibraries.Count - 1 do
      if TVKSoundLibrary(vSoundLibraries[i]).Name = aName then
      begin
        Result := TVKSoundLibrary(vSoundLibraries[i]);
        Break;
      end;
end;

// GetOrCreateSoundEmitter (TVKBehaviours)
//

function GetOrCreateSoundEmitter(behaviours: TVKBehaviours): TVKBSoundEmitter;
var
  i: Integer;
begin
  i := behaviours.IndexOfClass(TVKBSoundEmitter);
  if i >= 0 then
    Result := TVKBSoundEmitter(behaviours[i])
  else
    Result := TVKBSoundEmitter.Create(behaviours);
end;

// GetOrCreateSoundEmitter (TVKBaseSceneObject)
//

function GetOrCreateSoundEmitter(obj: TVKBaseSceneObject): TVKBSoundEmitter;
begin
  Result := GetOrCreateSoundEmitter(obj.Behaviours);
end;

// ------------------
// ------------------ TVKSoundSample ------------------
// ------------------

// Create
//

constructor TVKSoundSample.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

// Destroy
//

destructor TVKSoundSample.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

// Assign
//

procedure TVKSoundSample.Assign(Source: TPersistent);
begin
  if Source is TVKSoundSample then
  begin
    FName := TVKSoundSample(Source).Name;
    FData.Free;
    FData := TVKSoundFile(TVKSoundSample(Source).Data.CreateCopy(Self));
  end
  else
    inherited Assign(Source); // Assign error
end;

// DefineProperties
//

procedure TVKSoundSample.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('BinData', ReadData, WriteData, Assigned(FData));
end;

// ReadData
//

procedure TVKSoundSample.ReadData(Stream: TStream);
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
    FData := TVKSoundFileClass(FindClass(string(clName))).Create(Self);
    FData.LoadFromStream(Stream);
  end;
end;

// WriteData
//

procedure TVKSoundSample.WriteData(Stream: TStream);
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

function TVKSoundSample.GetDisplayName: string;
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

procedure TVKSoundSample.LoadFromFile(const fileName: string);
var
  sfc: TVKSoundFileClass;
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

procedure TVKSoundSample.PlayOnWaveOut;
begin
  if Assigned(FData) then
    FData.PlayOnWaveOut;
end;

// TVKSoundSample
//

function TVKSoundSample.Sampling: TVKSoundSampling;
begin
  if Assigned(FData) then
    Result := FData.Sampling
  else
    Result := nil;
end;

// LengthInBytes
//

function TVKSoundSample.LengthInBytes: Integer;
begin
  if Assigned(FData) then
    Result := FData.LengthInBytes
  else
    Result := 0;
end;

// LengthInSamples
//

function TVKSoundSample.LengthInSamples: Integer;
begin
  if Assigned(FData) then
    Result := FData.LengthInSamples
  else
    Result := 0;
end;

// LengthInSec
//

function TVKSoundSample.LengthInSec: Single;
begin
  if Assigned(FData) then
    Result := FData.LengthInSec
  else
    Result := 0;
end;

// SetData
//

procedure TVKSoundSample.SetData(const val: TVKSoundFile);
begin
  FData.Free;
  if Assigned(val) then
    FData := TVKSoundFile(val.CreateCopy(Self))
  else
    FData := nil;
end;

// ------------------
// ------------------ TVKSoundSamples ------------------
// ------------------

constructor TVKSoundSamples.Create(AOwner: TComponent);
begin
  Owner := AOwner;
  inherited Create(TVKSoundSample);
end;

function TVKSoundSamples.GetOwner: TPersistent;
begin
  Result := Owner;
end;

procedure TVKSoundSamples.SetItems(index: Integer; const val: TVKSoundSample);
begin
  inherited Items[index] := val;
end;

function TVKSoundSamples.GetItems(index: Integer): TVKSoundSample;
begin
  Result := TVKSoundSample(inherited Items[index]);
end;

function TVKSoundSamples.Add: TVKSoundSample;
begin
  Result := (inherited Add) as TVKSoundSample;
end;

function TVKSoundSamples.FindItemID(ID: Integer): TVKSoundSample;
begin
  Result := (inherited FindItemID(ID)) as TVKSoundSample;
end;

// GetByName
//

function TVKSoundSamples.GetByName(const aName: string): TVKSoundSample;
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

function TVKSoundSamples.AddFile(const fileName: string; const sampleName: string
  = ''): TVKSoundSample;
begin
  Result := Add;
  Result.LoadFromFile(fileName);
  if sampleName <> '' then
    Result.Name := sampleName;
end;

// ------------------
// ------------------ TVKSoundLibrary ------------------
// ------------------

constructor TVKSoundLibrary.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSamples := TVKSoundSamples.Create(Self);
  vSoundLibraries.Add(Self);
end;

destructor TVKSoundLibrary.Destroy;
begin
  vSoundLibraries.Remove(Self);
  FSamples.Free;
  inherited Destroy;
end;

// Notification
//

procedure TVKSoundLibrary.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited;
end;

// SetSamples
//

procedure TVKSoundLibrary.SetSamples(const val: TVKSoundSamples);
begin
  FSamples.Assign(val);
end;

// ------------------
// ------------------ TVKBaseSoundSource ------------------
// ------------------

// Create
//

constructor TVKBaseSoundSource.Create(Collection: TCollection);
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

destructor TVKBaseSoundSource.Destroy;
begin
  inherited Destroy;
end;

// GetDisplayName
//

function TVKBaseSoundSource.GetDisplayName: string;
begin
  Result := Format('%s', [FSoundName]);
end;

// Assign
//

procedure TVKBaseSoundSource.Assign(Source: TPersistent);
begin
  if Source is TVKBaseSoundSource then
  begin
    FPriority := TVKBaseSoundSource(Source).FPriority;
    FOrigin := TVKBaseSoundSource(Source).FOrigin;
    FVolume := TVKBaseSoundSource(Source).FVolume;
    FMinDistance := TVKBaseSoundSource(Source).FMinDistance;
    FMaxDistance := TVKBaseSoundSource(Source).FMaxDistance;
    FInsideConeAngle := TVKBaseSoundSource(Source).FInsideConeAngle;
    FOutsideConeAngle := TVKBaseSoundSource(Source).FOutsideConeAngle;
    FConeOutsideVolume := TVKBaseSoundSource(Source).FConeOutsideVolume;
    FSoundLibraryName := TVKBaseSoundSource(Source).FSoundLibraryName;
    FSoundLibrary := TVKBaseSoundSource(Source).FSoundLibrary;
    FSoundName := TVKBaseSoundSource(Source).FSoundName;
    FMute := TVKBaseSoundSource(Source).FMute;
    FPause := TVKBaseSoundSource(Source).FPause;
    FChanges := [sscTransformation, sscSample, sscStatus];
    FNbLoops := TVKBaseSoundSource(Source).FNbLoops;
    FFrequency := TVKBaseSoundSource(Source).FFrequency;
  end
  else
    inherited Assign(Source);
end;

// WriteToFiler
//

procedure TVKBaseSoundSource.WriteToFiler(writer: TWriter);
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

procedure TVKBaseSoundSource.ReadFromFiler(reader: TReader);
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

function TVKBaseSoundSource.Sample: TVKSoundSample;
begin
  if SoundLibrary <> nil then
    Result := FSoundLibrary.Samples.GetByName(FSoundName)
  else
    Result := nil;
end;

// SetPriority
//

procedure TVKBaseSoundSource.SetPriority(const val: Integer);
begin
  if val <> FPriority then
  begin
    FPriority := val;
    Include(FChanges, sscStatus);
  end;
end;

// SetOrigin
//

procedure TVKBaseSoundSource.SetOrigin(const val: TVKBaseSceneObject);
begin
  if val <> FOrigin then
  begin
    FOrigin := val;
    Include(FChanges, sscTransformation);
  end;
end;

// SetVolume
//

procedure TVKBaseSoundSource.SetVolume(const val: Single);
begin
  if val <> FVolume then
  begin
    FVolume := ClampValue(val, 0, 1);
    Include(FChanges, sscStatus);
  end;
end;

// SetMinDistance
//

procedure TVKBaseSoundSource.SetMinDistance(const val: Single);
begin
  if val <> FMinDistance then
  begin
    FMinDistance := ClampValue(val, 0);
    Include(FChanges, sscStatus);
  end;
end;

// SetMaxDistance
//

procedure TVKBaseSoundSource.SetMaxDistance(const val: Single);
begin
  if val <> FMaxDistance then
  begin
    FMaxDistance := ClampValue(val, 0);
    Include(FChanges, sscStatus);
  end;
end;

// SetInsideConeAngle
//

procedure TVKBaseSoundSource.SetInsideConeAngle(const val: Single);
begin
  if val <> FInsideConeAngle then
  begin
    FInsideConeAngle := ClampValue(val, 0, 360);
    Include(FChanges, sscStatus);
  end;
end;

// SetOutsideConeAngle
//

procedure TVKBaseSoundSource.SetOutsideConeAngle(const val: Single);
begin
  if val <> FOutsideConeAngle then
  begin
    FOutsideConeAngle := ClampValue(val, 0, 360);
    Include(FChanges, sscStatus);
  end;
end;

// SetConeOutsideVolume
//

procedure TVKBaseSoundSource.SetConeOutsideVolume(const val: Single);
begin
  if val <> FConeOutsideVolume then
  begin
    FConeOutsideVolume := ClampValue(val, 0, 1);
    Include(FChanges, sscStatus);
  end;
end;

// GetSoundLibrary
//

function TVKBaseSoundSource.GetSoundLibrary: TVKSoundLibrary;
begin
  if (FSoundLibrary = nil) and (FSoundLibraryName <> '') then
    FSoundLibrary := GetSoundLibraryByName(FSoundLibraryName);
  Result := FSoundLibrary;
end;

// SetSoundLibrary
//

procedure TVKBaseSoundSource.SetSoundLibrary(const val: TVKSoundLibrary);
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

procedure TVKBaseSoundSource.SetSoundName(const val: string);
begin
  if val <> FSoundName then
  begin
    FSoundName := val;
    Include(FChanges, sscSample);
  end;
end;

// SetPause
//

procedure TVKBaseSoundSource.SetPause(const val: Boolean);
begin
  if val <> FPause then
  begin
    FPause := val;
    if Collection <> nil then
      TVKSoundManager(TVKSoundSources(Collection).owner).PauseSource(Self,
        FPause);
  end;
end;

// SetNbLoops
//

procedure TVKBaseSoundSource.SetNbLoops(const val: Integer);
begin
  if val <> FNbLoops then
  begin
    FNbLoops := val;
    Include(FChanges, sscSample);
  end;
end;

// SetFrequency
//

procedure TVKBaseSoundSource.SetFrequency(const val: integer);
begin
  if val <> FFrequency then
  begin
    FFrequency := val;
    Include(FChanges, sscStatus);
  end;
end;

// SetMute
//

procedure TVKBaseSoundSource.SetMute(const val: Boolean);
begin
  if val <> FMute then
  begin
    FMute := val;
    if Collection <> nil then
      TVKSoundManager(TVKSoundSources(Collection).owner).MuteSource(Self,
        FMute);
  end;
end;

// ------------------
// ------------------ TVKSoundSource ------------------
// ------------------

// Destroy
//

destructor TVKSoundSource.Destroy;
begin
  if Assigned(FBehaviourToNotify) then
    FBehaviourToNotify.NotifySourceDestruction(Self);
  if Collection <> nil then
    ((Collection as TVKSoundSources).Owner as TVKSoundManager).KillSource(Self);
  inherited;
end;

// ------------------
// ------------------ TVKSoundSources ------------------
// ------------------

constructor TVKSoundSources.Create(AOwner: TComponent);
begin
  Owner := AOwner;
  inherited Create(TVKSoundSource);
end;

function TVKSoundSources.GetOwner: TPersistent;
begin
  Result := Owner;
end;

procedure TVKSoundSources.SetItems(index: Integer; const val: TVKSoundSource);
begin
  inherited Items[index] := val;
end;

function TVKSoundSources.GetItems(index: Integer): TVKSoundSource;
begin
  Result := TVKSoundSource(inherited Items[index]);
end;

function TVKSoundSources.Add: TVKSoundSource;
begin
  Result := (inherited Add) as TVKSoundSource;
end;

function TVKSoundSources.FindItemID(ID: Integer): TVKSoundSource;
begin
  Result := (inherited FindItemID(ID)) as TVKSoundSource;
end;

// ------------------
// ------------------ TVKSoundManager ------------------
// ------------------

// Create
//

constructor TVKSoundManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSources := TVKSoundSources.Create(Self);
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

destructor TVKSoundManager.Destroy;
begin
  Active := False;
  Listener := nil;
  FSources.Free;
  inherited Destroy;
end;

// Notification
//

procedure TVKSoundManager.Notification(AComponent: TComponent; Operation:
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

procedure TVKSoundManager.SetActive(const val: Boolean);
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

function TVKSoundManager.DoActivate: Boolean;
begin
  Result := True;
end;

// DeActivate
//

procedure TVKSoundManager.DoDeActivate;
begin
  StopAllSources;
end;

// SetMute
//

procedure TVKSoundManager.SetMute(const val: Boolean);
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

function TVKSoundManager.DoMute: Boolean;
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

procedure TVKSoundManager.DoUnMute;
var
  i: Integer;
begin
  for i := 0 to Sources.Count - 1 do
    if not Sources[i].Mute then
      MuteSource(Sources[i], False);
end;

// SetPause
//

procedure TVKSoundManager.SetPause(const val: Boolean);
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

procedure TVKSoundManager.Loaded;
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

procedure TVKSoundManager.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Doppler', ReadDoppler, WriteDoppler, (DopplerFactor <>
    1));
end;

// WriteDoppler
//

procedure TVKSoundManager.WriteDoppler(writer: TWriter);
begin
  writer.WriteFloat(DopplerFactor);
end;

// ReadDoppler
//

procedure TVKSoundManager.ReadDoppler(reader: TReader);
begin
  FDopplerFactor := reader.ReadFloat;
end;

// DoPause
//

function TVKSoundManager.DoPause: Boolean;
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

procedure TVKSoundManager.DoUnPause;
var
  i: Integer;
begin
  for i := 0 to Sources.Count - 1 do
    if not Sources[i].Pause then
      PauseSource(Sources[i], False);
end;

// SetMasterVolume
//

procedure TVKSoundManager.SetMasterVolume(const val: Single);
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

procedure TVKSoundManager.SetMaxChannels(const val: Integer);
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

procedure TVKSoundManager.SetOutputFrequency(const val: Integer);
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

procedure TVKSoundManager.SetUpdateFrequency(const val: Single);
begin
  FUpdateFrequency := ClampValue(val, 1, 60);
end;

// StoreUpdateFrequency
//

function TVKSoundManager.StoreUpdateFrequency: Boolean;
begin
  Result := (FUpdateFrequency <> 10);
end;

// SetCadencer
//

procedure TVKSoundManager.SetCadencer(const val: TVKCadencer);
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

procedure TVKSoundManager.SetDistanceFactor(const val: Single);
begin
  if val <= 0 then
    FDistanceFactor := 1
  else
    FDistanceFactor := val;
  Notify3DFactorsChanged;
end;

// StoreDistanceFactor
//

function TVKSoundManager.StoreDistanceFactor: Boolean;
begin
  Result := (FDistanceFactor <> 1);
end;

// SetRollOffFactor
//

procedure TVKSoundManager.SetRollOffFactor(const val: Single);
begin
  if val <= 0 then
    FRollOffFactor := 1
  else
    FRollOffFactor := val;
  Notify3DFactorsChanged;
end;

// StoreRollOffFactor
//

function TVKSoundManager.StoreRollOffFactor: Boolean;
begin
  Result := (FRollOffFactor <> 1);
end;

// SetDopplerFactor
//

procedure TVKSoundManager.SetDopplerFactor(const val: Single);
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

procedure TVKSoundManager.SetSoundEnvironment(const val: TVKSoundEnvironment);
begin
  if val <> FSoundEnvironment then
  begin
    FSoundEnvironment := val;
    NotifyEnvironmentChanged;
  end;
end;

// ListenerCoordinates
//

procedure TVKSoundManager.ListenerCoordinates(var position, velocity, direction,
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
    if (Listener is TVKCamera) and (TVKCamera(Listener).TargetObject <> nil)
      then
    begin
      // special case of the camera targeting something
      direction := TVKCamera(Listener).AbsoluteVectorToTarget;
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

procedure TVKSoundManager.NotifyMasterVolumeChange;
begin
  // nothing
end;

// Notify3DFactorsChanged
//

procedure TVKSoundManager.Notify3DFactorsChanged;
begin
  // nothing
end;

// NotifyEnvironmentChanged
//

procedure TVKSoundManager.NotifyEnvironmentChanged;
begin
  // nothing
end;

// SetListener
//

procedure TVKSoundManager.SetListener(const val: TVKBaseSceneObject);
begin
  if Assigned(FListener) then
    FListener.RemoveFreeNotification(Self);
  FListener := val;
  if Assigned(FListener) then
    FListener.FreeNotification(Self);
end;

// SetSources
//

procedure TVKSoundManager.SetSources(const val: TVKSoundSources);
begin
  FSources.Assign(val);
end;

// KillSource
//

procedure TVKSoundManager.KillSource(aSource: TVKBaseSoundSource);
begin
  // nothing
end;

// UpdateSource
//

procedure TVKSoundManager.UpdateSource(aSource: TVKBaseSoundSource);
begin
  aSource.FChanges := [];
end;

// MuteSource
//

procedure TVKSoundManager.MuteSource(aSource: TVKBaseSoundSource; muted:
  Boolean);
begin
  // nothing
end;

// PauseSource
//

procedure TVKSoundManager.PauseSource(aSource: TVKBaseSoundSource; paused:
  Boolean);
begin
  // nothing
end;

// UpdateSources
//

procedure TVKSoundManager.UpdateSources;
var
  i: Integer;
begin
  for i := Sources.Count - 1 downto 0 do
    UpdateSource(Sources[i]);
end;

// StopAllSources
//

procedure TVKSoundManager.StopAllSources;
var
  i: Integer;
begin
  for i := Sources.Count - 1 downto 0 do
    Sources.Delete(i);
end;

// DoProgress
//

procedure TVKSoundManager.DoProgress(const progressTime: TProgressTimes);
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

function TVKSoundManager.CPUUsagePercent: Single;
begin
  Result := -1;
end;

// EAXSupported
//

function TVKSoundManager.EAXSupported: Boolean;
begin
  Result := False;
end;

// ------------------
// ------------------ TVKBSoundEmitter ------------------
// ------------------

// Create
//

constructor TVKBSoundEmitter.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  FSource := TVKSoundSource.Create(nil);
end;

// Destroy
//

destructor TVKBSoundEmitter.Destroy;
begin
  if Assigned(FPlayingSource) then
    FPlayingSource.Free;
  FSource.Free;
  inherited Destroy;
end;

// Assign
//

procedure TVKBSoundEmitter.Assign(Source: TPersistent);
begin
  if Source is TVKBSoundEmitter then
  begin
    FSource.Assign(TVKBSoundEmitter(Source).FSource);
  end;
  inherited Assign(Source);
end;

// WriteToFiler
//

procedure TVKBSoundEmitter.WriteToFiler(writer: TWriter);
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

procedure TVKBSoundEmitter.ReadFromFiler(reader: TReader);
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

procedure TVKBSoundEmitter.Loaded;
begin
  inherited;
  if not (csDesigning in OwnerBaseSceneObject.ComponentState) then
    SetPlaying(FPlaying);
end;

// FriendlyName
//

class function TVKBSoundEmitter.FriendlyName: string;
begin
  Result := 'Sound Emitter';
end;

// FriendlyDescription
//

class function TVKBSoundEmitter.FriendlyDescription: string;
begin
  Result := 'A simple sound emitter behaviour';
end;

// UniqueBehaviour
//

class function TVKBSoundEmitter.UniqueItem: Boolean;
begin
  Result := False;
end;

// DoProgress
//

procedure TVKBSoundEmitter.DoProgress(const progressTime: TProgressTimes);
begin
  // nothing, yet
end;

// SetSource
//

procedure TVKBSoundEmitter.SetSource(const val: TVKBaseSoundSource);
begin
  FSource.Assign(val);
end;

// SetPlaying
//

procedure TVKBSoundEmitter.SetPlaying(const val: Boolean);
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

function TVKBSoundEmitter.GetPlaying: Boolean;
begin
  if csDesigning in OwnerBaseSceneObject.ComponentState then
    Result := FPlaying
  else
    Result := Assigned(FPlayingSource);
end;

// NotifySourceDestruction
//

procedure TVKBSoundEmitter.NotifySourceDestruction(aSource: TVKSoundSource);
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
  RegisterClasses([TVKSoundLibrary]);
  RegisterXCollectionItemClass(TVKBSoundEmitter);
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

  UnregisterXCollectionItemClass(TVKBSoundEmitter);

end.

