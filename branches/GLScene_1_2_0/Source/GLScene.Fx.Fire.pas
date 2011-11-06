//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLFireFX<p>

  Fire special effect<p>

  <b>Historique : </b><font size=-1><ul>
  <li>06/11/11 - Yar - Transition to indirect rendering objects
  <li>21/01/01 - DanB - Added "inherited" call to TGLBFireFX.WriteToFiler
  <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
  <li>14/06/10 - Yar - Bugfixed in TGLBFireFX.ReadFromFiler when assertion off (thanks olkondr)
  <li>22/04/10 - Yar - Fixes after GLState revision
  <li>11/04/10 - Yar -  Replaced glNewList to GLState.NewList in TGLBFireFX.Render
  <li>05/03/10 - DanB - More state added to TGLStateCache
  <li>06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
  <li>30/03/07 - DaStr - Added $I GLScene.inc
  <li>14/03/07 - DaStr - Added explicit pointer dereferencing
  (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
  <li>23/02/07 - DaStr - Fixed TGLFireFXManager.Create (TGLCoordinatesStyle stuff)
  <li>21/02/02 - EG - Added GetOrCreateFireFX helper functions
  <li>09/12/01 - EG - Added NoZWrite property
  <li>12/08/01 - EG - Fixed leak (color objects)
  <li>09/03/01 - EG - Fixed MaxParticles change, added RingExplosion
  <li>08/03/01 - EG - Revisited the effect and added new parameters,
  dropped/renamed some, started documentation (just started)
  <li>13/01/01 - EG - Another matrix compatibility update
  <li>22/12/00 - EG - Compatibility for new Matrix rules, and sometime
  ago, added in all new props from Danjel Grosar
  <li>11/08/00 - EG - A few speedups/enhancements
  <li>08/08/00 - EG - Creation, based on Roger Cao's "FireEffectUnit"
  </ul></font>
}
unit GLScene.Fx.Fire;

interface

{$I GLScene.inc}

uses
  Classes,
  GLScene.Core,
  GLScene.Base.XCollection,
  GLScene.Base.Vector.Geometry,
  GLScene.Cadencer,
  GLScene.Base.Transformation,
  GLScene.Base.Color,
  GLScene.Base.Classes,
  GLScene.Base.Coordinates,
  GLScene.Manager,
  GLScene.Base.Context.Info,
  GLScene.Base.GLStateMachine,
  GLScene.Material,
  GLScene.MaterialEx,
  GLScene.Texture.Format,
  GLScene.Mesh,
  GLScene.DrawTechnique;

type

  PFireParticle = ^TFireParticle;

  TFireParticle = record
    Position: TVector;
    Speed: TVector;
    Alpha: Single;
    TimeToLive, LifeLength: Single;
  end;

  TFireParticleArray = array [0 .. MAXINT shr 6] of TFireParticle;
  PFireParticleArray = ^TFireParticleArray;

  TGLBFireFX = class;

  // TGLFireFXManager
  //
  { : Fire special effect manager.<p>
    Defines the looks and behaviour of a particle system that can be made
    to look fire-like. }
  TGLFireFXManager = class(TGLCadenceAbleComponent)
  private
    { Private Declarations }
    FClients: TList;
    FFireParticles: PFireParticleArray;
    FFireDir, FInitialDir: TGLCoordinates;
    FCadencer: TGLCadencer;
    FMaxParticles, FParticleLife: Integer;
    FParticleSize, FFireDensity, FFireEvaporation: Single;
    FFireCrown, FParticleInterval, IntervalDelta: Single;
    NP: Integer;
    FInnerColor, FOuterColor: TGLColor;
    FFireBurst, FFireRadius: Single;
    FDisabled, FPaused, FUseInterval: Boolean;
    FReference: TGLBaseSceneObject;
    FNoZWrite: Boolean;
    procedure SetNoZWrite(const Value: Boolean);
  protected
    { Protected Declarations }
    FBatch: TDrawBatch;
    FTransformation: TTransformationRec;
    procedure RegisterClient(aClient: TGLBFireFX);
    procedure DeRegisterClient(aClient: TGLBFireFX);
    procedure DeRegisterAllClients;

    procedure SetFireDir(const val: TGLCoordinates);
    procedure SetInitialDir(const val: TGLCoordinates);
    procedure SetCadencer(const val: TGLCadencer);
    function StoreParticleSize: Boolean;
    procedure SetInnerColor(const val: TGLColor);
    procedure SetOuterColor(const val: TGLColor);
    procedure SetReference(const val: TGLBaseSceneObject);
    procedure SetMaxParticles(const val: Integer);

    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    procedure CalcFire(deltaTime: Double;
      ParticleInterval, ParticleLife: Single; FireAlpha: Single);
    procedure BuildMesh; dynamic;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { : Reinitializes the fire. }
    procedure FireInit;

    { : Spawns a large quantity of particles to simulate an isotropic explosion.<p>
      This method generates an isotropic explosion, i.e. there is no
      privilegied direction in the initial vector. }
    procedure IsotropicExplosion(minInitialSpeed, maxInitialSpeed,
      lifeBoostFactor: Single; nbParticles: Integer = -1);
    { : Spawns a large quantity of particles to simulate a ring explosion.<p>
      This method generates a ring explosion. The plane of the ring is described
      by ringVectorX/Y, which should be of unit length (but you may not
      make them of unit length if you want "elliptic" rings). }
    procedure RingExplosion(minInitialSpeed, maxInitialSpeed, lifeBoostFactor
      : Single; const ringVectorX, ringVectorY: TAffineVector;
      nbParticles: Integer = -1);

    { : Current Nb of particles. }
    property ParticleCount: Integer read NP;

    procedure DoProgress(const progressTime: TProgressTimes); override;

  published
    { Published Declarations }
    { : Adjusts the acceleration direction (abs coordinates). }
    property FireDir: TGLCoordinates read FFireDir write SetFireDir;
    { : Adjusts the initial direction (abs coordinates). }
    property InitialDir: TGLCoordinates read FInitialDir write SetInitialDir;
    { : The cadencer that will "drive" the animation of the system. }
    property Cadencer: TGLCadencer read FCadencer write SetCadencer;
    { : Maximum number of simultaneous particles in the system. }
    property MaxParticles: Integer read FMaxParticles write SetMaxParticles
      default 256;
    { : Size of the particle, in absolute units. }
    property ParticleSize: Single read FParticleSize write FParticleSize
      stored StoreParticleSize;
    { : Inner color of a particle. }
    property InnerColor: TGLColor read FInnerColor write SetInnerColor;
    { : Outer color of a particle. }
    property OuterColor: TGLColor read FOuterColor write SetOuterColor;
    // default clrWhite;
    property FireDensity: Single read FFireDensity write FFireDensity;
    property FireEvaporation: Single read FFireEvaporation
      write FFireEvaporation;
    { : Adjust a crown (circular) radius on which particles are spawned.<p>
      With a value of zero, the particles are spawned in the FireRadius
      cube around the origin, with a non zero value, they appear in
      a torus of major radius FireCrown, and minor radius FireRadius*1.73. }
    property FireCrown: Single read FFireCrown write FFireCrown;
    { : Life length of particle. }
    property ParticleLife: Integer read FParticleLife write FParticleLife
      default 3;
    property FireBurst: Single read FFireBurst write FFireBurst;
    { : Adjusts the random birth radius for particles (actually a birth cube). }
    property FireRadius: Single read FFireRadius write FFireRadius;
    { : If true, no new particles are spawn.<p>
      But current ones continue to live and die. }
    property Disabled: Boolean read FDisabled write FDisabled;
    { : When paused, the fire animation is freezed. }
    property Paused: Boolean read FPaused write FPaused;
    { : Interval between particles births (in sec).<p>
      The interval may not be honoured if MawParticles is reached. }
    property ParticleInterval: Single read FParticleInterval
      write FParticleInterval;
    { : Enable/disable use of ParticleInterval.<p>
      If true ParticleInterval is used, if False, the system will attempt
      to maintain a particle count of MaxParticles, by spawning new
      particles to replace the dead ones ASAP. }
    property UseInterval: Boolean read FUseInterval write FUseInterval;
    { : Particle's render won't write to Z-Buffer }
    property NoZWrite: Boolean read FNoZWrite write SetNoZWrite default True;

    { : Specifies an optional object whose position to use as reference.<p>
      This property allows switching between static/shared fires (for
      fireplaces or static torches) and dynamic fire trails.<br>
      The absolute position of the reference object is 'central' spawning
      point for new particles, usually, the object will be the one and only
      one on which the effect is applied. }
    property Reference: TGLBaseSceneObject read FReference write SetReference;
  end;

  // TGLBFireFX
  //
  { : Fire special effect.<p>
    This effect works as a client of TFireFXManager }
  TGLBFireFX = class(TGLObjectPostEffect)
  private
    { Private Declarations }
    FManager: TGLFireFXManager;
    FManagerName: string; // NOT persistent, temporarily used for persistence
  protected
    { Protected Declarations }
    procedure SetManager(const val: TGLFireFXManager);

    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;

  public
    { Public Declarations }
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;

    procedure Render(var ARci: TRenderContextInfo); override;

  published
    { Published Declarations }
    { : Refers the collision manager. }
    property Manager: TGLFireFXManager read FManager write SetManager;
  end;

  { : Returns or creates the TGLBFireFX within the given behaviours.<p>
    This helper function is convenient way to access a TGLBFireFX. }
function GetOrCreateFireFX(effects: TGLObjectEffects): TGLBFireFX; overload;
{ : Returns or creates the TGLBFireFX within the given object's behaviours.<p>
  This helper function is convenient way to access a TGLBFireFX. }
function GetOrCreateFireFX(obj: TGLBaseSceneObject): TGLBFireFX; overload;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  SysUtils,
  GLScene.Base.OpenGL.Tokens,
  GLScene.Base.Context,
  GLScene.Base.Vector.Lists,
  GLScene.Shader.Parameter
{$IFDEF GLS_DELPHI},
  GLScene.Base.Vector.Types{$ENDIF};

// GetOrCreateFireFX (TGLObjectEffects)
//

function GetOrCreateFireFX(effects: TGLObjectEffects): TGLBFireFX;
var
  i: Integer;
begin
  i := effects.IndexOfClass(TGLBFireFX);
  if i >= 0 then
    Result := TGLBFireFX(effects[i])
  else
    Result := TGLBFireFX.Create(effects);
end;

// GetOrCreateFireFX (TGLBaseSceneObject)
//

function GetOrCreateFireFX(obj: TGLBaseSceneObject): TGLBFireFX;
begin
  Result := GetOrCreateFireFX(obj.effects);
end;

// ------------------
// ------------------ TGLFireFXManager ------------------
// ------------------

// Create
//

constructor TGLFireFXManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClients := TList.Create;
  RegisterManager(Self);
  FFireDir := TGLCoordinates.CreateInitialized(Self,
    VectorMake(0, 0.5, 0), csPoint);
  FInitialDir := TGLCoordinates.CreateInitialized(Self, YHmgVector, csPoint);
  FMaxParticles := 256;
  FParticleSize := 1.0;
  FInnerColor := TGLColor.Create(Self);
  FInnerColor.Initialize(clrYellow);
  FOuterColor := TGLColor.Create(Self);
  FOuterColor.Initialize(clrOrange);
  FFireDensity := 1;
  FFireEvaporation := 0.86;
  FFireCrown := 0;
  FParticleLife := 3;
  FFireBurst := 0;
  FFireRadius := 1;
  FParticleInterval := 0.1;
  FDisabled := false;
  FPaused := false;
  FUseInterval := True;
  FNoZWrite := True;
  IntervalDelta := 0;
  FireInit;

  FBatch.Mesh := TMeshAtom.Create;
  FBatch.Transformation := @FTransformation;
  FBatch.Changed := True;
  FBatch.Mesh.TagName := ClassName;
  FBatch.Material := GetInternalMaterialLibrary.Materials.Add;
  with TGLLibMaterialEx(FBatch.Material) do
  begin
    Name := GetInternalMaterialLibrary.Materials.MakeUniqueName('FireFX');
    FixedFunction.BlendingMode := bmAdditive;
    FixedFunction.MaterialOptions := [moNoLighting];
    FixedFunction.FaceCulling := fcNoCull;
    FixedFunction.DepthProperties.DepthCompareFunction := cfLEqual;
    FixedFunction.DepthProperties.DepthWrite := false;
  end;
  // FBatch.InstancesChain := TInstancesChain.Create;
end;

// Destroy
//

destructor TGLFireFXManager.Destroy;
begin
  DeRegisterAllClients;
  DeRegisterManager(Self);
  FreeMem(FFireParticles);
  FInnerColor.Free;
  FOuterColor.Free;
  FClients.Free;
  FFireDir.Free;
  FInitialDir.Free;
  FBatch.Mesh.Free;
  FBatch.InstancesChain.Free;
  inherited Destroy;
end;

// RegisterClient
//

procedure TGLFireFXManager.RegisterClient(aClient: TGLBFireFX);
begin
  if Assigned(aClient) then
    if FClients.IndexOf(aClient) < 0 then
    begin
      FClients.Add(aClient);
      aClient.FManager := Self;
    end;
end;

// DeRegisterClient
//

procedure TGLFireFXManager.DeRegisterClient(aClient: TGLBFireFX);
begin
  if Assigned(aClient) then
  begin
    aClient.FManager := nil;
    FClients.Remove(aClient);
  end;
end;

// DeRegisterAllClients
//

procedure TGLFireFXManager.DeRegisterAllClients;
var
  i: Integer;
begin
  // Fast deregistration
  for i := 0 to FClients.Count - 1 do
    TGLBFireFX(FClients[i]).FManager := nil;
  FClients.Clear;
end;

// SetFireDir
//

procedure TGLFireFXManager.SetFireDir(const val: TGLCoordinates);
begin
  FFireDir.Assign(val);
end;

// SetInitialDir
//

procedure TGLFireFXManager.SetInitialDir(const val: TGLCoordinates);
begin
  FInitialDir.Assign(val);
end;

// SetCadencer
//

procedure TGLFireFXManager.SetCadencer(const val: TGLCadencer);
begin
  if FCadencer <> val then
  begin
    if Assigned(FCadencer) then
      FCadencer.UnSubscribe(Self);
    FCadencer := val;
    if Assigned(FCadencer) then
      FCadencer.Subscribe(Self);
  end;
end;

// StoreParticleSize
//

function TGLFireFXManager.StoreParticleSize: Boolean;
begin
  Result := (FParticleSize <> 1);
end;

// SetInnerColor
//

procedure TGLFireFXManager.SetInnerColor(const val: TGLColor);
begin
  if FInnerColor <> val then
  begin
    FInnerColor.Color := val.Color;
    FireInit;
  end;
end;

// SetOuterColor
//

procedure TGLFireFXManager.SetOuterColor(const val: TGLColor);
begin
  if FOuterColor <> val then
  begin
    FOuterColor.Color := val.Color;
    FireInit;
  end;
end;

// SetReference
//

procedure TGLFireFXManager.SetReference(const val: TGLBaseSceneObject);
begin
  // nothing more yet, maybe later
  FReference := val;
end;

// SetMaxParticles
//

procedure TGLFireFXManager.SetMaxParticles(const val: Integer);
begin
  if val <> MaxParticles then
  begin
    if val > 0 then
      FMaxParticles := val
    else
      FMaxParticles := 0;
    ReallocMem(FFireParticles, MaxParticles * Sizeof(TFireParticle));
    if NP > MaxParticles then
      NP := MaxParticles;
  end;
end;

procedure TGLFireFXManager.SetNoZWrite(const Value: Boolean);
begin
  if FNoZWrite <> Value then
  begin
    FNoZWrite := Value;
    with TGLLibMaterialEx(FBatch.Material) do
    begin
      FixedFunction.DepthProperties.DepthWrite := not Value;
    end;
  end;
end;

// Notification
//

procedure TGLFireFXManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FCadencer then
      Cadencer := nil
    else if AComponent = FReference then
      Reference := nil;
  end;
  inherited;
end;

// DoProgress
//

procedure TGLFireFXManager.DoProgress(const progressTime: TProgressTimes);
var
  i: Integer;
begin
  // Progress the particles
  if (not FPaused) and (FParticleInterval > 0) then
    CalcFire(progressTime.deltaTime * (1.0 + Abs(FFireBurst)),
      FParticleInterval, FParticleLife, FFireDensity);

  // Invalidate all clients
  for i := 0 to FClients.Count - 1 do
    TGLBFireFX(FClients[i]).OwnerBaseSceneObject.NotifyChange
      (TGLBFireFX(FClients[i]));
end;

// FireInit
//

procedure TGLFireFXManager.FireInit;
begin
  IntervalDelta := 0;
  NP := 0;
  ReallocMem(FFireParticles, FMaxParticles * Sizeof(TFireParticle));
end;

// IsotropicExplosion
//

procedure TGLFireFXManager.IsotropicExplosion(minInitialSpeed, maxInitialSpeed,
  lifeBoostFactor: Single; nbParticles: Integer = -1);
var
  n: Integer;
  tmp, refPos: TVector;
begin
  if nbParticles < 0 then
    n := MAXINT
  else
    n := nbParticles;
  if Assigned(Reference) then
    refPos := Reference.AbsolutePosition
  else
    refPos := NullHmgPoint;
  while (NP < MaxParticles) and (n > 0) do
  begin
    // okay, ain't exactly "isotropic"...
    SetVector(tmp, Random - 0.5, Random - 0.5, Random - 0.5, 0);
    NormalizeVector(tmp);
    ScaleVector(tmp, minInitialSpeed + Random *
      (maxInitialSpeed - minInitialSpeed));
    with FFireParticles^[NP] do
    begin
      Position := VectorAdd(refPos, VectorMake((2 * Random - 1) * FireRadius,
        (2 * Random - 1) * FireRadius, (2 * Random - 1) * FireRadius));
      Speed := tmp;
      TimeToLive := ParticleLife * (Random * 0.5 + 0.5) * lifeBoostFactor;
      LifeLength := TimeToLive;
      Alpha := FireDensity;
    end;
    Inc(NP);
    Dec(n);
  end;
end;

// RingExplosion
//

procedure TGLFireFXManager.RingExplosion(minInitialSpeed, maxInitialSpeed,
  lifeBoostFactor: Single; const ringVectorX, ringVectorY: TAffineVector;
  nbParticles: Integer = -1);
var
  n: Integer;
  tmp, refPos: TVector;
  Fx, fy, d: Single;
begin
  if nbParticles < 0 then
    n := MAXINT
  else
    n := nbParticles;
  if Assigned(Reference) then
    refPos := Reference.AbsolutePosition
  else
    refPos := NullHmgPoint;
  while (NP < MaxParticles) and (n > 0) do
  begin
    // okay, ain't exactly and "isotropic" ring...
    Fx := Random - 0.5;
    fy := Random - 0.5;
    d := RSqrt(Sqr(Fx) + Sqr(fy));
    PAffineVector(@tmp)^ := VectorCombine(ringVectorX, ringVectorY,
      Fx * d, fy * d);
    tmp[3] := 1;
    ScaleVector(tmp, minInitialSpeed + Random *
      (maxInitialSpeed - minInitialSpeed));
    with FFireParticles^[NP] do
    begin
      Position := VectorAdd(refPos, VectorMake((2 * Random - 1) * FireRadius,
        (2 * Random - 1) * FireRadius, (2 * Random - 1) * FireRadius));
      Speed := tmp;
      TimeToLive := ParticleLife * (Random * 0.5 + 0.5) * lifeBoostFactor;
      LifeLength := TimeToLive;
      Alpha := FireDensity;
    end;
    Inc(NP);
    Dec(n);
  end;
end;

// CalcFire
//

procedure TGLFireFXManager.BuildMesh;
begin
  // with FBatch.InstancesChain do
  // begin
  // Lock;
  // try
  // Clear;
  // if FNodesAspect <> lnaInvisible then
  // begin
  // if FNodesAspect <> lnaAxes then
  // begin
  // Attributes[attrColor] := True;
  // AttributesType[attrColor] := GLSLType4f;
  // for I := 0 to Nodes.Count - 1 do
  // with TGLLinesNode(Nodes[I]).Color do
  // AttributeLists[attrColor].Add(Red, Green, Blue, Alpha);
  // end;
  //
  // TransformationEnabled := True;
  // for I := 0 to Nodes.Count - 1 do
  // with TGLLinesNode(Nodes[I]) do
  // Transformations.Add(@FNodeTransformation);
  // end;
  // finally
  // UnLock;
  // end;
  // end;
end;

procedure TGLFireFXManager.CalcFire(deltaTime: Double;
  ParticleInterval, ParticleLife: Single; FireAlpha: Single);
var
  n, i: Integer;
  Fdelta: Single;
  tmp, refPos: TVector;
begin
  // Process live stuff
  n := 0;
  i := 0;
  while n < NP do
  begin
    FFireParticles^[i].TimeToLive := FFireParticles^[i].TimeToLive - deltaTime;
    if (FFireParticles^[i].TimeToLive <= 0) then
    begin
      // Get the prev element
      Dec(NP);
      FFireParticles^[i] := FFireParticles^[NP];
    end
    else
    begin
      // animate it
      with FFireParticles^[i] do
      begin
        Speed := VectorCombine(Speed, FireDir.AsVector, 1, deltaTime);
        Position := VectorCombine(Position, Speed, 1, deltaTime);
      end;
      Inc(n);
      Inc(i);
    end;
  end;
  // Spawn new particles
  if FDisabled then
    Exit;
  if Assigned(Reference) then
    refPos := Reference.AbsolutePosition
  else
    refPos := NullHmgPoint;
  IntervalDelta := IntervalDelta + deltaTime / ParticleInterval;
  if (not UseInterval) or (IntervalDelta > 1) then
  begin
    Fdelta := Frac(IntervalDelta);
    while (NP < MaxParticles) do
    begin
      SetVector(tmp, (2 * Random - 1) * FireRadius,
        (2 * Random - 1) * FireRadius, FireCrown + (2 * Random - 1) *
        FireRadius);
      RotateVectorAroundY(PAffineVector(@tmp)^, Random * 2 * PI);
      AddVector(tmp, refPos);
      with FFireParticles^[NP] do
      begin
        Position := tmp;
        Speed := InitialDir.AsVector;
        TimeToLive := ParticleLife * (Random * 0.5 + 0.5);
        LifeLength := TimeToLive;
        Alpha := FireAlpha;
      end;
      Inc(NP);
      if UseInterval then
        Break;
    end;
    IntervalDelta := Fdelta;
  end;
end;

// ------------------
// ------------------ TGLBFireFX ------------------
// ------------------

// Create
//

constructor TGLBFireFX.Create(AOwner: TXCollection);
begin
  inherited Create(AOwner);
end;

// Destroy
//

destructor TGLBFireFX.Destroy;
begin
  Manager := nil;
  inherited Destroy;
end;

// FriendlyName
//

class function TGLBFireFX.FriendlyName: string;
begin
  Result := 'FireFX';
end;

// FriendlyDescription
//

class function TGLBFireFX.FriendlyDescription: string;
begin
  Result := 'Fire FX';
end;

// WriteToFiler
//

procedure TGLBFireFX.WriteToFiler(writer: TWriter);
begin
  with writer do
  begin
    // ArchiveVersion 1, added inherited call
    WriteInteger(1);
    inherited;
    if Assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else
      WriteString('');
  end;
end;

// ReadFromFiler
//

procedure TGLBFireFX.ReadFromFiler(reader: TReader);
var
  archiveVersion: Integer;
begin
  with reader do
  begin
    archiveVersion := ReadInteger;
    Assert(archiveVersion in [0 .. 1]);
    if archiveVersion >= 1 then
      inherited;
    FManagerName := ReadString;
    Manager := nil;
  end;
end;

// Loaded
//

procedure TGLBFireFX.Loaded;
var
  mng: TComponent;
begin
  inherited;
  if FManagerName <> '' then
  begin
    mng := FindManager(TGLFireFXManager, FManagerName);
    if Assigned(mng) then
      Manager := TGLFireFXManager(mng);
    FManagerName := '';
  end;
end;

// Assign
//

procedure TGLBFireFX.Assign(Source: TPersistent);
begin
  if Source is TGLBFireFX then
  begin
    Manager := TGLBFireFX(Source).Manager;
  end;
  inherited Assign(Source);
end;

// SetManager
//

procedure TGLBFireFX.SetManager(const val: TGLFireFXManager);
begin
  if val <> FManager then
  begin
    if Assigned(FManager) then
      FManager.DeRegisterClient(Self);
    if Assigned(val) then
      val.RegisterClient(Self);
  end;
end;

// Render
//

procedure TGLBFireFX.Render(var ARci: TRenderContextInfo);
var
  n, i: Integer;
  vx, vy, vt: TVector;
  InnerColor, OuterColor: TVector;
  M: TMatrix;
  distList: TSingleList;
  objList: TList;
  fp: PFireParticle;
begin
  if Manager = nil then
    Exit;
  if Manager.Paused then
    Exit;

  ARci.PipelineTransformation.Push;
  // revert to the base model matrix in the case of a referenced fire
  if Assigned(Manager.Reference) then
    ARci.PipelineTransformation.ModelMatrix := IdentityHmgMatrix;
  M := ARci.PipelineTransformation.ViewMatrix;
  Manager.FTransformation := ARci.PipelineTransformation.StackTop;
  ARci.PipelineTransformation.Pop;

  n := Manager.NP;

  if n > 1 then
  begin
    distList := TSingleList.Create;
    objList := TList.Create;
    for i := 0 to n - 1 do
    begin
      fp := @(Manager.FFireParticles[i]);
      distList.Add(VectorDotProduct(ARci.cameraDirection, fp^.Position));
      objList.Add(fp);
    end;
    QuickSortLists(0, n - 1, distList, objList);

    for i := 0 to 2 do
    begin
      vx[i] := M[i][0] * Manager.ParticleSize;
      vy[i] := M[i][1] * Manager.ParticleSize;
    end;

    SetVector(InnerColor, Manager.InnerColor.Color);
    SetVector(OuterColor, Manager.OuterColor.Color);
    OuterColor[3] := 0.0;

    with Manager.FBatch.Mesh do
    begin
      Lock;
      try
        Clear;
        DeclareAttribute(attrPosition, GLSLType3f);
        DeclareAttribute(attrColor, GLSLType4f);

        BeginAssembly(mpTRIANGLE_FAN);
        for i := n - 1 downto 0 do
        begin
          fp := PFireParticle(objList[i]);
          vt := fp^.Position;
          InnerColor[3] := fp^.Alpha * fp^.TimeToLive / Sqr(fp^.LifeLength);
          Attribute4f(attrColor, InnerColor);
          Attribute3f(attrPosition, vt[0], vt[1], vt[2]);
          EmitVertex;
          Attribute4f(attrColor, OuterColor);
          Attribute3f(attrPosition, vt[0]-vx[0], vt[1]-vx[1], vt[2]-vx[2]);
          EmitVertex;
          Attribute3f(attrPosition,
            vt[0]-0.5 * vx[0] + Manager.FireEvaporation * +vy[0],
            vt[1]-0.5 * vx[1] + Manager.FireEvaporation * vy[1],
            vt[2]-0.5 * vx[2] + Manager.FireEvaporation * vy[2]);
          EmitVertex;
          Attribute3f(attrPosition,
            vt[0]+0.5 * vx[0] + Manager.FireEvaporation * vy[0],
            vt[1]+0.5 * vx[1] + Manager.FireEvaporation * vy[1],
            vt[2]+0.5 * vx[2] + Manager.FireEvaporation * vy[2]);
          EmitVertex;
          Attribute3f(attrPosition, vt[0]+vx[0], vt[1]+vx[1], vt[2]+vx[2]);
          EmitVertex;
          Attribute3f(attrPosition,
            vt[0]+0.5 * vx[0] - Manager.FireEvaporation * vy[0],
            vt[1]+0.5 * vx[1] - Manager.FireEvaporation * vy[1],
            vt[2]+0.5 * vx[2] - Manager.FireEvaporation * vy[2]);
          EmitVertex;
          Attribute3f(attrPosition,
            vt[0]-0.5 * vx[0] - Manager.FireEvaporation * vy[0],
            vt[1]-0.5 * vx[1] - Manager.FireEvaporation * vy[1],
            vt[2]-0.5 * vx[2] - Manager.FireEvaporation * vy[2]);
          EmitVertex;
          Attribute3f(attrPosition, vt[0]-vx[0], vt[1]-vx[1], vt[2]-vx[2]);
          EmitVertex;

          RestartStrip;
        end;
        EndAssembly;
      finally
        UnLock;
        Manager.FBatch.Changed := True;
      end;
    end;

    objList.Free;
    distList.Free;
  end;

  ARci.drawList.Add(@FManager.FBatch);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// class registrations
RegisterXCollectionItemClass(TGLBFireFX);

finalization

UnregisterXCollectionItemClass(TGLBFireFX);

end.
