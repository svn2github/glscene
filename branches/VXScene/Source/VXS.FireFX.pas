//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
 Fire special effect 
}

unit VXS.FireFX;

interface

{$I VXScene.inc}

uses
  System.Classes, System.SysUtils,

  VXS.Scene, VXS.XCollection, VXS.VectorGeometry,
  Winapi.OpenGL, Winapi.OpenGLext,  VXS.Context, VXS.VectorLists, VXS.VectorTypes,
  VXS.Cadencer, VXS.Color, VXS.BaseClasses, VXS.Coordinates,
  VXS.Manager, VXS.RenderContextInfo, VXS.State, VXS.TextureFormat;

type

  PFireParticle = ^TFireParticle;
  TFireParticle = record
    Position: TVector;
    Speed: TVector;
    Alpha: Single;
    TimeToLive, LifeLength: Single;
  end;
  TFireParticleArray = array[0..MAXINT shr 6] of TFireParticle;
  PFireParticleArray = ^TFireParticleArray;

  TVXBFireFX = class;

  // TVXFireFXManager
  //
    { Fire special effect manager. 
       Defines the looks and behaviour of a particle system that can be made
       to look fire-like. }
  TVXFireFXManager = class(TVXCadenceAbleComponent)
  private
    
    FClients: TList;
    FFireParticles: PFireParticleArray;
    FFireDir, FInitialDir: TVXCoordinates;
    FCadencer: TVXCadencer;
    FMaxParticles, FParticleLife: Integer;
    FParticleSize, FFireDensity, FFireEvaporation: Single;
    FFireCrown, FParticleInterval, IntervalDelta: Single;
    NP: Integer;
    FInnerColor, FOuterColor: TVXColor;
    FFireBurst, FFireRadius: Single;
    FDisabled, FPaused, FUseInterval: Boolean;
    FReference: TVXBaseSceneObject;
    FNoZWrite: Boolean;

  protected
    
    procedure RegisterClient(aClient: TVXBFireFX);
    procedure DeRegisterClient(aClient: TVXBFireFX);
    procedure DeRegisterAllClients;

    procedure SetFireDir(const val: TVXCoordinates);
    procedure SetInitialDir(const val: TVXCoordinates);
    procedure SetCadencer(const val: TVXCadencer);
    function StoreParticleSize: Boolean;
    procedure SetInnerColor(const val: TVXcolor);
    procedure SetOuterColor(const val: TVXcolor);
    procedure SetReference(const val: TVXBaseSceneObject);
    procedure SetMaxParticles(const val: Integer);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure CalcFire(deltaTime: Double; ParticleInterval, ParticleLife: Single;
      FireAlpha: Single);
    procedure AffParticle3d(Color2: TColorVector; const mat: TMatrix);

  public
    
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Reinitializes the fire. }
    procedure FireInit;

    { Spawns a large quantity of particles to simulate an isotropic explosion. 
       This method generates an isotropic explosion, i.e. there is no
       privilegied direction in the initial vector. }
    procedure IsotropicExplosion(minInitialSpeed, maxInitialSpeed, lifeBoostFactor: Single;
      nbParticles: Integer = -1);
    { Spawns a large quantity of particles to simulate a ring explosion. 
       This method generates a ring explosion. The plane of the ring is described
       by ringVectorX/Y, which should be of unit length (but you may not
       make them of unit length if you want "elliptic" rings). }
    procedure RingExplosion(minInitialSpeed, maxInitialSpeed, lifeBoostFactor: Single;
      const ringVectorX, ringVectorY: TAffineVector;
      nbParticles: Integer = -1);

    { Current Nb of particles. }
    property ParticleCount: Integer read NP;

    procedure DoProgress(const progressTime: TProgressTimes); override;

  published
    
          { Adjusts the acceleration direction (abs coordinates). }
    property FireDir: TVXCoordinates read FFireDir write SetFireDir;
    { Adjusts the initial direction (abs coordinates). }
    property InitialDir: TVXCoordinates read FInitialDir write SetInitialDir;
    { The cadencer that will "drive" the animation of the system. }
    property Cadencer: TVXCadencer read FCadencer write SetCadencer;
    { Maximum number of simultaneous particles in the system. }
    property MaxParticles: Integer read FMaxParticles write SetMaxParticles default 256;
    { Size of the particle, in absolute units. }
    property ParticleSize: Single read FParticleSize write FParticleSize stored StoreParticleSize;
    { Inner color of a particle. }
    property InnerColor: TVXcolor read FInnerColor write SetInnerColor;
    { Outer color of a particle. }
    property OuterColor: TVXcolor read FOuterColor write SetOuterColor; // default clrWhite;
    property FireDensity: Single read FFireDensity write FFireDensity;
    property FireEvaporation: Single read FFireEvaporation write FFireEvaporation;
    { Adjust a crown (circular) radius on which particles are spawned. 
       With a value of zero, the particles are spawned in the FireRadius
       cube around the origin, with a non zero value, they appear in
       a torus of major radius FireCrown, and minor radius FireRadius*1.73. }
    property FireCrown: Single read FFireCrown write FFireCrown;
    { Life length of particle. }
    property ParticleLife: Integer read FParticleLife write FParticleLife default 3;
    property FireBurst: Single read FFireBurst write FFireBurst;
    { Adjusts the random birth radius for particles (actually a birth cube). }
    property FireRadius: Single read FFireRadius write FFireRadius;
    { If true, no new particles are spawn. 
       But current ones continue to live and die. }
    property Disabled: Boolean read FDisabled write FDisabled;
    { When paused, the fire animation is freezed. }
    property Paused: Boolean read FPaused write FPaused;
    { Interval between particles births (in sec). 
       The interval may not be honoured if MawParticles is reached. }
    property ParticleInterval: Single read FParticleInterval write FParticleInterval;
    { Enable/disable use of ParticleInterval. 
       If true ParticleInterval is used, if False, the system will attempt
       to maintain a particle count of MaxParticles, by spawning new
       particles to replace the dead ones ASAP. }
    property UseInterval: Boolean read FUseInterval write FUseInterval;
    { Particle's render won't write to Z-Buffer }
    property NoZWrite: Boolean read FNoZWrite write FNoZWrite default True;

    { Specifies an optional object whose position to use as reference. 
       This property allows switching between static/shared fires (for
       fireplaces or static torches) and dynamic fire trails. 
       The absolute position of the reference object is 'central' spawning
       point for new particles, usually, the object will be the one and only
       one on which the effect is applied. }
    property Reference: TVXBaseSceneObject read FReference write SetReference;
  end;

  // TVXBFireFX
//
{ Fire special effect. 
     This effect works as a client of TFireFXManager }
  TVXBFireFX = class(TVXObjectPostEffect)
  private
    
    FManager: TVXFireFXManager;
    FManagerName: string; // NOT persistent, temporarily used for persistence

  protected
    
    procedure SetManager(const val: TVXFireFXManager);

    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;

  public
    
    constructor Create(aOwner: TVXXCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;

    procedure Render(var rci: TVXRenderContextInfo); override;

  published
    
          { Refers the collision manager. }
    property Manager: TVXFireFXManager read FManager write SetManager;
  end;

  { Returns or creates the TVXBFireFX within the given behaviours. 
   This helper function is convenient way to access a TVXBFireFX. }
function GetOrCreateFireFX(effects: TVXObjectEffects): TVXBFireFX; overload;
{ Returns or creates the TVXBFireFX within the given object's behaviours. 
 This helper function is convenient way to access a TVXBFireFX. }
function GetOrCreateFireFX(obj: TVXBaseSceneObject): TVXBFireFX; overload;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// GetOrCreateFireFX (TVXObjectEffects)
//

function GetOrCreateFireFX(effects: TVXObjectEffects): TVXBFireFX;
var
  i: Integer;
begin
  i := effects.IndexOfClass(TVXBFireFX);
  if i >= 0 then
    Result := TVXBFireFX(effects[i])
  else
    Result := TVXBFireFX.Create(effects);
end;

// GetOrCreateFireFX (TVXBaseSceneObject)
//

function GetOrCreateFireFX(obj: TVXBaseSceneObject): TVXBFireFX;
begin
  Result := GetOrCreateFireFX(obj.Effects);
end;

// ------------------
// ------------------ TVXFireFXManager ------------------
// ------------------

// Create
//

constructor TVXFireFXManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClients := TList.Create;
  RegisterManager(Self);
  FFireDir := TVXCoordinates.CreateInitialized(Self, VectorMake(0, 0.5, 0), csPoint);
  FInitialDir := TVXCoordinates.CreateInitialized(Self, YHmgVector, csPoint);
  FMaxParticles := 256;
  FParticleSize := 1.0;
  FInnerColor := TVXColor.Create(Self);
  FInnerColor.Initialize(clrYellow);
  FOuterColor := TVXColor.Create(Self);
  FOuterColor.Initialize(clrOrange);
  FFireDensity := 1;
  FFireEvaporation := 0.86;
  FFireCrown := 0;
  FParticleLife := 3;
  FFireBurst := 0;
  FFireRadius := 1;
  FParticleInterval := 0.1;
  FDisabled := false;
  Fpaused := false;
  FUseInterval := True;
  FNoZWrite := True;
  IntervalDelta := 0;
  FireInit;
end;

// Destroy
//

destructor TVXFireFXManager.Destroy;
begin
  DeRegisterAllClients;
  DeRegisterManager(Self);
  FreeMem(FFireParticles);
  FInnerColor.Free;
  FOuterColor.Free;
  FClients.Free;
  FFireDir.Free;
  FInitialDir.Free;
  inherited Destroy;
end;

// RegisterClient
//

procedure TVXFireFXManager.RegisterClient(aClient: TVXBFireFX);
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

procedure TVXFireFXManager.DeRegisterClient(aClient: TVXBFireFX);
begin
  if Assigned(aClient) then
  begin
    aClient.FManager := nil;
    FClients.Remove(aClient);
  end;
end;

// DeRegisterAllClients
//

procedure TVXFireFXManager.DeRegisterAllClients;
var
  i: Integer;
begin
  // Fast deregistration
  for i := 0 to FClients.Count - 1 do
    TVXBFireFX(FClients[i]).FManager := nil;
  FClients.Clear;
end;

// SetFireDir
//

procedure TVXFireFXManager.SetFireDir(const val: TVXCoordinates);
begin
  FFireDir.Assign(val);
end;

// SetInitialDir
//

procedure TVXFireFXManager.SetInitialDir(const val: TVXCoordinates);
begin
  FInitialDir.Assign(val);
end;

// SetCadencer
//

procedure TVXFireFXManager.SetCadencer(const val: TVXCadencer);
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

function TVXFireFXManager.StoreParticleSize: Boolean;
begin
  Result := (FParticleSize <> 1);
end;

// SetInnerColor
//

procedure TVXFireFXManager.SetInnerColor(const val: TVXcolor);
begin
  if FInnerColor <> val then
  begin
    FInnerColor.color := val.color;
    FireInit;
  end;
end;

// SetOuterColor
//

procedure TVXFireFXManager.SetOuterColor(const val: TVXcolor);
begin
  if FOuterColor <> val then
  begin
    FOuterColor.color := val.color;
    FireInit;
  end;
end;

// SetReference
//

procedure TVXFireFXManager.SetReference(const val: TVXBaseSceneObject);
begin
  // nothing more yet, maybe later
  FReference := val;
end;

// SetMaxParticles
//

procedure TVXFireFXManager.SetMaxParticles(const val: Integer);
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

// Notification
//

procedure TVXFireFXManager.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TVXFireFXManager.DoProgress(const progressTime: TProgressTimes);
var
  i: Integer;
begin
  // Progress the particles
  if (not FPaused) and (FParticleInterval > 0) then
    CalcFire(progressTime.deltaTime * (1.0 + Abs(FFireBurst)),
      FParticleInterval, FParticleLife, FFireDensity);

  // Invalidate all clients
  for i := 0 to FClients.Count - 1 do
    TVXBFireFX(FClients[i]).OwnerBaseSceneObject.NotifyChange(TVXBFireFX(FClients[i]));
end;

// FireInit
//

procedure TVXFireFXManager.FireInit;
begin
  IntervalDelta := 0;
  NP := 0;
  ReallocMem(FFireParticles, FMaxParticles * Sizeof(TFireParticle));
end;

// IsotropicExplosion
//

procedure TVXFireFXManager.IsotropicExplosion(minInitialSpeed, maxInitialSpeed, lifeBoostFactor: Single;
  nbParticles: Integer = -1);
var
  n: Integer;
  tmp, refPos: TVector;
begin
  if nbParticles < 0 then
    n := MaxInt
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
    ScaleVector(tmp, minInitialSpeed + Random * (maxInitialSpeed - minInitialSpeed));
    with FFireParticles^[NP] do
    begin
      Position := VectorAdd(refPos, VectorMake((2 * Random - 1) * FireRadius, (2 * Random - 1) * FireRadius, (2 * Random - 1) * FireRadius));
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

procedure TVXFireFXManager.RingExplosion(minInitialSpeed, maxInitialSpeed, lifeBoostFactor: Single;
  const ringVectorX, ringVectorY: TAffineVector;
  nbParticles: Integer = -1);
var
  n: Integer;
  tmp, refPos: TVector;
  fx, fy, d: Single;
begin
  if nbParticles < 0 then
    n := MaxInt
  else
    n := nbParticles;
  if Assigned(Reference) then
    refPos := Reference.AbsolutePosition
  else
    refPos := NullHmgPoint;
  while (NP < MaxParticles) and (n > 0) do
  begin
    // okay, ain't exactly and "isotropic" ring...
    fx := Random - 0.5;
    fy := Random - 0.5;
    d := RSqrt(Sqr(fx) + Sqr(fy));
    PAffineVector(@tmp)^ := VectorCombine(ringVectorX, ringVectorY, fx * d, fy * d);
    tmp.W := 1;
    ScaleVector(tmp, minInitialSpeed + Random * (maxInitialSpeed - minInitialSpeed));
    with FFireParticles^[NP] do
    begin
      Position := VectorAdd(refPos, VectorMake((2 * Random - 1) * FireRadius, (2 * Random - 1) * FireRadius, (2 * Random - 1) * FireRadius));
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

procedure TVXFireFXManager.CalcFire(deltaTime: Double;
  particleInterval, particleLife: Single; fireAlpha: Single);
var
  N, I: Integer;
  Fdelta: Single;
  tmp, refPos: TVector;
begin
  // Process live stuff
  N := 0;
  I := 0;
  while N < NP do
  begin
    FFireParticles^[I].TimeToLive := FFireParticles^[I].TimeToLive - deltaTime;
    if (FFireParticles^[I].TimeToLive <= 0) then
    begin
      //Get the prev element
      Dec(NP);
      FFireParticles^[I] := FFireParticles^[NP];
    end
    else
    begin
      //animate it
      with FFireParticles^[I] do
      begin
        Speed := VectorCombine(Speed, FireDir.AsVector, 1, deltaTime);
        Position := VectorCombine(Position, Speed, 1, deltaTime);
      end;
      Inc(N);
      Inc(I);
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
    fDelta := Frac(IntervalDelta);
    while (NP < MaxParticles) do
    begin
      SetVector(tmp, (2 * Random - 1) * FireRadius, (2 * Random - 1) * FireRadius,
        FireCrown + (2 * Random - 1) * FireRadius);
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
    IntervalDelta := fDelta;
  end;
end;

// AffParticle3d
//

procedure TVXFireFXManager.AffParticle3d(Color2: TColorVector; const mat: TMatrix);
var
  vx, vy: TVector;
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    vx.V[i] := mat.V[i].X * FParticleSize;
    vy.V[i] := mat.V[i].Y * FParticleSize;
  end;
  begin
    glBegin(GL_TRIANGLE_FAN);
    glVertex3fv(@NullVector);
    glColor4f(Color2.X, Color2.Y, Color2.Z, 0.0);
    glVertex3f(-vx.X, -vx.Y, -vx.Z);
    // those things should be composited in the model view matrix
    glVertex3f(-0.5 * vx.X + FFireEvaporation * vy.X,
      -0.5 * vx.Y + FFireEvaporation * vy.Y,
      -0.5 * vx.Z + FFireEvaporation * vy.Z);
    glVertex3f(+0.5 * vx.X + FFireEvaporation * vy.X,
      +0.5 * vx.Y + FFireEvaporation * vy.Y,
      +0.5 * vx.Z + FFireEvaporation * vy.Z);
    glVertex3f(+vx.X, +vx.Y, +vx.Z);
    glVertex3f(+0.5 * vx.X - FFireEvaporation * vy.X,
      +0.5 * vx.Y - FFireEvaporation * vy.Y,
      +0.5 * vx.Z - FFireEvaporation * vy.Z);
    glVertex3f(-0.5 * vx.X - FFireEvaporation * vy.X,
      -0.5 * vx.Y - FFireEvaporation * vy.Y,
      -0.5 * vx.Z - FFireEvaporation * vy.Z);
    glVertex3f(-vx.X, -vx.Y, -vx.Z);
    glEnd;
  end;
end;

// ------------------
// ------------------ TVXBFireFX ------------------
// ------------------

// Create
//

constructor TVXBFireFX.Create(aOwner: TVXXCollection);
begin
  inherited Create(aOwner);
end;

// Destroy
//

destructor TVXBFireFX.Destroy;
begin
  Manager := nil;
  inherited Destroy;
end;

// FriendlyName
//

class function TVXBFireFX.FriendlyName: string;
begin
  Result := 'FireFX';
end;

// FriendlyDescription
//

class function TVXBFireFX.FriendlyDescription: string;
begin
  Result := 'Fire FX';
end;

// WriteToFiler
//

procedure TVXBFireFX.WriteToFiler(writer: TWriter);
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

procedure TVXBFireFX.ReadFromFiler(reader: TReader);
var
   archiveVersion : Integer;
begin
  with reader do
  begin
    archiveVersion := ReadInteger;
    Assert(archiveVersion in [0..1]);
    if archiveVersion >= 1 then
      inherited;
    FManagerName := ReadString;
    Manager := nil;
  end;
end;

// Loaded
//

procedure TVXBFireFX.Loaded;
var
  mng: TComponent;
begin
  inherited;
  if FManagerName <> '' then
  begin
    mng := FindManager(TVXFireFXManager, FManagerName);
    if Assigned(mng) then
      Manager := TVXFireFXManager(mng);
    FManagerName := '';
  end;
end;

// Assign
//

procedure TVXBFireFX.Assign(Source: TPersistent);
begin
  if Source is TVXBFireFX then
  begin
    Manager := TVXBFireFX(Source).Manager;
  end;
  inherited Assign(Source);
end;

// SetManager
//

procedure TVXBFireFX.SetManager(const val: TVXFireFXManager);
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

procedure TVXBFireFX.Render(var rci: TVXRenderContextInfo);
var
  n: Integer;
  i: Integer;
  innerColor: TVector;
  lastTr: TAffineVector;
  distList: TSingleList;
  objList: TList;
  fp: PFireParticle;
begin
  if Manager = nil then
    Exit;

  rci.PipelineTransformation.Push;
  // revert to the base model matrix in the case of a referenced fire
  if Assigned(Manager.Reference) then
    rci.PipelineTransformation.ModelMatrix := IdentityHmgMatrix;

  rci.VKStates.CurrentProgram := 0;
  rci.VKStates.Disable(stCullFace);
  rci.VKStates.ActiveTextureEnabled[ttTexture2D] := False;
  rci.VKStates.Disable(stLighting);
  rci.VKStates.SetBlendFunc(bfSrcAlpha, bfOne);
  rci.VKStates.Enable(stBlend);
  rci.VKStates.Disable(stAlphaTest);
  rci.VKStates.Enable(stDepthTest);
  rci.VKStates.DepthFunc := cfLEqual;
  rci.VKStates.DepthWriteMask := not GLboolean(Manager.NoZWrite);

  n := Manager.NP;

  if n > 1 then
  begin
    distList := TSingleList.Create;
    objList := TList.Create;
    for i := 0 to n - 1 do
    begin
      fp := @(Manager.FFireParticles[i]);
      distList.Add(VectorDotProduct(rci.cameraDirection, fp^.Position));
      objList.Add(fp);
    end;
    QuickSortLists(0, N - 1, distList, objList);

      lastTr := NullVector;
      SetVector(innerColor, Manager.FInnerColor.Color);
      for i := n - 1 downto 0 do
      begin
        fp := PFireParticle(objList[i]);
        glTranslatef(fp^.Position.X - lastTr.X,
                      fp^.Position.Y - lastTr.Y,
                      fp^.Position.Z - lastTr.Z);
        SetVector(lastTr, fp^.Position);
        innerColor.W := fp^.Alpha * fp^.TimeToLive / Sqr(fp^.LifeLength);
        glColor4fv(@innerColor);
        Manager.AffParticle3d(Manager.FOuterColor.Color, rci.PipelineTransformation.ViewMatrix);
      end;

    objList.Free;
    distList.Free;
  end;

  rci.PipelineTransformation.Pop;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

   // class registrations
  RegisterXCollectionItemClass(TVXBFireFX);

finalization

  UnregisterXCollectionItemClass(TVXBFireFX);

end.

