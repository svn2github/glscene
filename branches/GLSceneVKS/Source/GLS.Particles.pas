//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
   Particle systems for GLS.Scene, based on replication of full-featured scene objects. 
  
}
unit GLS.Particles;

interface

{$I GLScene.inc}

uses
  System.Classes, System.SysUtils,

  GLS.Scene,
  GLS.VectorGeometry,
  GLS.OpenGLTokens,
  GLS.Context,
  GLS.Color,
  GLS.BaseClasses,
  GLS.RenderContextInfo,
  GLS.State;

type
  TVKParticleEvent = procedure(Sender: TObject; particle: TVKBaseSceneObject) of object;

  // TVKParticles
  //
  { Manager object of a particle system. 
   Particles in a TVKParticles system are described as normal scene objects,
   however their children are to be : 
    "particle template" : the first object (index=0), this one will be
    duplicated to create new particles, it does not receive progression
    events and is visible at design-time only.
    "live particle" : the other objects (index>0), this ones are rendered
    and receive progression events.
     TVKParticles may also maintain an internal, non-persistent
   ("freezed") set of objects : the allocated objects pool. Why ? Creating
   and freeing objects takes cpu-cycles, especially for the TComponent class,
   and GLScene objects are TComponent. To reduce this load (and at the expense
   of memory space), the particle systems can move "dead" particles to a pool
   instead of freeing them, and will pick in the pool instead of creating
   new objects when new particles are requested. To take advantage of this
   behaviour, you should set the ParticlePoolSize property to a non-null
   value and use the KillParticle function instead of "Free" to kill a
       particle. 
       All direct access to a TVKParticles children should be avoided. 
       For high-performance particle systems of basic particles, you should
       look into GLS.ParticleFX instead, TVKParticles being rather focused on
       complex particles.
  }
  TVKParticles = class(TVKImmaterialSceneObject)
  private
    { Private Declarations }
    FCubeSize: GLfloat;
    FEdgeColor: TVKColor;
    FVisibleAtRunTime: Boolean;
    particlePool: TList;
    FParticlePoolSize: Integer;
    FOnCreateParticle: TVKParticleEvent;
    FOnActivateParticle: TVKParticleEvent;
    FOnKillParticle: TVKParticleEvent;
    FOnDestroyParticle: TVKParticleEvent;
    FOnBeforeRenderParticles, FOnAfterRenderParticles: TDirectRenderEvent;

  protected
    { Protected Declarations }
    procedure SetCubeSize(const val: GLfloat);
    procedure SetEdgeColor(const val: TVKColor);
    procedure SetVisibleAtRunTime(const val: Boolean);
    procedure SetParticlePoolSize(val: Integer);

    procedure ClearParticlePool;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var ARci: TVKRenderContextInfo); override;
    procedure DoRender(var ARci: TVKRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    procedure DoProgress(const progressTime: TProgressTimes); override;

    { Request creation of a new particle. 
     Particle will be either created or retrieved from the particlePool. }
    function CreateParticle: TVKBaseSceneObject;
    { Kill given particle. 
     If particlePool is not full, particle will be sent to the pool,
     if not, it will be freed. }
    procedure KillParticle(aParticle: TVKBaseSceneObject);
    { Kill all particles. }
    procedure KillParticles;

  published
    { Published Declarations }
    property CubeSize: GLfloat read FCubeSize write SetCubeSize;
    property EdgeColor: TVKColor read FEdgeColor write SetEdgeColor;
    property VisibleAtRunTime: Boolean read FVisibleAtRunTime write SetVisibleAtRunTime default False;

    { Size of the particle pool (for storing killed particles). 
             Default size is zero, meaning the particlePool is disabled. }
    property ParticlePoolSize: Integer read FParticlePoolSize write SetParticlePoolSize default 0;

    { Fired a particle has been created as a template duplicate. 
       When the event is triggered, the particle has yet been added  to
       the scene. }
    property OnCreateParticle: TVKParticleEvent read FOnCreateParticle write FOnCreateParticle;
    { Fired when a particle will get in the "live" list. 
       The particle has just been "Assigned" with the template, may happen
       after a creation or a pick from the particle pool. }
    property OnActivateParticle: TVKParticleEvent read FOnActivateParticle write FOnActivateParticle;
    { Triggered when a particle is killed. 
             When the event is fired, the particle is still parented, after this
             event, the particle will either go to the pool or be destroyed if
             the pool is full. }
    property OnKillParticle: TVKParticleEvent read FOnKillParticle write FOnKillParticle;
    { Triggered just before destroying a particle. 
       The particle can be in the pool (ie. not parented). }
    property OnDestroyParticle: TVKParticleEvent read FOnDestroyParticle write FOnDestroyParticle;
    { Fired before rendering the first of the particles. }
    property OnBeforeRenderParticles: TDirectRenderEvent read FOnBeforeRenderParticles write FOnBeforeRenderParticles;
    { Fired after rendering the last of the particles. }
    property OnAfterRenderParticles: TDirectRenderEvent read FOnAfterRenderParticles write FOnAfterRenderParticles;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
//---------------------------------------------------------------------
//----------------- TVKParticles --------------------------------------
//---------------------------------------------------------------------
// Create
//

constructor TVKParticles.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FCubeSize := 1;
  FEdgeColor := TVKColor.Create(Self);
  FEdgeColor.Initialize(clrWhite);
  particlePool := TList.Create;
end;

// Destroy
//

destructor TVKParticles.Destroy;
begin
  FEdgeColor.Free;
  ClearParticlePool;
  particlePool.Free;
  inherited;
end;

// Assign
//

procedure TVKParticles.Assign(Source: TPersistent);
begin
  if Source is TVKParticles then
  begin
    FCubeSize := TVKParticles(Source).FCubeSize;
    FEdgeColor.Color := TVKParticles(Source).FEdgeColor.Color;
    FVisibleAtRunTime := TVKParticles(Source).FVisibleAtRunTime;
    ClearParticlePool;
    FParticlePoolSize := TVKParticles(Source).FParticlePoolSize;
    FOnCreateParticle := TVKParticles(Source).FOnCreateParticle;
    FOnActivateParticle := TVKParticles(Source).FOnActivateParticle;
    FOnKillParticle := TVKParticles(Source).FOnKillParticle;
    FOnDestroyParticle := TVKParticles(Source).FOnDestroyParticle;
  end;
  inherited Assign(Source);
end;

// ClearParticlePool
//

procedure TVKParticles.ClearParticlePool;
var
  particle: TVKBaseSceneObject;
  i: Integer;
begin
  if Assigned(FOnDestroyParticle) then
  begin
    for i := 0 to particlePool.Count - 1 do
    begin
      particle := TVKBaseSceneObject(particlePool[i]);
      FOnDestroyParticle(Self, particle);
      particle.Free;
    end;
  end
  else
    for i := 0 to particlePool.Count - 1 do
      TVKBaseSceneObject(particlePool[i]).Free;
  particlePool.Clear;
end;

// BuildList
//

procedure TVKParticles.BuildList(var ARci: TVKRenderContextInfo);
var
  mi, ma: Single;
begin
  ARci.GLStates.Disable(stLighting);
  ARci.GLStates.Enable(stLineStipple);
  ARci.GLStates.Enable(stLineSmooth);
  ARci.GLStates.Enable(stBlend);
  ARci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
  ARci.GLStates.LineWidth := 1;
  ARci.GLStates.LineStippleFactor := 1;
  ARci.GLStates.LineStipplePattern := $AAAA;
  ma := FCubeSize * 0.5;
  mi := -ma;
  with EdgeColor do
    glColor3f(Color.V[0], Color.V[1], Color.V[2]);
  glBegin(GL_LINE_STRIP);
  // front face
  glVertex3f(ma, mi, mi);
  glVertex3f(ma, ma, mi);
  glVertex3f(ma, ma, ma);
  glVertex3f(ma, mi, ma);
  glVertex3f(ma, mi, mi);
  // partial up back fac
  glVertex3f(mi, mi, mi);
  glVertex3f(mi, mi, ma);
  glVertex3f(mi, ma, ma);
  glVertex3f(mi, ma, mi);
  // right side low
  glVertex3f(ma, ma, mi);
  glEnd;
  glBegin(GL_LINES);
  // right high
  glVertex3f(ma, ma, ma);
  glVertex3f(mi, ma, ma);
  // back low
  glVertex3f(mi, mi, mi);
  glVertex3f(mi, ma, mi);
  // left high
  glVertex3f(ma, mi, ma);
  glVertex3f(mi, mi, ma);
  glEnd;
end;

// DoRender
//

procedure TVKParticles.DoRender(var ARci: TVKRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
begin
  if (csDesigning in ComponentState) or (FVisibleAtRunTime) then
    BuildList(ARci);
  if Assigned(FOnBeforeRenderParticles) then
    FOnBeforeRenderParticles(Self, ARci);
  if csDesigning in ComponentState then
  begin
    // design-time, everything is visible for user convenience
    if Count > 0 then
      Self.RenderChildren(0, Count - 1, ARci);
  end
  else
  begin
    // run-time, template is NOT visible
    if Count > 1 then
      Self.RenderChildren(1, Count - 1, ARci);
  end;
  if Assigned(FOnAfterRenderParticles) then
    FOnAfterRenderParticles(Self, ARci);
end;

// DoProgress
//

procedure TVKParticles.DoProgress(const progressTime: TProgressTimes);
var
  i: Integer;
begin
  for i := Count - 1 downto 1 do
    Children[i].DoProgress(progressTime);
  Behaviours.DoProgress(progressTime);
  if Assigned(OnProgress) then
    with progressTime do
      OnProgress(Self, deltaTime, newTime);
end;

// SetCubeSize
//

procedure TVKParticles.SetCubeSize(const val: GLfloat);
begin
  if val <> FCubeSize then
  begin
    FCubeSize := val;
    StructureChanged;
  end;
end;

// SetEdgeColor
//

procedure TVKParticles.SetEdgeColor(const val: TVKColor);
begin
  if val <> FEdgeColor then
  begin
    FEdgeColor.Assign(val);
    StructureChanged;
  end;
end;

// SetVisibleAtRunTime
//

procedure TVKParticles.SetVisibleAtRunTime(const val: Boolean);
begin
  if val <> FVisibleAtRunTime then
  begin
    FVisibleAtRunTime := val;
    StructureChanged;
  end;
end;

// SetParticlePoolSize
//

procedure TVKParticles.SetParticlePoolSize(val: Integer);
var
  particle: TVKBaseSceneObject;
begin
  if val < 0 then
    val := 0;
  if FParticlePoolSize <> val then
  begin
    FParticlePoolSize := val;
    with particlePool do
      while Count > val do
      begin
        particle := TVKBaseSceneObject(Items[Count - 1]);
        if Assigned(FOnDestroyParticle) then
          FOnDestroyParticle(Self, particle);
        particle.Free;
        Delete(Count - 1);
      end;
  end;
end;

// CreateParticle
//

function TVKParticles.CreateParticle: TVKBaseSceneObject;
begin
  if Count > 0 then
  begin
    if particlePool.Count > 0 then
    begin
      Result := TVKBaseSceneObject(particlePool[particlePool.Count - 1]);
      particlePool.Delete(particlePool.Count - 1);
      Result.Assign(Children[0]);
    end
    else
    begin
      Result := TVKSceneObjectClass(Children[0].ClassType).Create(Self);
      Result.Assign(Children[0]);
      if Assigned(FOnCreateParticle) then
        FOnCreateParticle(Self, Result);
    end;
    AddChild(Result);
    if Assigned(FOnActivateParticle) then
      FOnActivateParticle(Self, Result);
  end
  else
    Result := nil;
end;

// KillParticle
//

procedure TVKParticles.KillParticle(aParticle: TVKBaseSceneObject);
begin
  Assert(aParticle.Parent = Self, 'KillParticle : particle is not mine !');
  if Assigned(FOnKillParticle) then
    FOnKillParticle(Self, aParticle);
  if particlePool.Count < FParticlePoolSize then
  begin
    Remove(aParticle, False);
    particlePool.Add(aParticle)
  end
  else
  begin
    if Assigned(FOnDestroyParticle) then
      FOnDestroyParticle(Self, aParticle);
    aParticle.Free;
  end;
end;

// KillParticles
//

procedure TVKParticles.KillParticles;
begin
  while Count > 1 do
    KillParticle(Children[Count - 1]);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  RegisterClass(TVKParticles);

end.

