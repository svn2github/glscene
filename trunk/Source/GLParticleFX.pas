{: GLParticleFX<p>

   Base classes for scene-wide blended particles FX.<p>

   These provide a mechanism to render heterogenous particles systems with per
   particle depth-sorting (allowing correct rendering of interwoven separate
   fire and smoke particle systems for instance).<p>

   <b>Historique : </b><font size=-1><ul>
      <li>22/01/02 - EG - Another RenderParticle color lerp fix (GliGli)
      <li>20/01/02 - EG - Several optimization (35% faster on Volcano bench)
      <li>18/01/02 - EG - RenderParticle color lerp fix (GliGli)
      <li>08/09/01 - EG - Creation (GLParticleFX.omm)
   </ul></font>
}
unit GLParticleFX;

interface

uses Classes, PersistentClasses, GLScene, Geometry, XCollection, GLTexture,
     GLCadencer, GLMisc, VectorLists;

type

   TGLParticleList = class;
   TGLParticleFXManager = class;

   // TGLParticle
   //
   {: Base class for particles.<p>
      The class implements properties for position, velocity and time, whatever
      you need in excess of that will have to be placed in subclasses (this
      class should remain as compact as possible). }
   TGLParticle = class (TPersistentObject)
      private
         { Private Declarations }
         FOwner : TGLParticleList;  // NOT persistent
         FPosition : TAffineVector;
         FVelocity : TAffineVector;
         FCreationTime : Double;

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;
         procedure WriteToFiler(writer : TVirtualWriter); override;
         procedure ReadFromFiler(reader : TVirtualReader); override;

         {: Refers owner list }
         property Owner : TGLParticleList read FOwner write FOwner;
         {: Particle's absolute position.<p>
            Note that this property is read-accessed directly at rendering time
            in the innards of the depth-sorting code. }
         property Position : TAffineVector read FPosition write FPosition;
         {: Particle's velocity.<p>
            This velocity is indicative and is NOT automatically applied
            to the position during progression events by this class (subclasses
            may implement that). }
         property Velocity : TAffineVector read FVelocity write FVelocity;
         {: Time at which particle was created }
         property CreationTime : Double read FCreationTime write FCreationTime;

   end;

   TGLParticleArray = array [0..MaxInt shr 4] of TGLParticle;
   PGLParticleArray = ^TGLParticleArray;

   // TGLParticleList
   //
   {: List of particles.<p>
      This list is managed with particles and performance in mind, make sure to
      check methods doc. }
   TGLParticleList = class (TPersistentObject)
      private
         { Private Declarations }
         FOwner : TGLParticleFXManager;  // NOT persistent
         FItemList : TPersistentObjectList;
         FDirectList : PGLParticleArray; // NOT persistent

      protected
         { Protected Declarations }
         function GetItems(index : Integer) : TGLParticle;
         procedure SetItems(index : Integer; val : TGLParticle);
         procedure AfterItemCreated(Sender : TObject);

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;
         procedure WriteToFiler(writer : TVirtualWriter); override;
         procedure ReadFromFiler(reader : TVirtualReader); override;

         {: Refers owner manager }
         property Owner : TGLParticleFXManager read FOwner write FOwner;
         property Items[index : Integer] : TGLParticle read GetItems write SetItems; default;

         function ItemCount : Integer;
         {: Adds a particle to the list.<p>
            Particle owneship is defined blindly, if the particle was previously
            in another list, it won't be automatically removed from that list. }
         function AddItem(aItem : TGLParticle) : Integer;
         {: Removes and frees a particular item for the list.<p>
            If the item is not part of the list, nothing is done.<br>
            If found in the list, the item's "slot" is set to nil and item is
            freed (after setting its ownership to nil). The nils can be removed
            with a call to Pack. }
         procedure RemoveAndFreeItem(aItem : TGLParticle);
         function IndexOfItem(aItem : TGLParticle) : Integer;
         {: Packs the list by removing all "nil" items.<p>
            Note: this functions is orders of magnitude faster than the TList
            version. }
         procedure Pack;

         property List : PGLParticleArray read FDirectList;
   end;

   TGLParticleFXRenderer = class;

   // TGLParticleFXManager
   //
   {: Base class for particle FX managers.<p>
      Managers take care of life and death of particles for a particular
      particles FX system. You can have multiple scene-wide particle
      FX managers in a scene, handled by the same ParticleFxRenderer.<p>
      Before subclassing, make sure you understood how the Initialize/Finalize
      Rendering, Begin/End Particles and RenderParticles methods (and also
      understood that rendering of manager's particles may be interwoven). }
   TGLParticleFXManager = class (TGLUpdateAbleComponent)
      private
         { Private Declarations }
         FRenderer : TGLParticleFXRenderer;
         FParticles : TGLParticleList;
         FCadencer : TGLCadencer;

      protected
         { Protected Declarations }
         procedure SetRenderer(const val : TGLParticleFXRenderer);
         procedure SetParticles(const aParticles : TGLParticleList);
         procedure SetCadencer(const val : TGLCadencer);

         {: Invoked when the particles of the manager will be rendered.<p>
            This method is fired with the "base" OpenGL states and matrices
            that will be used throughout the whole rendering, per-frame
            initialization should take place here.<br>
            OpenGL states/matrices should not be altered in any way here. }
         procedure InitializeRendering; dynamic; abstract;
         {: Triggered just before rendering a set of particles.<p>
            The current OpenGL state should be assumed to be the "base" one as
            was found during InitializeRendering. Manager-specific states should
            be established here.<br>
            Multiple BeginParticles can occur during a render (but all will be
            between InitializeRendering and Finalizerendering, and at least one
            particle will be rendered before EndParticles is invoked). }
         procedure BeginParticles; virtual; abstract;
         {: Request to render a particular particle.<p>
            Due to the nature of the rendering, no particular order should be
            assumed. If possible, no OpenGL state changes should be made in this
            method, but should be placed in Begin/EndParticles. }
         procedure RenderParticle(aParticle : TGLParticle); virtual; abstract;
         {: Triggered after a set of particles as been rendered.<p>
            If OpenGL state were altered directly (ie. not through the states
            caches of GLMisc), it should be restored back to the "base" state. }
         procedure EndParticles; virtual; abstract;
         {: Invoked when rendering of particles for this manager is done. }
         procedure FinalizeRendering; dynamic; abstract;

      public
         { Public Declarations }
         constructor Create(aOwner : TComponent); override;
         destructor Destroy; override;

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
			procedure NotifyChange(Sender : TObject); override;

         {: Creates a new particle controled by the manager. }
         function CreateParticle : TGLParticle; virtual;

         {: A TGLParticleList property. }
         property Particles : TGLParticleList read FParticles write SetParticles;

		published
			{ Published Declarations }
         property Cadencer : TGLCadencer read FCadencer write SetCadencer;
         {: References the renderer.<p>
            The renderer takes care of ordering the particles of the manager
            (and other managers linked to it) and rendering them all depth-sorted. }
         property Renderer : TGLParticleFXRenderer read FRenderer write SetRenderer;
   end;

   // TGLParticleFXEffect
   //
   {: Base class for linking scene objects to a particle FX manager.<p> }
   TGLParticleFXEffect = class (TGLObjectPostEffect)
      private
         { Private Declarations }
         FManager : TGLParticleFXManager;
         FManagerName : String; // NOT persistent, temporarily used for persistence

      protected
         { Protected Declarations }
         procedure SetManager(val : TGLParticleFXManager);

			procedure WriteToFiler(writer : TWriter); override;
         procedure ReadFromFiler(reader : TReader); override;
         procedure Loaded; override;
         
      public
         { Public Declarations }
         constructor Create(aOwner : TXCollection); override;
         destructor Destroy; override;

		published
			{ Published Declarations }
         {: Reference to the Particle FX manager }
         property Manager : TGLParticleFXManager read FManager write SetManager;

   end;

   // TGLParticleFXRenderer
   //
   {: Rendering interface for scene-wide particle FX.<p>
      A renderer can take care of rendering any number of particle systems,
      its main task being to depth-sort the particles so that they are blended
      appropriately.<br>
      This object will usually be placed at the end of the scene hierarchy,
      just before the HUD overlays, its position, rotation etc. is of no
      importance and has no effect on the rendering of the particles. }
   TGLParticleFXRenderer = class (TGLBaseSceneObject)
      private
         { Private Declarations }
         FManagerList : TList;
         FLastSortTime : Double;

      protected
         { Protected Declarations }
         {: Register a manager }
         procedure RegisterManager(aManager : TGLParticleFXManager);
         {: UnRegister a manager }
         procedure UnRegisterManager(aManager : TGLParticleFXManager);

         procedure UnRegisterAll;

      public
         { Public Declarations }
         constructor Create(aOwner : TComponent); override;
         destructor Destroy; override;

         procedure BuildList(var rci : TRenderContextInfo); override;

         {: Time (in msec) spent sorting the particles for last render. }
         property LastSortTime : Double read FLastSortTime;
   end;

   // TGLSourcePFXVelocityMode
   //
   TGLSourcePFXVelocityMode = (svmAbsolute, svmRelative);

   // TGLSourcePFXDispersionMode
   //
   TGLSourcePFXDispersionMode = (sdmFast, sdmIsotropic);

   // TGLSourcePFXEffect
   //
   {: Simple Particles Source.<p> }
   TGLSourcePFXEffect = class (TGLParticleFXEffect)
      private
         { Private Declarations }
         FInitialVelocity : TGLCoordinates;
         FInitialPosition : TGLCoordinates;
         FVelocityDispersion : Single;
         FPositionDispersion : Single;
         FParticleInterval : Single;
         FVelocityMode : TGLSourcePFXVelocityMode;
         FDispersionMode : TGLSourcePFXDispersionMode;
         FTimeRemainder : Double;      // NOT persistent

      protected
         { Protected Declarations }
         procedure SetInitialVelocity(const val : TGLCoordinates);
         procedure SetInitialPosition(const val : TGLCoordinates);
         procedure SetVelocityDispersion(const val : Single);
         procedure SetPositionDispersion(const val : Single);
         procedure SetParticleInterval(const val : Single);

			procedure WriteToFiler(writer : TWriter); override;
         procedure ReadFromFiler(reader : TReader); override;

      public
         { Public Declarations }
         constructor Create(aOwner : TXCollection); override;
         destructor Destroy; override;

			class function FriendlyName : String; override;
			class function FriendlyDescription : String; override;

         procedure DoProgress(const deltaTime, newTime : Double); override;

         //: Instantaneously creates nb particles
         procedure Burst(time : Double; nb : Integer);

		published
			{ Published Declarations }
         property InitialVelocity : TGLCoordinates read FInitialVelocity write SetInitialVelocity;
         property VelocityDispersion : Single read FVelocityDispersion write SetVelocityDispersion;
         property InitialPosition : TGLCoordinates read FInitialPosition write SetInitialPosition;
         property PositionDispersion : Single read FPositionDispersion write SetPositionDispersion;
         property ParticleInterval : Single read FParticleInterval write SetParticleInterval;
         property VelocityMode : TGLSourcePFXVelocityMode read FVelocityMode write FVelocityMode default svmAbsolute;
         property DispersionMode : TGLSourcePFXDispersionMode read FDispersionMode write FDispersionMode default sdmFast;
   end;

	// TPFXLifeColor
	//
	TPFXLifeColor = class (TCollectionItem)
	   private
	      { Private Declarations }
         FColorInner : TGLColor;
         FColorOuter : TGLColor;
         FLifeTime, FInvLifeTime : Double;

	   protected
	      { Protected Declarations }
         function GetDisplayName : String; override;
         procedure SetColorInner(const val : TGLColor);
         procedure SetColorOuter(const val : TGLColor);
         procedure SetLifeTime(const val : Double);

         procedure OnChanged(Sender : TObject);

      public
	      { Public Declarations }
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;

	      procedure Assign(Source: TPersistent); override;

         {: Stores 1/LifeTime }
         property InvLifeTime : Double read FInvLifeTime;

	   published
	      { Published Declarations }
         property ColorInner : TGLColor read FColorInner write SetColorInner;
         property ColorOuter : TGLColor read FColorOuter write SetColorOuter;
         property LifeTime : Double read FLifeTime write SetLifeTime;
	end;

	// TPFXLifeColors
	//
	TPFXLifeColors = class (TCollection)
	   protected
	      { Protected Declarations }
	      owner : TComponent;
	      function GetOwner: TPersistent; override;
         procedure SetItems(index : Integer; const val : TPFXLifeColor);
	      function GetItems(index : Integer) : TPFXLifeColor;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent);

         function Add: TPFXLifeColor;
	      function FindItemID(ID: Integer): TPFXLifeColor;
	      property Items[index : Integer] : TPFXLifeColor read GetItems write SetItems; default;

         function MaxLifeTime : Double;
   end;

   // TGLDynamicPFXManager
   //
   {: An abstract PFX manager for simple dynamic particles.<p>
      Adds properties and progress implementation for handling moving particles
      (simple velocity and const acceleration integration). }
   TGLDynamicPFXManager = class (TGLParticleFXManager)
      private
         { Private Declarations }
         FAcceleration : TGLCoordinates;
         FCurrentTime : Double;           // NOT persistent

      protected
         { Protected Declarations }
         procedure SetAcceleration(const val : TGLCoordinates);

         {: Returns the maximum age for a particle.<p>
            Particles older than that will be killed by DoProgress. }
         function MaxParticleAge : Double; dynamic; abstract;

      public
         { Public Declarations }
         constructor Create(aOwner : TComponent); override;
         destructor Destroy; override;

         procedure DoProgress(const deltaTime, newTime : Double); override;

	   published
	      { Published Declarations }
         property Acceleration : TGLCoordinates read FAcceleration write SetAcceleration;
   end;

   // TGLPolygonPFXManager
   //
   {: Polygonal particles FX manager.<p>
      The particles of this manager are made of N-face regular polygon with
      a core and edge color. No texturing is available. }
   TGLPolygonPFXManager = class (TGLDynamicPFXManager)
      private
         { Private Declarations }
         FLifeColors : TPFXLifeColors;
         FParticleSize : Double;
         FNbSides : Integer;
         FColorInner : TGLColor;
         FColorOuter : TGLColor;
         Fvx, Fvy : TAffineVector;        // NOT persistent
         FVertices : TAffineVectorList;   // NOT persistent
         FVertBuf : TAffineVectorList;    // NOT persistent

      protected
         { Protected Declarations }
         procedure SetLifeColors(const val : TPFXLifeColors);
         procedure SetParticleSize(const val : Double);
         procedure SetNbSides(const val : Integer);
         procedure SetColorInner(const val : TGLColor);
         procedure SetColorOuter(const val : TGLColor);

         function MaxParticleAge : Double; override;

         procedure InitializeRendering; override;
         procedure BeginParticles; override;
         procedure RenderParticle(aParticle : TGLParticle); override;
         procedure EndParticles; override;
         procedure FinalizeRendering; override;

      public
         { Public Declarations }
         constructor Create(aOwner : TComponent); override;
         destructor Destroy; override;

	   published
	      { Published Declarations }
         property ColorInner : TGLColor read FColorInner write SetColorInner;
         property ColorOuter : TGLColor read FColorOuter write SetColorOuter;
         property LifeColors : TPFXLifeColors read FLifeColors write SetLifeColors;
         property ParticleSize : Double read FParticleSize write SetParticleSize;
         property NbSides : Integer read FNbSides write SetNbSides default 6;
   end;

{: Returns or creates the TGLBInertia within the given object's behaviours.<p> }
function GetOrCreateSourcePFX(obj : TGLBaseSceneObject; const name : String = '') : TGLSourcePFXEffect;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL12, GLCrossPlatform;

var
   vCounterFrequency : Int64;

// GetOrCreateSourcePFX
//
function GetOrCreateSourcePFX(obj : TGLBaseSceneObject; const name : String = '') : TGLSourcePFXEffect;
var
	i : Integer;
begin
   with obj.Effects do begin
      if name='' then begin
      	i:=IndexOfClass(TGLSourcePFXEffect);
      	if i>=0 then
	      	Result:=TGLSourcePFXEffect(Items[i])
      	else Result:=TGLSourcePFXEffect.Create(obj.Effects);
      end else begin
         i:=IndexOfName(name);
         if i>=0 then
            Result:=(Items[i] as TGLSourcePFXEffect)
         else begin
            Result:=TGLSourcePFXEffect.Create(obj.Effects);
            Result.Name:=name;
         end;
      end;
   end;
end;

// ------------------
// ------------------ TGLParticle ------------------
// ------------------

// Create
//
constructor TGLParticle.Create;
begin
   inherited Create;
end;

// Destroy
//
destructor TGLParticle.Destroy;
begin
   inherited Destroy;
end;

// WriteToFiler
//
procedure TGLParticle.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0); // Archive Version 0
      Write(FPosition, SizeOf(FPosition));
      Write(FVelocity, SizeOf(FVelocity));
      WriteFloat(FCreationTime);
   end;
end;

// ReadFromFiler
//
procedure TGLParticle.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      Read(FPosition, SizeOf(FPosition));
      Read(FVelocity, SizeOf(FVelocity));
      FCreationTime:=ReadFloat;
   end else RaiseFilerException(archiveVersion);
end;

// ------------------
// ------------------ TGLParticleList ------------------
// ------------------

// Create
//
constructor TGLParticleList.Create;
begin
   inherited Create;
   FItemList:=TPersistentObjectList.Create;
   FitemList.GrowthDelta:=64;
   FDirectList:=nil;
end;

// Destroy
//
destructor TGLParticleList.Destroy;
begin
   FItemList.CleanFree;
   inherited Destroy;
end;

// WriteToFiler
//
procedure TGLParticleList.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0); // Archive Version 0
      FItemList.WriteToFiler(writer);
   end;
end;

// ReadFromFiler
//
procedure TGLParticleList.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      FItemList.ReadFromFilerWithEvent(reader, AfterItemCreated);
      FDirectList:=PGLParticleArray(FItemList.List);
   end else RaiseFilerException(archiveVersion);
end;

// GetItems
//
function TGLParticleList.GetItems(index : Integer) : TGLParticle;
begin
   Result:=TGLParticle(FItemList[index]);
end;

// SetItems
//
procedure TGLParticleList.SetItems(index : Integer; val : TGLParticle);
begin
   FItemList[index]:=val;
end;

// AfterItemCreated
//
procedure TGLParticleList.AfterItemCreated(Sender : TObject);
begin
   (Sender as TGLParticle).Owner:=Self;
end;

// ItemCount
//
function TGLParticleList.ItemCount : Integer;
begin
   Result:=FItemList.Count;
end;

// AddItem
//
function  TGLParticleList.AddItem(aItem : TGLParticle) : Integer;
begin
   aItem.Owner:=Self;
   Result:=FItemList.Add(aItem);
   FDirectList:=PGLParticleArray(FItemList.List);
end;

// RemoveAndFreeItem
//
procedure TGLParticleList.RemoveAndFreeItem(aItem : TGLParticle);
var
   i : Integer;
begin
   i:=FItemList.IndexOf(aItem);
   if i>=0 then begin
      if aItem.Owner=Self then
         aItem.Owner:=nil;
      aItem.Free;
      FItemList.List[i]:=nil;
   end;
end;

// IndexOfItem
//
function TGLParticleList.IndexOfItem(aItem : TGLParticle) : Integer;
begin
   Result:=FItemList.IndexOf(aItem);
end;

// Pack
//
procedure TGLParticleList.Pack;
begin
   FItemList.Pack;
   FDirectList:=PGLParticleArray(FItemList.List);
end;

// ------------------
// ------------------ TGLParticleFXManager ------------------
// ------------------

// Create
//
constructor TGLParticleFXManager.Create(aOwner : TComponent);
begin
   inherited;
   FParticles:=TGLParticleList.Create;
   FParticles.Owner:=Self;
   RegisterManager(Self);
end;

// Destroy
//
destructor TGLParticleFXManager.Destroy;
begin
   DeRegisterManager(Self);
   Cadencer:=nil;
   Renderer:=nil;
   FParticles.Free;
   inherited Destroy;
end;

// Notification
//
procedure TGLParticleFXManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation=opRemove) and (AComponent=FCadencer) then
      Cadencer:=nil;
   inherited;
end;

// NotifyChange
//
procedure TGLParticleFXManager.NotifyChange(Sender : TObject);
begin
   if Assigned(FRenderer) then
      Renderer.StructureChanged;
end;

// CreateParticle
//
function TGLParticleFXManager.CreateParticle : TGLParticle;
begin
   Result:=TGLParticle.Create;
   FParticles.AddItem(Result);
end;

// SetRenderer
//
procedure TGLParticleFXManager.SetRenderer(const val : TGLParticleFXRenderer);
begin
   if FRenderer<>val then begin
      if Assigned(FRenderer) then
         FRenderer.UnRegisterManager(Self);
      FRenderer:=val;
      if Assigned(FRenderer) then
         FRenderer.RegisterManager(Self);
   end;
end;

// SetParticles
//
procedure TGLParticleFXManager.SetParticles(const aParticles : TGLParticleList);
begin
   FParticles.Assign(aParticles);
end;

// SetCadencer
//
procedure TGLParticleFXManager.SetCadencer(const val : TGLCadencer);
begin
   if FCadencer<>val then begin
      if Assigned(FCadencer) then
         FCadencer.UnSubscribe(Self);
      FCadencer:=val;
      if Assigned(FCadencer) then
         FCadencer.Subscribe(Self);
   end;
end;

// ------------------
// ------------------ TGLParticleFXEffect ------------------
// ------------------

// Create
//
constructor TGLParticleFXEffect.Create(aOwner : TXCollection);
begin
   inherited;
end;

// Destroy
//
destructor TGLParticleFXEffect.Destroy;
begin
   Manager:=nil;
   inherited Destroy;
end;

// WriteToFiler
//
procedure TGLParticleFXEffect.WriteToFiler(writer : TWriter);
begin
   with writer do begin
      WriteInteger(0); // ArchiveVersion 0
      if Assigned(FManager) then
         WriteString(FManager.GetNamePath)
      else WriteString('');
   end;
end;

// ReadFromFiler
//
procedure TGLParticleFXEffect.ReadFromFiler(reader : TReader);
begin
   with reader do begin
      Assert(ReadInteger=0);
      FManagerName:=ReadString;
      Manager:=nil;
   end;
end;

// Loaded
//
procedure TGLParticleFXEffect.Loaded;
var
   mng : TComponent;
begin
   inherited;
   if FManagerName<>'' then begin
      mng:=FindManager(TGLParticleFXManager, FManagerName);
      if Assigned(mng) then
         Manager:=TGLParticleFXManager(mng);
      FManagerName:='';
   end;
end;

// SetManager
//
procedure TGLParticleFXEffect.SetManager(val : TGLParticleFXManager);
begin
   FManager:=val;
   // nothing more, yet...
end;

// ------------------
// ------------------ TGLParticleFXRenderer ------------------
// ------------------

// Create
//
constructor TGLParticleFXRenderer.Create(aOwner : TComponent);
begin
   inherited;
   ObjectStyle:=ObjectStyle+[osNoVisibilityCulling, osDirectDraw];
   FManagerList:=TList.Create;
end;

// Destroy
//
destructor TGLParticleFXRenderer.Destroy;
begin
   UnRegisterAll;
   FManagerList.Free;
   inherited Destroy;
end;

// RegisterManager
//
procedure TGLParticleFXRenderer.RegisterManager(aManager : TGLParticleFXManager);
begin
   FManagerList.Add(aManager);
end;

// UnRegisterManager
//
procedure TGLParticleFXRenderer.UnRegisterManager(aManager : TGLParticleFXManager);
begin
   FManagerList.Remove(aManager);
end;

// UnRegisterAll
//
procedure TGLParticleFXRenderer.UnRegisterAll;
begin
   while FManagerList.Count>0 do
      TGLParticleFXManager(FManagerList[FManagerList.Count-1]).Renderer:=nil;
end;

// BuildList
// (beware, large and complex stuff below... this is the heart of the ParticleFX)
procedure TGLParticleFXRenderer.BuildList(var rci : TRenderContextInfo);
{
   Quick Explanation of what is below:

   The purpose is to depth-sort a large number (thousandths) of particles and
   render them back to front. The rendering part is not particularly complex,
   it just invokes the various PFX managers involved and request particle
   renderings.
   The sort uses a first-pass region partition (the depth range is split into
   regions, and particles are assigned directly to the region they belong to),
   then each region is sorted with a QuickSort.
   The QuickSort itself is the regular classic variant, but the comparison is
   made on singles as if they were integers, this is allowed by the IEEE format
   in a very efficient manner if all values are superior to 1, which is ensured
   by the distance calculation and a fixed offset of 1.
}
const
   cNbRegions = 64;     // number of distance regions
   cGranularity = 64;   // granularity of particles per region

type
   PInteger = ^Integer;
   TParticleReference = record
      particle : TGLParticle;
      distance : Integer;  // stores an IEEE single!
      manager : TGLParticleFXManager;
   end;
   PParticleReference = ^TParticleReference;
   TParticleReferenceArray = array [0..MaxInt shr 4] of TParticleReference;
   PParticleReferenceArray = ^TParticleReferenceArray;
   TRegion = record
      count, capacity : Integer;
      particleRef : PParticleReferenceArray;
      particleOrder : array of PParticleReference
   end;
   PRegion = ^TRegion;

   procedure QuickSortRegion(startIndex, endIndex : Integer; region : PRegion);
   var
      I, J : Integer;
      P : Integer;
      buf : Pointer;
      poptr : PPointerArray;
   begin
      if endIndex-startIndex>1 then begin
         poptr:=@region.particleOrder[0];
         repeat
            I:=startIndex;
            J:=endIndex;
            P:=PParticleReference(poptr[(I + J) shr 1]).distance;
            repeat
               while PParticleReference(poptr[I]).distance<P do Inc(I);
               while PParticleReference(poptr[J]).distance>P do Dec(J);
               if I<=J then begin
                  buf:=poptr[J];
                  poptr[J]:=poptr[I];
                  poptr[I]:=buf;
                  Inc(I); Dec(J);
               end;
            until I>J;
            if startIndex<J then
               QuickSortRegion(startIndex, J, region);
            startIndex:=I;
         until I >= endIndex;
      end else if endIndex-startIndex>0 then begin
         poptr:=@region.particleOrder[0];
         if PParticleReference(poptr[endIndex]).distance<PParticleReference(poptr[startIndex]).distance then begin
            buf:=poptr[startIndex];
            poptr[startIndex]:=poptr[endIndex];
            poptr[endIndex]:=buf;
         end;
      end;
   end;

var
   regions : array [0..cNbRegions-1] of TRegion;
   dist, minDist, maxDist, invRegionSize, distDelta : Single;
   managerIdx, particleIdx, regionIdx : Integer;
   curManager : TGLParticleFXManager;
   curList : PGLParticleArray;
   curParticle : TGLParticle;
   curRegion : PRegion;
   curParticleRef : PParticleReference;
   cameraPos, cameraVector : TAffineVector;
   timerStart, timerStop : Int64;
begin
   if csDesigning in ComponentState then Exit;
   QueryPerformanceCounter(timerStart);
   // precalcs
   with Scene.CurrentGLCamera do begin
      minDist:=NearPlane+1;
      maxDist:=NearPlane+DepthOfView+1;
      invRegionSize:=cNbRegions/(maxDist-minDist);
      distDelta:=minDist+0.49999/invRegionSize
   end;
   SetVector(cameraPos, rci.cameraPosition);
   SetVector(cameraVector, rci.cameraDirection);
   for regionIdx:=0 to cNbRegions-1 do begin
      with regions[regionIdx] do begin
         count:=0;
         capacity:=0;
         particleRef:=nil;
         SetLength(particleOrder, 0);
      end;
   end;
   try
      // Collect particles
      // only depth-clipping performed as of now.
      for managerIdx:=0 to FManagerList.Count-1 do begin
         curManager:=TGLParticleFXManager(FManagerList[managerIdx]);
         curList:=curManager.FParticles.List;
         for particleIdx:=0 to curManager.FParticles.ItemCount-1 do begin
            curParticle:=curList[particleIdx];
            dist:=VectorDotProduct(VectorSubtract(curParticle.FPosition, cameraPos),
                                   cameraVector)+1;
            if (dist>=minDist) and (dist<=maxDist) then begin
               // Round(x-0.5)=Trunc(x) and Round is faster (significantly faster)
               regionIdx:=Geometry.Round((dist-distDelta)*invRegionSize);
               if regionIdx>=cNbRegions then
                  regionIdx:=cNbRegions-1;
               curRegion:=@regions[regionIdx];
               // add particle to region
               if curRegion.count=curRegion.capacity then begin
                  Inc(curRegion.capacity, cGranularity);
                  ReallocMem(curRegion.particleRef, curRegion.capacity*SizeOf(TParticleReference));
               end;
               with curRegion.particleRef[curRegion.count] do begin
                  particle:=curParticle;
                  distance:=PInteger(@dist)^;
                  manager:=curManager;
               end;
               Inc(curRegion.count);
            end;
         end;
      end;
      // Sort regions
      for regionIdx:=0 to cNbRegions-1 do begin
         curRegion:=@regions[regionIdx];
         if curRegion.count>1 then begin
            // Prepare order table
            SetLength(curRegion.particleOrder, curRegion.Count);
            with curRegion^ do for particleIdx:=0 to Count-1 do
               particleOrder[particleIdx]:=@particleRef[particleIdx];
            // QuickSort
            QuickSortRegion(0, curRegion.count-1, curRegion);
         end else if curRegion.Count=1 then begin
            // Prepare order table
            SetLength(curRegion.particleOrder, 1);
            curRegion.particleOrder[0]:=@curRegion.particleRef[0];
         end;
      end;
      QueryPerformanceCounter(timerStop);
      FLastSortTime:=(timerStop-timerStart)*(1000/vCounterFrequency);

      glPushMatrix;
      glLoadMatrixf(@Scene.CurrentBuffer.ModelViewMatrix);

      glPushAttrib(GL_ALL_ATTRIB_BITS);
      glDisable(GL_CULL_FACE);
      glDisable(GL_TEXTURE_2D);
      glDisable(GL_LIGHTING);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE);
      glEnable(GL_BLEND);
      glDepthFunc(GL_LEQUAL);

      try
         // Initialize managers
         for managerIdx:=0 to FManagerList.Count-1 do
            TGLParticleFXManager(FManagerList[managerIdx]).InitializeRendering;
         // Start Rendering... at last ;)
         try
            curManager:=nil;
            for regionIdx:=cNbRegions-1 downto 0 do begin
               curRegion:=@regions[regionIdx];
               for particleIdx:=curRegion.count-1 downto 0 do begin
                  curParticleRef:=curRegion.particleOrder[particleIdx];
                  if curParticleRef.manager<>curManager then begin
                     if Assigned(curManager) then
                        curManager.EndParticles;
                     curManager:=curParticleRef.manager;
                     curManager.BeginParticles;
                  end;
                  curManager.RenderParticle(curParticleRef.particle);
               end;
            end;
            if Assigned(curManager) then
               curManager.EndParticles;
         finally
            // Finalize managers
            for managerIdx:=0 to FManagerList.Count-1 do
               TGLParticleFXManager(FManagerList[managerIdx]).FinalizeRendering;
         end;
      finally
         glPopMatrix;
         glPopAttrib;
      end;
   finally
      // cleanup
      for regionIdx:=cNbRegions-1 downto 0 do begin
         curRegion:=@regions[regionIdx];
         FreeMem(curRegion.particleRef);
         SetLength(curRegion.particleOrder, 0);
      end;
   end;
end;

// ------------------
// ------------------ TGLSourcePFXEffect ------------------
// ------------------

// Create
//
constructor TGLSourcePFXEffect.Create(aOwner : TXCollection);
begin
   inherited;
   FInitialVelocity:=TGLCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
   FInitialPosition:=TGLCoordinates.CreateInitialized(Self, NullHmgVector, csPoint);
   FVelocityDispersion:=0;
   FPositionDispersion:=0;
   FParticleInterval:=0.1;
   FVelocityMode:=svmAbsolute;
   FDispersionMode:=sdmFast;
end;

// Destroy
//
destructor TGLSourcePFXEffect.Destroy;
begin
   FInitialVelocity.Free;
   FInitialPosition.Free;
   inherited Destroy;
end;

// FriendlyName
//
class function TGLSourcePFXEffect.FriendlyName : String;
begin
   Result:='PFX Source';
end;

// FriendlyDescription
//
class function TGLSourcePFXEffect.FriendlyDescription : String;
begin
   Result:='Simple Particles FX Source';
end;

// WriteToFiler
//
procedure TGLSourcePFXEffect.WriteToFiler(writer : TWriter);
begin
   inherited;
   with writer do begin
      WriteInteger(0); // ArchiveVersion 0
      FInitialVelocity.WriteToFiler(writer);
      FInitialPosition.WriteToFiler(writer);
      WriteFloat(FVelocityDispersion);
      WriteFloat(FPositionDispersion);
      WriteFloat(FParticleInterval);
      WriteInteger(Integer(FVelocityMode));
   end;
end;

// ReadFromFiler
//
procedure TGLSourcePFXEffect.ReadFromFiler(reader : TReader);
begin
   inherited;
   with reader do begin
      Assert(ReadInteger=0);
      FInitialVelocity.ReadFromFiler(reader);
      FInitialPosition.ReadFromFiler(reader);
      FVelocityDispersion:=ReadFloat;
      FPositionDispersion:=ReadFloat;
      FParticleInterval:=ReadFloat;
      FVelocityMode:=TGLSourcePFXVelocityMode(ReadInteger);
   end;
end;

// SetInitialVelocity
//
procedure TGLSourcePFXEffect.SetInitialVelocity(const val : TGLCoordinates);
begin
   FInitialVelocity.Assign(val);
end;

// SetVelocityDispersion
//
procedure TGLSourcePFXEffect.SetVelocityDispersion(const val : Single);
begin
   FVelocityDispersion:=val;
   OwnerBaseSceneObject.NotifyChange(Self);
end;

// SetInitialPosition
//
procedure TGLSourcePFXEffect.SetInitialPosition(const val : TGLCoordinates);
begin
   FInitialPosition.Assign(val);
end;

// SetPositionDispersion
//
procedure TGLSourcePFXEffect.SetPositionDispersion(const val : Single);
begin
   FPositionDispersion:=val;
   OwnerBaseSceneObject.NotifyChange(Self);
end;

// SetParticleInterval
//
procedure TGLSourcePFXEffect.SetParticleInterval(const val : Single);
begin
   FParticleInterval:=val;
   // as of now, spawning a sustained 1000 particles/sec looks like a lot...
   if FParticleInterval<0.001 then FParticleInterval:=0.001;
   OwnerBaseSceneObject.NotifyChange(Self);
end;

// DoProgress
//
procedure TGLSourcePFXEffect.DoProgress(const deltaTime, newTime : Double);
begin
   if not Assigned(Manager) then Exit;
   FTimeRemainder:=FTimeRemainder+deltaTime;
   while FTimeRemainder>FParticleInterval do begin
      Burst(newTime, 1);
      FTimeRemainder:=FTimeRemainder-FParticleInterval;
   end;
end;

// Burst
//
procedure TGLSourcePFXEffect.Burst(time : Double; nb : Integer);

   function RndHalf(var f : Single) : Single;
   begin
      RndHalf:=(Random-0.5)*(2*f);
   end;

   procedure RndVector(var v : TAffineVector; var f : Single);
   begin
      repeat
         SetVector(v, RndHalf(f), RndHalf(f), RndHalf(f));
      until (DispersionMode=sdmFast) or (VectorNorm(v)<1);
   end;

var
   particle : TGLParticle;
   v : TVector;
   av, pos : TAffineVector;
begin
   if Manager=nil then Exit;
   SetVector(pos, OwnerBaseSceneObject.AbsolutePosition);
   AddVector(pos, InitialPosition.AsAffineVector);
   while nb>0 do begin
      particle:=Manager.CreateParticle;
      RndVector(av, FPositionDispersion);
      particle.Position:=VectorAdd(pos, av);
      RndVector(av, FVelocityDispersion);
      particle.Velocity:=VectorAdd(InitialVelocity.AsAffineVector, av);
      if VelocityMode=svmRelative then begin
         SetVector(v, particle.Velocity);
         v:=OwnerBaseSceneObject.LocalToAbsolute(v);
         SetVector(particle.FVelocity, v);
      end;
      particle.CreationTime:=time;
      Dec(nb);
   end;
end;

// ------------------
// ------------------ TPFXLifeColor ------------------
// ------------------

// Create
//
constructor TPFXLifeColor.Create(Collection : TCollection);
begin
	inherited Create(Collection);
   FColorInner:=TGLColor.CreateInitialized(Self, NullHmgVector, OnChanged);
   FColorOuter:=TGLColor.CreateInitialized(Self, NullHmgVector, OnChanged);
   FLifeTime:=1;
   FInvLifeTime:=1;
end;

// Destroy
//
destructor TPFXLifeColor.Destroy;
begin
   FColorOuter.Free;
   FColorInner.Free;
	inherited Destroy;
end;

// Assign
//
procedure TPFXLifeColor.Assign(Source: TPersistent);
begin
	if Source is TPFXLifeColor then begin
      FColorInner.Assign(TPFXLifeColor(Source).ColorInner);
      FColorOuter.Assign(TPFXLifeColor(Source).ColorOuter);
      FLifeTime:=TPFXLifeColor(Source).LifeTime;
	end else inherited;
end;

// GetDisplayName
//
function TPFXLifeColor.GetDisplayName : String;
begin
	Result:=Format('LifeTime %f', [LifeTime]);
end;

// OnChanged
//
procedure TPFXLifeColor.OnChanged(Sender : TObject);
begin
   ((Collection as TPFXLifeColors).GetOwner as TGLParticleFXManager).NotifyChange(Self);
end;

// SetColorInner
//
procedure TPFXLifeColor.SetColorInner(const val : TGLColor);
begin
   FColorInner.Assign(val);
end;

// SetColorOuter
//
procedure TPFXLifeColor.SetColorOuter(const val : TGLColor);
begin
   FColorOuter.Assign(val);
end;

// SetLifeTime
//
procedure TPFXLifeColor.SetLifeTime(const val : Double);
begin
   if FLifeTime<>val then begin
      FLifeTime:=val;
      if FLifeTime<=0 then FLifeTime:=1e-6;
      FInvLifeTime:=1/FLifeTime;
      OnChanged(Self);
   end;
end;

// ------------------
// ------------------ TPFXLifeColors ------------------
// ------------------

constructor TPFXLifeColors.Create(AOwner : TComponent);
begin
	Owner:=AOwner;
	inherited Create(TPFXLifeColor);
end;

function TPFXLifeColors.GetOwner: TPersistent;
begin
	Result:=Owner;
end;

procedure TPFXLifeColors.SetItems(index : Integer; const val : TPFXLifeColor);
begin
	inherited Items[index]:=val;
end;

function TPFXLifeColors.GetItems(index : Integer) : TPFXLifeColor;
begin
	Result:=TPFXLifeColor(inherited Items[index]);
end;

function TPFXLifeColors.Add: TPFXLifeColor;
begin
	Result:=(inherited Add) as TPFXLifeColor;
end;

// FindItemID
//
function TPFXLifeColors.FindItemID(ID: Integer): TPFXLifeColor;
begin
	Result:=(inherited FindItemID(ID)) as TPFXLifeColor;
end;

// MaxLifeTime
//
function TPFXLifeColors.MaxLifeTime : Double;
var
   i : Integer;
begin
   Result:=0;
   for i:=0 to Count-1 do
      if Items[i].LifeTime>Result then
         Result:=Items[i].LifeTime;
end;

// ------------------
// ------------------ TGLDynamicPFXManager ------------------
// ------------------

// Create
//
constructor TGLDynamicPFXManager.Create(aOwner : TComponent);
begin
   inherited;
   FAcceleration:=TGLCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
end;

// Destroy
//
destructor TGLDynamicPFXManager.Destroy;
begin
   FAcceleration.Free;
   inherited Destroy;
end;

// DoProgress
//
procedure TGLDynamicPFXManager.DoProgress(const deltaTime, newTime : Double);
var
   i : Integer;
   curParticle : TGLParticle;
   particleAge, maxAge : Double;
   accelVector : TAffineVector;
   dt : Single;
begin
   maxAge:=MaxParticleAge;
   accelVector:=Acceleration.AsAffineVector;
   dt:=deltaTime;
   FCurrentTime:=newTime;
   for i:=0 to Particles.ItemCount-1 do begin
      curParticle:=Particles.List[i];
      particleAge:=newTime-curParticle.CreationTime;
      if particleAge<maxAge then begin
         // particle alive, just update velocity and position
         with curParticle do begin
            CombineVector(FPosition, FVelocity, dt);
            CombineVector(FVelocity, accelVector, dt);
         end;
      end else begin
         // kill particle
         curParticle.Free;
         Particles.List[i]:=nil;
      end;
   end;
   Particles.Pack;
end;

// SetAcceleration
//
procedure TGLDynamicPFXManager.SetAcceleration(const val : TGLCoordinates);
begin
   FAcceleration.Assign(val);
end;

// ------------------
// ------------------ TGLPolygonPFXManager ------------------
// ------------------

// Create
//
constructor TGLPolygonPFXManager.Create(aOwner : TComponent);
begin
   inherited;
   FLifeColors:=TPFXLifeColors.Create(Self);
   FNbSides:=6;
   FColorInner:=TGLColor.CreateInitialized(Self, clrYellow);
   FColorOuter:=TGLColor.CreateInitialized(Self, NullHmgVector);
   with FLifeColors.Add do begin
      LifeTime:=3;
   end;
   FParticleSize:=1;
end;

// Destroy
//
destructor TGLPolygonPFXManager.Destroy;
begin
   FLifeColors.Free;
   inherited Destroy;
end;

// SetColorInner
//
procedure TGLPolygonPFXManager.SetColorInner(const val : TGLColor);
begin
   FColorInner.Assign(val);
end;

// SetColorOuter
//
procedure TGLPolygonPFXManager.SetColorOuter(const val : TGLColor);
begin
   FColorOuter.Assign(val);
end;

// SetLifeColors
//
procedure TGLPolygonPFXManager.SetLifeColors(const val : TPFXLifeColors);
begin
   FLifeColors.Assign(Self);
end;

// SetParticleSize
//
procedure TGLPolygonPFXManager.SetParticleSize(const val : Double);
begin
   if FParticleSize<>val then begin
      FParticleSize:=val;
      NotifyChange(Self);
   end;
end;

// SetNbSides
//
procedure TGLPolygonPFXManager.SetNbSides(const val : Integer);
begin
   if val<>FNbSides then begin
      FNbSides:=val;
      if FNbSides<3 then FNbSides:=3;
      NotifyChange(Self);
   end;
end;

// MaxParticleAge
//
function TGLPolygonPFXManager.MaxParticleAge : Double;
begin
   Result:=LifeColors.MaxLifeTime;
end;

// InitializeRendering
//
procedure TGLPolygonPFXManager.InitializeRendering;
var
   i : Integer;
   matrix : TMatrix;
   s, c : Single;
begin
   glGetFloatv(GL_MODELVIEW_MATRIX, @matrix);
   for i:=0 to 2 do begin
      Fvx[i]:=matrix[i][0]*FParticleSize;
      Fvy[i]:=matrix[i][1]*FParticleSize;
   end;
   FVertices:=TAffineVectorList.Create;
   FVertices.Capacity:=FNbSides+2;
   for i:=0 to FNbSides do begin
      SinCos(i*c2PI/FNbSides, FParticleSize, s, c);
      FVertices.Add(VectorCombine(FVx, Fvy, c, s));
   end;
   FVertBuf:=TAffineVectorList.Create;
   FVertBuf.Count:=FVertices.Count;
end;

// BeginParticles
//
procedure TGLPolygonPFXManager.BeginParticles;
begin
   glPushMatrix;
end;

// RenderParticle
//
procedure TGLPolygonPFXManager.RenderParticle(aParticle : TGLParticle);
var
   i, k, n : Integer;
   f : Single;
   lifeTime : Double;
   inner, outer : TColorVector;
   pos : TAffineVector;
   lck, lck1 : TPFXLifeColor;
begin
   lifeTime:=FCurrentTime-aParticle.CreationTime;
   with LifeColors do begin
      n:=Count-1;
      if n<0 then begin
         inner:=ColorInner.Color;
         outer:=ColorOuter.Color;
      end else begin
         k:=-1;
         for i:=0 to n do
            if Items[i].LifeTime<lifeTime then k:=i;
         if k<n then Inc(k);
         case k of
            0 : begin
               lck:=LifeColors[k];
               f:=lifeTime*lck.InvLifeTime;
               VectorLerp(ColorInner.Color, lck.ColorInner.Color, f, inner);
               VectorLerp(ColorOuter.Color, lck.ColorOuter.Color, f, outer);
            end;
         else
            lck:=LifeColors[k];
            lck1:=LifeColors[k-1];
            f:=(lifeTime-lck1.LifeTime)*(lck.LifeTime-lck1.LifeTime);
            VectorLerp(lck1.ColorInner.Color, lck.ColorInner.Color, f, inner);
            VectorLerp(lck1.ColorOuter.Color, lck.ColorOuter.Color, f, outer);
         end;
      end;
   end;

   pos:=aParticle.Position;

   VectorArrayAdd(FVertices.List, pos, FVertBuf.Count, FVertBuf.List);
   glBegin(GL_TRIANGLE_FAN);
      glColor4fv(@inner);
      glVertex3fv(@pos);
      glColor4fv(@outer);
      with FVertBuf do for i:=0 to Count-1 do
         glVertex3fv(@List[i]);
   glEnd;
end;

// EndParticles
//
procedure TGLPolygonPFXManager.EndParticles;
begin
   glPopMatrix;
end;

// FinalizeRendering
//
procedure TGLPolygonPFXManager.FinalizeRendering;
begin
   FVertBuf.Free;
   FVertices.Free;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   // class registrations
   RegisterClasses([TGLParticle, TGLParticleList,
                    TGLParticleFXEffect, TGLParticleFXRenderer,
                    TGLPolygonPFXManager]);
   RegisterXCollectionItemClass(TGLSourcePFXEffect);

   // preparation for high resolution timer
   QueryPerformanceFrequency(vCounterFrequency);
   
end.
