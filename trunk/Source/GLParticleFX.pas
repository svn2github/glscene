{: GLParticleFX<p>

   Base classes for scene-wide blended particles FX.<p>

   These provide a mechanism to render heterogenous particles systems with per
   particle depth-sorting (allowing correct rendering of interwoven separate
   fire and smoke particle systems for instance).<p>

   UNDER CONSTRUCTION.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>08/09/01 - EG - Creation (GLParticleFX.omm)
   </ul></font>
}
unit GLParticleFX;

interface

uses Classes, PersistentClasses, GLScene, Geometry, XCollection, GLTexture;

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

   // TGLParticleList
   //
   {: List of particles }
   TGLParticleList = class (TPersistentObject)
      private
         { Private Declarations }
         FOwner : TGLParticleFXManager;  // NOT persistent
         FItemList : TPersistentObjectList;

      protected
         { Protected Declarations }
         function GetItems(index : Integer) : TGLParticle;
         procedure AfterItemCreated(Sender : TObject);

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;
         procedure WriteToFiler(writer : TVirtualWriter); override;
         procedure ReadFromFiler(reader : TVirtualReader); override;

         {: Refers owner manager }
         property Owner : TGLParticleFXManager read FOwner write FOwner;
         property Items[index : Integer] : TGLParticle read GetItems; default;

         function ItemCount : Integer;
         function AddItem(aItem : TGLParticle) : Integer;
         procedure RemoveItem(aItem : TGLParticle);
         function IndexOfItem(aItem : TGLParticle) : Integer;

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
   TGLParticleFXManager = class (TComponent)
      private
         { Private Declarations }
         FRenderer : TGLParticleFXRenderer;
         FParticles : TGLParticleList;

      protected
         { Protected Declarations }
         procedure SetRenderer(const val : TGLParticleFXRenderer);
         procedure SetParticles(const aParticles : TGLParticleList);

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

         {: References the renderer.<p>
            The renderer takes care of ordering the particles of the manager
            (and other managers linked to it) and rendering them all depth-sorted. }
         property Renderer : TGLParticleFXRenderer read FRenderer write FRenderer;
         {: A TGLParticleList property. }
         property Particles : TGLParticleList read FParticles write SetParticles;

   end;

   // TGLParticleFXEffect
   //
   {: Base class for linking scene objects to a particle FX manager.<p> }
   TGLParticleFXEffect = class (TGLObjectPostEffect)
      private
         { Private Declarations }
         FManager : TGLParticleFXManager;

      protected
         { Protected Declarations }
         procedure SetManager(val : TGLParticleFXManager);

      public
         { Public Declarations }
         constructor Create(aOwner : TXCollection); override;
         destructor Destroy; override;

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
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils;

const
   cPOSITION = 'POSITION';
   cVELOCITY = 'VELOCITY';
   cCREATIONTIME = 'CREATIONTIME';
   cRENDERER = 'RENDERER';
   cMANAGER = 'MANAGER';

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
   end else RaiseFilerException(archiveVersion);
end;

// GetItems
//
function TGLParticleList.GetItems(index : Integer) : TGLParticle;
begin
   Result:=TGLParticle(FItemList[index]);
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
end;

// RemoveItem
//
procedure  TGLParticleList.RemoveItem(aItem : TGLParticle);
begin
   FItemList.Remove(aItem);
   if aItem.Owner=Self then
      aItem.Owner:=nil;
end;

// IndexOfItem
//
function  TGLParticleList.IndexOfItem(aItem : TGLParticle) : Integer;
begin
   Result:=FItemList.IndexOf(aItem);
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
end;

// Destroy
//
destructor TGLParticleFXManager.Destroy;
begin
   Renderer:=nil;
   FParticles.Free;
   inherited Destroy;
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

const
   cNbRegions = 64;     // number of distance regions
   cGranularity = 256;  // granularity of 256 particles per region

type
   TParticleReference = record
      particle : TGLParticle;
      distance : Single;
      manager : TGLParticleFXManager;
   end;
   PParticleReference = ^TParticleReference;
   TParticleReferenceArray = array of TParticleReference;
   TRegion = record
      count, capacity : Integer;
      particleRef : TParticleReferenceArray;
      particleOrder : array of PParticleReference
   end;
   PRegion = ^TRegion;

   procedure QuickSortRegion(startIndex, endIndex : Integer; region : PRegion);
   var
      I, J : Integer;
      P : single;
      buf : PParticleReference;
   begin
      if endIndex-startIndex>1 then begin
         repeat
            I:=startIndex;
            J:=endIndex;
            P:=region.particleOrder[(I + J) shr 1].distance;
            repeat
               while region.particleOrder[I].distance<P do Inc(I);
               while region.particleOrder[J].distance>P do Dec(J);
               if I <= J then begin
                  buf:=region.particleOrder[J];
                  region.particleOrder[J]:=region.particleOrder[I];
                  region.particleOrder[I]:=buf;
                  Inc(I); Dec(J);
               end;
            until I > J;
            if startIndex < J then
               QuickSortRegion(startIndex, J, region);
            startIndex:=I;
         until I >= endIndex;
      end else if endIndex-startIndex>0 then begin
         P:=region.particleOrder[startIndex].distance;
         if region.particleOrder[endIndex].distance<P then begin
            buf:=region.particleOrder[startIndex];
            region.particleOrder[startIndex]:=region.particleOrder[endIndex];
            region.particleOrder[endIndex]:=buf;
         end;
      end;
   end;

var
   regions : array [0..cNbRegions-1] of TRegion;
   dist, minDist, maxDist, invRegionSize : Single;
   managerIdx, particleIdx, regionIdx : Integer;
   curManager : TGLParticleFXManager;
   curList : PPointerObjectList;
   curParticle : TGLParticle;
   curRegion : PRegion;
   curParticleRef : PParticleReference;
   cameraPos, cameraVector : TAffineVector;
begin
   // precalcs
   with Scene.CurrentGLCamera do begin
      minDist:=NearPlane;
      maxDist:=NearPlane+DepthOfView;
      invRegionSize:=cNbRegions/(maxDist-minDist);
   end;
   SetVector(cameraPos, rci.cameraPosition);
   SetVector(cameraVector, rci.cameraDirection);
   // Collect particles
   // only depth-clipping performed as of now.
   for managerIdx:=0 to FManagerList.Count-1 do begin
      curManager:=TGLParticleFXManager(FManagerList[managerIdx]);
      curList:=curManager.FParticles.FItemList.List;
      for particleIdx:=0 to curManager.FParticles.ItemCount-1 do begin
         curParticle:=TGLparticle(curList[particleIdx]);
         dist:=VectorDotProduct(VectorSubtract(curParticle.FPosition, cameraPos),
                                cameraVector);
         if (dist>=minDist) and (dist<=maxDist) then begin
            // Round(x-0.5)=Trunc(x) and Round is faster (significantly faster)
            regionIdx:=Round((dist-minDist)*invRegionSize-0.49999);
            if regionIdx>=cNbRegions then
               regionIdx:=cNbRegions-1;
            curRegion:=@regions[regionIdx];
            // add particle to region
            if curRegion.count=curRegion.capacity then begin
               Inc(curRegion.capacity, cGranularity);
               SetLength(curRegion.particleRef, curRegion.capacity);
            end;
            with curRegion.particleRef[curRegion.count] do begin
               Particle:=curParticle;
               Distance:=dist;
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
         // Prepaer order table
         SetLength(curRegion.particleOrder, curRegion.Count);
         for particleIdx:=0 to curRegion.Count-1 do
            curRegion.particleOrder[particleIdx]:=@curRegion.particleRef[particleIdx];
         // QuickSort
         QuickSortRegion(0, curRegion.count-1, curRegion);
      end;
   end;
   // Initialize managers
   for managerIdx:=0 to FManagerList.Count-1 do
      TGLParticleFXManager(FManagerList[managerIdx]).InitializeRendering;
   // Start Rendering... at last ;)
   try
      curManager:=nil;
      for regionIdx:=0 to cNbRegions-1 do begin
         curRegion:=@regions[regionIdx];
         for particleIdx:=0 to curRegion.count-1 do begin
            curParticleRef:=curRegion.particleOrder[particleIdx];
            if curParticleRef.manager<>curManager then begin
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
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   // class registrations
   RegisterClasses([TGLParticle, TGLParticleList, TGLParticleFXManager,
                    TGLParticleFXEffect, TGLParticleFXRenderer]);

finalization

end.
