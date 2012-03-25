//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLShadowVolumes<p>

   Implements basic shadow volumes support.<p>

   Be aware that only objects that support silhouette determination have a chance
   to cast correct shadows. Transparent/blended/shader objects among the receivers
   or the casters will be rendered incorrectly.<p>

 <b>History : </b><font size=-1><ul>
      <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>31/05/10 - Yar - Fixes forLinux x64
      <li>01/05/10 - Yar - Moved ignoreBlendingRequests and ignoreDepthRequests behind RenderChildren
      <li>22/04/10 - Yar - Fixes after GLState revision
      <li>05/03/10 - DanB - More state added to TGLStateCache
      <li>31/03/07 - DaStr - Fixed issue with invalid typecasting
                            (thanks Burkhard Carstens) (Bugtracker ID = 1692016)
      <li>30/03/07 - DaStr - Added $I GLScene.inc
      <li>28/03/07 - DaStr - Renamed parameters in some methods
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678658)
      <li>08/12/04 - DB - Fixed bug in TGLShadowVolumeCaster.SetCaster
      <li>02/12/04 - MF - Added some documentation
      <li>23/03/04 - EG - Added Active property
      <li>29/11/03 - MF - Removed a "feature" that would draw the shadow of
                          (hierarchially) invisible objects
      <li>27/11/03 - MF - TGLShadowVolumeCaster now registers with the FCaster
                          for delete notification
      <li>11/06/03 - EG - Added silhouette cache
      <li>04/06/03 - EG - Creation (based on code from Mattias Fagerlund)
  </ul></font>
}
unit GLScene_ShadowVolume;

interface

{$I GLScene.inc}

uses
  Classes,
  GLScene_Core,
  GLScene_Base_Vector_Types,
  GLScene_Base_Vector_Geometry,
  GLScene_Base_OpenGL_Tokens,
  GLScene_Base_Context,
  GLScene_Silhouette,
  GLScene_Platform,
  GLScene_Base_PersistentClasses,
  GLScene_Base_GeometryBB,
  GLScene_Base_Color,
  GLScene_Base_Context_Info,
  GLScene_Base_Transformation,
  GLScene_DrawTechnique;

type

  TGLShadowVolume = class;

  {: Determines when a shadow volume should generate a cap at the beginning and
   end of the volume. This is ONLY necessary when there's a chance that the
   camera could end up inside the shadow _or_ between the light source and
   the camera. If those two situations can't occur then not using capping is
   the best option.<br>
   Note that if you use the capping, you must either set the depth of view of
   your camera to something very large (f.i. 1e9), or you could use the infinite
   mode (csInfinitePerspective) of your camera.
   <ul>
     <li>svcDefault : Default behaviour
     <li>svcAlways : Always generates caps
     <li>svcNever : Never generates caps
   </ul>
   }
  TGLShadowVolumeCapping = (svcDefault, svcAlways, svcNever);

  {: Determines when a caster should actually produce a shadow;
  <ul>
   <li>scmAlways : Caster always produces a shadow, ignoring visibility
   <li>scmVisible : Caster casts shadow if the object has visible=true
   <li>scmRecursivelyVisible : Caster casts shadow if ancestors up the hierarchy
     all have visible=true
   <li>scmParentVisible : Caster produces shadow if parent has visible=true
   <li>scmParentRecursivelyVisible : Caster casts shadow if ancestors up the hierarchy
     all have visible=true, starting from the parent (ignoring own visible setting)
  </ul> }

  TGLShadowCastingMode = (scmAlways, scmVisible, scmRecursivelyVisible,
    scmParentVisible, scmParentRecursivelyVisible);

  // TGLShadowVolumeCaster
  //
  {: Specifies an individual shadow caster.<p>
     Can be a light or an opaque object. }
  TGLShadowVolumeCaster = class(TCollectionItem)
  private
    { Private Declarations }
    FCaster: TGLBaseSceneObject;
    FEffectiveRadius: Single;
    FCapping: TGLShadowVolumeCapping;
    FCastingMode: TGLShadowCastingMode;

  protected
    { Protected Declarations }
    procedure SetCaster(const val: TGLBaseSceneObject);
    function GetGLShadowVolume: TGLShadowVolume;

    procedure RemoveNotification(aComponent: TComponent);
    function GetDisplayName: string; override;

  public
    { Public Declarations }
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    {: Shadow casting object.<p>
       Can be an opaque object or a lightsource. }
    property Caster: TGLBaseSceneObject read FCaster write SetCaster;

    property GLShadowVolume: TGLShadowVolume read GetGLShadowVolume;

  published
    { Published Declarations }

          {: Radius beyond which the caster can be ignored.<p>
             Zero (default value) means the caster can never be ignored. }
    property EffectiveRadius: Single read FEffectiveRadius write
      FEffectiveRadius;
    {: Specifies if the shadow volume should be capped.<p>
       Capping helps solve shadowing artefacts, at the cost of performance. }
    property Capping: TGLShadowVolumeCapping read FCapping write FCapping default
      svcDefault;
    {: Determines when an object should cast a shadow or not. Typically, objects
    should only cast shadows when recursively visible. But if you're using
    dummy shadow casters which are less complex than their parent objects,
    you should use scmParentRecursivelyVisible.}
    property CastingMode: TGLShadowCastingMode read FCastingMode write
      FCastingMode default scmRecursivelyVisible;
  end;

  // TGLShadowVolumeOccluder
  //
  {: Specifies an individual shadow casting occluder.<p> }
  TGLShadowVolumeOccluder = class(TGLShadowVolumeCaster)
  published
    { Published Declarations }
    property Caster;
  end;

  // TGLShadowVolumeLight
  //
  {: Specifies an individual shadow casting light.<p> }
  TGLShadowVolumeLight = class(TGLShadowVolumeCaster)
  private
    { Private Declarations }
    FSilhouettes: TPersistentObjectList;

  protected
    { Protected Declarations }
    function GetLightSource: TGLLightSource;
    procedure SetLightSource(const ls: TGLLightSource);

    function GetCachedSilhouette(AIndex: Integer): TGLSilhouette;
    procedure StoreCachedSilhouette(AIndex: Integer; ASil: TGLSilhouette);

    {: Compute and setup scissor clipping rect for the light.<p>
       Returns true if a scissor rect was setup }
    function SetupScissorRect(const AnAABB: TAABB;
      var ARci: TRenderContextInfo): Boolean;

  public
    { Public Declarations }
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure FlushSilhouetteCache;

  published
    { Published Declarations }
          {: Shadow casting lightsource.<p> }
    property LightSource: TGLLightSource read GetLightSource write
      SetLightSource;

  end;

  // TGLShadowVolumeCasters
  //
  {: Collection of TGLShadowVolumeCaster. }
  TGLShadowVolumeCasters = class(TOwnedCollection)
  private
    { Private Declarations }

  protected
    { Protected Declarations }
    function GetItems(index: Integer): TGLShadowVolumeCaster;
    procedure RemoveNotification(aComponent: TComponent);

  public
    { Public Declarations }
    function AddCaster(obj: TGLBaseSceneObject; effectiveRadius: Single = 0;
      CastingMode: TGLShadowCastingMode = scmRecursivelyVisible):
      TGLShadowVolumeCaster;
    procedure RemoveCaster(obj: TGLBaseSceneObject);
    function IndexOfCaster(obj: TGLBaseSceneObject): Integer;

    property Items[index: Integer]: TGLShadowVolumeCaster read GetItems;
    default;
  end;

  // TGLShadowVolumeOption
  //
  {: Shadow volume rendering options/optimizations.<p>
     <ul>
     <li>svoShowVolumes : make the shadow volumes visible
     <li>svoDesignVisible : the shadow are visible at design-time
     <li>svoCacheSilhouettes : cache shadow volume silhouettes, beneficial when
        some objects are static relatively to their light(s)
     <li>svoScissorClips : use scissor clipping per light, beneficial when
        lights are attenuated and don't illuminate the whole scene
     <li>svoWorldScissorClip : use scissor clipping for the world, beneficial
        when shadow receivers don't cover the whole viewer surface
     </ul> }
  TGLShadowVolumeOption = (svoShowVolumes, svoCacheSilhouettes, svoScissorClips,
    svoWorldScissorClip, svoDesignVisible);
  TGLShadowVolumeOptions = set of TGLShadowVolumeOption;

  // TGLShadowVolumeMode
  //
  {: Shadow rendering modes.<p>
     <ul>
     <li>svmAccurate : will render the scene with ambient lighting only, then
        for each light will make a diffuse+specular pass
     <li>svmDarkening : renders the scene with lighting on as usual, then darkens
        shadowed areas (i.e. inaccurate lighting, but will "shadow" objects
        that don't honour to diffuse or specular lighting)
     <li>svmOff : no shadowing will take place
     </ul> }
  TGLShadowVolumeMode = (svmAccurate, svmDarkening, svmOff);

  // TGLShadowVolume
  //
  {: Simple shadow volumes.<p>
     Shadow receiving objects are the ShadowVolume's children, shadow casters
     (opaque objects or lights) must be explicitly specified in the Casters
     collection.<p>
     Shadow volumes require that the buffer allows stencil buffers,
     GLSceneViewer.Buffer.ContextOptions contain roStencinBuffer. Without stencil
     buffers, shadow volumes will not work properly.<p>
     Another issue to look out for is the fact that shadow volume capping requires
     that the camera depth of view is either very high (fi 1e9) or that the
     camera style is csInfinitePerspective.
      }
  TGLShadowVolume = class(TGLImmaterialSceneObject)
  private
    { Private Declarations }
    FActive: Boolean;
    FRendering: Boolean;
    FLights: TGLShadowVolumeCasters;
    FOccluders: TGLShadowVolumeCasters;
    FCapping: TGLShadowVolumeCapping;
    FOptions: TGLShadowVolumeOptions;
    FMode: TGLShadowVolumeMode;
    FDarkeningColor: TGLColor;
    FPart1: TDrawBatch;
    FPart2: TDrawBatch;
    FPart3: TDrawBatch;
    FTransformation: TTransformationRec;
    FOpaques: TList;
    FOpaqueCapping: TList;
    FLightIteration: Integer;
    FWorldAABB: TAABB;
  protected
    { Protected Declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

    procedure SetActive(const val: Boolean);
    procedure SetLights(const val: TGLShadowVolumeCasters);
    procedure SetOccluders(const val: TGLShadowVolumeCasters);
    procedure SetOptions(const val: TGLShadowVolumeOptions);
    procedure SetMode(const val: TGLShadowVolumeMode);
    procedure SetDarkeningColor(const val: TGLColor);
    procedure TurnOffLights(var ARci: TRenderContextInfo);
    procedure BeforeShadowDraw(var ARci: TRenderContextInfo);
    procedure AfterShadowDraw(var ARci: TRenderContextInfo);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoRender(var ARci: TRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;

    procedure Assign(Source: TPersistent); override;

    procedure FlushSilhouetteCache;

  published
    { Public Declarations }
          {: Determines if shadow volume rendering is active.<p>
             When set to false, children will be rendered without any shadowing
             or multipass lighting. }
    property Active: Boolean read FActive write SetActive default True;
    {: Lights that cast shadow volumes. }
    property Lights: TGLShadowVolumeCasters read FLights write SetLights;
    {: Occluders that cast shadow volumes. }
    property Occluders: TGLShadowVolumeCasters read FOccluders write
      SetOccluders;

    {: Specifies if the shadow volume should be capped.<p>
       Capping helps solve shadowing artefacts, at the cost of performance. }
    property Capping: TGLShadowVolumeCapping read FCapping write FCapping default
      svcAlways;
    {: Shadow volume rendering options. }
    property Options: TGLShadowVolumeOptions read FOptions write SetOptions
      default [svoCacheSilhouettes, svoScissorClips];
    {: Shadow rendering mode. }
    property Mode: TGLShadowVolumeMode read FMode write SetMode default
      svmAccurate;
    {: Darkening color used in svmDarkening mode. }
    property DarkeningColor: TGLColor read FDarkeningColor write
      SetDarkeningColor;
  end;

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses
  SysUtils,
  GLScene_Base_Vector_Lists,
  GLScene_Base_GLStateMachine;

// ------------------
// ------------------ TGLShadowVolumeCaster ------------------
// ------------------

// Create
//

constructor TGLShadowVolumeCaster.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FCapping := svcDefault;
  FCastingMode := scmRecursivelyVisible;
end;

type
  // Required for Delphi 5 support.
  THackOwnedCollection = class(TOwnedCollection);

  // GetGLShadowVolume
  //

function TGLShadowVolumeCaster.GetGLShadowVolume: TGLShadowVolume;
begin
  Result := TGLShadowVolume(THackOwnedCollection(Collection).GetOwner);
end;

// Destroy
//

destructor TGLShadowVolumeCaster.Destroy;
begin
  if Assigned(FCaster) then
    FCaster.RemoveFreeNotification(GLShadowVolume);
  inherited;
end;

// Assign
//

procedure TGLShadowVolumeCaster.Assign(Source: TPersistent);
begin
  if Source is TGLShadowVolumeCaster then
  begin
    FCaster := TGLShadowVolumeCaster(Source).FCaster;
    FEffectiveRadius := TGLShadowVolumeCaster(Source).FEffectiveRadius;
    FCapping := TGLShadowVolumeCaster(Source).FCapping;
    GetGLShadowVolume.StructureChanged;
  end;
  inherited;
end;

// SetCaster
//

procedure TGLShadowVolumeCaster.SetCaster(const val: TGLBaseSceneObject);
begin
  if FCaster <> val then
  begin
    if FCaster <> nil then
      FCaster.RemoveFreeNotification(GLShadowVolume);
    FCaster := val;
    if FCaster <> nil then
      FCaster.FreeNotification(GLShadowVolume);
    GetGLShadowVolume.StructureChanged;
  end;
end;

// RemoveNotification
//

procedure TGLShadowVolumeCaster.RemoveNotification(aComponent: TComponent);
begin
  if aComponent = FCaster then
  begin
    // No point in keeping the TGLShadowVolumeCaster once the FCaster has been
    // destroyed.
    FCaster := nil;
    Free;
  end;
end;

// GetDisplayName
//

function TGLShadowVolumeCaster.GetDisplayName: string;
begin
  if Assigned(FCaster) then
  begin
    if FCaster is TGLLightSource then
      Result := '[Light]'
    else
      Result := '[Object]';
    Result := Result + ' ' + FCaster.Name;
    if EffectiveRadius > 0 then
      Result := Result + Format(' (%.1f)', [EffectiveRadius]);
  end
  else
    Result := 'nil';
end;

// ------------------
// ------------------ TGLShadowVolumeLight ------------------
// ------------------

// Create
//

constructor TGLShadowVolumeLight.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FSilhouettes := TPersistentObjectList.Create;
end;

// Destroy
//

destructor TGLShadowVolumeLight.Destroy;
begin
  FlushSilhouetteCache;
  FSilhouettes.Free;
  inherited;
end;

// FlushSilhouetteCache
//

procedure TGLShadowVolumeLight.FlushSilhouetteCache;
begin
  FSilhouettes.Clean;
end;

// Create
//

function TGLShadowVolumeLight.GetLightSource: TGLLightSource;
begin
  Result := TGLLightSource(Caster);
end;

// SetLightSource
//

procedure TGLShadowVolumeLight.SetLightSource(const ls: TGLLightSource);
begin
  SetCaster(ls);
end;

// GetCachedSilhouette
//

function TGLShadowVolumeLight.GetCachedSilhouette(AIndex: Integer):
  TGLSilhouette;
begin
  if AIndex < FSilhouettes.Count then
    Result := TGLSilhouette(FSilhouettes[AIndex])
  else
    Result := nil;
end;

// StoreCachedSilhouette
//

procedure TGLShadowVolumeLight.StoreCachedSilhouette(AIndex: Integer; ASil:
  TGLSilhouette);
begin
  while AIndex >= FSilhouettes.Count do
    FSilhouettes.Add(nil);
  if ASil <> FSilhouettes[AIndex] then
  begin
    if assigned(FSilhouettes[AIndex]) then
      FSilhouettes[AIndex].Free;
    FSilhouettes[AIndex] := ASil;
  end;
end;

// TGLShadowVolumeLight
//

function TGLShadowVolumeLight.SetupScissorRect(const AnAABB: TAABB;
  var ARci: TRenderContextInfo): Boolean;
var
  mvp: TMatrix;
  ls: TGLLightSource;
  aabb: TAABB;
  clipRect: TClipRect;
  box: TVector4i;
begin
  ls := LightSource;
  if (EffectiveRadius <= 0) or (not ls.Attenuated) then
  begin
    // non attenuated lights can't be clipped
    aabb := AnAABB;
  end
  else
  begin
    aabb := BSphereToAABB(ls.AbsolutePosition, EffectiveRadius);
    aabb := AABBIntersection(aabb, AnAABB);
  end;

  if PointInAABB(ARci.cameraPosition, aabb) then
  begin
    // camera inside light volume radius, can't clip
    Result := False;
    Exit;
  end;

  // Calculate the window-space bounds of the light's bounding box.
  mvp := ARci.PipelineTransformation.ViewProjectionMatrix;

  clipRect := AABBToClipRect(aabb, mvp, ARci.viewPortSize.cx,
    ARci.viewPortSize.cy);

  if (clipRect.Right < 0) or (clipRect.Left > ARci.viewPortSize.cx)
    or (clipRect.Top < 0) or (clipRect.Bottom > ARci.viewPortSize.cy) then
  begin
    Result := False;
    Exit;
  end;

  with clipRect do
  begin
    box[0] := Round(Left);
    box[1] := Round(Top);
    box[2] := Round(Right - Left);
    box[3] := Round(Bottom - Top);
  end;
  ARci.GLStates.ScissorBox := box;

  Result := True;
end;

// ------------------
// ------------------ TGLShadowVolumeCasters ------------------
// ------------------

// RemoveNotification
//

procedure TGLShadowVolumeCasters.RemoveNotification(aComponent: TComponent);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    Items[i].RemoveNotification(aComponent);
end;

// GetItems
//

function TGLShadowVolumeCasters.GetItems(index: Integer): TGLShadowVolumeCaster;
begin
  Result := TGLShadowVolumeCaster(inherited Items[index]);
end;

// AddCaster
//

function TGLShadowVolumeCasters.AddCaster(obj: TGLBaseSceneObject;
  effectiveRadius: Single = 0;
  CastingMode: TGLShadowCastingMode = scmRecursivelyVisible):
  TGLShadowVolumeCaster;
var
  newCaster: TGLShadowVolumeCaster;
begin
  newCaster := TGLShadowVolumeCaster(Add);
  newCaster.Caster := obj;
  newCaster.EffectiveRadius := effectiveRadius;
  newCaster.CastingMode := CastingMode;

  result := newCaster;
end;

// RemoveCaster
//

procedure TGLShadowVolumeCasters.RemoveCaster(obj: TGLBaseSceneObject);
var
  i: Integer;
begin
  i := IndexOfCaster(obj);
  if i >= 0 then
    Delete(i);
end;

// IndexOfCaster
//

function TGLShadowVolumeCasters.IndexOfCaster(obj: TGLBaseSceneObject): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].Caster = obj then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

// ------------------
// ------------------ TGLShadowVolume ------------------
// ------------------

// Create
//

constructor TGLShadowVolume.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDeferredDraw, osNoVisibilityCulling];
  FActive := True;
  FLights := TGLShadowVolumeCasters.Create(self, TGLShadowVolumeLight);
  FOccluders := TGLShadowVolumeCasters.Create(self, TGLShadowVolumeOccluder);
  FCapping := svcAlways;
  FMode := svmAccurate;
  FOptions := [svoCacheSilhouettes, svoScissorClips];
  FDarkeningColor := TGLColor.CreateInitialized(Self, VectorMake(0, 0, 0, 0.5));
  FPart1.CustomDraw := TurnOffLights;
  FPart2.CustomDraw := BeforeShadowDraw;
  FPart3.CustomDraw := AfterShadowDraw;
  FOpaques := TList.Create;
  FOpaqueCapping := TList.Create;
end;

// Destroy
//

destructor TGLShadowVolume.Destroy;
begin
  inherited;
  FDarkeningColor.Free;
  FLights.Free;
  FOccluders.Free;
  FOpaques.Free;
  FOpaqueCapping.Free;
end;

// Notification
//

procedure TGLShadowVolume.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if Operation = opRemove then
  begin
    FLights.RemoveNotification(AComponent);
    FOccluders.RemoveNotification(AComponent);
  end;
  inherited;
end;

// Assign
//

procedure TGLShadowVolume.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLShadowVolume) then
  begin
    FLights.Assign(TGLShadowVolume(Source).Lights);
    FOccluders.Assign(TGLShadowVolume(Source).Occluders);
    FCapping := TGLShadowVolume(Source).FCapping;
    StructureChanged;
  end;
  inherited Assign(Source);
end;

// FlushSilhouetteCache
//

procedure TGLShadowVolume.FlushSilhouetteCache;
var
  i: Integer;
begin
  for i := 0 to Lights.Count - 1 do
    (Lights[i] as TGLShadowVolumeLight).FlushSilhouetteCache;
end;

// SetActive
//

procedure TGLShadowVolume.SetActive(const val: Boolean);
begin
  if FActive <> val then
  begin
    FActive := val;
    StructureChanged;
  end;
end;

// SetLights
//

procedure TGLShadowVolume.SetLights(const val: TGLShadowVolumeCasters);
begin
  Assert(val.ItemClass = TGLShadowVolumeLight);
  FLights.Assign(val);
  StructureChanged;
end;

// SetOccluders
//

procedure TGLShadowVolume.SetOccluders(const val: TGLShadowVolumeCasters);
begin
  Assert(val.ItemClass = TGLShadowVolumeOccluder);
  FOccluders.Assign(val);
  StructureChanged;
end;

// SetOptions
//

procedure TGLShadowVolume.SetOptions(const val: TGLShadowVolumeOptions);
begin
  if FOptions <> val then
  begin
    FOptions := val;
    if not (svoCacheSilhouettes in FOptions) then
      FlushSilhouetteCache;
    StructureChanged;
  end;
end;

// SetMode
//

procedure TGLShadowVolume.SetMode(const val: TGLShadowVolumeMode);
begin
  if FMode <> val then
  begin
    FMode := val;
    StructureChanged;
  end;
end;

// SetDarkeningColor
//

procedure TGLShadowVolume.SetDarkeningColor(const val: TGLColor);
begin
  FDarkeningColor.Assign(val);
end;


// DoRender
//

procedure TGLShadowVolume.DoRender(var ARci: TRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);

// Function that determines if an object is "recursively visible". It halts when
// * it finds an invisible ancestor (=> invisible)
// * it finds the root (=> visible)
// * it finds the shadow volume as an ancestor (=> visible)
//
// This does _not_ mean that the object is actually visible on the screen

  function DirectHierarchicalVisibility(obj: TGLBaseSceneObject): boolean;
  var
    p: TGLBaseSceneObject;
  begin
    if not Assigned(obj) then
    begin
      Result := True;
      exit;
    end;
    if not obj.Visible then
    begin
      Result := False;
      Exit;
    end;
    p := obj.Parent;
    while Assigned(p) and (p <> obj) and (p <> Self) do
    begin
      if not p.Visible then
      begin
        Result := False;
        Exit;
      end;
      p := p.Parent;
    end;
    Result := True;
  end;

var
  i: Integer;
  obj: TGLBaseSceneObject;
  caster: TGLShadowVolumeCaster;
begin
  if Active and ARenderSelf
    and (not (csDesigning in ComponentState) or (svoDesignVisible in Options))
    and (Mode <> svmOff) and (ARci.drawState <> dsPicking)
    and not FRendering then
  begin
    // Collect shadow casters
    FOpaques.Count := 0;
    FOpaqueCapping.Count := 0;
    FLightIteration := 0;

    for i := 0 to Occluders.Count - 1 do
    begin
      caster := Occluders[i];
      obj := caster.Caster;
      if Assigned(obj)
        and
        // Determine when to render this object or not
      (
        (Caster.CastingMode = scmAlways) or
        ((Caster.CastingMode = scmVisible) and obj.Visible) or
        ((Caster.CastingMode = scmRecursivelyVisible) and
        DirectHierarchicalVisibility(obj)) or
        ((Caster.CastingMode = scmParentRecursivelyVisible) and
        DirectHierarchicalVisibility(obj.Parent)) or
        ((Caster.CastingMode = scmParentVisible) and (not Assigned(obj.Parent)
        or
        obj.Parent.Visible))
        )
        and ((caster.EffectiveRadius <= 0)
        or (obj.DistanceTo(ARci.cameraPosition) < caster.EffectiveRadius)) then
      begin
        FOpaques.Add(obj);
        FOpaqueCapping.Add(Pointer(PtrUInt(ord((caster.Capping = svcAlways)
          or ((caster.Capping = svcDefault)
          and (Capping = svcAlways))))));
      end
      else
      begin
        FOpaques.Add(nil);
        FOpaqueCapping.Add(nil);
      end;
    end;

    FTransformation := ARci.PipelineTransformation.StackTop;
    FRendering := True;
    // turn off lights
    if Mode = svmAccurate then
      ARci.drawList.Add(@FPart1);
    // render shadow receivers with ambient lighting

    if ARenderChildren then
      RenderChildren(0, Count - 1, ARci);
    for I := 0 to Lights.Count - 1 do
    begin
      with TGLShadowVolumeLight(Lights[I]) do
        if (not Assigned(LightSource)) or (not LightSource.Shining) then
          Continue;
      if ARenderChildren and (Mode = svmAccurate) then
        RenderChildren(0, Count - 1, ARci);
      ARci.drawList.Add(@FPart2);
    end;
    if Lights.Count > 0 then
      ARci.drawList.Add(@FPart3);
    FRendering := False;
  end
  else if ARenderChildren then
    RenderChildren(0, Count - 1, ARci);
end;

procedure TGLShadowVolume.TurnOffLights(var ARci: TRenderContextInfo);
var
  I: Integer;
  lightSource: TGLLightSource;
  lightCaster: TGLShadowVolumeLight;
begin
  with ARci.GLStates do
  begin
    // first turn off all the shadow casting lights diffuse and specular
    for I := 0 to Lights.Count - 1 do
    begin
      lightCaster := TGLShadowVolumeLight(Lights[i]);
      lightSource := lightCaster.LightSource;
      if Assigned(lightSource) and lightSource.Shining then
      begin
        LightDiffuse[lightSource.LightID] := NullHmgVector;
        LightSpecular[lightSource.LightID] := NullHmgVector;
      end;
    end;
  end;
end;

procedure TGLShadowVolume.BeforeShadowDraw(var ARci: TRenderContextInfo);
var
  i, k: Integer;
  silParams: TGLSilhouetteParameters;
  sil: TGLSilhouette;
  lightSource: TGLLightSource;
  lightCaster: TGLShadowVolumeLight;
  obj: TGLBaseSceneObject;
  PM: TMatrix;
  lightID: Integer;
begin

  if FLightIteration = 0 then
  begin
    if svoWorldScissorClip in Options then
    begin
      // compute shadow receiving world AABB in absolute coordinates
      FWorldAABB := AxisAlignedBoundingBox;
      AABBTransform(FWorldAABB, AbsoluteMatrix);
    end;
    // turn off *all* lights
    for I := 0 to TGLScene(ARci.scene).Lights.Count - 1 do
    begin
      lightSource := (TGLScene(ARci.scene).Lights.Items[I]) as TGLLightSource;
      if Assigned(lightSource) and lightSource.Shining then
        ARci.GLStates.LightEnabling[lightSource.LightID] := False;
    end;

    GL.LightModelfv(GL_LIGHT_MODEL_AMBIENT, @NullHmgPoint);
  end
  else
  begin
    // disable light, but restore its ambient component
    lightCaster := TGLShadowVolumeLight(lights[FLightIteration - 1]);
    lightSource := lightCaster.LightSource;
    lightID := lightSource.LightID;
    ARci.GLStates.LightEnabling[lightID] := False;
    ARci.GLStates.LightAmbient[lightID] := lightSource.Ambient.Color;
  end;

  ARci.ignoreBlendingRequests := True;
  ARci.ignoreDepthRequests := True;
  // render the shadow volumes
  with ARci.GLStates do
  begin
    DepthWriteMask := False;
    Enable(stDepthTest);
    SetBlendFunc(bfSrcAlpha, bfOne);
    Disable(stAlphaTest);
    Enable(stStencilTest);

    // render contribution of all shadow casting lights
    lightCaster := TGLShadowVolumeLight(lights[FLightIteration]);
    lightSource := lightCaster.LightSource;
    lightID := lightSource.LightID;

    SetVector(silParams.LightDirection, lightSource.SpotDirection.DirectVector);
    case lightSource.LightStyle of
      lsParallel, lsParallelSpot: silParams.Style := ssParallel
    else
      silParams.Style := ssOmni;
    end;
    silParams.CappingRequired := True;

    if (svoWorldScissorClip in Options) or (svoScissorClips in Options) then
    begin
      if lightCaster.SetupScissorRect(FWorldAABB, ARci) then
        Enable(stScissorTest)
      else
        Disable(stScissorTest);
    end;

    // clear the stencil and prepare for shadow volume pass
    GL.Clear(GL_STENCIL_BUFFER_BIT);
    SetStencilFunc(cfAlways, 0, 255);
    DepthFunc := cfLess;

    if svoShowVolumes in Options then
    begin
      GL.Color3f(0.05 * FLightIteration, 0.1, 0);
      Enable(stBlend);
    end
    else
    begin
      SetGLColorWriting(False);
      Disable(stBlend);
    end;
    Enable(stCullFace);

    Disable(stLighting);
    ARci.GLStates.CurrentProgram := 0;

    // Disable all client states
    if GL.ARB_vertex_buffer_object then
    begin
      VertexArrayBinding := 0;
      ArrayBufferBinding := 0;
      ElementBufferBinding := 0;
    end;
    for I := 15 downto 0 do
      GL.DisableVertexAttribArray(I);

    GL.EnableClientState(GL_VERTEX_ARRAY);
    SetPolygonOffset(1, 1);

    ARci.PipelineTransformation.StackTop := FTransformation;

    // for all opaque shadow casters
    for k := 0 to FOpaques.Count - 1 do
    begin
      obj := TGLBaseSceneObject(FOpaques[k]);
      if obj = nil then
        Continue;

      SetVector(silParams.SeenFrom,
        obj.AbsoluteToLocal(lightSource.AbsolutePosition));

      sil := lightCaster.GetCachedSilhouette(k);
      if (not Assigned(sil)) or (not CompareMem(@sil.Parameters, @silParams,
        SizeOf(silParams))) then
      begin
        sil := obj.GenerateSilhouette(silParams);
        sil.Parameters := silParams;
        // extrude vertices to infinity
        sil.ExtrudeVerticesToInfinity(silParams.SeenFrom);
      end;
      if Assigned(sil) then
        try
          // render the silhouette
          ARci.PipelineTransformation.ModelMatrix := obj.AbsoluteMatrix;
          GL.VertexPointer(4, GL_FLOAT, 0, sil.Vertices.List);

          if GL.GREMEDY_frame_terminator then
            GL.FrameTerminatorGREMEDY();

          if Assigned(FOpaqueCapping[k]) then
          begin
            // z-fail

            CullFaceMode := cmFront;
            SetStencilOp(soKeep, soIncr, soKeep);

            with sil do
            begin
              GL.DrawElements(GL_QUADS, Indices.Count, GL_UNSIGNED_INT,
                Indices.List);
              Enable(stPolygonOffsetFill);
              GL.DrawElements(GL_TRIANGLES, CapIndices.Count,
                GL_UNSIGNED_INT,
                CapIndices.List);
              Disable(stPolygonOffsetFill);
            end;

            CullFaceMode := cmBack;
            SetStencilOp(soKeep, soDecr, soKeep);

            with sil do
            begin
              GL.DrawElements(GL_QUADS, Indices.Count, GL_UNSIGNED_INT,
                Indices.List);
              Enable(stPolygonOffsetFill);
              GL.DrawElements(GL_TRIANGLES, CapIndices.Count,
                GL_UNSIGNED_INT,
                CapIndices.List);
              Disable(stPolygonOffsetFill);
            end;

          end
          else
          begin
            // z-pass
            CullFaceMode := cmBack;
            SetStencilOp(soKeep, soKeep, soIncr);

            GL.DrawElements(GL_QUADS, sil.Indices.Count, GL_UNSIGNED_INT,
              sil.Indices.List);

            CullFaceMode := cmFront;
            SetStencilOp(soKeep, soKeep, soDecr);

            GL.DrawElements(GL_QUADS, sil.Indices.Count, GL_UNSIGNED_INT,
              sil.Indices.List);
          end;

        finally
          if (svoCacheSilhouettes in Options) and (not (osDirectDraw in
            ObjectStyle)) then
            lightCaster.StoreCachedSilhouette(k, sil)
          else
            sil.Free;
        end;
    end; // for k

    GL.DisableClientState(GL_VERTEX_ARRAY);

    // re-enable light's diffuse and specular, but no ambient
    LightEnabling[LightID] := True;
    LightAmbient[LightID] := NullHmgVector;
    LightDiffuse[LightID] := lightSource.Diffuse.Color;
    LightSpecular[LightID] := lightSource.Specular.Color;

    SetGLColorWriting(True);
    SetStencilOp(soKeep, soKeep, soKeep);

    Enable(stBlend);

    CullFaceMode := cmBack;

    if Mode = svmAccurate then
    begin
      SetStencilFunc(cfEqual, 0, 255);
      DepthFunc := cfEqual;
    end
    else
      with GL do
      begin
        SetStencilFunc(cfNotEqual, 0, 255);

        ARci.GLStates.DepthFunc := cfAlways;
        SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);

        MatrixMode(GL_PROJECTION);
        PM := CreateOrthoMatrix(0, 1, 1, 0, -1, 1);
        LoadMatrixf(PGLFloat(@PM));

        Color4fv(FDarkeningColor.AsAddress);
        Begin_(GL_QUADS);
        Vertex2f(0, 0);
        Vertex2f(0, 1);
        Vertex2f(1, 1);
        Vertex2f(1, 0);
        End_;

        MatrixMode(GL_MODELVIEW);
        SetBlendFunc(bfSrcAlpha, bfOne);
      end;
  end; // with with ARci.GLStates

  Inc(FLightIteration);
end;

procedure TGLShadowVolume.AfterShadowDraw(var ARci: TRenderContextInfo);
begin
  // restore OpenGL state
  GL.LightModelfv(GL_LIGHT_MODEL_AMBIENT, @ARci.sceneAmbientColor);
  ARci.PipelineTransformation.StackTop := FTransformation;
  Scene.SetupLights(ARci.GLStates.MaxLights);
  ARci.GLStates.Disable(stStencilTest);
  ARci.GLStates.Disable(stScissorTest);
  ARci.GLStates.SetPolygonOffset(0, 0);
  ARci.ignoreBlendingRequests := False;
  ARci.ignoreDepthRequests := False;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

initialization
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  RegisterClasses([TGLShadowVolume]);

end.
