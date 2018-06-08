//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Implements specific proxying classes. 
    
}
unit VXS.ProxyObjects;

interface

{$I VXScene.inc}

uses
  System.Classes,
  System.SysUtils,

  VXS.OpenGL,
  VXS.Scene,
  VXS.XCollection,
  VXS.PersistentClasses,
  VXS.VectorGeometry,
  VXS.Texture,
  VXS.VectorFileObjects,
  VXS.Strings,
  VXS.RenderContextInfo,
  VXS.BaseClasses,
  VXS.Material,
  VXS.Context,
  VXS.PipelineTransformation,
  VXS.VectorTypes;

type
  EGLProxyException = class(Exception);

  { A proxy object with its own color.
     This proxy object can have a unique color. Note that multi-material
     objects (Freeforms linked to a material library f.i.) won't honour
     the color. }
  TVXColorProxy = class(TVXProxyObject)
  private
    
    FFrontColor: TVXFaceProperties;
    function GetMasterMaterialObject: TVXCustomSceneObject;
    procedure SetMasterMaterialObject(const Value: TVXCustomSceneObject);
    procedure SetFrontColor(AValue: TVXFaceProperties);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TVXRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
  published
    property FrontColor: TVXFaceProperties read FFrontColor write
      SetFrontColor;
    // Redeclare as TVXCustomSceneObject.
    property MasterObject: TVXCustomSceneObject read GetMasterMaterialObject
      write SetMasterMaterialObject;
  end;

  { A proxy object with its own material.
     This proxy object can take a mesh from one master and a materia from
     a material library. }
  TVXMaterialProxy = class(TVXProxyObject, IVXMaterialLibrarySupported)
  private
    FTempLibMaterialName: string;
    FMasterLibMaterial: TVXLibMaterial;
    FMaterialLibrary: TVXMaterialLibrary;
    procedure SetMaterialLibrary(const Value: TVXMaterialLibrary);
    function GetMasterLibMaterialName: TVXLibMaterialName;
    procedure SetMasterLibMaterialName(const Value: TVXLibMaterialName);
    function GetMasterMaterialObject: TVXCustomSceneObject;
    procedure SetMasterMaterialObject(const Value: TVXCustomSceneObject);
    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TVXAbstractMaterialLibrary;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TVXRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    { Specifies the Material, that current master object will use.
       Provides a faster way to access FMasterLibMaterial, compared to
       MasterLibMaterialName }
    property MasterLibMaterial: TVXLibMaterial read FMasterLibMaterial write
      FMasterLibMaterial stored False;
  published
    
    property MaterialLibrary: TVXMaterialLibrary read FMaterialLibrary write
      SetMaterialLibrary;
    { Specifies the Material, that current master object will use. }
    property MasterLibMaterialName: TVXLibMaterialName read
      GetMasterLibMaterialName write SetMasterLibMaterialName;
    { Redeclare as TVXCustomSceneObject. }
    property MasterObject: TVXCustomSceneObject read GetMasterMaterialObject
      write SetMasterMaterialObject;
  end;

  { A proxy object specialized for FreeForms.  }
  TVXFreeFormProxy = class(TVXProxyObject)
  private
    function GetMasterFreeFormObject: TVXFreeForm;
    procedure SetMasterFreeFormObject(const Value: TVXFreeForm);
  public
    { If the MasterObject is a FreeForm, you can raycast against the Octree,
       which is alot faster.  You must build the octree before using. }
    function OctreeRayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean;
    { WARNING: This function is not yet 100% reliable with scale+rotation. }
    function OctreeSphereSweepIntersect(const rayStart, rayVector: TVector;
      const velocity, radius, modelscale: Single;
      intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean;
  published
   // Redeclare as TVXFreeForm.
    property MasterObject: TVXFreeForm read GetMasterFreeFormObject write
      SetMasterFreeFormObject;
  end;

  { An object containing the bone matrix for TVXActorProxy.  }
  TBoneMatrixObj = class
  public
    Matrix: TMatrix;
    BoneName: string;
    BoneIndex: integer;
  end;

  // pamLoop mode was too difficalt to implement, so it was discarded ...for now.
  // pamPlayOnce only works if Actor.AnimationMode <> aamNone.
  TVXActorProxyAnimationMode = (pamInherited, pamNone, pamPlayOnce);

  { A proxy object specialized for Actors.  }
  TVXActorProxy = class(TVXProxyObject, IVXMaterialLibrarySupported)
  private
    FCurrentFrame: Integer;
    FStartFrame: Integer;
    FEndFrame: Integer;
    FLastFrame: Integer;
    FCurrentFrameDelta: Single;
    FCurrentTime: TVXProgressTimes;
    FAnimation: TVXActorAnimationName;
    FTempLibMaterialName: string;
    FMasterLibMaterial: TVXLibMaterial;
    FMaterialLibrary: TVXMaterialLibrary;
    FBonesMatrices: TStringList;
    FStoreBonesMatrix: boolean;
    FStoredBoneNames: TStrings;
    FOnBeforeRender: TVXProgressEvent;
    FAnimationMode: TVXActorProxyAnimationMode;
    procedure SetAnimation(const Value: TVXActorAnimationName);
    procedure SetMasterActorObject(const Value: TVXActor);
    function GetMasterActorObject: TVXActor;
    function GetLibMaterialName: TVXLibMaterialName;
    procedure SetLibMaterialName(const Value: TVXLibMaterialName);
    procedure SetMaterialLibrary(const Value: TVXMaterialLibrary);
    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TVXAbstractMaterialLibrary;
    procedure SetStoreBonesMatrix(const Value: boolean);
    procedure SetStoredBoneNames(const Value: TStrings);
    procedure SetOnBeforeRender(const Value: TVXProgressEvent);
  protected
    procedure DoStoreBonesMatrices;
      // stores matrices of bones of the current frame rendered
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure DoRender(var ARci: TVXRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    procedure DoProgress(const progressTime: TVXProgressTimes); override;
    property CurrentFrame: Integer read FCurrentFrame;
    property StartFrame: Integer read FStartFrame;
    property EndFrame: Integer read FEndFrame;
    property CurrentFrameDelta: Single read FCurrentFrameDelta;
    property CurrentTime: TVXProgressTimes read FCurrentTime;
    { Gets the Bones Matrix in the current animation frame.
     (since the masterobject is shared between all proxies, each proxy will have it's bones matrices) }
    function BoneMatrix(BoneIndex: integer): TMatrix; overload;
    function BoneMatrix(BoneName: string): TMatrix; overload;
    procedure BoneMatricesClear;
    { A standard version of the RayCastIntersect function. }
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean; override;
    { Raycasts on self, but actually on the "RefActor" Actor.
       Note that the "RefActor" parameter does not necessarily have to be
       the same Actor refernced by the MasterObject property:
       This allows to pass a low-low-low-poly Actor to raycast in the "RefActor" parameter,
       while using a high-poly Actor in the "MasterObject" property,
       of course we assume that the two Masterobject Actors have same animations. }
    function RayCastIntersectEx(RefActor: TVXActor; const rayStart, rayVector:
      TVector;
      intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean; overload;
  published
    property AnimationMode: TVXActorProxyAnimationMode read FAnimationMode write
      FAnimationMode default pamInherited;
    property Animation: TVXActorAnimationName read FAnimation write SetAnimation;
    // Redeclare as TVXActor.
    property MasterObject: TVXActor read GetMasterActorObject write
      SetMasterActorObject;
    // Redeclare without pooTransformation
    // (Don't know why it causes the object to be oriented incorrecly.)
    property ProxyOptions default [pooEffects, pooObjects];
    { Specifies the MaterialLibrary, that current proxy will use. }
    property MaterialLibrary: TVXMaterialLibrary read FMaterialLibrary write
      SetMaterialLibrary;
    { Specifies the Material, that current proxy will use. }
    property LibMaterialName: TVXLibMaterialName read GetLibMaterialName write
      SetLibMaterialName;
    { Specifies if it will store the Bones Matrices, accessible via the BoneMatrix function
     (since the masterobject is shared between all proxies, each proxy will have it's bones matrices) }
    property StoreBonesMatrix: boolean read FStoreBonesMatrix write
      SetStoreBonesMatrix;
    { Specifies the names of the bones we want the matrices to be stored. If empty, all bones will be stored
     (since the masterobject is shared between all proxies, each proxy will have it's bones matrices) }
    property StoredBoneNames: TStrings read FStoredBoneNames write
      SetStoredBoneNames;
    { Event allowing to apply extra transformations (f.ex: bone rotations) to the referenced
       Actor on order to have the proxy render these changes.  }
    property OnBeforeRender: TVXProgressEvent read FOnBeforeRender write
      SetOnBeforeRender;
  end;

//-------------------------------------------------------------
implementation
//-------------------------------------------------------------

// ------------------
// ------------------ TVXColorProxy ------------------
// ------------------

constructor TVXColorProxy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFrontColor := TVXFaceProperties.Create(Self);
end;

destructor TVXColorProxy.Destroy;
begin
  FFrontColor.Free;

  inherited Destroy;
end;

procedure TVXColorProxy.DoRender(var ARci: TVXRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  gotMaster, masterGotEffects, oldProxySubObject: Boolean;
begin
  if FRendering then
    Exit;
  FRendering := True;
  try
    gotMaster := Assigned(MasterObject);
    masterGotEffects := gotMaster and (pooEffects in ProxyOptions)
      and (MasterObject.Effects.Count > 0);
    if gotMaster then
    begin
      if pooObjects in ProxyOptions then
      begin
        oldProxySubObject := ARci.proxySubObject;
        ARci.proxySubObject := True;
        if pooTransformation in ProxyOptions then
          glMultMatrixf(PGLFloat(MasterObject.Matrix));
        GetMasterMaterialObject.Material.FrontProperties.Assign(FFrontColor);
        MasterObject.DoRender(ARci, ARenderSelf, MasterObject.Count > 0);
        ARci.proxySubObject := oldProxySubObject;
      end;
    end;
    // now render self stuff (our children, our effects, etc.)
    if ARenderChildren and (Count > 0) then
      Self.RenderChildren(0, Count - 1, ARci);
    if masterGotEffects then
      MasterObject.Effects.RenderPostEffects(ARci);
  finally
    FRendering := False;
  end;
  ClearStructureChanged;
end;

function TVXColorProxy.GetMasterMaterialObject: TVXCustomSceneObject;
begin
  Result := TVXCustomSceneObject(inherited MasterObject);
end;

procedure TVXColorProxy.SetFrontColor(AValue: TVXFaceProperties);
begin
  FFrontColor.Assign(AValue);
end;

procedure TVXColorProxy.SetMasterMaterialObject(
  const Value: TVXCustomSceneObject);
begin
  inherited SetMasterObject(Value);
end;

// ------------------
// ------------------ TVXFreeFormProxy ------------------
// ------------------

function TVXFreeFormProxy.OctreeRayCastIntersect(const rayStart, rayVector:
  TVector;
  intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): Boolean;
var
  localRayStart, localRayVector: TVector;
begin
  if Assigned(MasterObject) then
  begin
    SetVector(localRayStart, AbsoluteToLocal(rayStart));
    SetVector(localRayStart, MasterObject.LocalToAbsolute(localRayStart));
    SetVector(localRayVector, AbsoluteToLocal(rayVector));
    SetVector(localRayVector, MasterObject.LocalToAbsolute(localRayVector));
    NormalizeVector(localRayVector);

    Result := GetMasterFreeFormObject.OctreeRayCastIntersect(localRayStart,
      localRayVector,
      intersectPoint, intersectNormal);
    if Result then
    begin
      if Assigned(intersectPoint) then
      begin
        SetVector(intersectPoint^,
          MasterObject.AbsoluteToLocal(intersectPoint^));
        SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
      end;
      if Assigned(intersectNormal) then
      begin
        SetVector(intersectNormal^,
          MasterObject.AbsoluteToLocal(intersectNormal^));
        SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      end;
    end;
  end
  else
    Result := False;
end;

function TVXFreeFormProxy.OctreeSphereSweepIntersect(const rayStart, rayVector:
  TVector;
  const velocity, radius, modelscale: Single;
  intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): Boolean;
var
  localRayStart, localRayVector: TVector;
  localVelocity, localRadius: single;
begin
  Result := False;
  if Assigned(MasterObject) then
  begin
    localVelocity := velocity * modelscale;
    localRadius := radius * modelscale;

    SetVector(localRayStart, AbsoluteToLocal(rayStart));
    SetVector(localRayStart, MasterObject.LocalToAbsolute(localRayStart));
    SetVector(localRayVector, AbsoluteToLocal(rayVector));
    SetVector(localRayVector, MasterObject.LocalToAbsolute(localRayVector));
    NormalizeVector(localRayVector);

    Result := GetMasterFreeFormObject.OctreeSphereSweepIntersect(localRayStart,
      localRayVector,
      localVelocity, localRadius,
      intersectPoint, intersectNormal);
    if Result then
    begin
      if Assigned(intersectPoint) then
      begin
        SetVector(intersectPoint^,
          MasterObject.AbsoluteToLocal(intersectPoint^));
        SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
      end;
      if Assigned(intersectNormal) then
      begin
        SetVector(intersectNormal^,
          MasterObject.AbsoluteToLocal(intersectNormal^));
        SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      end;
    end;

  end;
end;

function TVXFreeFormProxy.GetMasterFreeFormObject: TVXFreeForm;
begin
  Result := TVXFreeForm(inherited MasterObject);
end;

procedure TVXFreeFormProxy.SetMasterFreeFormObject(
  const Value: TVXFreeForm);
begin
  inherited SetMasterObject(Value);
end;

// ------------------
// ------------------ TVXActorProxy ------------------
// ------------------

function TVXActorProxy.BoneMatrix(BoneIndex: integer): TMatrix;
begin
  if BoneIndex < FBonesMatrices.count then
    result := TBoneMatrixObj(FBonesMatrices.Objects[BoneIndex]).Matrix;
end;

function TVXActorProxy.BoneMatrix(BoneName: string): TMatrix;
var
  i: Integer;
begin
  i := FBonesMatrices.IndexOf(BoneName);
  if i > -1 then
    result := TBoneMatrixObj(FBonesMatrices.Objects[i]).Matrix;
end;

procedure TVXActorProxy.BoneMatricesClear;
var
  i: Integer;
begin
  for i := 0 to FBonesMatrices.Count - 1 do
  begin
    TBoneMatrixObj(FBonesMatrices.Objects[i]).free;
  end;
  FBonesMatrices.Clear;
end;

constructor TVXActorProxy.Create(AOwner: TComponent);
begin
  inherited;
  FAnimationMode := pamInherited;
  ProxyOptions := ProxyOptions - [pooTransformation];
  FBonesMatrices := TStringList.create;
  FStoredBoneNames := TStringList.create;
  FStoreBonesMatrix := false;
    // default is false to speed up a little if we don't need bones info
end;

destructor TVXActorProxy.Destroy;
begin
  BoneMatricesClear;
  FBonesMatrices.free;
  FStoredBoneNames.free;
  inherited;
end;

procedure TVXActorProxy.DoProgress(const progressTime: TVXProgressTimes);
begin
  inherited;
  FCurrentTime := progressTime;
end;

procedure TVXActorProxy.DoRender(var ARci: TVXRenderContextInfo; ARenderSelf,
  ARenderChildren: Boolean);
var
  // TVXActorProxy specific
  cf, sf, ef: Integer;
  cfd: Single;
  // General proxy stuff.
  gotMaster, masterGotEffects, oldProxySubObject: Boolean;
  MasterActor: TVXActor;
begin
  try
    MasterActor := GetMasterActorObject;
    gotMaster := MasterActor <> nil;
    masterGotEffects := gotMaster and (pooEffects in ProxyOptions) and
      (MasterObject.Effects.Count > 0);
    if gotMaster then
    begin
      if pooObjects in ProxyOptions then
      begin
        oldProxySubObject := ARci.proxySubObject;
        ARci.proxySubObject := True;
        if pooTransformation in ProxyOptions then
          with ARci.PipelineTransformation do
            SetModelMatrix(MatrixMultiply(MasterActor.Matrix^, ModelMatrix^));

        // At last TVXActorProxy specific stuff!
        with MasterActor do
        begin
          cfd := CurrentFrameDelta;
          cf := CurrentFrame;
          sf := startframe;
          ef := endframe;

          case FAnimationMode of
            pamInherited: CurrentFrameDelta := FCurrentFrameDelta;
            pamPlayOnce:
              begin
                if (FLastFrame <> FEndFrame - 1) then
                  CurrentFrameDelta := FCurrentFrameDelta
                else
                begin
                  FCurrentFrameDelta := 0;
                  FAnimationMode := pamNone;
                end;
              end;
            pamNone: CurrentFrameDelta := 0;
          else
            Assert(False, strUnknownType);
          end;

          SetCurrentFrameDirect(FCurrentFrame);
          FLastFrame := FCurrentFrame;
          StartFrame := FStartFrame;
          EndFrame := FEndFrame;

          if (FMasterLibMaterial <> nil) and (FMaterialLibrary <> nil) then
            MasterActor.Material.QuickAssignMaterial(
              FMaterialLibrary, FMasterLibMaterial);

          DoProgress(FCurrentTime);

          if Assigned(FOnBeforeRender) then
            FOnBeforeRender(self, FCurrentTime.deltaTime, FCurrentTime.newTime);

          DoRender(ARci, ARenderSelf, Count > 0);

          // Stores Bones matrices of the current frame
          if (FStoreBonesMatrix) and (MasterActor.Skeleton <> nil) then
            DoStoreBonesMatrices;

          FCurrentFrameDelta := CurrentFrameDelta;
          FCurrentFrame := CurrentFrame;
          CurrentFrameDelta := cfd;
          SetCurrentFrameDirect(cf);
          startframe := sf;
          endframe := ef;
        end;

        ARci.proxySubObject := oldProxySubObject;
      end;
    end;
    // now render self stuff (our children, our effects, etc.)
    oldProxySubObject := ARci.proxySubObject;
    ARci.proxySubObject := True;
    if ARenderChildren and (Count > 0) then
      Self.RenderChildren(0, Count - 1, ARci);
    if masterGotEffects then
      MasterActor.Effects.RenderPostEffects(ARci);
    ARci.proxySubObject := oldProxySubObject;
  finally
    ClearStructureChanged;
  end;
end;

procedure TVXActorProxy.DoStoreBonesMatrices;
var
  i, n: integer;
  Bmo: TBoneMatrixObj;
  Bone: TVXSkeletonBone;
begin
  if FStoredBoneNames.count > 0 then
  begin
    // If we specified some bone names, only those bones matrices will be stored (save some cpu)
    if FBonesMatrices.Count < FStoredBoneNames.Count then
    begin
      n := FBonesMatrices.Count;
      for i := n to FStoredBoneNames.Count - 1 do
      begin
        Bone := MasterObject.Skeleton.BoneByName(FStoredBoneNames[i]);
        if Bone <> nil then
        begin
          Bmo := TBoneMatrixObj.Create;
          Bmo.BoneName := Bone.Name;
          Bmo.BoneIndex := Bone.BoneID;
          FBonesMatrices.AddObject(Bone.Name, Bmo);
        end;

      end;
    end;
  end
  else
  begin
    // Add (missing) TBoneMatrixObjects (actually ony 1st time) from all bones in skeleton
    if FBonesMatrices.Count < MasterObject.Skeleton.BoneCount - 1 then
      // note : BoneCount actually returns 1 count more.
    begin
      n := FBonesMatrices.Count;
      for i := n to MasterObject.Skeleton.BoneCount - 2 do
        // note : BoneCount actually returns 1 count more.
      begin
        Bone := MasterObject.Skeleton.BoneByID(i);
        if Bone <> nil then
        begin
          Bmo := TBoneMatrixObj.Create;
          Bmo.BoneName := Bone.Name;
          Bmo.BoneIndex := Bone.BoneID;
          FBonesMatrices.AddObject(Bone.Name, Bmo);
        end;

      end;
    end;
  end;

  // fill FBonesMatrices list
  for i := 0 to FBonesMatrices.count - 1 do
  begin
    Bmo := TBoneMatrixObj(FBonesMatrices.Objects[i]);
    Bmo.Matrix := MasterObject.Skeleton.BoneByID(Bmo.BoneIndex).GlobalMatrix;
  end;
end;

function TVXActorProxy.GetMasterActorObject: TVXActor;
begin
  Result := TVXActor(inherited MasterObject);
end;

function TVXActorProxy.GetLibMaterialName: TVXLibMaterialName;
begin
  Result := FMaterialLibrary.GetNameOfLibMaterial(FMasterLibMaterial);
  if Result = '' then
    Result := FTempLibMaterialName;
end;

function TVXActorProxy.GetMaterialLibrary: TVXAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TVXActorProxy.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FMaterialLibrary then
      FMaterialLibrary := nil;
  end;
end;

function TVXActorProxy.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint, intersectNormal: PVector): Boolean;
begin
  if MasterObject <> nil then
    Result := RayCastIntersectEx(GetMasterActorObject, rayStart, rayVector,
      intersectPoint, intersectNormal)
  else
    Result := inherited RayCastIntersect(rayStart, rayVector, intersectPoint,
      intersectNormal);
end;

// Gain access to TVXDummyActor.DoAnimate().
type
  TVXDummyActor = class(TVXActor);

function TVXActorProxy.RayCastIntersectEx(RefActor: TVXActor; const rayStart,
  rayVector: TVector; intersectPoint, intersectNormal: PVector): Boolean;
var
  localRayStart, localRayVector: TVector;
  cf, sf, ef: Integer;
  cfd: Single;
  HaspooTransformation: boolean;
begin
  // Set RefObject frame as current ActorProxy frame
  with RefActor do
  begin
    // VARS FOR ACTOR TO ASSUME ACTORPROXY CURRENT ANIMATION FRAME
    cfd := RefActor.CurrentFrameDelta;
    cf := RefActor.CurrentFrame;
    sf := RefActor.startframe;
    ef := RefActor.endframe;
    RefActor.CurrentFrameDelta := self.CurrentFrameDelta;
    RefActor.SetCurrentFrameDirect(self.CurrentFrame);
    RefActor.StartFrame := self.StartFrame;
    RefActor.EndFrame := self.EndFrame;
    RefActor.CurrentFrame := self.CurrentFrame;

    // FORCE ACTOR TO ASSUME ACTORPROXY CURRENT ANIMATION FRAME
    TVXDummyActor(RefActor).DoAnimate();

    HaspooTransformation := pooTransformation in self.ProxyOptions;

    // transform RAYSTART
    SetVector(localRayStart, self.AbsoluteToLocal(rayStart));
    if not HaspooTransformation then
      SetVector(localRayStart, RefActor.LocalToAbsolute(localRayStart));

    // transform RAYVECTOR
    SetVector(localRayVector, self.AbsoluteToLocal(rayVector));
    if not HaspooTransformation then
      SetVector(localRayVector, RefActor.LocalToAbsolute(localRayVector));

    NormalizeVector(localRayVector);

    Result := RefActor.RayCastIntersect(localRayStart, localRayVector,
      intersectPoint, intersectNormal);
    if Result then
    begin
      if Assigned(intersectPoint) then
      begin
        if not HaspooTransformation then
          SetVector(intersectPoint^, RefActor.AbsoluteToLocal(intersectPoint^));
        SetVector(intersectPoint^, self.LocalToAbsolute(intersectPoint^));
      end;
      if Assigned(intersectNormal) then
      begin
        if not HaspooTransformation then
          SetVector(intersectNormal^,
            RefActor.AbsoluteToLocal(intersectNormal^));
        SetVector(intersectNormal^, self.LocalToAbsolute(intersectNormal^));
      end;
    end;

    // Return RefObject to it's old time
    CurrentFrameDelta := cfd;
    SetCurrentFrameDirect(cf);
    CurrentFrame := cf;
    startframe := sf;
    endframe := ef;

    // REVERT ACTOR TO ASSUME ORIGINAL ANIMATION FRAME
    TVXDummyActor(RefActor).DoAnimate();
  end;
end;

procedure TVXActorProxy.SetAnimation(const Value: TVXActorAnimationName);
var
  anAnimation: TVXActorAnimation;
begin
  // We first assign the value (for persistency support), then check it.
  FAnimation := Value;

  if Assigned(MasterObject) then
  begin
    anAnimation := GetMasterActorObject.Animations.FindName(Value);
    if Assigned(anAnimation) then
    begin
      FStartFrame := anAnimation.StartFrame;
      FEndFrame := anAnimation.EndFrame;
      FCurrentFrame := FStartFrame;
      FLastFrame := FCurrentFrame;
    end;
  end;
end;

procedure TVXActorProxy.SetStoredBoneNames(const Value: TStrings);
begin
  if value <> nil then
    FStoredBoneNames.Assign(Value);
end;

procedure TVXActorProxy.SetMasterActorObject(const Value: TVXActor);
begin
  inherited SetMasterObject(Value);
  BoneMatricesClear;
end;

procedure TVXActorProxy.SetLibMaterialName(
  const Value: TVXLibMaterialName);
begin
  if FMaterialLibrary = nil then
  begin
    FTempLibMaterialName := Value;
    if not (csLoading in ComponentState) then
      raise ETexture.Create(strErrorEx + strMatLibNotDefined);
  end
  else
  begin
    FMasterLibMaterial := FMaterialLibrary.LibMaterialByName(Value);
    FTempLibMaterialName := '';
  end;
end;

procedure TVXActorProxy.SetMaterialLibrary(const Value: TVXMaterialLibrary);
begin
  if FMaterialLibrary <> Value then
  begin
    if FMaterialLibrary <> nil then
      FMaterialLibrary.RemoveFreeNotification(Self);
    FMaterialLibrary := Value;

    if FMaterialLibrary <> nil then
    begin
      FMaterialLibrary.FreeNotification(Self);
      if FTempLibMaterialName <> '' then
        SetLibMaterialName(FTempLibMaterialName);
    end
    else
    begin
      FTempLibMaterialName := '';
    end;
  end;
end;

procedure TVXActorProxy.SetOnBeforeRender(const Value: TVXProgressEvent);
begin
  FOnBeforeRender := Value;
end;

procedure TVXActorProxy.SetStoreBonesMatrix(const Value: boolean);
begin
  FStoreBonesMatrix := Value;
end;

{ TVXMaterialProxy }

constructor TVXMaterialProxy.Create(AOwner: TComponent);
begin
  inherited;
  // Nothing here.
end;

destructor TVXMaterialProxy.Destroy;
begin
  // Nothing here.
  inherited;
end;

procedure TVXMaterialProxy.DoRender(var ARci: TVXRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  gotMaster, masterGotEffects, oldProxySubObject: Boolean;
begin
  if FRendering then
    Exit;
  FRendering := True;
  try
    gotMaster := Assigned(MasterObject);
    masterGotEffects := gotMaster and (pooEffects in ProxyOptions)
      and (MasterObject.Effects.Count > 0);
    if gotMaster then
    begin
      if pooObjects in ProxyOptions then
      begin
        oldProxySubObject := ARci.proxySubObject;
        ARci.proxySubObject := True;
        if pooTransformation in ProxyOptions then
          glMultMatrixf(PGLFloat(MasterObject.Matrix));

        if (FMasterLibMaterial <> nil) and (FMaterialLibrary <> nil) then
          GetMasterMaterialObject.Material.QuickAssignMaterial(
            FMaterialLibrary, FMasterLibMaterial);

        MasterObject.DoRender(ARci, ARenderSelf, MasterObject.Count > 0);
        ARci.proxySubObject := oldProxySubObject;
      end;
    end;
    // now render self stuff (our children, our effects, etc.)
    if ARenderChildren and (Count > 0) then
      Self.RenderChildren(0, Count - 1, ARci);
    if masterGotEffects then
      MasterObject.Effects.RenderPostEffects(ARci);
  finally
    FRendering := False;
  end;
  ClearStructureChanged;
end;

function TVXMaterialProxy.GetMasterLibMaterialName: TVXLibMaterialName;
begin
  Result := FMaterialLibrary.GetNameOfLibMaterial(FMasterLibMaterial);
  if Result = '' then
    Result := FTempLibMaterialName;
end;

function TVXMaterialProxy.GetMasterMaterialObject: TVXCustomSceneObject;
begin
  Result := TVXCustomSceneObject(inherited MasterObject);
end;

function TVXMaterialProxy.GetMaterialLibrary: TVXAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TVXMaterialProxy.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FMaterialLibrary then
      FMaterialLibrary := nil;
  end;
end;

procedure TVXMaterialProxy.SetMasterLibMaterialName(
  const Value: TVXLibMaterialName);
begin
  if FMaterialLibrary = nil then
  begin
    FTempLibMaterialName := Value;
    if not (csLoading in ComponentState) then
      raise ETexture.Create(strErrorEx + strMatLibNotDefined);
  end
  else
  begin
    FMasterLibMaterial := FMaterialLibrary.LibMaterialByName(Value);
    FTempLibMaterialName := '';
  end;
end;

procedure TVXMaterialProxy.SetMasterMaterialObject(
  const Value: TVXCustomSceneObject);
begin
  inherited SetMasterObject(Value);
end;

procedure TVXMaterialProxy.SetMaterialLibrary(
  const Value: TVXMaterialLibrary);
begin
  if FMaterialLibrary <> Value then
  begin
    if FMaterialLibrary <> nil then
      FMaterialLibrary.RemoveFreeNotification(Self);
    FMaterialLibrary := Value;

    if FMaterialLibrary <> nil then
    begin
      FMaterialLibrary.FreeNotification(Self);
      if FTempLibMaterialName <> '' then
        SetMasterLibMaterialName(FTempLibMaterialName);
    end
    else
    begin
      FTempLibMaterialName := '';
    end;
  end;
end;

//-------------------------------------------------------------
initialization
//-------------------------------------------------------------

  RegisterClasses([TVXColorProxy, TVXFreeFormProxy, TVXActorProxy,
    TVXMaterialProxy]);

end.

