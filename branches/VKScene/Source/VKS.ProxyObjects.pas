//
// VKScene project, http://glscene.sourceforge.net 
//
{
   Implements specific proxying classes. 
    
}
unit VKS.ProxyObjects;

interface

{$I VKScene.inc}

uses
  System.Classes, System.SysUtils,

  VKS.Scene, VKS.VectorGeometry, VKS.Texture, VKS.VectorFileObjects,
  VKS.Strings, VKS.RenderContextInfo, VKS.BaseClasses, VKS.Material,
  Winapi.OpenGL, Winapi.OpenGLext,  VKS.Context, VKS.VectorTypes;

type
  EGLProxyException = class(Exception);

  // TVKColorProxy
  //
  { A proxy object with its own color. 
     This proxy object can have a unique color. Note that multi-material
     objects (Freeforms linked to a material library f.i.) won't honour
     the color. }
  TVKColorProxy = class(TVKProxyObject)
  private
    { Private Declarations }
    FFrontColor: TVKFaceProperties;
    function GetMasterMaterialObject: TVKCustomSceneObject;
    procedure SetMasterMaterialObject(const Value: TVKCustomSceneObject);
    procedure SetFrontColor(AValue: TVKFaceProperties);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoRender(var ARci: TVKRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
  published
    { Published Declarations }
    property FrontColor: TVKFaceProperties read FFrontColor write
      SetFrontColor;
    // Redeclare as TVKCustomSceneObject.
    property MasterObject: TVKCustomSceneObject read GetMasterMaterialObject
      write SetMasterMaterialObject;
  end;

  // TVKMaterialProxy
  //
  { A proxy object with its own material. 
     This proxy object can take a mesh from one master and a materia from
     a material library. }
  TVKMaterialProxy = class(TVKProxyObject, IGLMaterialLibrarySupported)
  private
    { Private Declarations }
    FTempLibMaterialName: string;
    FMasterLibMaterial: TVKLibMaterial;
    FMaterialLibrary: TVKMaterialLibrary;
    procedure SetMaterialLibrary(const Value: TVKMaterialLibrary);
    function GetMasterLibMaterialName: TVKLibMaterialName;
    procedure SetMasterLibMaterialName(const Value: TVKLibMaterialName);
    function GetMasterMaterialObject: TVKCustomSceneObject;
    procedure SetMasterMaterialObject(const Value: TVKCustomSceneObject);
    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TVKAbstractMaterialLibrary;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    destructor Destroy; override;

    procedure DoRender(var ARci: TVKRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    { Specifies the Material, that current master object will use.
       Provides a faster way to access FMasterLibMaterial, compared to
       MasterLibMaterialName }
    property MasterLibMaterial: TVKLibMaterial read FMasterLibMaterial write
      FMasterLibMaterial stored False;
  published
    { Published Declarations }
    property MaterialLibrary: TVKMaterialLibrary read FMaterialLibrary write
      SetMaterialLibrary;
    { Specifies the Material, that current master object will use. }
    property MasterLibMaterialName: TVKLibMaterialName read
      GetMasterLibMaterialName write SetMasterLibMaterialName;
    { Redeclare as TVKCustomSceneObject. }
    property MasterObject: TVKCustomSceneObject read GetMasterMaterialObject
      write SetMasterMaterialObject;
  end;

  // TVKFreeFormProxy
  //
  { A proxy object specialized for FreeForms.  }
  TVKFreeFormProxy = class(TVKProxyObject)
  private
    function GetMasterFreeFormObject: TVKFreeForm;
    procedure SetMasterFreeFormObject(const Value: TVKFreeForm);
  protected
    { Protected Declarations }

  public
    { Public Declarations }

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
    { Published Declarations }
   // Redeclare as TVKFreeForm.
    property MasterObject: TVKFreeForm read GetMasterFreeFormObject write
      SetMasterFreeFormObject;
  end;

  // TBoneMatrixObj
  //
  { An object containing the bone matrix for TVKActorProxy.  }
  TBoneMatrixObj = class
  public
    Matrix: TMatrix;
    BoneName: string;
    BoneIndex: integer;
  end;

  // pamLoop mode was too difficalt to implement, so it was discarded ...for now.
  // pamPlayOnce only works if Actor.AnimationMode <> aamNone.
  TVKActorProxyAnimationMode = (pamInherited, pamNone, pamPlayOnce);

  // TVKActorProxy
  //
  { A proxy object specialized for Actors.  }
  TVKActorProxy = class(TVKProxyObject, IGLMaterialLibrarySupported)
  private
    { Private Declarations }
    FCurrentFrame: Integer;
    FStartFrame: Integer;
    FEndFrame: Integer;
    FLastFrame: Integer;
    FCurrentFrameDelta: Single;
    FCurrentTime: TProgressTimes;
    FAnimation: TVKActorAnimationName;

    FTempLibMaterialName: string;
    FMasterLibMaterial: TVKLibMaterial;
    FMaterialLibrary: TVKMaterialLibrary;

    FBonesMatrices: TStringList;
    FStoreBonesMatrix: boolean;
    FStoredBoneNames: TStrings;
    FOnBeforeRender: TVKProgressEvent;
    FAnimationMode: TVKActorProxyAnimationMode;

    procedure SetAnimation(const Value: TVKActorAnimationName);
    procedure SetMasterActorObject(const Value: TVKActor);
    function GetMasterActorObject: TVKActor;
    function GetLibMaterialName: TVKLibMaterialName;
    procedure SetLibMaterialName(const Value: TVKLibMaterialName);
    procedure SetMaterialLibrary(const Value: TVKMaterialLibrary);
    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TVKAbstractMaterialLibrary;
    procedure SetStoreBonesMatrix(const Value: boolean);
    procedure SetStoredBoneNames(const Value: TStrings);
    procedure SetOnBeforeRender(const Value: TVKProgressEvent);
  protected
    { Protected Declarations }
    procedure DoStoreBonesMatrices;
      // stores matrices of bones of the current frame rendered
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure DoRender(var ARci: TVKRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    procedure DoProgress(const progressTime: TProgressTimes); override;
    property CurrentFrame: Integer read FCurrentFrame;
    property StartFrame: Integer read FStartFrame;
    property EndFrame: Integer read FEndFrame;
    property CurrentFrameDelta: Single read FCurrentFrameDelta;
    property CurrentTime: TProgressTimes read FCurrentTime;
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
       of course we assume that the two Masterobject Actors have same animations.
      }
    function RayCastIntersectEx(RefActor: TVKActor; const rayStart, rayVector:
      TVector;
      intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean; overload;

  published
    { Published Declarations }
    property AnimationMode: TVKActorProxyAnimationMode read FAnimationMode write
      FAnimationMode default pamInherited;
    property Animation: TVKActorAnimationName read FAnimation write SetAnimation;
    // Redeclare as TVKActor.
    property MasterObject: TVKActor read GetMasterActorObject write
      SetMasterActorObject;
    // Redeclare without pooTransformation
    // (Don't know why it causes the object to be oriented incorrecly.)
    property ProxyOptions default [pooEffects, pooObjects];
    { Specifies the MaterialLibrary, that current proxy will use. }
    property MaterialLibrary: TVKMaterialLibrary read FMaterialLibrary write
      SetMaterialLibrary;
    { Specifies the Material, that current proxy will use. }
    property LibMaterialName: TVKLibMaterialName read GetLibMaterialName write
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
    property OnBeforeRender: TVKProgressEvent read FOnBeforeRender write
      SetOnBeforeRender;
  end;

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------
implementation
// ------------------
// ------------------ TVKColorProxy ------------------
// ------------------

// Create
//

constructor TVKColorProxy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFrontColor := TVKFaceProperties.Create(Self);
end;

// Destroy
//

destructor TVKColorProxy.Destroy;
begin
  FFrontColor.Free;

  inherited Destroy;
end;

// Render
//

procedure TVKColorProxy.DoRender(var ARci: TVKRenderContextInfo;
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
          glMultMatrixf(PGLFloat(MasterObject.MatrixAsAddress));
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

// GetMasterMaterialObject
//

function TVKColorProxy.GetMasterMaterialObject: TVKCustomSceneObject;
begin
  Result := TVKCustomSceneObject(inherited MasterObject);
end;

// SetMasterMaterialObject
//

procedure TVKColorProxy.SetFrontColor(AValue: TVKFaceProperties);
begin
  FFrontColor.Assign(AValue);
end;

procedure TVKColorProxy.SetMasterMaterialObject(
  const Value: TVKCustomSceneObject);
begin
  inherited SetMasterObject(Value);
end;

// ------------------
// ------------------ TVKFreeFormProxy ------------------
// ------------------

// OctreeRayCastIntersect
//

function TVKFreeFormProxy.OctreeRayCastIntersect(const rayStart, rayVector:
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

// OctreeSphereSweepIntersect
//

function TVKFreeFormProxy.OctreeSphereSweepIntersect(const rayStart, rayVector:
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

// GetMasterFreeFormObject
//

function TVKFreeFormProxy.GetMasterFreeFormObject: TVKFreeForm;
begin
  Result := TVKFreeForm(inherited MasterObject);
end;

// SetMasterFreeFormObject
//

procedure TVKFreeFormProxy.SetMasterFreeFormObject(
  const Value: TVKFreeForm);
begin
  inherited SetMasterObject(Value);
end;

// ------------------
// ------------------ TVKActorProxy ------------------
// ------------------

// Create
//

function TVKActorProxy.BoneMatrix(BoneIndex: integer): TMatrix;
begin
  if BoneIndex < FBonesMatrices.count then
    result := TBoneMatrixObj(FBonesMatrices.Objects[BoneIndex]).Matrix;
end;

function TVKActorProxy.BoneMatrix(BoneName: string): TMatrix;
var
  i: Integer;
begin
  i := FBonesMatrices.IndexOf(BoneName);
  if i > -1 then
    result := TBoneMatrixObj(FBonesMatrices.Objects[i]).Matrix;
end;

procedure TVKActorProxy.BoneMatricesClear;
var
  i: Integer;
begin
  for i := 0 to FBonesMatrices.Count - 1 do
  begin
    TBoneMatrixObj(FBonesMatrices.Objects[i]).free;
  end;
  FBonesMatrices.Clear;
end;

constructor TVKActorProxy.Create(AOwner: TComponent);
begin
  inherited;
  FAnimationMode := pamInherited;
  ProxyOptions := ProxyOptions - [pooTransformation];
  FBonesMatrices := TStringList.create;
  FStoredBoneNames := TStringList.create;
  FStoreBonesMatrix := false;
    // default is false to speed up a little if we don't need bones info
end;

// DoProgress
//

destructor TVKActorProxy.Destroy;
begin
  BoneMatricesClear;
  FBonesMatrices.free;
  FStoredBoneNames.free;
  inherited;
end;

procedure TVKActorProxy.DoProgress(const progressTime: TProgressTimes);
begin
  inherited;
  FCurrentTime := progressTime;
end;

// DoRender
//

procedure TVKActorProxy.DoRender(var ARci: TVKRenderContextInfo; ARenderSelf,
  ARenderChildren: Boolean);
var
  // TVKActorProxy specific
  cf, sf, ef: Integer;
  cfd: Single;
  // General proxy stuff.
  gotMaster, masterGotEffects, oldProxySubObject: Boolean;
  MasterActor: TVKActor;
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
            ModelMatrix := MatrixMultiply(MasterActor.Matrix, ModelMatrix);

        // At last TVKActorProxy specific stuff!
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

procedure TVKActorProxy.DoStoreBonesMatrices;
var
  i, n: integer;
  Bmo: TBoneMatrixObj;
  Bone: TVKSkeletonBone;
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

// GetMasterObject
//

function TVKActorProxy.GetMasterActorObject: TVKActor;
begin
  Result := TVKActor(inherited MasterObject);
end;

function TVKActorProxy.GetLibMaterialName: TVKLibMaterialName;
begin
  Result := FMaterialLibrary.GetNameOfLibMaterial(FMasterLibMaterial);
  if Result = '' then
    Result := FTempLibMaterialName;
end;

function TVKActorProxy.GetMaterialLibrary: TVKAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TVKActorProxy.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FMaterialLibrary then
      FMaterialLibrary := nil;
  end;
end;

function TVKActorProxy.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint, intersectNormal: PVector): Boolean;
begin
  if MasterObject <> nil then
    Result := RayCastIntersectEx(GetMasterActorObject, rayStart, rayVector,
      intersectPoint, intersectNormal)
  else
    Result := inherited RayCastIntersect(rayStart, rayVector, intersectPoint,
      intersectNormal);
end;

// Gain access to TVKDummyActor.DoAnimate().
type
  TVKDummyActor = class(TVKActor);

function TVKActorProxy.RayCastIntersectEx(RefActor: TVKActor; const rayStart,
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
    TVKDummyActor(RefActor).DoAnimate();

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
    TVKDummyActor(RefActor).DoAnimate();
  end;
end;

// SetAnimation
//

procedure TVKActorProxy.SetAnimation(const Value: TVKActorAnimationName);
var
  anAnimation: TVKActorAnimation;
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

procedure TVKActorProxy.SetStoredBoneNames(const Value: TStrings);
begin
  if value <> nil then
    FStoredBoneNames.Assign(Value);
end;

// SetMasterObject
//

procedure TVKActorProxy.SetMasterActorObject(const Value: TVKActor);
begin
  inherited SetMasterObject(Value);
  BoneMatricesClear;
end;

procedure TVKActorProxy.SetLibMaterialName(
  const Value: TVKLibMaterialName);
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

procedure TVKActorProxy.SetMaterialLibrary(const Value: TVKMaterialLibrary);
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

procedure TVKActorProxy.SetOnBeforeRender(const Value: TVKProgressEvent);
begin
  FOnBeforeRender := Value;
end;

procedure TVKActorProxy.SetStoreBonesMatrix(const Value: boolean);
begin
  FStoreBonesMatrix := Value;
end;

{ TVKMaterialProxy }

constructor TVKMaterialProxy.Create(AOwner: TComponent);
begin
  inherited;
  // Nothing here.
end;

destructor TVKMaterialProxy.Destroy;
begin
  // Nothing here.
  inherited;
end;

procedure TVKMaterialProxy.DoRender(var ARci: TVKRenderContextInfo;
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
          glMultMatrixf(PGLFloat(MasterObject.MatrixAsAddress));

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

function TVKMaterialProxy.GetMasterLibMaterialName: TVKLibMaterialName;
begin
  Result := FMaterialLibrary.GetNameOfLibMaterial(FMasterLibMaterial);
  if Result = '' then
    Result := FTempLibMaterialName;
end;

function TVKMaterialProxy.GetMasterMaterialObject: TVKCustomSceneObject;
begin
  Result := TVKCustomSceneObject(inherited MasterObject);
end;

function TVKMaterialProxy.GetMaterialLibrary: TVKAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TVKMaterialProxy.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FMaterialLibrary then
      FMaterialLibrary := nil;
  end;
end;

procedure TVKMaterialProxy.SetMasterLibMaterialName(
  const Value: TVKLibMaterialName);
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

procedure TVKMaterialProxy.SetMasterMaterialObject(
  const Value: TVKCustomSceneObject);
begin
  inherited SetMasterObject(Value);
end;

procedure TVKMaterialProxy.SetMaterialLibrary(
  const Value: TVKMaterialLibrary);
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
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  RegisterClasses([TVKColorProxy, TVKFreeFormProxy, TVKActorProxy,
    TVKMaterialProxy]);

end.

