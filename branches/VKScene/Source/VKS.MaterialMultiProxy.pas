//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Implements a multi-proxy object, useful for discreet LOD. 
   Allows assign a unique material for each proxy master. 

  What changed compared to GLMultiProxy:
    1) Allows assign a unique material for each proxy master
    2) TVKMaterialMultiProxyMaster: FDistanceMin, FDistanceMax removed
    3) TVKMaterialMultiProxy = class(TVKBaseSceneObject)!!!
    4) TVKMaterialMultiProxyMaster.Visible removed
    5) TVKMaterialMultiProxy.MaterialLibrary added
    6) TVKMaterialMultiProxyMaster.MasterLibMaterial added
    7) TVKMaterialMultiProxyMasters.Add overloaded
    8) Implemented a new mechanizm of connecting TVKLibMaterial and TVKLibMaterialName
      (they are connected on assigning, not while rendering; full persistency support;
       allows to assign directly to TVKLibMaterial)
    9) FMX-style code formating

}

unit VKS.MaterialMultiProxy;

interface

{$I VKScene.inc}

uses
  System.Classes, System.SysUtils,
   
  VKS.Scene, VKS.VectorGeometry, VKS.Texture, VKS.Material, VKS.Silhouette, VKS.Strings,
  VKS.CrossPlatform, VKS.PersistentClasses, VKS.RenderContextInfo, VKS.BaseClasses,
  VKS.Context , VKS.VectorTypes;

type

  TVKMaterialMultiProxy = class;

  // TVKMaterialMultiProxyMaster
  //
  { MasterObject description for a MultiProxy object. }
  TVKMaterialMultiProxyMaster = class(TVKInterfacedCollectionItem, IGLMaterialLibrarySupported)
  private
    { Private Declarations }
    FMasterObject: TVKBaseSceneObject;
    FMasterLibMaterial: TVKLibMaterial;
    FTempLibMaterialName: TVKLibMaterialName;
    FDistanceMin2, FDistanceMax2: Single;

    procedure SetMasterLibMaterialName(const Value: TVKLibMaterialName);
    function GetMasterLibMaterialName: TVKLibMaterialName;

    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TVKAbstractMaterialLibrary;
  protected
    { Protected Declarations }
    function GetDisplayName: string; override;
    procedure SetMasterObject(const Val: TVKBaseSceneObject);
    procedure SetDistanceMin(const Val: Single);
    procedure SetDistanceMax(const Val: Single);
    function GetDistanceMin: Single;
    function GetDistanceMax: Single;

  public
    { Public Declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function OwnerObject: TVKMaterialMultiProxy;
    procedure NotifyChange;

    { Specifies the Material, that current master object will use.
       Provides a faster way to access FMasterLibMaterial, compared to
       MasterLibMaterialName }
    property MasterLibMaterial: TVKLibMaterial read FMasterLibMaterial write FMasterLibMaterial stored False;
  published
    { Published Declarations }
    { Specifies the Master object which will be proxy'ed. }
    property MasterObject: TVKBaseSceneObject read FMasterObject write SetMasterObject;
    { Specifies the Material, that current master object will use. }
    property MasterLibMaterialName: TVKLibMaterialName read GetMasterLibMaterialName write SetMasterLibMaterialName;
    { Minimum visibility Distance (inclusive). }
    property DistanceMin: Single read GetDistanceMin write SetDistanceMin;
    { Maximum visibility Distance (exclusive). }
    property DistanceMax: Single read GetDistanceMax write SetDistanceMax;
  end;

  // TVKMaterialMultiProxyMasters
  //
  { Collection of TVKMaterialMultiProxyMaster. }
  TVKMaterialMultiProxyMasters = class(TOwnedCollection)
  private
    { Private Declarations }

  protected
    { Protected Declarations }
    procedure SetItems(index: Integer; const Val: TVKMaterialMultiProxyMaster);
    function GetItems(index: Integer): TVKMaterialMultiProxyMaster;
    procedure Update(Item: TCollectionItem); override;
    procedure Notification(AComponent: TComponent); virtual;
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent);

    function Add: TVKMaterialMultiProxyMaster; overload;
    function Add(Master: TVKBaseSceneObject; DistanceMin, DistanceMax: Single): TVKMaterialMultiProxyMaster; overload;
    function Add(Master: TVKBaseSceneObject; MasterLibMaterial: TVKLibMaterial; DistanceMin, DistanceMax: Single): TVKMaterialMultiProxyMaster; overload;
    property Items[index: Integer]: TVKMaterialMultiProxyMaster read GetItems write SetItems; default;

    procedure NotifyChange;
    procedure EndUpdate; override;
  end;

  // TVKMaterialMultiProxy
  //
   { Multiple Proxy object. 
      This proxy has multiple Master objects, which are individually made visible
      depending on a Distance to the camera criterion. It can be used to implement
      discreet level of detail directly for static objects, or objects that
      go through cyclic animation. 
      For dimensionsn raycasting and silhouette purposes, the first Master is used
      (item zero in the MasterObjects collection). }
  TVKMaterialMultiProxy = class(TVKBaseSceneObject)
  private
    { Private Declarations }
    FMasterObjects: TVKMaterialMultiProxyMasters;
    FRendering: Boolean; // internal use (loop protection)
    FMaterialLibrary: TVKMaterialLibrary;
    procedure SetMaterialLibrary(const Value: TVKMaterialLibrary);
  protected
    { Protected Declarations }
    procedure SetMasterObjects(const Val: TVKMaterialMultiProxyMasters);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function PrimaryMaster: TVKBaseSceneObject;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure DoRender(var rci: TVKRenderContextInfo; renderSelf, renderChildren: Boolean); override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean; override;
    function GenerateSilhouette(const silhouetteParameters: TVKSilhouetteParameters): TVKSilhouette; override;

  published
    { Published Declarations }
    property MasterObjects: TVKMaterialMultiProxyMasters read FMasterObjects write SetMasterObjects;
    property MaterialLibrary: TVKMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;

    property ObjectsSorting;
    property Direction;
    property PitchAngle;
    property Position;
    property RollAngle;
    property Scale;
    property ShowAxes;
    property TurnAngle;
    property Up;
    property Visible;
    property OnProgress;
    property Behaviours;
    property Effects;
  end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

// ------------------
// ------------------ TVKMaterialMultiProxyMaster ------------------
// ------------------

// Create
//
constructor TVKMaterialMultiProxyMaster.Create(Collection: TCollection);
begin
  inherited Create(Collection);

end;

// Destroy
//
destructor TVKMaterialMultiProxyMaster.Destroy;
begin
  MasterObject := nil;
  inherited Destroy;
end;

// Assign
//
procedure TVKMaterialMultiProxyMaster.Assign(Source: TPersistent);
begin
  if Source is TVKMaterialMultiProxyMaster then
  begin
    FMasterObject := TVKMaterialMultiProxyMaster(Source).FMasterObject;
    FTempLibMaterialName := TVKMaterialMultiProxyMaster(Source).FTempLibMaterialName;
    FDistanceMin2 := TVKMaterialMultiProxyMaster(Source).FDistanceMin2;
    FDistanceMax2 := TVKMaterialMultiProxyMaster(Source).FDistanceMax2;
    NotifyChange;
  end
  else
    inherited;
end;

// OwnerObject
//
function TVKMaterialMultiProxyMaster.OwnerObject: TVKMaterialMultiProxy;
begin
  if Collection = nil then
    Result := nil
  else
    Result := TVKMaterialMultiProxy(TVKMaterialMultiProxyMasters(Collection).GetOwner);
end;

// NotifyChange
//
procedure TVKMaterialMultiProxyMaster.NotifyChange;
begin
  TVKMaterialMultiProxyMasters(Collection).NotifyChange;
end;

// GetDisplayName
//
function TVKMaterialMultiProxyMaster.GetDisplayName: string;
begin
  if MasterObject <> nil then
    Result := MasterObject.Name
  else
    Result := '???';
  Result := Result + Format(' [%.2f; %.2f[', [DistanceMin, DistanceMax]);
end;

// SetMasterObject
//
procedure TVKMaterialMultiProxyMaster.SetMasterObject(const Val: TVKBaseSceneObject);
begin
  if FMasterObject <> Val then
  begin
    if Assigned(FMasterObject) then
      FMasterObject.RemoveFreeNotification(OwnerObject);
    FMasterObject := Val;
    if Assigned(FMasterObject) then
      FMasterObject.FreeNotification(OwnerObject);
    NotifyChange;
  end;
end;

// SetDistanceMin
//
procedure TVKMaterialMultiProxyMaster.SetDistanceMin(const Val: Single);
var
  tmp: Single;
begin
  tmp := Sqr(Val);
  if FDistanceMin2 <> tmp then
  begin
    FDistanceMin2 := tmp;
    NotifyChange;
  end;
end;

// SetDistanceMax
//
procedure TVKMaterialMultiProxyMaster.SetDistanceMax(const Val: Single);
var
  tmp: Single;
begin
  tmp := Sqr(Val);
  if FDistanceMax2 <> tmp then
  begin
    FDistanceMax2 := tmp;
    NotifyChange;
  end;
end;

// GetMaterialLibrary
//
function TVKMaterialMultiProxyMaster.GetMaterialLibrary: TVKAbstractMaterialLibrary;
begin
  if OwnerObject = nil then
    Result := nil
  else
    Result := OwnerObject.FMaterialLibrary;
end;

// GetDistanceMax
//
function TVKMaterialMultiProxyMaster.GetDistanceMax: Single;
begin
  Result := sqrt(FDistanceMax2);
end;

// GetDistanceMin
//
function TVKMaterialMultiProxyMaster.GetDistanceMin: Single;
begin
  Result := sqrt(FDistanceMin2);
end;

// SetMasterLibMaterialName
//
procedure TVKMaterialMultiProxyMaster.SetMasterLibMaterialName(
  const Value: TVKLibMaterialName);
begin
  if OwnerObject.FMaterialLibrary = nil then
  begin
    FTempLibMaterialName := Value;
    if not (csLoading in OwnerObject.ComponentState) then
      raise ETexture.Create(strErrorEx + strMatLibNotDefined);
  end
  else
  begin
    FMasterLibMaterial := OwnerObject.FMaterialLibrary.LibMaterialByName(Value);
    FTempLibMaterialName := '';
  end;
end;

// GetMasterLibMaterialName
//
function TVKMaterialMultiProxyMaster.GetMasterLibMaterialName: TVKLibMaterialName;
begin
  Result := OwnerObject.FMaterialLibrary.GetNameOfLibMaterial(FMasterLibMaterial);
  if Result = '' then
    Result := FTempLibMaterialName;
end;


// ------------------
// ------------------ TVKMaterialMultiProxyMasters ------------------
// ------------------

// Create
//
constructor TVKMaterialMultiProxyMasters.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TVKMaterialMultiProxyMaster);
end;

// SetItems
//
procedure TVKMaterialMultiProxyMasters.SetItems(index: Integer;
  const Val: TVKMaterialMultiProxyMaster);
begin
  inherited Items[index] := Val;
end;

// GetItems
//
function TVKMaterialMultiProxyMasters.GetItems(index: Integer): TVKMaterialMultiProxyMaster;
begin
  Result := TVKMaterialMultiProxyMaster(inherited Items[index]);
end;

// Update
//
procedure TVKMaterialMultiProxyMasters.Update(Item: TCollectionItem);
begin
  inherited;
  NotifyChange;
end;

// Add (simple)
//
function TVKMaterialMultiProxyMasters.Add: TVKMaterialMultiProxyMaster;
begin
  Result := (inherited Add) as TVKMaterialMultiProxyMaster;
end;

// Add (classic params)
//
function TVKMaterialMultiProxyMasters.Add(Master: TVKBaseSceneObject;
  DistanceMin, DistanceMax: Single): TVKMaterialMultiProxyMaster;
begin
  BeginUpdate;
  Result := (inherited Add) as TVKMaterialMultiProxyMaster;
  Result.MasterObject := Master;
  Result.DistanceMin := DistanceMin;
  Result.DistanceMax := DistanceMax;
  EndUpdate;
end;

// Notification
//
procedure TVKMaterialMultiProxyMasters.Notification(AComponent: TComponent);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with Items[I] do
      if FMasterObject = AComponent then
        FMasterObject := nil;
end;

// NotifyChange
//
procedure TVKMaterialMultiProxyMasters.NotifyChange;
begin
  if (UpdateCount = 0) and (GetOwner <> nil) and (GetOwner is TVKUpdateAbleComponent) then
    TVKUpdateAbleComponent(GetOwner).NotifyChange(Self);
end;

// EndUpdate
//
procedure TVKMaterialMultiProxyMasters.EndUpdate;
begin
  inherited EndUpdate;
  // Workaround for a bug in VCL's EndUpdate
  if UpdateCount = 0 then
    NotifyChange;
end;


// Add
//
function TVKMaterialMultiProxyMasters.Add(Master: TVKBaseSceneObject;
  MasterLibMaterial: TVKLibMaterial;
  DistanceMin, DistanceMax: Single): TVKMaterialMultiProxyMaster;
begin
  BeginUpdate;
  Result := (inherited Add) as TVKMaterialMultiProxyMaster;
  Result.MasterObject := Master;
  Result.FMasterLibMaterial := MasterLibMaterial;
  Result.DistanceMin := DistanceMin;
  Result.DistanceMax := DistanceMax;
  EndUpdate;
end;

// ------------------
// ------------------ TVKMaterialMultiProxy ------------------
// ------------------

// Create
//
constructor TVKMaterialMultiProxy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FMasterObjects := TVKMaterialMultiProxyMasters.Create(Self);
end;

// Destroy
//
destructor TVKMaterialMultiProxy.Destroy;
begin
  inherited Destroy;
  FMasterObjects.Free;
end;

// Notification
//
procedure TVKMaterialMultiProxy.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    FMasterObjects.Notification(AComponent);
  end;
  inherited;
end;

// SetMasterObjects
//
procedure TVKMaterialMultiProxy.SetMasterObjects(const Val: TVKMaterialMultiProxyMasters);
begin
  FMasterObjects.Assign(Val);
  StructureChanged;
end;

// Assign
//
procedure TVKMaterialMultiProxy.Assign(Source: TPersistent);
begin
  if Source is TVKMaterialMultiProxy then
    MasterObjects := TVKMaterialMultiProxy(Source).MasterObjects;
  inherited;
end;

// Render
//
procedure TVKMaterialMultiProxy.DoRender(var rci: TVKRenderContextInfo;
  renderSelf, renderChildren: Boolean);
var
  I:  Integer;
  oldProxySubObject: Boolean;
  mpMaster: TVKMaterialMultiProxyMaster;
  d2: Single;
begin
  if FRendering then
    Exit;
  FRendering := True;
  try
    d2 := VectorDistance2(rci.cameraPosition, AbsolutePosition);
    for I := 0 to MasterObjects.Count - 1 do
    begin
      mpMaster := MasterObjects[I];
      if (mpMaster.MasterObject <> nil) and (d2 >= mpMaster.FDistanceMin2) and
         (d2 < mpMaster.FDistanceMax2) then
      begin
        oldProxySubObject := rci.proxySubObject;
        rci.proxySubObject := True;
        with rci.PipelineTransformation do
          ModelMatrix := MatrixMultiply(mpMaster.MasterObject.Matrix, ModelMatrix);
        if (mpMaster.MasterObject is TVKCustomSceneObject) and (FMaterialLibrary <> nil) then
        begin
          TVKCustomSceneObject(mpMaster.MasterObject).Material.QuickAssignMaterial(
            FMaterialLibrary, mpMaster.FMasterLibMaterial);
        end;
        mpMaster.MasterObject.DoRender(rci, renderSelf, (mpMaster.MasterObject.Count > 0));
        rci.proxySubObject := oldProxySubObject;
      end;
    end;
    // now render self stuff (our children, our effects, etc.)
    if renderChildren and (Count > 0) then
      Self.RenderChildren(0, Count - 1, rci);
    //      if MasterGotEffects then
    //         FMasterObject.Effects.RenderPostEffects(Scene.CurrentBuffer, rci);
  finally
    FRendering := False;
  end;
  ClearStructureChanged;
end;

// PrimaryMaster
//
function TVKMaterialMultiProxy.PrimaryMaster: TVKBaseSceneObject;
begin
  if MasterObjects.Count > 0 then
    Result := MasterObjects[0].MasterObject
  else
    Result := nil;
end;

// AxisAlignedDimensions
//
function TVKMaterialMultiProxy.AxisAlignedDimensionsUnscaled: TVector;
var
  Master: TVKBaseSceneObject;
begin
  Master := PrimaryMaster;
  if Assigned(Master) then
    Result := Master.AxisAlignedDimensionsUnscaled
  else
    Result := inherited AxisAlignedDimensionsUnscaled;
end;

// RayCastIntersect
//
function TVKMaterialMultiProxy.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  localRayStart, localRayVector: TVector;
  Master: TVKBaseSceneObject;
begin
  Master := PrimaryMaster;
  if Assigned(Master) then
  begin
    SetVector(localRayStart, AbsoluteToLocal(rayStart));
    SetVector(localRayStart, Master.LocalToAbsolute(localRayStart));
    SetVector(localRayVector, AbsoluteToLocal(rayVector));
    SetVector(localRayVector, Master.LocalToAbsolute(localRayVector));
    NormalizeVector(localRayVector);

    Result := Master.RayCastIntersect(localRayStart, localRayVector,
      intersectPoint, intersectNormal);
    if Result then
    begin
      if Assigned(intersectPoint) then
      begin
        SetVector(intersectPoint^, Master.AbsoluteToLocal(intersectPoint^));
        SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
      end;
      if Assigned(intersectNormal) then
      begin
        SetVector(intersectNormal^, Master.AbsoluteToLocal(intersectNormal^));
        SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      end;
    end;
  end
  else
    Result := False;
end;

// GenerateSilhouette
//
function TVKMaterialMultiProxy.GenerateSilhouette(
  const silhouetteParameters: TVKSilhouetteParameters): TVKSilhouette;
var
  Master: TVKBaseSceneObject;
begin
  Master := PrimaryMaster;
  if Assigned(Master) then
    Result := Master.GenerateSilhouette(silhouetteParameters)
  else
    Result := nil;
end;

// SetMaterialLibrary
//
procedure TVKMaterialMultiProxy.SetMaterialLibrary(
  const Value: TVKMaterialLibrary);
var
  I: Integer;
begin
  if FMaterialLibrary <> Value then
  begin
    if FMaterialLibrary <> nil then
      FMaterialLibrary.RemoveFreeNotification(Self);
    FMaterialLibrary := Value;

    if FMaterialLibrary <> nil then
    begin
      FMaterialLibrary.FreeNotification(Self);
      if FMasterObjects.Count <> 0 then
        for I := 0 to FMasterObjects.Count - 1 do
          with FMasterObjects.GetItems(I) do
          begin
            if FTempLibMaterialName <> '' then
              SetMasterLibMaterialName(FTempLibMaterialName);
          end;
    end
    else
    begin
      if FMasterObjects.Count <> 0 then
        for I := 0 to FMasterObjects.Count - 1 do
          FMasterObjects.GetItems(I).FTempLibMaterialName := '';
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

  RegisterClasses([TVKMaterialMultiProxyMaster, TVKMaterialMultiProxyMasters,
                   TVKMaterialMultiProxy]);

end.

