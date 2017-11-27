//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Implements a multi-proxy object, useful for discreet LOD. 
   Allows assign a unique material for each proxy master. 

  What changed compared to GLMultiProxy:
    1) Allows assign a unique material for each proxy master
    2) TVXMaterialMultiProxyMaster: FDistanceMin, FDistanceMax removed
    3) TVXMaterialMultiProxy = class(TVXBaseSceneObject)!!!
    4) TVXMaterialMultiProxyMaster.Visible removed
    5) TVXMaterialMultiProxy.MaterialLibrary added
    6) TVXMaterialMultiProxyMaster.MasterLibMaterial added
    7) TVXMaterialMultiProxyMasters.Add overloaded
    8) Implemented a new mechanizm of connecting TVXLibMaterial and TVXLibMaterialName
      (they are connected on assigning, not while rendering; full persistency support;
       allows to assign directly to TVXLibMaterial)
    9) FMX-style code formating

}

unit VXS.MaterialMultiProxy;

interface

{$I VXScene.inc}

uses
  System.Classes, 
  System.SysUtils,
   
  VXS.Scene, 
  VXS.VectorGeometry, 
  VXS.Texture, 
  VXS.Material, 
  VXS.Silhouette, 
  VXS.Strings,
  VXS.CrossPlatform, 
  VXS.PersistentClasses, 
  VXS.RenderContextInfo, 
  VXS.BaseClasses,
  VXS.Context, 
  VXS.VectorTypes;

type

  TVXMaterialMultiProxy = class;

  { MasterObject description for a MultiProxy object. }
  TVXMaterialMultiProxyMaster = class(TVXInterfacedCollectionItem, IGLMaterialLibrarySupported)
  private
    FMasterObject: TVXBaseSceneObject;
    FMasterLibMaterial: TVXLibMaterial;
    FTempLibMaterialName: TVXLibMaterialName;
    FDistanceMin2, FDistanceMax2: Single;
    procedure SetMasterLibMaterialName(const Value: TVXLibMaterialName);
    function GetMasterLibMaterialName: TVXLibMaterialName;
    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TVXAbstractMaterialLibrary;
  protected
    function GetDisplayName: string; override;
    procedure SetMasterObject(const Val: TVXBaseSceneObject);
    procedure SetDistanceMin(const Val: Single);
    procedure SetDistanceMax(const Val: Single);
    function GetDistanceMin: Single;
    function GetDistanceMax: Single;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function OwnerObject: TVXMaterialMultiProxy;
    procedure NotifyChange;
    { Specifies the Material, that current master object will use.
       Provides a faster way to access FMasterLibMaterial, compared to
       MasterLibMaterialName }
    property MasterLibMaterial: TVXLibMaterial read FMasterLibMaterial write FMasterLibMaterial stored False;
  published
    { Specifies the Master object which will be proxy'ed. }
    property MasterObject: TVXBaseSceneObject read FMasterObject write SetMasterObject;
    { Specifies the Material, that current master object will use. }
    property MasterLibMaterialName: TVXLibMaterialName read GetMasterLibMaterialName write SetMasterLibMaterialName;
    { Minimum visibility Distance (inclusive). }
    property DistanceMin: Single read GetDistanceMin write SetDistanceMin;
    { Maximum visibility Distance (exclusive). }
    property DistanceMax: Single read GetDistanceMax write SetDistanceMax;
  end;

  { Collection of TVXMaterialMultiProxyMaster. }
  TVXMaterialMultiProxyMasters = class(TOwnedCollection)
  protected
    procedure SetItems(index: Integer; const Val: TVXMaterialMultiProxyMaster);
    function GetItems(index: Integer): TVXMaterialMultiProxyMaster;
    procedure Update(Item: TCollectionItem); override;
    procedure Notification(AComponent: TComponent); virtual;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TVXMaterialMultiProxyMaster; overload;
    function Add(Master: TVXBaseSceneObject; DistanceMin, DistanceMax: Single): TVXMaterialMultiProxyMaster; overload;
    function Add(Master: TVXBaseSceneObject; MasterLibMaterial: TVXLibMaterial; DistanceMin, DistanceMax: Single): TVXMaterialMultiProxyMaster; overload;
    property Items[index: Integer]: TVXMaterialMultiProxyMaster read GetItems write SetItems; default;
    procedure NotifyChange;
    procedure EndUpdate; override;
  end;

   { Multiple Proxy object. 
      This proxy has multiple Master objects, which are individually made visible
      depending on a Distance to the camera criterion. It can be used to implement
      discreet level of detail directly for static objects, or objects that
      go through cyclic animation. 
      For dimensionsn raycasting and silhouette purposes, the first Master is used
      (item zero in the MasterObjects collection). }
  TVXMaterialMultiProxy = class(TVXBaseSceneObject)
  private
    FMasterObjects: TVXMaterialMultiProxyMasters;
    FRendering: Boolean; // internal use (loop protection)
    FMaterialLibrary: TVXMaterialLibrary;
    procedure SetMaterialLibrary(const Value: TVXMaterialLibrary);
  protected
    procedure SetMasterObjects(const Val: TVXMaterialMultiProxyMasters);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function PrimaryMaster: TVXBaseSceneObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DoRender(var rci: TVXRenderContextInfo; renderSelf, renderChildren: Boolean); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean; override;
    function GenerateSilhouette(const silhouetteParameters: TVXSilhouetteParameters): TVXSilhouette; override;
  published
    property MasterObjects: TVXMaterialMultiProxyMasters read FMasterObjects write SetMasterObjects;
    property MaterialLibrary: TVXMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
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
implementation
//-------------------------------------------------------------

// ------------------
// ------------------ TVXMaterialMultiProxyMaster ------------------
// ------------------

constructor TVXMaterialMultiProxyMaster.Create(Collection: TCollection);
begin
  inherited Create(Collection);

end;

destructor TVXMaterialMultiProxyMaster.Destroy;
begin
  MasterObject := nil;
  inherited Destroy;
end;

procedure TVXMaterialMultiProxyMaster.Assign(Source: TPersistent);
begin
  if Source is TVXMaterialMultiProxyMaster then
  begin
    FMasterObject := TVXMaterialMultiProxyMaster(Source).FMasterObject;
    FTempLibMaterialName := TVXMaterialMultiProxyMaster(Source).FTempLibMaterialName;
    FDistanceMin2 := TVXMaterialMultiProxyMaster(Source).FDistanceMin2;
    FDistanceMax2 := TVXMaterialMultiProxyMaster(Source).FDistanceMax2;
    NotifyChange;
  end
  else
    inherited;
end;

function TVXMaterialMultiProxyMaster.OwnerObject: TVXMaterialMultiProxy;
begin
  if Collection = nil then
    Result := nil
  else
    Result := TVXMaterialMultiProxy(TVXMaterialMultiProxyMasters(Collection).GetOwner);
end;

procedure TVXMaterialMultiProxyMaster.NotifyChange;
begin
  TVXMaterialMultiProxyMasters(Collection).NotifyChange;
end;

function TVXMaterialMultiProxyMaster.GetDisplayName: string;
begin
  if MasterObject <> nil then
    Result := MasterObject.Name
  else
    Result := '???';
  Result := Result + Format(' [%.2f; %.2f[', [DistanceMin, DistanceMax]);
end;

procedure TVXMaterialMultiProxyMaster.SetMasterObject(const Val: TVXBaseSceneObject);
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

procedure TVXMaterialMultiProxyMaster.SetDistanceMin(const Val: Single);
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

procedure TVXMaterialMultiProxyMaster.SetDistanceMax(const Val: Single);
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

function TVXMaterialMultiProxyMaster.GetMaterialLibrary: TVXAbstractMaterialLibrary;
begin
  if OwnerObject = nil then
    Result := nil
  else
    Result := OwnerObject.FMaterialLibrary;
end;

function TVXMaterialMultiProxyMaster.GetDistanceMax: Single;
begin
  Result := sqrt(FDistanceMax2);
end;

function TVXMaterialMultiProxyMaster.GetDistanceMin: Single;
begin
  Result := sqrt(FDistanceMin2);
end;

procedure TVXMaterialMultiProxyMaster.SetMasterLibMaterialName(
  const Value: TVXLibMaterialName);
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

function TVXMaterialMultiProxyMaster.GetMasterLibMaterialName: TVXLibMaterialName;
begin
  Result := OwnerObject.FMaterialLibrary.GetNameOfLibMaterial(FMasterLibMaterial);
  if Result = '' then
    Result := FTempLibMaterialName;
end;


// ------------------
// ------------------ TVXMaterialMultiProxyMasters ------------------
// ------------------

constructor TVXMaterialMultiProxyMasters.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TVXMaterialMultiProxyMaster);
end;

procedure TVXMaterialMultiProxyMasters.SetItems(index: Integer;
  const Val: TVXMaterialMultiProxyMaster);
begin
  inherited Items[index] := Val;
end;

function TVXMaterialMultiProxyMasters.GetItems(index: Integer): TVXMaterialMultiProxyMaster;
begin
  Result := TVXMaterialMultiProxyMaster(inherited Items[index]);
end;

procedure TVXMaterialMultiProxyMasters.Update(Item: TCollectionItem);
begin
  inherited;
  NotifyChange;
end;

function TVXMaterialMultiProxyMasters.Add: TVXMaterialMultiProxyMaster;
begin
  Result := (inherited Add) as TVXMaterialMultiProxyMaster;
end;

function TVXMaterialMultiProxyMasters.Add(Master: TVXBaseSceneObject;
  DistanceMin, DistanceMax: Single): TVXMaterialMultiProxyMaster;
begin
  BeginUpdate;
  Result := (inherited Add) as TVXMaterialMultiProxyMaster;
  Result.MasterObject := Master;
  Result.DistanceMin := DistanceMin;
  Result.DistanceMax := DistanceMax;
  EndUpdate;
end;

procedure TVXMaterialMultiProxyMasters.Notification(AComponent: TComponent);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with Items[I] do
      if FMasterObject = AComponent then
        FMasterObject := nil;
end;

procedure TVXMaterialMultiProxyMasters.NotifyChange;
begin
  if (UpdateCount = 0) and (GetOwner <> nil) and (GetOwner is TVXUpdateAbleComponent) then
    TVXUpdateAbleComponent(GetOwner).NotifyChange(Self);
end;

procedure TVXMaterialMultiProxyMasters.EndUpdate;
begin
  inherited EndUpdate;
  // Workaround for a bug in VCL's EndUpdate
  if UpdateCount = 0 then
    NotifyChange;
end;


function TVXMaterialMultiProxyMasters.Add(Master: TVXBaseSceneObject;
  MasterLibMaterial: TVXLibMaterial;
  DistanceMin, DistanceMax: Single): TVXMaterialMultiProxyMaster;
begin
  BeginUpdate;
  Result := (inherited Add) as TVXMaterialMultiProxyMaster;
  Result.MasterObject := Master;
  Result.FMasterLibMaterial := MasterLibMaterial;
  Result.DistanceMin := DistanceMin;
  Result.DistanceMax := DistanceMax;
  EndUpdate;
end;

// ------------------
// ------------------ TVXMaterialMultiProxy ------------------
// ------------------

constructor TVXMaterialMultiProxy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FMasterObjects := TVXMaterialMultiProxyMasters.Create(Self);
end;

destructor TVXMaterialMultiProxy.Destroy;
begin
  inherited Destroy;
  FMasterObjects.Free;
end;

procedure TVXMaterialMultiProxy.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    FMasterObjects.Notification(AComponent);
  end;
  inherited;
end;

procedure TVXMaterialMultiProxy.SetMasterObjects(const Val: TVXMaterialMultiProxyMasters);
begin
  FMasterObjects.Assign(Val);
  StructureChanged;
end;

procedure TVXMaterialMultiProxy.Assign(Source: TPersistent);
begin
  if Source is TVXMaterialMultiProxy then
    MasterObjects := TVXMaterialMultiProxy(Source).MasterObjects;
  inherited;
end;

procedure TVXMaterialMultiProxy.DoRender(var rci: TVXRenderContextInfo;
  renderSelf, renderChildren: Boolean);
var
  I:  Integer;
  oldProxySubObject: Boolean;
  mpMaster: TVXMaterialMultiProxyMaster;
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
          SetModelMatrix(MatrixMultiply(mpMaster.MasterObject.Matrix^, ModelMatrix^));
        if (mpMaster.MasterObject is TVXCustomSceneObject) and (FMaterialLibrary <> nil) then
        begin
          TVXCustomSceneObject(mpMaster.MasterObject).Material.QuickAssignMaterial(
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

function TVXMaterialMultiProxy.PrimaryMaster: TVXBaseSceneObject;
begin
  if MasterObjects.Count > 0 then
    Result := MasterObjects[0].MasterObject
  else
    Result := nil;
end;

function TVXMaterialMultiProxy.AxisAlignedDimensionsUnscaled: TVector;
var
  Master: TVXBaseSceneObject;
begin
  Master := PrimaryMaster;
  if Assigned(Master) then
    Result := Master.AxisAlignedDimensionsUnscaled
  else
    Result := inherited AxisAlignedDimensionsUnscaled;
end;

function TVXMaterialMultiProxy.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  localRayStart, localRayVector: TVector;
  Master: TVXBaseSceneObject;
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

function TVXMaterialMultiProxy.GenerateSilhouette(
  const silhouetteParameters: TVXSilhouetteParameters): TVXSilhouette;
var
  Master: TVXBaseSceneObject;
begin
  Master := PrimaryMaster;
  if Assigned(Master) then
    Result := Master.GenerateSilhouette(silhouetteParameters)
  else
    Result := nil;
end;

procedure TVXMaterialMultiProxy.SetMaterialLibrary(
  const Value: TVXMaterialLibrary);
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
initialization
//-------------------------------------------------------------

  RegisterClasses([TVXMaterialMultiProxyMaster, TVXMaterialMultiProxyMasters,
                   TVXMaterialMultiProxy]);

end.

