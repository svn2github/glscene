//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  Implements a multi-proxy objects, useful for discreet LOD. 
    
}
unit VXS.MultiProxy;

interface

uses
  System.Classes, System.SysUtils,

  Winapi.OpenGL, Winapi.OpenGLext,  VXS.Context,  VXS.Scene, VXS.VectorGeometry, VXS.Silhouette,
  VXS.RenderContextInfo, VXS.BaseClasses, VXS.VectorTypes;

type

   TVXMultiProxy = class;

	// TVXMultiProxyMaster
	//
   { MasterObject description for a MultiProxy object. }
	TVXMultiProxyMaster = class (TCollectionItem)
	   private
	      
         FMasterObject : TVXBaseSceneObject;
         FDistanceMin, FDistanceMin2 : Single;
         FDistanceMax, FDistanceMax2 : Single;
         FVisible : Boolean;

	   protected
	      
         function GetDisplayName : String; override;
         procedure SetMasterObject(const val : TVXBaseSceneObject);
         procedure SetDistanceMin(const val : Single);
         procedure SetDistanceMax(const val : Single);
         procedure SetVisible(const val : Boolean);

      public
	      
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

         function OwnerObject : TVXMultiProxy;
         procedure NotifyChange;

      published
         
         { Specifies the Master object which will be proxy'ed. }
         property MasterObject : TVXBaseSceneObject read FMasterObject write SetMasterObject;
         { Minimum visibility distance (inclusive). }
         property DistanceMin : Single read FDistanceMin write SetDistanceMin;
         { Maximum visibility distance (exclusive). }
         property DistanceMax : Single read FDistanceMax write SetDistanceMax;
         { Determines if the master object can be visible (proxy'ed). 
            Note: the master object's distance also has to be within DistanceMin
            and DistanceMax.}
         property Visible : Boolean read FVisible write SetVisible default True;
   end;

	// TVXMultiProxyMasters
	//
   { Collection of TVXMultiProxyMaster. }
	TVXMultiProxyMasters = class (TOwnedCollection)
	   private
	      

	   protected
	      
         procedure SetItems(index : Integer; const val : TVXMultiProxyMaster);
	      function GetItems(index : Integer) : TVXMultiProxyMaster;
         procedure Update(Item: TCollectionItem); override;

      public
	      
	      constructor Create(AOwner : TPersistent);

         function Add : TVXMultiProxyMaster; overload;
         function Add(master : TVXBaseSceneObject; distanceMin, distanceMax : Single) : TVXMultiProxyMaster; overload;
	      property Items[index : Integer] : TVXMultiProxyMaster read GetItems write SetItems; default;

         procedure Notification(AComponent: TComponent);
         
         procedure NotifyChange;
         procedure EndUpdate; override;
   end;

   // TVXMultiProxy
   //
   { Multiple Proxy object. 
      This proxy has multiple master objects, which are individually made visible
      depending on a distance to the camera criterion. It can be used to implement
      discreet level of detail directly for static objects, or objects that
      go through cyclic animation. 
      For dimensionsn raycasting and silhouette purposes, the first master is used
      (item zero in the MasterObjects collection). }
   TVXMultiProxy = class (TVXSceneObject)
      private
			
         FMasterObjects : TVXMultiProxyMasters;
         FRendering : Boolean; // internal use (loop protection)

	   protected
	      
         procedure SetMasterObjects(const val : TVXMultiProxyMasters);
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

         function PrimaryMaster : TVXBaseSceneObject;

      public
			
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

	      procedure Assign(Source: TPersistent); override;
         procedure DoRender(var rci : TVXRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;
                            
         function AxisAlignedDimensionsUnscaled : TVector; override;
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                 intersectPoint : PVector = nil;
                                 intersectNormal : PVector = nil) : Boolean; override;
         function GenerateSilhouette(const silhouetteParameters : TVXSilhouetteParameters) : TVXSilhouette; override;

      published
         
         property MasterObjects : TVXMultiProxyMasters read FMasterObjects write SetMasterObjects;
         
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
   end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
// ------------------
// ------------------ TVXMultiProxyMaster ------------------
// ------------------

// Create
//
constructor TVXMultiProxyMaster.Create(Collection : TCollection);
begin
	inherited Create(Collection);
   FVisible:=True;
end;

// Destroy
//
destructor TVXMultiProxyMaster.Destroy;
begin
   MasterObject:=nil;
	inherited Destroy;
end;

// Assign
//
procedure TVXMultiProxyMaster.Assign(Source: TPersistent);
begin
	if Source is TVXMultiProxyMaster then begin
      MasterObject:=TVXMultiProxyMaster(Source).MasterObject;
      FDistanceMin:=TVXMultiProxyMaster(Source).FDistanceMin;
      FDistanceMin2:=TVXMultiProxyMaster(Source).FDistanceMin2;
      FDistanceMax:=TVXMultiProxyMaster(Source).FDistanceMax;
      FDistanceMax2:=TVXMultiProxyMaster(Source).FDistanceMax2;
      FVisible:=TVXMultiProxyMaster(Source).FVisible;
      NotifyChange;
	end else inherited;
end;

// OwnerObject
//
function TVXMultiProxyMaster.OwnerObject : TVXMultiProxy;
begin
   Result:=TVXMultiProxy(TVXMultiProxyMasters(Collection).GetOwner);
end;

// NotifyChange
//
procedure TVXMultiProxyMaster.NotifyChange;
begin
   TVXMultiProxyMasters(Collection).NotifyChange;
end;

// GetDisplayName
//
function TVXMultiProxyMaster.GetDisplayName : String;
begin
   if MasterObject<>nil then
      Result:=MasterObject.Name
   else Result:='???';
	Result:=Result+Format(' [%.2f; %.2f[', [DistanceMin, DistanceMax]);
   if not Visible then
      Result:=Result+' (hidden)';
end;

// SetMasterObject
//
procedure TVXMultiProxyMaster.SetMasterObject(const val : TVXBaseSceneObject);
begin
   if FMasterObject<>val then begin
      if Assigned(FMasterObject) then
         FMasterObject.RemoveFreeNotification(OwnerObject);
      FMasterObject:=val;
      if Assigned(FMasterObject) then
         FMasterObject.FreeNotification(OwnerObject);
      NotifyChange;
   end;
end;

// SetDistanceMin
//
procedure TVXMultiProxyMaster.SetDistanceMin(const val : Single);
begin
   if FDistanceMin<>val then begin
      FDistanceMin:=val;
      FDistanceMin2:=Sqr(val);
      NotifyChange;
   end;
end;

// SetDistanceMax
//
procedure TVXMultiProxyMaster.SetDistanceMax(const val : Single);
begin
   if FDistanceMax<>val then begin
      FDistanceMax:=val;
      FDistanceMax2:=Sqr(val);
      NotifyChange;
   end;
end;

// SetVisible
//
procedure TVXMultiProxyMaster.SetVisible(const val : Boolean);
begin
   if FVisible<>val then begin
      FVisible:=val;
      NotifyChange;
   end;
end;

// ------------------
// ------------------ TVXMultiProxyMasters ------------------
// ------------------

// Create
//
constructor TVXMultiProxyMasters.Create(AOwner : TPersistent);
begin
   inherited Create(AOwner, TVXMultiProxyMaster)
end;

// SetItems
//
procedure TVXMultiProxyMasters.SetItems(index : Integer; const val : TVXMultiProxyMaster);
begin
	inherited Items[index]:=val;
end;

// GetItems
//
function TVXMultiProxyMasters.GetItems(index : Integer) : TVXMultiProxyMaster;
begin
	Result:=TVXMultiProxyMaster(inherited Items[index]);
end;

// Update
//
procedure TVXMultiProxyMasters.Update(Item : TCollectionItem);
begin
   inherited;
   NotifyChange;
end;

// Add (simple)
//
function TVXMultiProxyMasters.Add : TVXMultiProxyMaster;
begin
	Result:=(inherited Add) as TVXMultiProxyMaster;
end;

// Add (classic params)
//
function TVXMultiProxyMasters.Add(master : TVXBaseSceneObject; distanceMin, distanceMax : Single) : TVXMultiProxyMaster;
begin
   BeginUpdate;
	Result:=(inherited Add) as TVXMultiProxyMaster;
   Result.MasterObject:=master;
   Result.DistanceMin:=distanceMin;
   Result.DistanceMax:=distanceMax;
   EndUpdate;
end;

// Notification
//
procedure TVXMultiProxyMasters.Notification(AComponent: TComponent);
var
   i : Integer;
begin
   for i:=0 to Count-1 do with Items[i] do
      if FMasterObject=AComponent then FMasterObject:=nil;
end;

// NotifyChange
//
procedure TVXMultiProxyMasters.NotifyChange;
begin
   if (UpdateCount=0) and (GetOwner<>nil) and (GetOwner is TVXUpdateAbleComponent) then
      TVXUpdateAbleComponent(GetOwner).NotifyChange(Self);
end;

// EndUpdate
//
procedure TVXMultiProxyMasters.EndUpdate;
begin
   inherited EndUpdate;
   // Workaround for a bug in VCL's EndUpdate
   if UpdateCount=0 then NotifyChange;
end;

// ------------------
// ------------------ TVXMultiProxy ------------------
// ------------------

// Create
//
constructor TVXMultiProxy.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   FMasterObjects:=TVXMultiProxyMasters.Create(Self);
end;

// Destroy
//
destructor TVXMultiProxy.Destroy;
begin
  inherited Destroy;
  FMasterObjects.Free;
end;

// Notification
//
procedure TVXMultiProxy.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if Operation=opRemove then
      FMasterObjects.Notification(AComponent);
   inherited;
end;

// SetMasterObjects
//
procedure TVXMultiProxy.SetMasterObjects(const val : TVXMultiProxyMasters);
begin
   FMasterObjects.Assign(val);
   StructureChanged;
end;

// Assign
//
procedure TVXMultiProxy.Assign(Source: TPersistent);
begin
   if Source is TVXMultiProxy then begin
      MasterObjects:=TVXMultiProxy(Source).MasterObjects;
   end;
   inherited;
end;

// Render
//
procedure TVXMultiProxy.DoRender(var rci : TVXRenderContextInfo;
                                  renderSelf, renderChildren : Boolean);
var
   i : Integer;
   oldProxySubObject : Boolean;
   mpMaster : TVXMultiProxyMaster;
   master : TVXBaseSceneObject;
   d2 : Single;
begin
   if FRendering then Exit;
   FRendering:=True;
   try
      d2:=VectorDistance2(rci.cameraPosition, AbsolutePosition);
      for i:=0 to MasterObjects.Count-1 do begin
         mpMaster:=MasterObjects[i];
         if mpMaster.Visible then begin
            master:=mpMaster.MasterObject;
            if (master<>nil) and (d2>=mpMaster.FDistanceMin2) and (d2<mpMaster.FDistanceMax2) then begin
               oldProxySubObject:=rci.proxySubObject;
               rci.proxySubObject:=True;
               glMultMatrixf(PGLFloat(master.MatrixAsAddress));
               master.DoRender(rci, renderSelf, (master.Count>0));
               rci.proxySubObject:=oldProxySubObject;
            end;
         end;
      end;
      // now render self stuff (our children, our effects, etc.)
      if renderChildren and (Count>0) then
         Self.RenderChildren(0, Count-1, rci);
//      if masterGotEffects then
//         FMasterObject.Effects.RenderPostEffects(Scene.CurrentBuffer, rci);
   finally
      FRendering:=False;
   end;
   ClearStructureChanged;
end;

// PrimaryMaster
//
function TVXMultiProxy.PrimaryMaster : TVXBaseSceneObject;
begin
   if MasterObjects.Count>0 then
      Result:=MasterObjects[0].MasterObject
   else Result:=nil;
end;

// AxisAlignedDimensions
//
function TVXMultiProxy.AxisAlignedDimensionsUnscaled : TVector;
var
   master : TVXBaseSceneObject;
begin
   master:=PrimaryMaster;
   if Assigned(master) then begin
      Result:=master.AxisAlignedDimensionsUnscaled;
   end else Result:=inherited AxisAlignedDimensionsUnscaled;
end;

// RayCastIntersect
//
function TVXMultiProxy.RayCastIntersect(const rayStart, rayVector : TVector;
                                 intersectPoint : PVector = nil;
                                 intersectNormal : PVector = nil) : Boolean;
var
   localRayStart, localRayVector : TVector;
   master : TVXBaseSceneObject;
begin
   master:=PrimaryMaster;
   if Assigned(master) then begin
      SetVector(localRayStart, AbsoluteToLocal(rayStart));
      SetVector(localRayStart, master.LocalToAbsolute(localRayStart));
      SetVector(localRayVector, AbsoluteToLocal(rayVector));
      SetVector(localRayVector, master.LocalToAbsolute(localRayVector));
      NormalizeVector(localRayVector);

      Result:=master.RayCastIntersect(localRayStart, localRayVector,
                                            intersectPoint, intersectNormal);
      if Result then begin
         if Assigned(intersectPoint) then begin
            SetVector(intersectPoint^, master.AbsoluteToLocal(intersectPoint^));
            SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
         end;
         if Assigned(intersectNormal) then begin
            SetVector(intersectNormal^, master.AbsoluteToLocal(intersectNormal^));
            SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
         end;
      end;
   end else Result:=False;
end;

// GenerateSilhouette
//
function TVXMultiProxy.GenerateSilhouette(const silhouetteParameters : TVXSilhouetteParameters) : TVXSilhouette;
var
   master : TVXBaseSceneObject;
begin
   master:=PrimaryMaster;
   if Assigned(master) then
      Result:=master.GenerateSilhouette(silhouetteParameters)
   else Result:=nil;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClasses([TVXMultiProxy]);

end.
