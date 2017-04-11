//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  Implements a multi-proxy objects, useful for discreet LOD. 
    
}
unit VKS.MultiProxy;

interface

uses
  System.Classes, System.SysUtils,

  Winapi.OpenGL, Winapi.OpenGLext,  VKS.Context,  VKS.Scene, VKS.VectorGeometry, VKS.Silhouette,
  VKS.RenderContextInfo, VKS.BaseClasses, VKS.VectorTypes;

type

   TVKMultiProxy = class;

	// TVKMultiProxyMaster
	//
   { MasterObject description for a MultiProxy object. }
	TVKMultiProxyMaster = class (TCollectionItem)
	   private
	      
         FMasterObject : TVKBaseSceneObject;
         FDistanceMin, FDistanceMin2 : Single;
         FDistanceMax, FDistanceMax2 : Single;
         FVisible : Boolean;

	   protected
	      
         function GetDisplayName : String; override;
         procedure SetMasterObject(const val : TVKBaseSceneObject);
         procedure SetDistanceMin(const val : Single);
         procedure SetDistanceMax(const val : Single);
         procedure SetVisible(const val : Boolean);

      public
	      
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

         function OwnerObject : TVKMultiProxy;
         procedure NotifyChange;

      published
         
         { Specifies the Master object which will be proxy'ed. }
         property MasterObject : TVKBaseSceneObject read FMasterObject write SetMasterObject;
         { Minimum visibility distance (inclusive). }
         property DistanceMin : Single read FDistanceMin write SetDistanceMin;
         { Maximum visibility distance (exclusive). }
         property DistanceMax : Single read FDistanceMax write SetDistanceMax;
         { Determines if the master object can be visible (proxy'ed). 
            Note: the master object's distance also has to be within DistanceMin
            and DistanceMax.}
         property Visible : Boolean read FVisible write SetVisible default True;
   end;

	// TVKMultiProxyMasters
	//
   { Collection of TVKMultiProxyMaster. }
	TVKMultiProxyMasters = class (TOwnedCollection)
	   private
	      

	   protected
	      
         procedure SetItems(index : Integer; const val : TVKMultiProxyMaster);
	      function GetItems(index : Integer) : TVKMultiProxyMaster;
         procedure Update(Item: TCollectionItem); override;

      public
	      
	      constructor Create(AOwner : TPersistent);

         function Add : TVKMultiProxyMaster; overload;
         function Add(master : TVKBaseSceneObject; distanceMin, distanceMax : Single) : TVKMultiProxyMaster; overload;
	      property Items[index : Integer] : TVKMultiProxyMaster read GetItems write SetItems; default;

         procedure Notification(AComponent: TComponent);
         
         procedure NotifyChange;
         procedure EndUpdate; override;
   end;

   // TVKMultiProxy
   //
   { Multiple Proxy object. 
      This proxy has multiple master objects, which are individually made visible
      depending on a distance to the camera criterion. It can be used to implement
      discreet level of detail directly for static objects, or objects that
      go through cyclic animation. 
      For dimensionsn raycasting and silhouette purposes, the first master is used
      (item zero in the MasterObjects collection). }
   TVKMultiProxy = class (TVKSceneObject)
      private
			
         FMasterObjects : TVKMultiProxyMasters;
         FRendering : Boolean; // internal use (loop protection)

	   protected
	      
         procedure SetMasterObjects(const val : TVKMultiProxyMasters);
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

         function PrimaryMaster : TVKBaseSceneObject;

      public
			
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

	      procedure Assign(Source: TPersistent); override;
         procedure DoRender(var rci : TVKRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;
                            
         function AxisAlignedDimensionsUnscaled : TVector; override;
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                 intersectPoint : PVector = nil;
                                 intersectNormal : PVector = nil) : Boolean; override;
         function GenerateSilhouette(const silhouetteParameters : TVKSilhouetteParameters) : TVKSilhouette; override;

      published
         
         property MasterObjects : TVKMultiProxyMasters read FMasterObjects write SetMasterObjects;
         
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
// ------------------ TVKMultiProxyMaster ------------------
// ------------------

// Create
//
constructor TVKMultiProxyMaster.Create(Collection : TCollection);
begin
	inherited Create(Collection);
   FVisible:=True;
end;

// Destroy
//
destructor TVKMultiProxyMaster.Destroy;
begin
   MasterObject:=nil;
	inherited Destroy;
end;

// Assign
//
procedure TVKMultiProxyMaster.Assign(Source: TPersistent);
begin
	if Source is TVKMultiProxyMaster then begin
      MasterObject:=TVKMultiProxyMaster(Source).MasterObject;
      FDistanceMin:=TVKMultiProxyMaster(Source).FDistanceMin;
      FDistanceMin2:=TVKMultiProxyMaster(Source).FDistanceMin2;
      FDistanceMax:=TVKMultiProxyMaster(Source).FDistanceMax;
      FDistanceMax2:=TVKMultiProxyMaster(Source).FDistanceMax2;
      FVisible:=TVKMultiProxyMaster(Source).FVisible;
      NotifyChange;
	end else inherited;
end;

// OwnerObject
//
function TVKMultiProxyMaster.OwnerObject : TVKMultiProxy;
begin
   Result:=TVKMultiProxy(TVKMultiProxyMasters(Collection).GetOwner);
end;

// NotifyChange
//
procedure TVKMultiProxyMaster.NotifyChange;
begin
   TVKMultiProxyMasters(Collection).NotifyChange;
end;

// GetDisplayName
//
function TVKMultiProxyMaster.GetDisplayName : String;
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
procedure TVKMultiProxyMaster.SetMasterObject(const val : TVKBaseSceneObject);
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
procedure TVKMultiProxyMaster.SetDistanceMin(const val : Single);
begin
   if FDistanceMin<>val then begin
      FDistanceMin:=val;
      FDistanceMin2:=Sqr(val);
      NotifyChange;
   end;
end;

// SetDistanceMax
//
procedure TVKMultiProxyMaster.SetDistanceMax(const val : Single);
begin
   if FDistanceMax<>val then begin
      FDistanceMax:=val;
      FDistanceMax2:=Sqr(val);
      NotifyChange;
   end;
end;

// SetVisible
//
procedure TVKMultiProxyMaster.SetVisible(const val : Boolean);
begin
   if FVisible<>val then begin
      FVisible:=val;
      NotifyChange;
   end;
end;

// ------------------
// ------------------ TVKMultiProxyMasters ------------------
// ------------------

// Create
//
constructor TVKMultiProxyMasters.Create(AOwner : TPersistent);
begin
   inherited Create(AOwner, TVKMultiProxyMaster)
end;

// SetItems
//
procedure TVKMultiProxyMasters.SetItems(index : Integer; const val : TVKMultiProxyMaster);
begin
	inherited Items[index]:=val;
end;

// GetItems
//
function TVKMultiProxyMasters.GetItems(index : Integer) : TVKMultiProxyMaster;
begin
	Result:=TVKMultiProxyMaster(inherited Items[index]);
end;

// Update
//
procedure TVKMultiProxyMasters.Update(Item : TCollectionItem);
begin
   inherited;
   NotifyChange;
end;

// Add (simple)
//
function TVKMultiProxyMasters.Add : TVKMultiProxyMaster;
begin
	Result:=(inherited Add) as TVKMultiProxyMaster;
end;

// Add (classic params)
//
function TVKMultiProxyMasters.Add(master : TVKBaseSceneObject; distanceMin, distanceMax : Single) : TVKMultiProxyMaster;
begin
   BeginUpdate;
	Result:=(inherited Add) as TVKMultiProxyMaster;
   Result.MasterObject:=master;
   Result.DistanceMin:=distanceMin;
   Result.DistanceMax:=distanceMax;
   EndUpdate;
end;

// Notification
//
procedure TVKMultiProxyMasters.Notification(AComponent: TComponent);
var
   i : Integer;
begin
   for i:=0 to Count-1 do with Items[i] do
      if FMasterObject=AComponent then FMasterObject:=nil;
end;

// NotifyChange
//
procedure TVKMultiProxyMasters.NotifyChange;
begin
   if (UpdateCount=0) and (GetOwner<>nil) and (GetOwner is TVKUpdateAbleComponent) then
      TVKUpdateAbleComponent(GetOwner).NotifyChange(Self);
end;

// EndUpdate
//
procedure TVKMultiProxyMasters.EndUpdate;
begin
   inherited EndUpdate;
   // Workaround for a bug in VCL's EndUpdate
   if UpdateCount=0 then NotifyChange;
end;

// ------------------
// ------------------ TVKMultiProxy ------------------
// ------------------

// Create
//
constructor TVKMultiProxy.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   FMasterObjects:=TVKMultiProxyMasters.Create(Self);
end;

// Destroy
//
destructor TVKMultiProxy.Destroy;
begin
  inherited Destroy;
  FMasterObjects.Free;
end;

// Notification
//
procedure TVKMultiProxy.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if Operation=opRemove then
      FMasterObjects.Notification(AComponent);
   inherited;
end;

// SetMasterObjects
//
procedure TVKMultiProxy.SetMasterObjects(const val : TVKMultiProxyMasters);
begin
   FMasterObjects.Assign(val);
   StructureChanged;
end;

// Assign
//
procedure TVKMultiProxy.Assign(Source: TPersistent);
begin
   if Source is TVKMultiProxy then begin
      MasterObjects:=TVKMultiProxy(Source).MasterObjects;
   end;
   inherited;
end;

// Render
//
procedure TVKMultiProxy.DoRender(var rci : TVKRenderContextInfo;
                                  renderSelf, renderChildren : Boolean);
var
   i : Integer;
   oldProxySubObject : Boolean;
   mpMaster : TVKMultiProxyMaster;
   master : TVKBaseSceneObject;
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
function TVKMultiProxy.PrimaryMaster : TVKBaseSceneObject;
begin
   if MasterObjects.Count>0 then
      Result:=MasterObjects[0].MasterObject
   else Result:=nil;
end;

// AxisAlignedDimensions
//
function TVKMultiProxy.AxisAlignedDimensionsUnscaled : TVector;
var
   master : TVKBaseSceneObject;
begin
   master:=PrimaryMaster;
   if Assigned(master) then begin
      Result:=master.AxisAlignedDimensionsUnscaled;
   end else Result:=inherited AxisAlignedDimensionsUnscaled;
end;

// RayCastIntersect
//
function TVKMultiProxy.RayCastIntersect(const rayStart, rayVector : TVector;
                                 intersectPoint : PVector = nil;
                                 intersectNormal : PVector = nil) : Boolean;
var
   localRayStart, localRayVector : TVector;
   master : TVKBaseSceneObject;
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
function TVKMultiProxy.GenerateSilhouette(const silhouetteParameters : TVKSilhouetteParameters) : TVKSilhouette;
var
   master : TVKBaseSceneObject;
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

   RegisterClasses([TVKMultiProxy]);

end.
