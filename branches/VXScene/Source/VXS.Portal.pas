//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Portal Rendering support.  
   The portal structures are subclasses of the Mesh structures, with a "sector"
   being assimilated to a "MeshObject" and sector polygons to facegroups. 
     
}
unit VXS.Portal;

interface

{$I VXScene.inc}

uses
  System.Classes, System.SysUtils,

  VXS.VectorFileObjects, VXS.Scene, VXS.Material, VXS.VectorGeometry,
  VXS.RenderContextInfo;

type

   // TPortalMeshObjectList
   //
   { A mesh object list that handles portal rendering. 
      The items are treated as being sectors. } 
   TPortalMeshObjectList = class (TVXMeshObjectList)
      private
         

      protected
         

      public
         
         constructor CreateOwned(AOwner : TVXBaseMesh);
         destructor Destroy; override;

         procedure BuildList(var mrci : TVXRenderContextInfo); override;
   end;


   // TSectorMeshObject
   //
   { A portal renderer sector.  }
   TSectorMeshObject = class (TVXMorphableMeshObject)
      private
         
         FRenderDone : Boolean;

      protected
         

      public
         
         constructor CreateOwned(AOwner : TVXMeshObjectList);
         destructor Destroy; override;

         procedure BuildList(var mrci : TVXRenderContextInfo); override;
         procedure Prepare; override;

         property RenderDone : Boolean read FRenderDone write FRenderDone;
   end;

	// TFGPolygon
	//
   { A portal polygon. 
      This is the base class for portal polygons, the TFGPortalPolygon class
      implements the portal. }
	TFGPolygon = class (TFGVertexNormalTexIndexList)
	   private
	      

	   protected
	      

	   public
	      
	      constructor CreateOwned(AOwner : TVXFaceGroups); override;
         destructor Destroy; override;

         procedure Prepare; override;
	end;

	// TFGPolygon
	//
   { A portal polygon. 
      This is the base class for portal polygons, the TFGPortalPolygon class
      implements the portal. }
	TFGPortalPolygon = class (TFGPolygon)
	   private
	      
         FDestinationSectorIndex : Integer;
         FCenter, FNormal : TAffineVector;
         FRadius : Single;

	   protected
	      

	   public
	      
	      constructor CreateOwned(AOwner : TVXFaceGroups); override;
         destructor Destroy; override;

         procedure BuildList(var mrci : TVXRenderContextInfo); override;

         procedure Prepare; override;

         property DestinationSectorIndex : Integer read FDestinationSectorIndex write FDestinationSectorIndex;
	end;

   // TVXPortal
   //
   { Portal Renderer class. }
   TVXPortal = class(TVXBaseMesh)
      private
         

      protected
         

      public
         
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

      published
         
         property MaterialLibrary;
    end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------
// ------------------ TPortalMeshObjectList ------------------
// ------------------

// CreateOwned
//
constructor TPortalMeshObjectList.CreateOwned(AOwner : TVXBaseMesh);
begin
   inherited CreateOwned(AOwner);
end;

// Destroy
//
destructor TPortalMeshObjectList.Destroy;
begin
   inherited;
end;

// BuildList
//
procedure TPortalMeshObjectList.BuildList(var mrci : TVXRenderContextInfo);
var
   i : Integer;
   startSector : TVXMeshObject;
begin
   for i:=0 to Count-1 do with TSectorMeshObject(Items[i]) do
      if InheritsFrom(TSectorMeshObject) then RenderDone:=False;
   startSector:=nil;
   for i:=0 to Count-1 do begin
      if Items[i].PointInObject(PAffineVector(@mrci.cameraPosition)^) then begin
         startSector:=Items[i];
         Break;
      end;
   end;
   if startSector<>nil then
      startSector.BuildList(mrci)
   else for i:=0 to Count-1 do Items[i].BuildList(mrci);
end;

// ------------------
// ------------------ TSectorMeshObject ------------------
// ------------------

// CreateOwned
//
constructor TSectorMeshObject.CreateOwned(AOwner : TVXMeshObjectList);
begin
	inherited;
   Mode:=momFaceGroups;
end;

// Destroy
//
destructor TSectorMeshObject.Destroy;
begin
	inherited;
end;

// BuildList
//
procedure TSectorMeshObject.BuildList(var mrci : TVXRenderContextInfo);
var
   i : Integer;
   libMat : TVXLibMaterial;
begin
   if not RenderDone then begin
      RenderDone:=True;
      // single pass : portals/polygons were sorted earlier
      if Assigned(mrci.materialLibrary) then begin
         for i:=0 to FaceGroups.Count-1 do with FaceGroups[i] do begin
            if Length(MaterialName)>0 then begin
               libMat:=TVXMaterialLibrary(mrci.materialLibrary).Materials.GetLibMaterialByName(MaterialName);
               if Assigned(libMat) then begin
                  libMat.Apply(mrci);
                  repeat
                     BuildList(mrci);
                  until not libMat.UnApply(mrci);
               end else BuildList(mrci);
            end else BuildList(mrci);
         end;
      end else for i:=0 to FaceGroups.Count-1 do
         FaceGroups[i].BuildList(mrci);
   end;
end;

// Prepare
//
procedure TSectorMeshObject.Prepare;
var
   i : Integer;
begin
   for i:=0 to FaceGroups.Count-1 do
      TFGPolygon(FaceGroups[i]).Prepare;
   FaceGroups.SortByMaterial; // this brings portals first
end;

// ------------------
// ------------------ TFGPolygon ------------------
// ------------------

// CreateOwned
//
constructor TFGPolygon.CreateOwned(AOwner : TVXFaceGroups);
begin
	inherited;
   Mode:=fgmmTriangleFan;
end;

// Destroy
//
destructor TFGPolygon.Destroy;
begin
	inherited;
end;

// Prepare
//
procedure TFGPolygon.Prepare;
begin
   // nothing, ain't no portal !
end;

// ------------------
// ------------------ TFGPortalPolygon ------------------
// ------------------

// CreateOwned
//
constructor TFGPortalPolygon.CreateOwned(AOwner : TVXFaceGroups);
begin
	inherited;
end;

// Destroy
//
destructor TFGPortalPolygon.Destroy;
begin
	inherited;
end;

// BuildList
//
procedure TFGPortalPolygon.BuildList(var mrci : TVXRenderContextInfo);
var
   dir : TAffineVector;
begin
   if FDestinationSectorIndex>=0 then begin
      VectorSubtract(FCenter, PAffineVector(@mrci.rcci.origin)^, dir);
      if (VectorDotProduct(FNormal, dir)<=0) and
            (not IsVolumeClipped(FCenter, FRadius, mrci.rcci.frustum)) then begin
         Owner.Owner.Owner.Items[FDestinationSectorIndex].BuildList(mrci);
      end
   end;
end;

// Prepare
//
procedure TFGPortalPolygon.Prepare;
var
   min, max : TAffineVector;
begin
   GetExtents(min, max);
   FNormal:=GetNormal;
   VectorAdd(min, max, FCenter);
   ScaleVector(FCenter, 0.5);
   FRadius:=VectorDistance(min, max)*0.5;
end;

// ------------------
// ------------------ TVXPortal ------------------
// ------------------

// Create
//
constructor TVXPortal.Create(AOwner: TComponent);
begin
   FMeshObjects:=TPortalMeshObjectList.CreateOwned(Self);
   inherited;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   UseMeshMaterials:=True;
end;

// Destroy
//
destructor TVXPortal.Destroy;
begin
   inherited;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClasses([TVXPortal, TSectorMeshObject, TFGPolygon, TFGPortalPolygon]);

end.

