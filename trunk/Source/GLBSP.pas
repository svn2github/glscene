{: GLBSP<p>

	Binary Space Partion mesh support for GLScene.<p>

   ***IN PROGRESS***<p>

   The classes of this unit are designed to operate within a TGLBaseMesh.<p>

	<b>Historique : </b><font size=-1><ul>
	   <li>30/01/03 - Egg - Creation
	</ul></font>
}
unit GLBSP;

interface

uses Classes, GLVectorFileObjects, GLScene, GLTexture, GLMisc, Geometry;

type

   // TBSPMeshObject
   //
   {: A BSP mesh object.<p>
      Stores the geometry information, BSP rendering options and offers some
      basic BSP utility methods. Geometry information is indexed in the facegroups,
      the 1st facegroup (of index 0) being the root node of the BSP tree. }
   TBSPMeshObject = class (TMeshObject)
      private
         { Private Declarations }

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         constructor CreateOwned(AOwner : TMeshObjectList);
         destructor Destroy; override;

         procedure BuildList(var mrci : TRenderContextInfo); override;
   end;

	// TFGBSPNode
	//
   {: A node in the BSP tree.<p>
      The description does not explicitly differentiates nodes and leafs,
      nodes are referred by their index. }
	TFGBSPNode = class (TFGVertexIndexList)
	   private
	      { Private Declarations }

	   protected
	      { Protected Declarations }

	   public
	      { Public Declarations }
	      constructor CreateOwned(AOwner : TFaceGroups); override;
         destructor Destroy; override;
	end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TBSPMeshObject ------------------
// ------------------

// CreateOwned
//
constructor TBSPMeshObject.CreateOwned(AOwner : TMeshObjectList);
begin
	inherited;
   Mode:=momFaceGroups;
end;

// Destroy
//
destructor TBSPMeshObject.Destroy;
begin
	inherited;
end;

// BuildList
//
procedure TBSPMeshObject.BuildList(var mrci : TRenderContextInfo);
begin
   if Mode<>momFaceGroups then begin
      inherited BuildList(mrci);
      Exit;
   end;
   // render BSP
end;

// ------------------
// ------------------ TFGBSPNode ------------------
// ------------------

// CreateOwned
//
constructor TFGBSPNode.CreateOwned(AOwner : TFaceGroups);
begin
	inherited;
end;

// Destroy
//
destructor TFGBSPNode.Destroy;
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
   RegisterClasses([TBSPMeshObject, TFGBSPNode]);

end.
