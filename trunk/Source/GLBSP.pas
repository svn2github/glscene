{: GLBSP<p>

	Binary Space Partion mesh support for GLScene.<p>

   The classes of this unit are designed to operate within a TGLBaseMesh.<p>

	<b>Historique : </b><font size=-1><ul>
	   <li>30/01/03 - Egg - Creation
	</ul></font>
}
unit GLBSP;

interface

uses Classes, GLVectorFileObjects, GLScene, GLTexture, GLMisc, Geometry;

type

   // TBSPRenderContextInfo
   //
   TBSPRenderContextInfo = record
      //: Local coordinates of the camera (can be a vector or point)
      cameraLocal : TVector;
      rci : PRenderContextInfo;
   end;

   // TBSPRenderSort
   //
   TBSPRenderSort = (rsNone, rsBackToFront, rsFrontToBack);

   // TBSPMeshObject
   //
   {: A BSP mesh object.<p>
      Stores the geometry information, BSP rendering options and offers some
      basic BSP utility methods. Geometry information is indexed in the facegroups,
      the 1st facegroup (of index 0) being the root node of the BSP tree. }
   TBSPMeshObject = class (TMeshObject)
      private
         { Private Declarations }
         FRenderSort : TBSPRenderSort;

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         constructor CreateOwned(AOwner : TMeshObjectList);
         destructor Destroy; override;

         procedure BuildList(var mrci : TRenderContextInfo); override;

         {: Rendering sort mode.<p>
            This sort mode can currently *not* blend with the sort by materials
            flag, default mode is rsBackToFront.<br>
            Note that in rsNone mode, the hierarchical nature of the tree is
            still honoured (positive subnode, self, then negative subnode). }
         property RenderSort : TBSPRenderSort read FRenderSort write FRenderSort;
   end;

	// TFGBSPNode
	//
   {: A node in the BSP tree.<p>
      The description does not explicitly differentiates nodes and leafs,
      nodes are referred by their index. }
	TFGBSPNode = class (TFGVertexIndexList)
	   private
	      { Private Declarations }
         FSplitPlane : THmgPlane;
         FPositiveSubNodeIndex : Integer;
         FNegativeSubNodeIndex : Integer;

	   protected
	      { Protected Declarations }

	   public
	      { Public Declarations }
	      constructor CreateOwned(AOwner : TFaceGroups); override;
         destructor Destroy; override;

         procedure BuildListNoSort(var bsprci : TBSPRenderContextInfo);
         procedure BuildListFrontToBack(var bsprci : TBSPRenderContextInfo);
         procedure BuildListBackToFront(var bsprci : TBSPRenderContextInfo);

         {: BSP node split plane.<p>
            Divides space between positive and negative half-space, positive
            half-space being the one were the evaluation of an homogeneous
            vector against the plane is positive. }
         property SplitPlane : THmgPlane read FSplitPlane write FSplitPlane;
         {: Index of the positive sub-node index in the list.<p>
            Zero if empty. }
         property PositiveSubNodeIndex : Integer read FPositiveSubNodeIndex write FPositiveSubNodeIndex;
         {: Index of the negative sub-node index in the list.<p>
            Zero if empty. }
         property NegativeSubNodeIndex : Integer read FNegativeSubNodeIndex write FNegativeSubNodeIndex;
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
   RenderSort:=rsBackToFront;
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
var
   bsprci : TBSPRenderContextInfo;
begin
   if Mode<>momFaceGroups then begin
      inherited BuildList(mrci);
      Exit;
   end;
   // render BSP
   if FaceGroups.Count>0 then begin
      bsprci.cameraLocal:=Owner.Owner.AbsoluteToLocal(mrci.cameraPosition);
      bsprci.rci:=@mrci;
      case RenderSort of
         rsNone        : (FaceGroups[0] as TFGBSPNode).BuildListNoSort(bsprci);
         rsBackToFront : (FaceGroups[0] as TFGBSPNode).BuildListBackToFront(bsprci);
         rsFrontToBack : (FaceGroups[0] as TFGBSPNode).BuildListFrontToBack(bsprci);
      else
         Assert(False);
      end;
   end;
end;

// ------------------
// ------------------ TFGBSPNode ------------------
// ------------------

// CreateOwned
//
constructor TFGBSPNode.CreateOwned(AOwner : TFaceGroups);
begin
	inherited;
   FPositiveSubNodeIndex:=0;
   FNegativeSubNodeIndex:=0;
end;

// Destroy
//
destructor TFGBSPNode.Destroy;
begin
	inherited;
end;

// BuildListNoSort
//
procedure TFGBSPNode.BuildListNoSort(var bsprci : TBSPRenderContextInfo);
begin
   if PositiveSubNodeIndex>0 then
      TFGBSPNode(Owner[PositiveSubNodeIndex]).BuildListNoSort(bsprci);
   BuildList(bsprci.rci^);
   if NegativeSubNodeIndex>0 then
      TFGBSPNode(Owner[PositiveSubNodeIndex]).BuildListNoSort(bsprci);
end;

// BuildListFrontToBack
//
procedure TFGBSPNode.BuildListFrontToBack(var bsprci : TBSPRenderContextInfo);
begin
   if PlaneEvaluatePoint(SplitPlane, bsprci.cameraLocal)>=0 then begin
      if PositiveSubNodeIndex>0 then
         TFGBSPNode(Owner[PositiveSubNodeIndex]).BuildListNoSort(bsprci);
      BuildList(bsprci.rci^);
      if NegativeSubNodeIndex>0 then
         TFGBSPNode(Owner[NegativeSubNodeIndex]).BuildListNoSort(bsprci);
   end else begin
      if NegativeSubNodeIndex>0 then
         TFGBSPNode(Owner[NegativeSubNodeIndex]).BuildListNoSort(bsprci);
      BuildList(bsprci.rci^);
      if PositiveSubNodeIndex>0 then
         TFGBSPNode(Owner[PositiveSubNodeIndex]).BuildListNoSort(bsprci);
   end;
end;

// BuildListBackToFront
//
procedure TFGBSPNode.BuildListBackToFront(var bsprci : TBSPRenderContextInfo);
begin
   if PlaneEvaluatePoint(SplitPlane, bsprci.cameraLocal)>=0 then begin
      if NegativeSubNodeIndex>0 then
         TFGBSPNode(Owner[NegativeSubNodeIndex]).BuildListNoSort(bsprci);
      BuildList(bsprci.rci^);
      if PositiveSubNodeIndex>0 then
         TFGBSPNode(Owner[PositiveSubNodeIndex]).BuildListNoSort(bsprci);
   end else begin
      if PositiveSubNodeIndex>0 then
         TFGBSPNode(Owner[PositiveSubNodeIndex]).BuildListNoSort(bsprci);
      BuildList(bsprci.rci^);
      if NegativeSubNodeIndex>0 then
         TFGBSPNode(Owner[NegativeSubNodeIndex]).BuildListNoSort(bsprci);
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
   RegisterClasses([TBSPMeshObject, TFGBSPNode]);

end.
