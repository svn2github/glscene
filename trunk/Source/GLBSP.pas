{: GLBSP<p>

	Binary Space Partion mesh support for GLScene.<p>

   The classes of this unit are designed to operate within a TGLBaseMesh.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>05/03/03 - EG - Preliminary BSP splitting support
	   <li>31/01/03 - EG - Materials support, added CleanupUnusedNodes,
                          MaterialCache support 
	   <li>30/01/03 - EG - Creation
	</ul></font>
}
unit GLBSP;

interface

uses Classes, GLVectorFileObjects, GLScene, GLTexture, GLMisc, Geometry;

type

   TBSPCullingSphere = record
      position : TVector;
      radius : Single;
   end;

   // TBSPRenderContextInfo
   //
   TBSPRenderContextInfo = record
      //: Local coordinates of the camera (can be a vector or point)
      cameraLocal : TVector;
      rci : PRenderContextInfo;
      faceGroups : TList;
      cullingSpheres : array of TBSPCullingSphere;
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

         {: Drops all unused nodes from the facegroups list.<p>
            An unused node is a node that renders nothing and whose children
            render nothing. Indices are remapped in the process. }
         procedure CleanupUnusedNodes;

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

         procedure IsCulled(const bsprci : TBSPRenderContextInfo;
                            var positive, negative : Boolean);
         procedure CollectNoSort(var bsprci : TBSPRenderContextInfo);
         procedure CollectFrontToBack(var bsprci : TBSPRenderContextInfo);
         procedure CollectBackToFront(var bsprci : TBSPRenderContextInfo);

         {: Try to find a 'decent' split plane for the node.<p>
            Use this function to build a BSP tree, on leafy nodes. The split
            plane is chosen among the polygon planes, the coefficient are used
            to determine what a 'good' split plane is by assigning a cost
            to splitted triangles (cut by the split plane) and tree imbalance. }
         function FindSplitPlane(triangleSplitCost : Single = 1;
                                 triangleImbalanceCost : Single = 0.5) : THmgPlane;
         {: Evaluates a split plane.<p>
            Used by FindSplitPlane. For splitted triangles, the extra spawned
            triangles required are accounted for in the nbXxxTriangles values. }
         procedure EvaluateSplitPlane(const splitPlane : THmgPlane;
                                      var nbTriangleSplit : Integer;
                                      var nbPositiveTriangles : Integer;
                                      var nbNegativeTriangles : Integer);
         {: Splits a leafy node along the specified plane.<p>
            Will trigger an exception if the node already has subnodes. Currently
            also changes the mode from strips/fan to list. }
         procedure PerformSplit(const splitPlane : THmgPlane;
                                const maxTrianglesPerLeaf : Integer = MaxInt);

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

uses VectorLists, SysUtils;

const
   cOwnTriangleEpsilon = 1e-5;

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
   i, j, k, n : Integer;
   bsprci : TBSPRenderContextInfo;
   libMat : TGLLibMaterial;
   faceGroupList : TList;
   bspNodeList : PPointerList;

   procedure AbsoluteSphereToLocal(const absPos : TVector; absRadius : Single;
                                   var local : TBSPCullingSphere);
   var
      v : TVector;
   begin
      local.position:=Owner.Owner.AbsoluteToLocal(absPos);
      SetVector(v, absRadius, absRadius, absRadius, 0);
      v:=Owner.Owner.AbsoluteToLocal(v);
      local.radius:=MaxFloat(v);
   end;

begin
   if Mode<>momFaceGroups then begin
      inherited BuildList(mrci);
      Exit;
   end;
   // render BSP
   if FaceGroups.Count>0 then begin
      bsprci.cameraLocal:=Owner.Owner.AbsoluteToLocal(mrci.cameraPosition);
      SetLength(bsprci.cullingSpheres, 2);

      AbsoluteSphereToLocal(mrci.cameraPosition, 1, bsprci.cullingSpheres[0]);
      AbsoluteSphereToLocal(VectorCombine(mrci.cameraPosition,mrci.rcci.clippingDirection,
                                          1, mrci.rcci.farClippingDistance),
                            mrci.rcci.viewPortRadius*mrci.rcci.farClippingDistance,
                            bsprci.cullingSpheres[1]);

      bsprci.rci:=@mrci;
      faceGroupList:=TList.Create;
      try
         bsprci.faceGroups:=faceGroupList;
         bsprci.faceGroups.Capacity:=FaceGroups.Count div 2;
         // collect all facegroups
         case RenderSort of
            rsNone        : (FaceGroups[0] as TFGBSPNode).CollectNoSort(bsprci);
            rsBackToFront : (FaceGroups[0] as TFGBSPNode).CollectBackToFront(bsprci);
            rsFrontToBack : (FaceGroups[0] as TFGBSPNode).CollectFrontToBack(bsprci);
         else
            Assert(False);
         end;
         // render facegroups
         bspNodeList:=faceGroupList.List;
         n:=bsprci.faceGroups.Count;
         i:=0;
         while i<n do with TFGBSPNode(bspNodeList[i]) do begin
            libMat:=MaterialCache;
            if Assigned(libMat) then begin
               j:=i+1;
               while (j<n) and (TFGBSPNode(bspNodeList[j]).MaterialCache=libMat) do
                  Inc(j);
               libMat.Apply(mrci);
               repeat
                  for k:=i to j-1 do
                     TFGBSPNode(bspNodeList[k]).BuildList(mrci);
               until not libMat.UnApply(mrci);
            end else begin
               j:=i;
               while (j<n) and (TFGBSPNode(bspNodeList[j]).MaterialCache=nil) do begin
                  TFGBSPNode(bspNodeList[j]).BuildList(mrci);
                  Inc(j);
               end;
            end;
            i:=j;
         end;
      finally
         faceGroupList.Free;
      end;
   end;
   DisableOpenGLArrays(mrci);
end;

// CleanupUnusedNodes
//
procedure TBSPMeshObject.CleanupUnusedNodes;
var
   i, j, n : Integer;
   nodeParents : array of Integer;
   remapIndex : array of Integer;
   indicesToCheck : TIntegerList;
   node : TFGBSPNode;
begin
   n:=FaceGroups.Count;
   if n=0 then Exit;
   SetLength(nodeParents, n);
   indicesToCheck:=TIntegerList.Create;
   try
      // build nodes parent information
      FillChar(nodeParents[0], SizeOf(Integer)*n, 255);
      for i:=0 to n-1 do with TFGBSPNode(FaceGroups[i]) do begin
         if PositiveSubNodeIndex>0 then
            nodeParents[PositiveSubNodeIndex]:=i;
         if NegativeSubNodeIndex>0 then
            nodeParents[NegativeSubNodeIndex]:=i;
      end;
      // now proceed to deleting all the unused nodes
      indicesToCheck.AddSerie(n-1, -1, n);
      while indicesToCheck.Count>0 do begin
         i:=indicesToCheck.Pop;
         node:=TFGBSPNode(FaceGroups[i]);
         if Assigned(node) then begin
            if node.PositiveSubNodeIndex>0 then begin
               if TFGBSPNode(FaceGroups[node.PositiveSubNodeIndex])=nil then
                  node.PositiveSubNodeIndex:=0;
            end;
            if node.NegativeSubNodeIndex>0 then begin
               if TFGBSPNode(FaceGroups[node.NegativeSubNodeIndex])=nil then
                  node.NegativeSubNodeIndex:=0;
            end;
            if (node.PositiveSubNodeIndex<=0) and (node.NegativeSubNodeIndex<=0) then begin
               if node.VertexIndices.Count=0 then begin
                  if nodeParents[i]>=0 then
                     indicesToCheck.Push(nodeParents[i]);
                  FaceGroups.List[i]:=nil;
                  node.Owner:=nil;
                  node.Free;
               end;
            end;
         end;
      end;
      // build a remap index
      SetLength(remapIndex, n);
      j:=0;
      for i:=0 to n-1 do begin
         remapIndex[i]:=j;
         if FaceGroups[i]<>nil then
            Inc(j);
      end;
      // apply remap index
      for i:=0 to n-1 do begin
         node:=TFGBSPNode(FaceGroups[i]);
         if Assigned(node) then begin
            node.PositiveSubNodeIndex:=remapIndex[node.PositiveSubNodeIndex];
            node.NegativeSubNodeIndex:=remapIndex[node.NegativeSubNodeIndex];
         end;
      end;
      // and pack then FaceGroups, done, pfew!! The things we do to remain fast...
      FaceGroups.Pack;
   finally
      indicesToCheck.Free;
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

// IsCulled
//
procedure TFGBSPNode.IsCulled(const bsprci : TBSPRenderContextInfo;
                              var positive, negative : Boolean);
var
   i, n : Integer;
   d : Single;
begin
   n:=Length(bsprci.cullingSpheres);
   if n>0 then begin
      positive:=True;
      negative:=True;
      for i:=0 to n-1 do with bsprci.cullingSpheres[i] do begin
         d:=PlaneEvaluatePoint(SplitPlane, position);
         if d>=-radius then
            positive:=False;
         if d<=radius then
            negative:=False;
      end;
   end else begin
      positive:=False;
      negative:=False;
   end;
end;

// CollectNoSort
//
procedure TFGBSPNode.CollectNoSort(var bsprci : TBSPRenderContextInfo);
begin
   if (PositiveSubNodeIndex>0) then
      TFGBSPNode(Owner[PositiveSubNodeIndex]).CollectNoSort(bsprci);
   if VertexIndices.Count>0 then
      bsprci.faceGroups.Add(Self);
   if (NegativeSubNodeIndex>0) then
      TFGBSPNode(Owner[NegativeSubNodeIndex]).CollectNoSort(bsprci);
end;

// CollectFrontToBack
//
procedure TFGBSPNode.CollectFrontToBack(var bsprci : TBSPRenderContextInfo);
begin
   if PlaneEvaluatePoint(SplitPlane, bsprci.cameraLocal)>=0 then begin
      if PositiveSubNodeIndex>0 then
         TFGBSPNode(Owner[PositiveSubNodeIndex]).CollectFrontToBack(bsprci);
      if VertexIndices.Count>0 then
         bsprci.faceGroups.Add(Self);
      if NegativeSubNodeIndex>0 then
         TFGBSPNode(Owner[NegativeSubNodeIndex]).CollectFrontToBack(bsprci);
   end else begin
      if NegativeSubNodeIndex>0 then
         TFGBSPNode(Owner[NegativeSubNodeIndex]).CollectFrontToBack(bsprci);
      if VertexIndices.Count>0 then
         bsprci.faceGroups.Add(Self);
      if PositiveSubNodeIndex>0 then
         TFGBSPNode(Owner[PositiveSubNodeIndex]).CollectFrontToBack(bsprci);
   end;
end;

// CollectBackToFront
//
procedure TFGBSPNode.CollectBackToFront(var bsprci : TBSPRenderContextInfo);
begin
   if PlaneEvaluatePoint(SplitPlane, bsprci.cameraLocal)>=0 then begin
      if NegativeSubNodeIndex>0 then
         TFGBSPNode(Owner[NegativeSubNodeIndex]).CollectBackToFront(bsprci);
      if VertexIndices.Count>0 then
         bsprci.faceGroups.Add(Self);
      if PositiveSubNodeIndex>0 then
         TFGBSPNode(Owner[PositiveSubNodeIndex]).CollectBackToFront(bsprci);
   end else begin
      if PositiveSubNodeIndex>0 then
         TFGBSPNode(Owner[PositiveSubNodeIndex]).CollectBackToFront(bsprci);
      if VertexIndices.Count>0 then
         bsprci.faceGroups.Add(Self);
      if NegativeSubNodeIndex>0 then
         TFGBSPNode(Owner[NegativeSubNodeIndex]).CollectBackToFront(bsprci);
   end;
end;

// FindSplitPlane
//
function TFGBSPNode.FindSplitPlane(triangleSplitCost : Single = 1;
                                   triangleImbalanceCost : Single = 0.5) : THmgPlane;
var
   i, k, n : Integer;
   ns, np, nn : Integer;
   evalPlane : THmgPlane;
   bestEval, eval : Single;
   vertices : TAffineVectorList;
begin
   Result:=NullHmgVector;
   bestEval:=1e30;
   n:=VertexIndices.Count;
   vertices:=Owner.Owner.Vertices;
   if n>0 then for k:=0 to n div 4 do begin
      case Mode of
         fgmmTriangles, fgmmFlatTriangles : begin
            i:=Random((n div 3)-1)*3;
            evalPlane:=PlaneMake(vertices[VertexIndices[i]],
                                 vertices[VertexIndices[i+1]],
                                 vertices[VertexIndices[i+2]]);
         end;
         fgmmTriangleStrip : begin
            i:=Random(n-2);
            evalPlane:=PlaneMake(vertices[VertexIndices[i]],
                                 vertices[VertexIndices[i+1]],
                                 vertices[VertexIndices[i+2]]);
         end;
      else
         // fgmmTriangleFan
         i:=Random(n-2);
         evalPlane:=PlaneMake(vertices[VertexIndices[0]],
                              vertices[VertexIndices[i]],
                              vertices[VertexIndices[i+1]]);
      end;
      EvaluateSplitPlane(evalPlane, ns, np, nn);
      eval:=ns*triangleSplitCost+Abs(np-nn)*0.5*triangleImbalanceCost;
      if eval<bestEval then begin
         bestEval:=eval;
         Result:=evalPlane;
      end;
   end;
end;

// EvaluateSplitPlane
//
procedure TFGBSPNode.EvaluateSplitPlane(const splitPlane : THmgPlane;
                                        var nbTriangleSplit : Integer;
                                        var nbPositiveTriangles : Integer;
                                        var nbNegativeTriangles : Integer);
var
   i, n, inci, lookupIdx : Integer;
   a, b, c : Boolean;
   vertices : TAffineVectorList;
const
   // case resolution lookup tables (node's tris unaccounted for)
   cTriangleSplit : array [0..7] of Integer = (0, 1, 1, 1, 1, 1, 1, 0);
   cPositiveTris  : array [0..7] of Integer = (0, 1, 1, 2, 1, 2, 2, 1);
   cNegativeTris  : array [0..7] of Integer = (1, 2, 2, 1, 2, 1, 1, 0);
begin
   nbTriangleSplit:=0;
   nbPositiveTriangles:=0;
   nbNegativeTriangles:=0;
   n:=VertexIndices.Count;
   if n<3 then Exit;
   vertices:=Owner.Owner.Vertices;
   case Mode of
      fgmmTriangleStrip, fgmmTriangleFan : begin
         n:=n-2;
         inci:=1;
      end;
   else
      inci:=3;
   end;
   i:=0; while i<n do begin
      case Mode of
         fgmmTriangleFan : begin
            a:=PlaneEvaluatePoint(splitPlane, vertices[VertexIndices[0]])>0;
            b:=PlaneEvaluatePoint(splitPlane, vertices[VertexIndices[i]])>0;
            c:=PlaneEvaluatePoint(splitPlane, vertices[VertexIndices[i+1]])>0;
         end;
      else
         // fgmmTriangles, fgmmFlatTriangles, fgmmTriangleStrip
         a:=PlaneEvaluatePoint(splitPlane, vertices[VertexIndices[i]])>0;
         b:=PlaneEvaluatePoint(splitPlane, vertices[VertexIndices[i+1]])>0;
         c:=PlaneEvaluatePoint(splitPlane, vertices[VertexIndices[i+2]])>0;
      end;
      lookupIdx:=(Integer(a) shl 2)+(Integer(b) shl 1)+Integer(c);
      Inc(nbTriangleSplit, cTriangleSplit[lookupIdx]);
      Inc(nbPositiveTriangles, cPositiveTris[lookupIdx]);
      Inc(nbNegativeTriangles, cNegativeTris[lookupIdx]);
      Inc(i, inci);
   end;
end;

// PerformSplit
//
procedure TFGBSPNode.PerformSplit(const splitPlane : THmgPlane;
                                  const maxTrianglesPerLeaf : Integer = MaxInt);
var
   fgPos, fgNeg : TFGBSPNode;
   fgPosIndices, fgNegIndices : TIntegerList;
   indices : TIntegerList;

   function AddLerp(iA, iB : Integer; fB, fA : Single) : Integer;
   begin
      with Owner.Owner do begin
         with Vertices do Result:=Add(VectorCombine(List[iA], List[iB], fA, fB));
         with Normals do if Count>0 then
            Add(VectorCombine(List[iA], List[iB], fA, fB));
         with Colors do if Count>0 then
            Add(VectorCombine(List[iA], List[iB], fA, fB));
         with TexCoords do if Count>0 then
            Add(VectorCombine(List[iA], List[iB], fA, fB));
         with LighmapTexCoords do if Count>0 then
            Add(TexPointCombine(List[iA], List[iB], fA, fB));
      end;
   end;

   procedure SplitTriangleMid(strayID, strayNext, strayPrev : Integer;
                              eNext, ePrev : Single);
   var
      iOpp : Integer;
      invSum : Single;
   begin
      invSum:=1/(Abs(eNext)+Abs(ePrev));
      iOpp:=AddLerp(strayNext, strayPrev, Abs(eNext)*invSum, Abs(ePrev)*invSum);
      if eNext>0 then begin
         fgPosIndices.Add(strayID, strayNext, iOpp);
         fgNegIndices.Add(iOpp, strayPrev, strayID);
      end else begin
         fgNegIndices.Add(strayID, strayNext, iOpp);
         fgPosIndices.Add(iOpp, strayPrev, strayID);
      end;
   end;

   procedure SplitTriangle(strayID, strayNext, strayPrev : Integer;
                           eStray, eNext, ePrev : Single);
   var
      iNext, iPrev : Integer;
      invSum : Single;
   begin
      invSum:=1/(Abs(eNext)+Abs(eStray));
      iNext:=AddLerp(strayNext, strayID, Abs(eNext)*invSum, Abs(eStray)*invSum);
      invSum:=1/(Abs(ePrev)+Abs(eStray));
      iPrev:=AddLerp(strayPrev, strayID, Abs(ePrev)*invSum, Abs(eStray)*invSum);
      if eStray>0 then begin
         fgPos.VertexIndices.Add(strayID, iNext, iPrev);
         fgNeg.VertexIndices.Add(strayNext, strayPrev, iPrev);
         fgNeg.VertexIndices.Add(iPrev, iNext, strayNext);
      end else if eStray<0 then begin
         fgNeg.VertexIndices.Add(strayID, iNext, iPrev);
         fgPos.VertexIndices.Add(strayNext, strayPrev, iPrev);
         fgPos.VertexIndices.Add(iPrev, iNext, strayNext);
      end;
   end;

var
   i, i1, i2, i3, se1, se2, se3 : Integer;
   e1, e2, e3 : Single;
   vertices : TAffineVectorList;
   subSplitPlane : THmgPlane;
begin
   Assert((PositiveSubNodeIndex=0) and (NegativeSubNodeIndex=0));
   ConvertToList;
   // prepare sub nodes
   FPositiveSubNodeIndex:=Owner.Count;
   fgPos:=TFGBSPNode.CreateOwned(Owner);
   fgPosIndices:=fgPos.VertexIndices;
   FNegativeSubNodeIndex:=Owner.Count;
   fgNeg:=TFGBSPNode.CreateOwned(Owner);
   fgNegIndices:=fgNeg.VertexIndices;
   // initiate split
   Self.FSplitPlane:=splitPlane;
   indices:=TIntegerList.Create;
   vertices:=Owner.Owner.Vertices;
   i:=0; while i<VertexIndices.Count do begin
      // evaluate all points
      i1:=VertexIndices[i];
      e1:=PlaneEvaluatePoint(splitPlane, vertices.List[i1]);
      i2:=VertexIndices[i+1];
      e2:=PlaneEvaluatePoint(splitPlane, vertices.List[i2]);
      i3:=VertexIndices[i+2];
      e3:=PlaneEvaluatePoint(splitPlane, vertices.List[i3]);
      if Abs(e1)<cOwnTriangleEpsilon then begin
         e1:=0;
         se1:=0;
      end else se1:=Sign(e1);
      if Abs(e2)<cOwnTriangleEpsilon then begin
         e2:=0;
         se2:=0;
      end else se2:=Sign(e2);
      if Abs(e3)<cOwnTriangleEpsilon then begin
         e3:=0;
         se3:=0;
      end else se3:=Sign(e3);
      // case disjunction
      case se1 of
         -1 : case se2 of
            -1 : case se3 of
               -1, 0 : fgNegIndices.Add(i1, i2, i3);
               +1 : SplitTriangle(i3, i1, i2, e3, e1, e2);
            end;
            0 : case se3 of
               -1, 0 : fgNegIndices.Add(i1, i2, i3);
               +1 : SplitTriangleMid(i2, i3, i1, e3, e1);
            end;
            +1 : case se3 of
               -1 : SplitTriangle(i2, i3, i1, e2, e3, e1);
               0  : SplitTriangleMid(i3, i1, i2, e1, e2);
               +1 : SplitTriangle(i1, i2, i3, e1, e2, e3);
            end;
         end;
         0 : case se2 of
            -1 : case se3 of
               -1, 0 : fgNegIndices.Add(i1, i2, i3);
               +1 : SplitTriangleMid(i1, i2, i3, e2, e3);
            end;
            0 : case se3 of
               -1 : fgNegIndices.Add(i1, i2, i3);
               0  : indices.Add(i1, i2, i3);
               +1 : fgPosIndices.Add(i1, i2, i3);
            end;
            +1 : case se3 of
               -1 : SplitTriangleMid(i1, i2, i3, e2, e3);
               0, +1 : fgPosIndices.Add(i1, i2, i3);
            end;
         end;
         +1 : case se2 of
            -1 : case se3 of
               -1 : SplitTriangle(i1, i2, i3, e1, e2, e3);
               0  : SplitTriangleMid(i3, i1, i2, e1, e2);
               +1 : SplitTriangle(i2, i3, i1, e2, e3, e1);
            end;
            0 : case se3 of
               -1 : SplitTriangleMid(i2, i3, i1, e3, e1);
               0, +1 : fgPosIndices.Add(i1, i2, i3);
            end;
            +1 : case se3 of
               -1 : SplitTriangle(i3, i1, i2, e3, e1, e2);
               0, +1 : fgPosIndices.Add(i1, i2, i3);
            end;
         end;
      end;
      Inc(i, 3);
   end;
   VertexIndices:=indices;
   indices.Free;
   if fgPos.TriangleCount=0 then begin
      FPositiveSubNodeIndex:=0;
      FNegativeSubNodeIndex:=FNegativeSubNodeIndex-1;
      FreeAndNil(fgPos);
   end;
   if fgNeg.TriangleCount=0 then begin
      FNegativeSubNodeIndex:=0;
      FreeAndNil(fgNeg);
   end;
   if Assigned(fgPos) and (fgPos.TriangleCount>maxTrianglesPerLeaf) then begin
      subSplitPlane:=fgPos.FindSplitPlane;
      fgPos.PerformSplit(subSplitPlane, maxTrianglesPerLeaf);
   end;
   if Assigned(fgNeg) and (fgNeg.TriangleCount>maxTrianglesPerLeaf) then begin
      subSplitPlane:=fgNeg.FindSplitPlane;
      fgNeg.PerformSplit(subSplitPlane, maxTrianglesPerLeaf);
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
