// GLROAMPatch
{: Class for managing a ROAM (square) patch.<p>

	<b>History : </b><font size=-1><ul>
      <li>06/02/03 - EG - Adaptative variance computation
      <li>03/12/02 - EG - Minor ROAM tessel/render optimizations
      <li>15/06/02 - EG - Fixed patch rendering bug "introduced" by TBaseList fix
      <li>24/02/02 - EG - Hybrid ROAM-stripifier engine
      <li>10/09/01 - EG - Creation
	</ul></font>
}
unit GLROAMPatch;

interface

uses Geometry, GLHeightData, VectorLists, GLCrossPlatform, GLContext;

type

   // TROAMTriangleNode
   //
   PROAMTriangleNode = ^TROAMTriangleNode;
   TROAMTriangleNode = packed record
      base, left, right : PROAMTriangleNode;
      leftChild, rightChild : PROAMTriangleNode;
   end;

   // TROAMRenderPoint
   //
   TROAMRenderPoint = packed record
      X, Y : Integer;
      idx : Integer;
   end;

   TCardinalArray = array [0..MaxInt shr 3] of Cardinal;
   PCardinalArray = ^TCardinalArray;

	// TGLROAMPatch
	//
  	TGLROAMPatch = class (TObject)
	   private
	      { Private Declarations }
         FHeightData : THeightData; // Referred, not owned
         FHeightRaster : PSmallIntRaster;
         FTLNode, FBRNode : PROAMTriangleNode;
         FMaxDepth : Integer;
         FMaxTLVarianceDepth, FMaxBRVarianceDepth : Integer;
         FTLVariance, FBRVariance : array of Cardinal;
         FPatchSize, FTriangleCount : Integer;
         FListHandle : TGLListHandle;
         FTag : Integer;
         FVertexScale, FVertexOffset : TAffineVector;
         FTextureScale, FTextureOffset : TAffineVector;
         FObserverPosition : TAffineIntVector;
         FNorth, FSouth, FWest, FEast : TGLROAMPatch; // neighbours
         FHighRes, FNoDetails : Boolean;

	   protected
	      { Protected Declarations }
         procedure SetHeightData(val : THeightData);

         procedure RenderROAM(vertices : TAffineVectorList;
                              vertexIndices : TIntegerList;
                              texCoords : TTexPointList);
         procedure RenderAsStrips(vertices : TAffineVectorList;
                                  vertexIndices : TIntegerList;
                                  texCoords : TTexPointList);

	   public
	      { Public Declarations }
	      constructor Create;
         destructor Destroy; override;

         procedure ComputeVariance(variance : Integer);

         procedure ResetTessellation;
         procedure ConnectToTheWest(westPatch : TGLROAMPatch);
         procedure ConnectToTheNorth(northPatch : TGLROAMPatch);
         procedure Tesselate;

         {: Render the patch.<p>
            The lists are assumed to have enough capacity to allow AddNC calls
            (additions without capacity check). }
         procedure Render(vertices : TAffineVectorList;
                          vertexIndices : TIntegerList;
                          texCoords : TTexPointList);

         property HeightData : THeightData read FHeightData write SetHeightData;
         property VertexScale : TAffineVector read FVertexScale write FVertexScale;
         property VertexOffset : TAffineVector read FVertexOffset write FVertexOffset;

         property ObserverPosition : TAffineIntVector read FObserverPosition write FObserverPosition;

         property TextureScale : TAffineVector read FTextureScale write FTextureScale;
         property TextureOffset : TAffineVector read FTextureOffset write FTextureOffset;

         property HighRes : Boolean read FHighRes write FHighRes;
         property NoDetails : Boolean read FNoDetails write FNoDetails;

         property TriangleCount : Integer read FTriangleCount;
         property Tag : Integer read FTag write FTag;
	end;

{: Specifies the maximum number of ROAM triangles that may be allocated. }
procedure SetROAMTrianglesCapacity(nb : Integer);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses OpenGL12, XOpenGL, SysUtils;

type

   // TROAMVariancePoint
   //
   TROAMVariancePoint = packed record
      X, Y : Integer;
      Z : Integer;
   end;

var
   vNbTris, vTriangleNodesCapacity : Integer;
   vTriangleNodes : array of TROAMTriangleNode;

// SetROAMTrianglesCapacity
//
procedure SetROAMTrianglesCapacity(nb : Integer);
begin
   vNbTris:=0;
   if vTriangleNodesCapacity<>nb then begin
      SetLength(vTriangleNodes, nb);
      vTriangleNodesCapacity:=nb;
   end;
end;

// AllocTriangleNode
//
function AllocTriangleNode : PROAMTriangleNode;
begin
   if vNbTris<vTriangleNodesCapacity then begin
      Result:=@vTriangleNodes[vNbTris];
      with Result^ do begin
         leftChild:=nil;
         rightChild:=nil;
      end;
      Inc(vNbTris);
   end else Result:=nil;
end;

// Split
//
function Split(tri : PROAMTriangleNode) : Boolean;
var
   buf : PROAMTriangleNode;
begin
   with tri^ do if not Assigned(leftChild) then begin
   	// If this triangle is not in a proper diamond, force split our base neighbor
	   if Assigned(base) and (base.base<>tri) then
         Split(base);

	   // Create children and cross-link them
      if vNbTris<vTriangleNodesCapacity then begin
         leftChild:=@vTriangleNodes[vNbTris];
         rightChild:=PROAMTriangleNode(Integer(leftChild)+SizeOf(TROAMTriangleNode));
         with leftChild^ do begin
            base:=tri.left;
            leftChild:=nil;
            rightChild:=tri.leftChild;
            left:=tri.rightChild;
         end;
         with rightChild^ do begin
            base:=tri.right;
            leftChild:=nil;
            rightChild:=tri.leftChild;
            right:=tri.leftChild;
         end;
         Inc(vNbTris, 2);
      end else begin
         Result:=False;
         Exit;
      end;

	   // Link our Left Neighbor to the new children
	   if Assigned(left) then begin
         if left.base=tri then
            left.base:=leftChild
         else if left.left=tri then
            left.left:=leftChild
         else left.right:=leftChild
      end;

	   // Link our Right Neighbor to the new children
	   if Assigned(right) then begin
         if right.base=tri then
            right.base:=rightChild
         else if right.left=tri then
            right.left:=rightChild
         else right.right:=rightChild
      end;

      // Link our Base Neighbor to the new children
      if Assigned(base) then begin
         if Assigned(base.leftChild) then begin
            base.leftChild.right:=rightChild;
            rightChild.left:=base.leftChild;
            base.rightChild.left:=leftChild;
            leftChild.right:=base.rightChild;
         end else Split(base);
      end else begin
		   // An edge triangle, trivial case.
         buf:=nil;
		   leftChild.right:=buf;
		   rightChild.left:=buf;
      end;
   end;
   Result:=True;
end;

// ------------------
// ------------------ TGLROAMPatch ------------------
// ------------------

// Create
//
constructor TGLROAMPatch.Create;
begin
	inherited Create;
   FListHandle:=TGLListHandle.Create;
end;

// Destroy
//
destructor TGLROAMPatch.Destroy;
begin
   FListHandle.Free;
	inherited Destroy;
end;

// SetHeightData
//
procedure TGLROAMPatch.SetHeightData(val : THeightData);
begin
   FHeightData:=val;
   FPatchSize:=FHeightData.Size-1;
   FHeightRaster:=val.SmallIntRaster;
end;

// ConnectToTheWest
//
procedure TGLROAMPatch.ConnectToTheWest(westPatch : TGLROAMPatch);
begin
   if Assigned(westPatch) then begin
      if not (westPatch.HighRes or HighRes) then begin
         FTLNode.left:=westPatch.FBRNode;
         westPatch.FBRNode.left:=FTLNode;
      end;
      FWest:=westPatch;
      westPatch.FEast:=Self;
   end;
end;

// ConnectToTheNorth
//
procedure TGLROAMPatch.ConnectToTheNorth(northPatch : TGLROAMPatch);
begin
   if Assigned(northPatch) then begin
      if not (northPatch.HighRes or HighRes) then begin
         FTLNode.right:=northPatch.FBRNode;
         northPatch.FBRNode.right:=FTLNode;
      end;
      FNorth:=northPatch;
      northPatch.FSouth:=Self;
   end;
end;

// ComputeVariance
//
procedure TGLROAMPatch.ComputeVariance(variance : Integer);
var
   raster : PSmallIntRaster;
   currentVariance : PIntegerArray;
   maxVarianceDepth : Integer;
   maxNonNullIndex : Integer;

   function ROAMVariancePoint(anX, anY : Integer) : TROAMVariancePoint;
   begin
      Result.X:=anX;
      Result.Y:=anY;
      Result.Z:=(Integer(FHeightRaster[anY][anX]) shl 8);
   end;

   function RecursComputeVariance(const left, right, apex : TROAMVariancePoint;
                                  node : Integer) : Cardinal;
   var
      half : TROAMVariancePoint;
      v : Cardinal;
      n2 : Integer;
   begin
      with half do begin
         X:=(left.X+right.X) shr 1;
         Y:=(left.Y+right.Y) shr 1;
         Z:=Integer(raster[Y][X]) shl 8;
         Result:=Abs(((left.Z+right.Z) div 2)-Z) div variance;
      end;

      n2:=node shl 1;
      if n2<maxVarianceDepth then begin
         v:=RecursComputeVariance(apex,  left, half,   n2);
         if v>Result then Result:=v;
         v:=RecursComputeVariance(right, apex, half, 1+n2);
         if v>Result then Result:=v;
      end;
      currentVariance[node]:=Result;
   end;

   procedure ScaleVariance(n, d : Integer);
   var
      newVal : Integer;
   begin
      if d>=0 then
         newVal:=(currentVariance[n] shl (d shr 1))
      else newVal:=(currentVariance[n] shr (-d shr 1));
      currentVariance[n]:=newVal;
      if newVal>0 then
         if n>maxNonNullIndex then
            maxNonNullIndex:=n;
      n:=n shl 1;
    	if n<maxVarianceDepth then begin
         Dec(d);
         ScaleVariance(n,   d);
         ScaleVariance(n+1, d);
      end;
   end;

var
   s, p : Integer;
begin
   s:=Sqr(FPatchSize);
   raster:=FHeightRaster;
   FMaxDepth:=1;
   p:=-1-8;
   repeat
      FMaxDepth:=FMaxDepth shl 2;
      Inc(p);
   until FMaxDepth>=s;
   maxVarianceDepth:=FMaxDepth;
   SetLength(FTLVariance, maxVarianceDepth);
   SetLength(FBRVariance, maxVarianceDepth);

   s:=FPatchSize;
   currentVariance:=@FTLVariance[0];
   maxNonNullIndex:=1;
   RecursComputeVariance(ROAMVariancePoint(0, s), ROAMVariancePoint(s, 0),
                         ROAMVariancePoint(0, 0), 1);
   ScaleVariance(1, p);
   FMaxTLVarianceDepth:=maxNonNullIndex+1;
   SetLength(FTLVariance, FMaxTLVarianceDepth);
   currentVariance:=@FBRVariance[0];
   maxNonNullIndex:=1;
   RecursComputeVariance(ROAMVariancePoint(s, 0), ROAMVariancePoint(0, s),
                         ROAMVariancePoint(s, s), 1);
   ScaleVariance(1, p);
   FMaxBRVarianceDepth:=maxNonNullIndex+1;
   SetLength(FBRVariance, FMaxBRVarianceDepth);
end;

// ResetTessellation
//
procedure TGLROAMPatch.ResetTessellation;
begin
   FTLNode:=AllocTriangleNode;
   FBRNode:=AllocTriangleNode;
   FTLNode.base:=FBRNode;
   FTLNode.left:=nil;
   FTLNode.right:=nil;
   FBRNode.base:=FTLNode;
   FBRNode.left:=nil;
   FBRNode.right:=nil;
   FNorth:=nil;
   FSouth:=nil;
   FWest:=nil;
   FEast:=nil;
end;

// Tesselate
//
var
   tessFrameVarianceDelta : Integer;
   tessMaxVariance : Cardinal;
   tessMaxDepth : Cardinal;
   tessCurrentVariance : PIntegerArray;

procedure RecursTessellate(tri : PROAMTriangleNode;
                           n : Cardinal;
                           const left, right, apex : Cardinal);
var
   d : Integer;
begin
   d:=((left+right) shr 1);//+tessFrameVarianceDelta;
   if tessCurrentVariance[n]>d then begin
      if Split(tri) then begin
         n:=n shl 1;
         if n<tessMaxVariance then begin
            RecursTessellate(tri.leftChild,  n,   apex,  left, d);
            RecursTessellate(tri.rightChild, n+1, right, apex, d);
         end;
      end;
   end;
end;

procedure TGLROAMPatch.Tesselate;

   function VertexDist(x, y : Integer) : Cardinal;
   var
      f : Single;
   const
      c1Div100 : Single = 0.01;
   begin
      f:=Sqr(x-FObserverPosition[0])+Sqr(y-FObserverPosition[1])+tessFrameVarianceDelta;
      Result:=Round(Sqrt(f)+f*c1Div100);
   end;

   procedure FullBaseTess(tri : PROAMTriangleNode; n : Cardinal); forward;

   procedure FullLeftTess(tri : PROAMTriangleNode; n : Cardinal);
   begin
      if Split(tri) then begin
         n:=n shl 1;
         if n<tessMaxDepth then
            FullBaseTess(tri.leftChild, n);
      end;
   end;

   procedure FullRightTess(tri : PROAMTriangleNode; n : Cardinal);
   begin
      if Split(tri) then begin
         n:=n shl 1;
         if n<tessMaxDepth then
            FullBaseTess(tri.rightChild, n);
      end;
   end;

   procedure FullBaseTess(tri : PROAMTriangleNode; n : Cardinal);
   begin
      if Split(tri) then begin
         n:=n shl 1;
         if n<tessMaxDepth then begin
            FullRightTess(tri.leftChild, n);
            FullLeftTess(tri.rightChild, n);
         end;
      end;
   end;

var
   s : Integer;
begin
   if HighRes then Exit;

   tessMaxDepth:=FMaxDepth;

   if Assigned(FNorth) and FNorth.HighRes then
      FullRightTess(FTLNode, 1);
   if Assigned(FSouth) and FSouth.HighRes then
      FullRightTess(FBRNode, 1);
   if Assigned(FEast) and FEast.HighRes then
      FullLeftTess(FBRNode, 1);
   if Assigned(FWest) and FWest.HighRes then
      FullLeftTess(FTLNode, 1);
   if FObserverPosition[2]>0 then
      tessFrameVarianceDelta:=Sqr(FObserverPosition[2] shr 1)
   else tessFrameVarianceDelta:=0;
   s:=FPatchSize;
   tessCurrentVariance:=@FTLVariance[0];
   tessMaxVariance:=FMaxTLVarianceDepth;
   RecursTessellate(FTLNode, 1, VertexDist(0, s), VertexDist(s, 0), VertexDist(0, 0));
   tessCurrentVariance:=@FBRVariance[0];
   tessMaxVariance:=FMaxBRVarianceDepth;
   RecursTessellate(FBRNode, 1, VertexDist(s, 0), VertexDist(0, s), VertexDist(s, s));
end;

// Render
//
procedure TGLROAMPatch.Render(vertices : TAffineVectorList;
                              vertexIndices : TIntegerList;
                              texCoords : TTexPointList);
begin
   if FNoDetails then begin
      glActiveTextureARB(GL_TEXTURE1_ARB);
      glDisable(GL_TEXTURE_2D);
      glActiveTextureARB(GL_TEXTURE0_ARB);
   end;
   if HighRes then begin
      // High-res tiles use a brute-force stripifier and display lists
      if FListHandle.Handle=0 then begin
         RenderAsStrips(vertices, vertexIndices, texCoords);
         vertices.Translate(VertexOffset);
         texCoords.ScaleAndTranslate(PTexPoint(@TextureScale)^,
                                     PTexPoint(@TextureOffset)^);

         FListHandle.AllocateHandle;
         glNewList(FListHandle.Handle, GL_COMPILE);
         glDrawElements(GL_TRIANGLE_STRIP, vertexIndices.Count,
                        GL_UNSIGNED_INT, vertexIndices.List);
         glEndList;

         FTriangleCount:=vertexIndices.Count-2*FPatchSize;
      end;
      glCallList(FListHandle.Handle);
   end else begin
      // CLOD tiles are rendered via ROAM
      RenderROAM(vertices, vertexIndices, texCoords);
      vertices.Translate(VertexOffset);
      texCoords.ScaleAndTranslate(PTexPoint(@TextureScale)^,
                                  PTexPoint(@TextureOffset)^);
      if GL_EXT_compiled_vertex_array then begin
         glLockArraysEXT(0, vertices.Count);
         glDrawElements(GL_TRIANGLES, vertexIndices.Count, GL_UNSIGNED_INT, vertexIndices.List);
         glUnLockArraysEXT;
      end else begin
         glDrawElements(GL_TRIANGLES, vertexIndices.Count, GL_UNSIGNED_INT, vertexIndices.List);
      end;
      FTriangleCount:=vertexIndices.Count div 3;
   end;
   if FNoDetails then begin
      glActiveTextureARB(GL_TEXTURE1_ARB);
      glEnable(GL_TEXTURE_2D);
      glActiveTextureARB(GL_TEXTURE0_ARB);
   end;
end;

// RenderROAM
//
var
   renderRaster : PSmallIntRaster;
   renderIndices : PIntegerArray;
   renderVertices : TAffineVectorList;
   renderTexCoords : TTexPointList;

procedure RecursRender(const tri : PROAMTriangleNode;
                       const left, right, apex : TROAMRenderPoint);
var
   half : TROAMRenderPoint;
   localIndices : PIntegerArray;
begin
   if Assigned(tri.leftChild) then begin
      with half do begin
         X:=(left.X+right.X) shr 1;
         Y:=(left.Y+right.Y) shr 1;
         renderTexCoords.AddNC(@X);
         Idx:=renderVertices.AddNC(@X, renderRaster[Y][X]);
      end;
      RecursRender(tri.leftChild , apex , left, half);
      RecursRender(tri.rightChild, right, apex, half);
   end else begin
      localIndices:=renderIndices;
      localIndices[0]:=left.idx;
      localIndices[1]:=apex.idx;
      localIndices[2]:=right.idx;
      renderIndices:=PIntegerArray(Integer(localIndices)+3*SizeOf(Integer));
   end;
end;

procedure TGLROAMPatch.RenderROAM(vertices : TAffineVectorList;
                                  vertexIndices : TIntegerList;
                                  texCoords : TTexPointList);

   procedure ROAMRenderPoint(var p : TROAMRenderPoint; anX, anY : Integer);
   begin
      p.X:=anX;
      p.Y:=anY;
      p.Idx:=vertices.Add(anX, anY, renderRaster[anY][anX]);
      texCoords.Add(anX, anY);
   end;

var
   rtl, rtr, rbl, rbr : TROAMRenderPoint;
begin
   vertices.Count:=0;
   texCoords.Count:=0;
   renderVertices:=vertices;
   renderTexCoords:=texCoords;
   vertexIndices.AdjustCapacityToAtLeast(Sqr(FPatchSize)*6);
   // this is required, the actual item count is maintained out of the list scope
   vertexIndices.SetCountResetsMemory:=False;
   renderIndices:=vertexIndices.List;

   renderRaster:=FHeightData.SmallIntRaster;

   ROAMRenderPoint(rtl, 0,          0);
   ROAMRenderPoint(rtr, FPatchSize, 0);
   ROAMRenderPoint(rbl, 0,          FPatchSize);
   ROAMRenderPoint(rbr, FPatchSize, FPatchSize);

   RecursRender(FTLNode, rbl, rtr, rtl);
   RecursRender(FBRNode, rtr, rbl, rbr);

   vertexIndices.Count:=(Integer(renderIndices)-Integer(vertexIndices.List)) div SizeOf(Integer);
end;

// RenderAsStrips
//
procedure TGLROAMPatch.RenderAsStrips(vertices : TAffineVectorList;
                                      vertexIndices : TIntegerList;
                                      texCoords : TTexPointList);

var
   x, y, baseTop, rowLength : Integer;
   p : TAffineVector;
   row : PSmallIntArray;
   raster : PSmallIntRaster;
   tex : TTexPoint;
   verticesList : PAffineVector;
   texCoordsList : PTexPoint;
   indicesList : PInteger;
begin
   raster:=FHeightData.SmallIntRaster;
   rowLength:=FPatchSize+1;
   // prepare vertex data
   vertices.Count:=Sqr(rowLength);
   verticesList:=PAffineVector(vertices.List);
   texCoords.Count:=Sqr(rowLength);
   texCoordsList:=PTexPoint(texCoords.List);
   for y:=0 to FPatchSize do begin
      p[1]:=y;
      tex.T:=p[1];
      row:=raster[y];
      for x:=0 to FPatchSize do begin
         p[0]:=x;
         tex.S:=p[0];
         p[2]:=row[x];
         verticesList^:=p;
         Inc(verticesList);
         texCoordsList^:=tex;
         Inc(texCoordsList);
      end;
   end;
   // build indices list
   baseTop:=0;
   vertexIndices.Count:=(rowLength*2+2)*FPatchSize-1;
   indicesList:=PInteger(vertexIndices.List);
   y:=0; while y<FPatchSize do begin
      if y>0 then begin
         indicesList^:=baseTop+FPatchSize;
         Inc(indicesList);
      end;
      for x:=baseTop+FPatchSize downto baseTop do begin
         indicesList^:=x;
         PIntegerArray(indicesList)[1]:=rowLength+x;
         Inc(indicesList, 2);
      end;
      indicesList^:=baseTop+rowLength;
      Inc(baseTop, rowLength);
      PIntegerArray(indicesList)[1]:=baseTop+rowLength;
      Inc(indicesList, 2);
      for x:=baseTop to baseTop+FPatchSize do begin
         indicesList^:=rowLength+x;
         PIntegerArray(indicesList)[1]:=x;
         Inc(indicesList, 2);
      end;
      indicesList^:=baseTop+FPatchSize;
      Inc(indicesList);
      Inc(baseTop, rowLength);
      Inc(y, 2);
   end;
   vertexIndices.Count:=vertexIndices.Count-1;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

finalization

   SetROAMTrianglesCapacity(0);

end.
