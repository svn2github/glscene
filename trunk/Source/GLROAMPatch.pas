// GLROAMPatch
{: Class for managing a ROAM (square) patch.<p>

   *** UNDER CONSTRUCTION ***

	<b>History : </b><font size=-1><ul>
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

   // TROAMVariancePoint
   //
   TROAMVariancePoint = packed record
      X, Y : Integer;
      Z : Integer;
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
         FVarianceDepth, FMaxVariance : Integer;
         FTLVariance, FBRVariance : array of Cardinal;
         FPatchSize, FTriangleCount : Integer;
         FListHandle : TGLListHandle;
         FTag : Integer;
         FVertexScale, FVertexOffset : TAffineVector;
         FTextureScale, FTextureOffset : TAffineVector;
         FObserverPosition : TAffineIntVector;
         FNorth, FSouth, FWest, FEast : TGLROAMPatch; // neighbours
         FHighRes : Boolean;

	   protected
	      { Protected Declarations }
         procedure SetHeightData(val : THeightData);

         function ROAMVariancePoint(anX, anY : Integer) : TROAMVariancePoint;
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

         procedure ComputeVariance(depth : Integer; variance : Integer);

         procedure ResetTessellation;
         procedure ConnectToTheWest(westPatch : TGLROAMPatch);
         procedure ConnectToTheNorth(northPatch : TGLROAMPatch);
         procedure Tesselate;

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

uses OpenGL12;

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
procedure Split(tri : PROAMTriangleNode);
var
   buf : PROAMTriangleNode;
begin
   with tri^ do begin
      if Assigned(leftChild) then Exit;

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
      end else Exit;

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
            base.rightChild.left:=leftChild;
            leftChild.right:=base.rightChild;
            rightChild.left:=base.leftChild;
         end else Split(base);
      end else begin
		   // An edge triangle, trivial case.
         buf:=nil;
		   leftChild.right:=buf;
		   rightChild.left:=buf;
      end;
   end;
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

// ROAMVariancePoint
//
function TGLROAMPatch.ROAMVariancePoint(anX, anY : Integer) : TROAMVariancePoint;
begin
   Result.X:=anX;
   Result.Y:=anY;
   Result.Z:=FHeightRaster[anY][anX];
end;

// ConnectToTheWest
//
procedure TGLROAMPatch.ConnectToTheWest(westPatch : TGLROAMPatch);
begin
   FTLNode.left:=westPatch.FBRNode;
   westPatch.FBRNode.left:=FTLNode;
   FWest:=westPatch;
   westPatch.FEast:=Self;
end;

// ConnectToTheNorth
//
procedure TGLROAMPatch.ConnectToTheNorth(northPatch : TGLROAMPatch);
begin
   FTLNode.right:=northPatch.FBRNode;
   northPatch.FBRNode.right:=FTLNode;
   FNorth:=northPatch;
   northPatch.FSouth:=Self;
end;

// ComputeVariance
//
procedure TGLROAMPatch.ComputeVariance(depth : Integer; variance : Integer);
var
   raster : PSmallIntRaster;
   currentVariance : PIntegerArray;

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
         Z:=raster[Y][X];
         Result:=Sqr(((left.Z+right.Z) div 2)-Z) div variance;
      end;

      n2:=node*2;
      if n2<FMaxVariance then begin
         v:=RecursComputeVariance(apex,  left, half,   n2);
         if v>Result then Result:=v;
         v:=RecursComputeVariance(right, apex, half, 1+n2);
         if v>Result then Result:=v;
      end;
      currentVariance[node]:=Result;
   end;

   procedure ScaleVariance(n, d : Integer);
   begin
      if d>0 then
         currentVariance[n]:=(currentVariance[n] shl d)
      else if d<0 then
         currentVariance[n]:=(currentVariance[n] shr (-d));
      n:=n*2;
    	if n<FMaxVariance then begin
         Dec(d);
         ScaleVariance(n,   d);
         ScaleVariance(n+1, d);
      end;
   end;

var
   s, p : Integer;
begin
   s:=Sqr(FPatchSize);
   FVarianceDepth:=depth+1;
   raster:=FHeightRaster;
   repeat
      Dec(FVarianceDepth);
      FMaxVariance:=1 shl FVarianceDepth;
   until FMaxVariance<=s;
   SetLength(FTLVariance, FMaxVariance);
   SetLength(FBRVariance, FMaxVariance);

   s:=FPatchSize;
   p:=1; while (1 shl p)<s do Inc(p);
   currentVariance:=@FTLVariance[0];
   RecursComputeVariance(ROAMVariancePoint(0, s), ROAMVariancePoint(s, 0),
                         ROAMVariancePoint(0, 0), 1);
   ScaleVariance(1, p);
   currentVariance:=@FBRVariance[0];
   RecursComputeVariance(ROAMVariancePoint(s, 0), ROAMVariancePoint(0, s),
                         ROAMVariancePoint(s, s), 1);
   ScaleVariance(1, p);

   for s:=0 to FMaxVariance-1 do begin
   end;
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
procedure TGLROAMPatch.Tesselate;
var
   observerX, observerY : Integer;
   frameVarianceDelta, maxVariance : Integer;
   currentVariance : PIntegerArray;

   function VertexDist(x, y : Integer) : Cardinal;
   begin
      Result:=Sqr(x-observerX)+Sqr(y-observerY);
   end;

   procedure RecursTessellate(tri : PROAMTriangleNode;
                              const left, right, apex : Cardinal;
                              n : Integer);
   var
      d : Integer;
   begin
      d:=(left+right) shr 1;
      if currentVariance[n]-frameVarianceDelta>d then begin
         Split(tri);
         if Assigned(tri.leftChild) then begin
            n:=n*2;
            if n<maxVariance then begin
               RecursTessellate(tri.leftChild,   apex, left, d,   n);
               RecursTessellate(tri.rightChild, right, apex, d, 1+n);
            end;
         end;
      end;
   end;

   procedure FullTessellation(tri : PROAMTriangleNode; n : Integer);
   begin
      if Assigned(tri.leftChild) then begin
         n:=n*2;
         if n<maxVariance then begin
            Split(tri.leftChild);
            Split(tri.rightChild);
            FullTessellation(tri.leftChild,    n);
            FullTessellation(tri.rightChild, 1+n);
         end;
      end;
   end;

var
   s : Integer;
begin
   maxVariance:=FMaxVariance;

   if HighRes then begin
      if    (Assigned(FNorth) and not FNorth.HighRes)
         or (Assigned(FSouth) and not FSouth.HighRes)
         or (Assigned(FWest) and not FWest.HighRes)
         or (Assigned(FEast) and not FEast.HighRes) then begin
         Split(FTLNode);
         Split(FBRNode);
         FullTessellation(FTLNode, 1);
         FullTessellation(FBRNode, 1);
      end;
   end else begin
      if FObserverPosition[2]>0 then
         frameVarianceDelta:=1+Sqr(FObserverPosition[2] shr 8)
      else frameVarianceDelta:=1;
      observerX:=FObserverPosition[0];
      observerY:=FObserverPosition[1];
      s:=FPatchSize;
      currentVariance:=@FTLVariance[0];
      RecursTessellate(FTLNode, VertexDist(0, s), VertexDist(s, 0), VertexDist(0, 0), 1);
      currentVariance:=@FBRVariance[0];
      RecursTessellate(FBRNode, VertexDist(s, 0), VertexDist(0, s), VertexDist(s, s), 1);
   end;
end;

// Render
//
procedure TGLROAMPatch.Render(vertices : TAffineVectorList;
                              vertexIndices : TIntegerList;
                              texCoords : TTexPointList);
begin
   vertices.Count:=0;
   texCoords.Count:=0;
   vertexIndices.Count:=0;
   if HighRes then begin
      if FListHandle.Handle=0 then begin
         FListHandle.AllocateHandle;
         glNewList(FListHandle.Handle, GL_COMPILE);
         RenderAsStrips(vertices, vertexIndices, texCoords);
         vertices.Translate(VertexOffset);
         texCoords.ScaleAndTranslate(PTexPoint(@TextureScale)^,
                                     PTexPoint(@TextureOffset)^);
         glDrawElements(GL_TRIANGLE_STRIP, vertexIndices.Count,
                        GL_UNSIGNED_INT, vertexIndices.List);
         FTriangleCount:=vertexIndices.Count-2;
         glEndList;
      end;
      glCallList(FListHandle.Handle);
   end else begin
      RenderROAM(vertices, vertexIndices, texCoords);
      vertices.Translate(VertexOffset);
      texCoords.ScaleAndTranslate(PTexPoint(@TextureScale)^,
                                  PTexPoint(@TextureOffset)^);
      glDrawElements(GL_TRIANGLES, vertexIndices.Count, GL_UNSIGNED_INT, vertexIndices.List);
      FTriangleCount:=vertexIndices.Count div 3;
   end;
end;

// RenderROAM
//
procedure TGLROAMPatch.RenderROAM(vertices : TAffineVectorList;
                                  vertexIndices : TIntegerList;
                                  texCoords : TTexPointList);
var
   raster : PSmallIntRaster;

   procedure ROAMRenderPoint(var p : TROAMRenderPoint; anX, anY : Integer);
   begin
      p.X:=anX;
      p.Y:=anY;
      p.Idx:=vertices.Add(anX, anY, raster[anY][anX]);
      texCoords.Add(anX, anY);
   end;

   procedure RecursRender(var tri : PROAMTriangleNode;
                          var left, right, apex : TROAMRenderPoint);
   var
      half : TROAMRenderPoint;
   begin
      if Assigned(tri.leftChild) then begin
         with half do begin
            X:=(left.X+right.X) shr 1;
            Y:=(left.Y+right.Y) shr 1;
            Idx:=vertices.Add(X, Y, raster[Y][X]);
            texCoords.Add(X, Y);
         end;
         RecursRender(tri.leftChild , apex , left, half);
         RecursRender(tri.rightChild, right, apex, half);
      end else begin
         vertexIndices.Add(left.idx, apex.idx, right.idx);
      end;
   end;

var
   rtl, rtr, rbl, rbr : TROAMRenderPoint;
begin
   raster:=FHeightData.SmallIntRaster;

   ROAMRenderPoint(rtl, 0,          0);
   ROAMRenderPoint(rtr, FPatchSize, 0);
   ROAMRenderPoint(rbl, 0,          FPatchSize);
   ROAMRenderPoint(rbr, FPatchSize, FPatchSize);

   RecursRender(FTLNode, rbl, rtr, rtl);
   RecursRender(FBRNode, rtr, rbl, rbr);
end;

// RenderAsStrips
//
procedure TGLROAMPatch.RenderAsStrips(vertices : TAffineVectorList;
                                      vertexIndices : TIntegerList;
                                      texCoords : TTexPointList);

var
   x, y, dx, ex : Integer;
   pTop, pBottom : TAffineVector;
   bottomRow, topRow : PSmallIntArray;
   raster : PSmallIntRaster;

   procedure IssueVertex(const v : TAffineVector; x, y : Integer);
   begin
      texCoords.Add(x, y);
      vertices.Add(v);
   end;

begin
   raster:=FHeightData.SmallIntRaster;
   for y:=0 to FPatchSize-1 do begin
      pTop[1]:=y;
      topRow:=raster[y];
      pBottom[1]:=y+1;
      bottomRow:=raster[y+1];
      if (y and 1)=0 then begin
         x:=FPatchSize;
         ex:=-1;
         dx:=-1;
      end else begin
         x:=0;
         ex:=FPatchSize+1;
         dx:=1;
      end;
      // Strips direction is reversed from one strip to another,
      // this increases vertex coherency
      while x<>ex do begin
         pTop[0]:=x;
         pBottom[0]:=pTop[0];
         pBottom[2]:=bottomRow[x];
         pTop[2]:=topRow[x];
         if dx=1 then begin
            IssueVertex(pBottom, x, y+1);
            IssueVertex(pTop, x, y);
            Inc(x);
         end else begin
            IssueVertex(pTop, x, y);
            IssueVertex(pBottom, x, y+1);
            Dec(x);
         end;
      end;
   end;
   vertexIndices.AddSerie(0, 1, vertices.Count);
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
