// GLROAMPatch
{: Class for managing a ROAM (square) patch.<p>

   *** UNDER CONSTRUCTION ***

	<b>History : </b><font size=-1><ul>
      <li>10/09/01 - EG - Creation
	</ul></font>
}
unit GLROAMPatch;

interface

uses Geometry, GLHeightData, VectorLists, GLCrossPlatform;

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
         FCurrentVariance : PCardinalArray;
         FTerrainSize : Integer;
         FFrameVariance : Cardinal;
         FTargetVertices : TAffineVectorList;
         FTargetVertexIndices : TIntegerList;
         FTargetTexPoints : TTexPointList;
         FVertexScale, FVertexOffset : TAffineVector;
         FTextureScale, FTextureOffset : TAffineVector;
         FObserverPosition : TAffineIntVector;

	   protected
	      { Protected Declarations }
         procedure SetHeightData(val : THeightData);

         function ROAMVariancePoint(anX, anY : Integer) : TROAMVariancePoint;
         function RecursComputeVariance(const left, right, apex : TROAMVariancePoint;
                                        node : Integer) : Word;
         procedure RecursTessellate(tri : PROAMTriangleNode;
                                    const left, right, apex : TGLPoint;
                                    n : Integer);
         procedure RecursRender(tri : PROAMTriangleNode;
                                var left, right, apex : TROAMRenderPoint);
         procedure PrepareVertex(var p : TROAMRenderPoint);

	   public
	      { Public Declarations }
	      constructor Create;
         destructor Destroy; override;

         procedure ComputeVariance(depth : Integer);

         procedure ResetTessellation;
         procedure ConnectToTheLeft(leftPatch : TGLROAMPatch);
         procedure ConnectToTheTop(topPatch : TGLROAMPatch);
         procedure Tesselate(variance : Integer);
         
         procedure Render(vertices : TAffineVectorList;
                          vertexIndices : TIntegerList;
                          texCoords : TTexPointList);

         property HeightData : THeightData read FHeightData write SetHeightData;
         property VertexScale : TAffineVector read FVertexScale write FVertexScale;
         property VertexOffset : TAffineVector read FVertexOffset write FVertexOffset;

         property ObserverPosition : TAffineIntVector read FObserverPosition write FObserverPosition;

         property TextureScale : TAffineVector read FTextureScale write FTextureScale;
         property TextureOffset : TAffineVector read FTextureOffset write FTextureOffset;
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

var
   vNbTris : Integer;
   vTriangleNodes : array of TROAMTriangleNode;

// SetROAMTrianglesCapacity
//
procedure SetROAMTrianglesCapacity(nb : Integer);
begin
   vNbTris:=0;
   SetLength(vTriangleNodes, nb);
end;

// AllocTriangleNode
//
function AllocTriangleNode : PROAMTriangleNode;
begin
   if vNbTris<High(vTriangleNodes) then begin
      Result:=@vTriangleNodes[vNbTris];
      Result.leftChild:=nil;
      Result.rightChild:=nil;
      Inc(vNbTris);
   end else Result:=nil;
end;

// AllocTrianglesNodes
//
procedure AllocTrianglesNodes(var n1, n2 : PROAMTriangleNode);
begin
   if vNbTris<High(vTriangleNodes) then begin
      n1:=@vTriangleNodes[vNbTris];
      n2:=PROAMTriangleNode(Integer(n1)+SizeOf(TROAMTriangleNode));
      with n1^ do begin
         leftChild:=nil;
         rightChild:=nil;
         left:=n2;
      end;
      with n2^ do begin
         leftChild:=nil;
         rightChild:=nil;
         right:=n1;
      end;
      Inc(vNbTris, 2);
   end else n1:=nil;
end;

// Split
//
procedure Split(tri : PROAMTriangleNode);
begin
   with tri^ do begin
      if Assigned(leftChild) then Exit;

   	// If this triangle is not in a proper diamond, force split our base neighbor
	   if Assigned(base) and (base.base<>tri) then
         Split(base);

	   // Create children and cross-link them
      AllocTrianglesNodes(leftChild, rightChild);
      if leftChild=nil then Exit;

	   // Connect children to sides
	   leftChild.base:=left;
	   rightChild.base:=right;

	   // Link our Left Neighbor to the new children
	   if Assigned(left) then begin
         if left.base=tri then
            left.base:=leftChild
         else if left.left=tri then
            left.left:=leftChild
         else if left.right=tri then
            left.right:=leftChild
         else Assert(False);
      end;

	   // Link our Left Neighbor to the new children
	   if Assigned(right) then begin
         if right.base=tri then
            right.base:=rightChild
         else if right.left=tri then
            right.left:=rightChild
         else if right.right=tri then
            right.right:=rightChild
         else Assert(False);
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
		   leftChild.right:=nil;
		   rightChild.left:=nil;
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
end;

// Destroy
//
destructor TGLROAMPatch.Destroy;
begin
	inherited Destroy;
end;

// SetHeightData
//
procedure TGLROAMPatch.SetHeightData(val : THeightData);
begin
   FHeightData:=val;
   FTerrainSize:=FHeightData.Size-1;
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

// ComputeVariance
//
procedure TGLROAMPatch.ComputeVariance(depth : Integer);
var
   s : Integer;
begin
   s:=FTerrainSize*FTerrainSize;
   FVarianceDepth:=depth+1;
   repeat
      Dec(FVarianceDepth);
      FMaxVariance:=1 shl FVarianceDepth;
   until FMaxVariance<s;
   SetLength(FTLVariance, FMaxVariance);
   SetLength(FBRVariance, FMaxVariance);

   s:=FTerrainSize;
   FCurrentVariance:=@FTLVariance[0];
   RecursComputeVariance(ROAMVariancePoint(0, s), ROAMVariancePoint(s, 0),
                         ROAMVariancePoint(0, 0), 1);
   FCurrentVariance:=@FBRVariance[0];
   RecursComputeVariance(ROAMVariancePoint(s, 0), ROAMVariancePoint(0, s),
                         ROAMVariancePoint(s, s), 1);
end;

// RecursComputeVariance
//
function TGLROAMPatch.RecursComputeVariance(const left, right, apex : TROAMVariancePoint;
                                            node : Integer) : Word;
var
   half : TROAMVariancePoint;
   v : Word;
   h, hr : SmallInt;
begin
	with half do begin
      X:=(left.X+right.X) shr 1;
	   Y:=(left.Y+right.Y) shr 1;
      hr:=(left.Z+right.Z) shr 1;
      h:=FHeightRaster[Y][X];
      Z:=h;
      Result:=Abs(hr-h);
   end;

	if node<FMaxVariance then begin
      v:=RecursComputeVariance( apex, left, half,    node*2 );
      if v>Result then Result:=v;
      v:=RecursComputeVariance(right, apex, half, 1+(node*2));
      if v>Result then Result:=v;
     	FCurrentVariance[node]:=Result*Result;
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
end;

// ConnectToTheLeft
//
procedure TGLROAMPatch.ConnectToTheLeft(leftPatch : TGLROAMPatch);
begin
   FTLNode.left:=leftPatch.FBRNode;
   leftPatch.FBRNode.left:=FTLNode;
end;

// ConnectToTheTop
//
procedure TGLROAMPatch.ConnectToTheTop(topPatch : TGLROAMPatch);
begin
   FTLNode.right:=topPatch.FBRNode;
   topPatch.FBRNode.right:=FTLNode;
end;

// Tesselate
//
procedure TGLROAMPatch.Tesselate(variance : Integer);
var
   s : Integer;
begin
   FFrameVariance:=variance;
   s:=FTerrainSize;
   FCurrentVariance:=@FTLVariance[0];
   RecursTessellate(FTLNode, GLPoint(0, s), GLPoint(s, 0), GLPoint(0, 0), 1);
   FCurrentVariance:=@FBRVariance[0];
   RecursTessellate(FBRNode, GLPoint(s, 0), GLPoint(0, s), GLPoint(s, s), 1);
end;

// RecursTessellate
//
procedure TGLROAMPatch.RecursTessellate(tri : PROAMTriangleNode;
                                        const left, right, apex : TGLPoint;
                                        n : Integer);
var
   half : TGLPoint;
   d, v : Cardinal;
   halfZ : SmallInt;

   function GetVariance(n : Integer) : Integer;
   var
      k : Integer;
   begin
      k:=0;
      while n>=FMaxVariance do begin
         n:=n shr 1;
         Inc(k);
      end;
      Result:=(FCurrentVariance[n] shr k);
   end;

begin
   half.X:=(left.X+right.X) shr 1;
   half.Y:=(left.Y+right.Y) shr 1;
   halfZ:=FHeightRaster[half.Y][half.X];

   d:=1+( Abs(half.x-FObserverPosition[0])*Abs(half.x-FObserverPosition[0])
         +Abs(half.y-FObserverPosition[1])*Abs(half.y-FObserverPosition[1])
         +(Abs(halfZ-FObserverPosition[2]) shr 9));
   v:=GetVariance(n);
   if v>d*FFrameVariance then begin
      if (Abs(left.X-right.X)>=2) or (Abs(left.Y-right.Y)>=2) then begin
         Split(tri);
         if Assigned(tri.leftChild) then begin
            RecursTessellate(tri.leftChild,   apex, left, half,   n*2);
            RecursTessellate(tri.rightChild, right, apex, half, 1+n*2);
         end;
      end;
   end;
end;

// Render
//
procedure TGLROAMPatch.Render(vertices : TAffineVectorList;
                              vertexIndices : TIntegerList;
                              texCoords : TTexPointList);

   procedure ROAMRenderPoint(var p : TROAMRenderPoint; anX, anY : Integer);
   begin
      p.X:=anX;
      p.Y:=anY;
      PrepareVertex(p);
   end;

var
   rtl, rtr, rbl, rbr : TROAMRenderPoint;
begin
   FTargetVertices:=vertices;
   FTargetVertexIndices:=vertexIndices;
   FTargetTexPoints:=texCoords;

   ROAMRenderPoint(rtl, 0, 0);
   ROAMRenderPoint(rtr, FTerrainSize, 0);
   ROAMRenderPoint(rbl, 0, FTerrainSize);
   ROAMRenderPoint(rbr, FTerrainSize, FTerrainSize);

   RecursRender(FTLNode, rbl, rtr, rtl);
   RecursRender(FBRNode, rtr, rbl, rbr);
end;

// RecursRender
//
procedure TGLROAMPatch.RecursRender(tri : PROAMTriangleNode;
                                    var left, right, apex : TROAMRenderPoint);
var
   half : TROAMRenderPoint;
begin
   if Assigned(tri.leftChild) then begin
      half.X:=(left.X+right.X) shr 1;
      half.Y:=(left.Y+right.Y) shr 1;
      PrepareVertex(half);
      RecursRender(tri.leftChild , apex , left, half);
      RecursRender(tri.rightChild, right, apex, half);
   end else begin
      FTargetVertexIndices.Add(left.idx, apex.idx, right.idx);
   end;
end;

// PrepareVertex
//
procedure TGLROAMPatch.PrepareVertex(var p : TROAMRenderPoint);
begin
   with p do begin
      Idx:=FTargetVertices.Add(x, y, FHeightData.SmallIntRaster[y][x]);
      FTargetTexPoints.Add(x*FTextureScale[0]+FTextureOffset[0],
                           FTextureOffset[1]-y*FTextureScale[1]);
   end;
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
