// GLROAMPatch
{: Class for managing a ROAM (square) patch.<p>

	<b>History : </b><font size=-1><ul>
      <li>26/01/07 - LIN- Many changes from Lord CRC:

             - Introduced LerpPixel and FilteredPixel methods
               which interpolates the height data. FilteredPixel
               uses a fourth-order filter.

             - Refactored RenderHighRes, and extracted display
               list generation to a new PrepareHighRes method.
              This allows the generation of high-res tiles to
                be interleaved in the same manner as tesselation
               during rendering.

             - New property MaxVariance, which is set in ComputeVariance
               Holds the maximum variance for the tile.

             - New property HighResAvailable, true if high-res display list
               has been generated.

             - Two new constants MAX_QUALITY_VARIANCE and MED_QUALITY_VARIANCE
               control interpolation levels during high-res generation

             - RenderAsStrips method has new parameter scaleFactor which
               indicates how much the height data should be scaled when
               interpolating. Specified as a power of two, ie passing 2
                 results in 4*4 = 16 times as many data points. Default of
               zero results in old behaviour (ie no scaling).

             - Modified AllocTriangleNode and Split to raise an assertion error
               if it needs to grow the vTriangleNodes array. SetLength does not
               guarantee in-place reallocations!


      <li>25/04/04 - EG - Occlusion testing support
      <li>06/02/03 - EG - Adaptative variance computation
      <li>03/12/02 - EG - Minor ROAM tessel/render optimizations
      <li>15/06/02 - EG - Fixed patch rendering bug "introduced" by TBaseList fix
      <li>24/02/02 - EG - Hybrid ROAM-stripifier engine
      <li>10/09/01 - EG - Creation
	</ul></font>
}
unit GLROAMPatch;

interface

uses VectorTypes, VectorGeometry, GLHeightData, VectorLists, GLCrossPlatform, GLContext;

type

   // TROAMTriangleNode
   //
   PROAMTriangleNode = ^TROAMTriangleNode;
   TROAMTriangleNode = packed record
      base, left, right : PROAMTriangleNode;
      leftChild, rightChild : PROAMTriangleNode;
      magic: cardinal;
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
         FID : Integer;
         FHeightData : THeightData; // Referred, not owned
         FHeightRaster : PSmallIntRaster;
         FTLNode, FBRNode : PROAMTriangleNode;
         FTLVariance, FBRVariance : array of Cardinal;
         FMaxVariance: cardinal;
         FAvgVariance: cardinal;
         FPatchSize, FTriangleCount : Integer;
         FListHandle : TGLListHandle;
         FTag : Integer;
         FObserverPosition : TAffineVector;
         FNorth, FSouth, FWest, FEast : TGLROAMPatch; // neighbours
         FHighRes : Boolean;
         FMaxDepth : Integer;
         FVertexScale, FVertexOffset : TAffineVector;
         FTextureScale, FTextureOffset : TAffineVector;
         FMaxTLVarianceDepth, FMaxBRVarianceDepth : Integer;

         FOcclusionQuery : TGLOcclusionQueryHandle;
         FOcclusionSkip, FOcclusionCounter : Integer;
         FLastOcclusionTestPassed : Boolean;

      function GetHighResAvailable: boolean;

          {: Interpolates the height data using bilinear interpolation<p>
            raster is the original height data<p>
            x, y is the requested sample position, in 24.8 fixed point }
         function LerpPixel(raster: PSmallIntRaster; x, y: integer): single;

          {: Interpolates the height data using high quality interpolation<p>
            raster is the original height data<p>
            x, y is the requested sample position, in 24.8 fixed point }
         function FilteredPixel(raster: PSmallIntRaster; x, y: integer): single;

	   protected
	      { Protected Declarations }
         procedure SetHeightData(val : THeightData);
         procedure SetOcclusionSkip(val : Integer);

         procedure RenderROAM(vertices : TAffineVectorList;
                              vertexIndices : TIntegerList;
                              texCoords : TTexPointList);

         {: Renders the entire tile, scaled by scaleFactor.
            scaleFactor is specified as a power of 2, so
            passing 2 would result in 2^2 * 2^2 = 4*4 = 16
            times as many samples (and thus polygons). 
            Maximum factor supported is 4. Default of 0 gives
            no scaling (old behaviour). }
         procedure RenderAsStrips(vertices : TAffineVectorList;
                                  vertexIndices : TIntegerList;
                                  texCoords : TTexPointList;
                                  scaleFactor: integer = 0);
	   public
	      { Public Declarations }
	      constructor Create;
         destructor Destroy; override;

         procedure ComputeVariance(variance : Integer);

         procedure ResetTessellation;
         procedure ConnectToTheWest(westPatch : TGLROAMPatch);
         procedure ConnectToTheNorth(northPatch : TGLROAMPatch);
         procedure Tesselate;

         {: Prepares the high-resolution data. Possibly interpolates
            the height data, and creates a display list representing
            the high-resolution tile }
         procedure PrepareHighRes(vertices : TAffineVectorList;
                                  vertexIndices : TIntegerList;
                                  texCoords : TTexPointList;
                                  forceROAM: boolean);

         {: Render the patch in high-resolution.<p>
            If the high-resolution data is not prepared, it will
            call PrepareHighRes before rendering. }
         procedure RenderHighRes(vertices : TAffineVectorList;
                                 vertexIndices : TIntegerList;
                                 texCoords : TTexPointList;
                                 forceROAM : Boolean);
         {: Render the patch by accumulating triangles.<p>
            The lists are assumed to have enough capacity to allow AddNC calls
            (additions without capacity check).<br>
            Once at least autoFlushVertexCount vertices have been accumulated,
            perform a FlushAccum }
         procedure RenderAccum(vertices : TAffineVectorList;
                               vertexIndices : TIntegerList;
                               texCoords : TTexPointList;
                               autoFlushVertexCount : Integer);
         {: Render all vertices accumulated in the arrays and set their count
            back to zero. }
         class procedure FlushAccum(vertices : TAffineVectorList;
                                    vertexIndices : TIntegerList;
                                    texCoords : TTexPointList);

         property HeightData : THeightData read FHeightData write SetHeightData;
         property VertexScale : TAffineVector read FVertexScale write FVertexScale;
         property VertexOffset : TAffineVector read FVertexOffset write FVertexOffset;

         property ObserverPosition : TAffineVector read FObserverPosition write FObserverPosition;

         property TextureScale : TAffineVector read FTextureScale write FTextureScale;
         property TextureOffset : TAffineVector read FTextureOffset write FTextureOffset;

         property HighRes : Boolean read FHighRes write FHighRes;

         {: Number of frames to skip after an occlusion test returned zero pixels. }
         property OcclusionSkip : Integer read FOcclusionSkip write SetOcclusionSkip;
         {: Number of frames remaining to next occlusion test. }
         property OcclusionCounter : Integer read FOcclusionCounter write FOcclusionCounter;
         {: Result for the last occlusion test.<p>
            Note that this value is updated upon rendering the tile in
            non-high-res mode only. }
         property LastOcclusionTestPassed : Boolean read FLastOcclusionTestPassed;

         property MaxVariance : Cardinal read FMaxVariance;

         property HighResAvailable: boolean read GetHighResAvailable;

         property ID : Integer read FID;
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

uses OpenGL1x, XOpenGL, SysUtils, Math;

const
  // Sets minimum variance for max quality interpolation
  MAX_QUALITY_VARIANCE = 60000;
  // Sets minimum variance for medium quality interpolation
  MED_QUALITY_VARIANCE = 30000;

var
   FVBOVertHandle, FVBOTexHandle : TGLVBOArrayBufferHandle;
   FVBOIndicesHandle : TGLVBOElementArrayHandle;

type

   // TROAMVariancePoint
   //
   TROAMVariancePoint = packed record
      X, Y : Integer;
      Z : Integer;
   end;

const
  MAGIC_WORD = $1F2E3D4C;
var
   vNextPatchID : Integer;
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
var
   nilNode : PROAMTriangleNode;
begin
   if vNbTris>=vTriangleNodesCapacity then begin
      // grow by 50%
      Assert(false, 'ROAM: too many triangles');
      vTriangleNodesCapacity:=vTriangleNodesCapacity+(vTriangleNodesCapacity shr 1);
      SetLength(vTriangleNodes, vTriangleNodesCapacity);
   end;
   Result:=@vTriangleNodes[vNbTris];
   with Result^ do begin
      nilNode:=nil;
      leftChild:=nilNode;
      rightChild:=nilNode;
      magic:= MAGIC_WORD;
   end;
   Inc(vNbTris);
end;

// Split
//
function Split(tri : PROAMTriangleNode) : Boolean;
var
   buf : PROAMTriangleNode;
   n : Integer;
   t : PROAMTriangleNode;
begin
   with tri^ do if not Assigned(leftChild) then begin
   	// If this triangle is not in a proper diamond, force split our base neighbor
	   if Assigned(base) and (base.base<>tri) then
         Split(base);

      n:=vNbTris;
      if n>=vTriangleNodesCapacity then begin
         // grow by 50%
         Assert(false, 'ROAM: too many triangles');
         vTriangleNodesCapacity:=vTriangleNodesCapacity+(vTriangleNodesCapacity shr 1);
         SetLength(vTriangleNodes, vTriangleNodesCapacity);
      end;

	    // Create children and cross-link them
      t:=@vTriangleNodes[n];
      leftChild:= t;
      Inc(t);
      rightChild:= t;

      rightChild^.base:=tri.right;
      rightChild^.leftChild:=nil;
      t:= tri.leftChild;
      rightChild^.rightChild:=t;
      rightChild^.right:=t;
      rightChild^.magic:= MAGIC_WORD;

      leftChild^.base:=tri.left;
      leftChild^.leftChild:=nil;
      leftChild^.rightChild:=tri.leftChild;
      leftChild^.left:=tri.rightChild;
      leftChild^.magic:= MAGIC_WORD;

      Inc(vNbTris, 2);

	   // Link our Left Neighbor to the new children
	   if Assigned(left) then begin
         t:=leftChild;
         if left.base=tri then
            left.base:=t
         else if left.left=tri then
            left.left:=t
         else left.right:=t;
      end;

	   // Link our Right Neighbor to the new children
	   if Assigned(right) then begin
         t:=rightChild;
         if right.base=tri then
            right.base:=t
         else if right.left=tri then
            right.left:=t
         else right.right:=t;
      end;

      // Link our Base Neighbor to the new children
      if Assigned(base) then begin
         if Assigned(base.leftChild) then begin
            // base.leftChild.right:=rightChild
            // rightChild.left:=base.leftChild
            t:=base.leftChild;
            buf:=rightChild;
            t.right:=buf;
            buf.left:=t;
            // base.rightChild.left:=leftChild
            // leftChild.right:=base.rightChild
            t:=base.rightChild;
            buf:=leftChild;
            t.left:=buf;
            buf.right:=t;
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
   FID:=vNextPatchID;
   Inc(vNextPatchID);
   FListHandle:=TGLListHandle.Create;
   FOcclusionQuery:=TGLOcclusionQueryHandle.Create;
end;

// Destroy
//
destructor TGLROAMPatch.Destroy;
begin
   FListHandle.Free;
   FOcclusionQuery.Free;
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

// SetOcclusionSkip
//
procedure TGLROAMPatch.SetOcclusionSkip(val : Integer);
begin
   if val<0 then val:=0;
   if FOcclusionSkip<>val then begin
      FOcclusionSkip:=val;
      FOcclusionQuery.DestroyHandle;
   end;
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
   invVariance : Single;

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
         Result:=ScaleAndRound(Abs(((left.Z+right.Z) div 2)-Z), invVariance);
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

  procedure ComputeMaxAndAvgVariance;
  var
    i: integer;
    tot, c: cardinal;
  begin
    tot:= 0;
    c:= 0;
    FMaxVariance:= 0;
    for i := 0 to high(FTLVariance) do
    begin
      if FTLVariance[i] > 0 then
      begin
        if FTLVariance[i] > FMaxVariance then
          FMaxVariance:= FTLVariance[i];
        tot:= tot + FTLVariance[i];
        c:= c + 1;
      end;
    end;

    for i := 0 to high(FBRVariance) do
    begin
      if FBRVariance[i] > 0 then
      begin
        if FBRVariance[i] > FMaxVariance then
          FMaxVariance:= FBRVariance[i];
        tot:= tot + FBRVariance[i];
        c:= c + 1;
      end;
    end;

    if c > 0 then
      tot:= tot div c;

    FAvgVariance:= tot;
  end;

var
   s, p : Integer;
begin
   invVariance:= 1/variance;
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

   ComputeMaxAndAvgVariance;
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
   tessMaxVariance : Cardinal;
   tessMaxDepth : Cardinal;
   tessCurrentVariance : PIntegerArray;
   tessObserverPosX, tessObserverPosY : Integer;
   tessObserverDirZ: single;

procedure RecursTessellate(tri : PROAMTriangleNode;
                           n : Cardinal;
                           const left, right, apex : Cardinal);
var
   d : Integer;
begin
   d:=((left+right) shr 1);
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
var
   tessFrameVarianceDelta : Integer;

   function VertexDist(x, y : Integer) : Cardinal;
   var
      f : Single;
   const
      c1Div100 : Single = 0.01;
   begin
      if HighRes then
         f:=0.2*Sqr(FPatchSize)
      else f:=Sqr(x-tessObserverPosX)+Sqr(y-tessObserverPosY)+tessFrameVarianceDelta;
      //f:= f * tessObserverDirZ;
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
   tessMaxDepth:=FMaxDepth;
   tessObserverPosX:=Round(FObserverPosition[0]);
   tessObserverPosY:=Round(FObserverPosition[1]);
   tessObserverDirZ:= VectorNormalize(FObserverPosition)[2];

   if HighRes then begin
      FullRightTess(FTLNode, 1);
      FullRightTess(FBRNode, 1);
      FullLeftTess(FBRNode, 1);
      FullLeftTess(FTLNode, 1);
      tessFrameVarianceDelta:=0;
   end else begin
      if Assigned(FNorth) and FNorth.HighRes then
         FullRightTess(FTLNode, 1);
      if Assigned(FSouth) and FSouth.HighRes then
         FullRightTess(FBRNode, 1);
      if Assigned(FEast) and FEast.HighRes then
         FullLeftTess(FBRNode, 1);
      if Assigned(FWest) and FWest.HighRes then
         FullLeftTess(FTLNode, 1);
      if FObserverPosition[2]>0 then
         tessFrameVarianceDelta:=Round(Sqr(FObserverPosition[2]*(1/16)))
      else tessFrameVarianceDelta:=0;
   end;
   s:=FPatchSize;
   tessCurrentVariance:=@FTLVariance[0];
   tessMaxVariance:=FMaxTLVarianceDepth;
   RecursTessellate(FTLNode, 1, VertexDist(0, s), VertexDist(s, 0), VertexDist(0, 0));
   tessCurrentVariance:=@FBRVariance[0];
   tessMaxVariance:=FMaxBRVarianceDepth;
   RecursTessellate(FBRNode, 1, VertexDist(s, 0), VertexDist(0, s), VertexDist(s, s));
end;

// RenderHighRes
//
procedure TGLROAMPatch.RenderHighRes(vertices : TAffineVectorList;
                                     vertexIndices : TIntegerList;
                                     texCoords : TTexPointList;
                                     forceROAM : Boolean);
begin
   // Prepare display list if needed
   PrepareHighRes(vertices, vertexIndices, texCoords, forceROAM);
   // perform the render
   glCallList(FListHandle.Handle);
end;

// RenderAccum
//
procedure TGLROAMPatch.RenderAccum(vertices : TAffineVectorList;
                                   vertexIndices : TIntegerList;
                                   texCoords : TTexPointList;
                                   autoFlushVertexCount : Integer);
var
   occlusionPassed : Boolean;
   n, nb, nvi : Integer;
begin
   // CLOD tiles are rendered via ROAM
   if (FOcclusionSkip>0) and GL_NV_occlusion_query then begin
      if FOcclusionQuery.Handle=0 then begin
         FOcclusionQuery.AllocateHandle;
         FOcclusionCounter:=-(ID mod (FOcclusionSkip));
      end;
      occlusionPassed:=(FOcclusionCounter<=0) or (FOcclusionQuery.PixelCount>0);
      Dec(FOcclusionCounter);
      if occlusionPassed then begin
         if FOcclusionCounter<=0 then
            Inc(FOcclusionCounter, FOcclusionSkip);
         FOcclusionQuery.BeginOcclusionQuery;
      end;
   end else occlusionPassed:=True;
   FLastOcclusionTestPassed:=occlusionPassed;
   if occlusionPassed then begin
      nvi:=vertexIndices.Count;
      n:=vertices.Count;
      RenderROAM(vertices, vertexIndices, texCoords);
      nb:=vertices.Count-n;
      FTriangleCount:=(vertexIndices.Count-nvi) div 3;

      vertices.Translate(VertexOffset, n, nb);
      texCoords.ScaleAndTranslate(PTexPoint(@TextureScale)^,
                                  PTexPoint(@TextureOffset)^, n, nb);

      if FOcclusionQuery.Active then begin
         FlushAccum(vertices, vertexIndices, texCoords);
         FOcclusionQuery.EndOcclusionQuery;
      end else if vertexIndices.Count>autoFlushVertexCount then
         FlushAccum(vertices, vertexIndices, texCoords);
   end else FTriangleCount:=0;
end;

// FlushAccum
//
class procedure TGLROAMPatch.FlushAccum(vertices : TAffineVectorList;
                                        vertexIndices : TIntegerList;
                                        texCoords : TTexPointList);
begin
   if vertexIndices.Count=0 then Exit;

//   if GL_ARB_vertex_buffer_object then begin
   if False then begin // VBO currently off (slower)
      if FVBOVertHandle.Handle=0 then
         FVBOVertHandle.AllocateHandle;
      FVBOVertHandle.BindBufferData(vertices.List, vertices.DataSize, GL_STREAM_DRAW_ARB);
      glVertexPointer(3, GL_FLOAT, 0, nil);

      if FVBOTexHandle.Handle=0 then
         FVBOTexHandle.AllocateHandle;
      FVBOTexHandle.BindBufferData(texCoords.List, texCoords.DataSize, GL_STREAM_DRAW_ARB);
      xglTexCoordPointer(2, GL_FLOAT, 0, nil);

//      if FVBOIndicesHandle.Handle=0 then
//         FVBOIndicesHandle.AllocateHandle;
//      FVBOIndicesHandle.BindBufferData(vertexIndices.List, vertexIndices.DataSize, GL_STREAM_DRAW_ARB);

      glDrawRangeElements(GL_TRIANGLES, 0, vertices.Count-1, vertexIndices.Count,
                          GL_UNSIGNED_INT, vertexIndices.List);
//      glDrawRangeElements(GL_TRIANGLES, 0, vertices.Count-1, vertexIndices.Count,
//                          GL_UNSIGNED_INT, nil);

      glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0);
      glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, 0);
   end else if GL_EXT_compiled_vertex_array then begin
      glLockArraysEXT(0, vertices.Count);
      glDrawRangeElements(GL_TRIANGLES, 0, vertices.Count-1, vertexIndices.Count,
                          GL_UNSIGNED_INT, vertexIndices.List);
      glUnLockArraysEXT;
   end else begin
      glDrawElements(GL_TRIANGLES, vertexIndices.Count, GL_UNSIGNED_INT, vertexIndices.List);
   end;
   vertices.Count:=0;
   texCoords.Count:=0;
   vertexIndices.Count:=0;
end;

function TGLROAMPatch.GetHighResAvailable: boolean;
begin
  result:= FListHandle.Handle <> 0;
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
   if Assigned(tri.leftChild) then begin  // = if node is split
      half.Y:=(left.Y+right.Y) shr 1;
      half.X:=(left.X+right.X) shr 1;
      renderTexCoords.AddNC(@half.X);
      half.Idx:=renderVertices.AddNC(@half.X, renderRaster[half.Y][half.X]);
      RecursRender(tri.leftChild , apex , left, half);
      RecursRender(tri.rightChild, right, apex, half);
   end else begin
      localIndices:=renderIndices;
      localIndices[0]:=left.Idx;
      localIndices[1]:=apex.Idx;
      localIndices[2]:=right.Idx;
      renderIndices:=PIntegerArray(@localIndices[3]);
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
   renderVertices:=vertices;
   renderTexCoords:=texCoords;
   vertexIndices.AdjustCapacityToAtLeast(Sqr(FPatchSize)*6+15000);
   // this is required, the actual item count is maintained out of the list scope
   vertexIndices.SetCountResetsMemory:=False;
   renderIndices:=@vertexIndices.List[vertexIndices.Count];

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
function TGLROAMPatch.LerpPixel(raster: PSmallIntRaster; x, y: integer): single;
var
  x0, x1, y0: integer;
  line0, line1: PSmallIntArray;
  v0, v1: integer;
begin
  x0:= x shr 8;
  x1:= min(x0 + 1, FPatchSize);
  y0:= y shr 8;

  x:= x and 255;
  y:= y and 255;

  line0:= raster[y0];
  line1:= raster[min(y0+1, FPatchSize)];
  v0:= (line0[x0] shl 8 + (line0[x1] - line0[x0]) * x) div 256;
  v1:= (line1[x0] shl 8 + (line1[x1] - line1[x0]) * x) div 256;

  result:= (v0 shl 8 + (v1 - v0) * y) * (1 / 256);
end;

procedure TGLROAMPatch.PrepareHighRes(vertices : TAffineVectorList;
                                     vertexIndices : TIntegerList;
                                     texCoords : TTexPointList;
                                     forceROAM: boolean);
var
  primitive : TGLEnum;
  verts : TAffineVectorList;
  vertIndices : TIntegerList;
  tCoords : TTexPointList;
  scaleFactor: integer;
begin
  if FListHandle.Handle <> 0 then
    exit;

  verts:= TAffineVectorList.Create;
  tCoords:= TTexPointList.Create;
  vertIndices:= TIntegerList.Create;

  // either use brute-force strips or a high-res static tesselation
  if (forceROAM) then begin
     ResetTessellation;
     Tesselate;
     RenderROAM(verts, vertIndices, tCoords);
     primitive:=GL_TRIANGLES;
     FTriangleCount:= vertIndices.Count div 3;
  end else begin
    if FMaxVariance > MAX_QUALITY_VARIANCE then
      scaleFactor:= 2
    else if FMaxVariance > MED_QUALITY_VARIANCE then
      scaleFactor:= 1
    else
      scaleFactor:= 0;

    RenderAsStrips(verts, vertIndices, tCoords, scaleFactor);

     primitive:=GL_TRIANGLE_STRIP;
     FTriangleCount:= vertIndices.Count-2*FPatchSize;
  end;

  verts.Translate(VertexOffset);
  tCoords.ScaleAndTranslate(PTexPoint(@TextureScale)^,
                              PTexPoint(@TextureOffset)^);

  glVertexPointer(3, GL_FLOAT, 0, verts.List);
  xglTexCoordPointer(2, GL_FLOAT, 0, tCoords.List);

  FListHandle.AllocateHandle;
  glNewList(FListHandle.Handle, GL_COMPILE);
  glDrawElements(primitive, vertIndices.Count,
                 GL_UNSIGNED_INT, vertIndices.List);
  glEndList;

  verts.Free;
  tCoords.Free;
  vertIndices.Free;

  // Reset vertex and texcoord pointers
  glVertexPointer(3, GL_FLOAT, 0, vertices.List);
  xglTexCoordPointer(2, GL_FLOAT, 0, texCoords.List);
end;

function TGLROAMPatch.FilteredPixel(raster: PSmallIntRaster; x, y: integer): single;
const
  // Array of weights based on a fourth-order filter
  Weights: array[0..15, 0..5] of single = (
    (0.0, 0.0, 1.0, 0.0, 0.0, 0.0), // .0
    (0.0045776367188, -0.036926269531, 0.9912109375, 0.0478515625, -0.0070190429688, 0.00030517578125),
    (0.0079752607271, -0.06494140625, 0.96614581347, 0.10677083582, -0.01708984375, 0.0011393228779),
    (0.010314941406, -0.084899902344, 0.9267578125, 0.1748046875, -0.029357910156, 0.0023803710938),
    (0.01171875, -0.09765625, 0.875, 0.25, -0.04296875, 0.00390625),
    (0.012308756821, -0.10406494141, 0.81282550097, 0.33040365577, -0.057067871094, 0.0055948891677),
    (0.01220703125, -0.10498046875, 0.7421875, 0.4140625, -0.07080078125, 0.00732421875),
    (0.011535644531, -0.10125732422, 0.6650390625, 0.4990234375, -0.083312988281, 0.0089721679688),
    (0.010416666977, -0.09375, 0.58333331347, 0.58333331347, -0.09375, 0.010416666977), // .5
    (0.0089721679688, -0.083312988281, 0.4990234375, 0.6650390625, -0.10125732422, 0.011535644531),
    (0.00732421875, -0.07080078125, 0.4140625, 0.7421875, -0.10498046875, 0.01220703125),
    (0.0055948891677, -0.057067871094, 0.33040365577, 0.81282550097, -0.10406494141, 0.012308756821),
    (0.00390625, -0.04296875, 0.25, 0.875, -0.09765625, 0.01171875),
    (0.0023803710938, -0.029357910156, 0.1748046875, 0.9267578125, -0.084899902344, 0.010314941406),
    (0.0011393228779, -0.01708984375, 0.10677083582, 0.96614581347, -0.06494140625, 0.0079752607271),
    (0.00030517578125, -0.0070190429688, 0.0478515625, 0.9912109375, -0.036926269531, 0.0045776367188));

  function FindWeights(v: integer): integer;
  begin
    // filter weights are in increments of 16
    result:= v shr 4;
  end;

var
  x_2, x_1, x0, x1, x2, x3, j: integer;
  yp: array[0..5] of integer;
  line: PSmallIntArray;
  wix, wiy: integer;
  wy: single;
  tx, ty: integer;
begin
  x0:= x shr 8;
  x_2:= max(x0 - 2, 0);
  x_1:= max(x0 - 1, 0);
  x1:= min(x0 + 1, FPatchSize);
  x2:= min(x0 + 2, FPatchSize);
  x3:= min(x0 + 3, FPatchSize);

  yp[2]:= y shr 8;
  yp[0]:= max(yp[2] - 2, 0);
  yp[1]:= max(yp[2] - 1, 0);
  yp[3]:= min(yp[2] + 1, FPatchSize);
  yp[4]:= min(yp[2] + 2, FPatchSize);
  yp[5]:= min(yp[2] + 3, FPatchSize);

  x:= x and 255;
  y:= y and 255;

  // find which weights to use depending
  // on the fractional parts of x and y
  wix:= FindWeights(x);
  wiy:= FindWeights(y);

  result:= 0;

  for j := 0 to 5 do
  begin
    line:= raster[yp[j]];
    result:= result +
      Weights[wiy, j] * (
      Weights[wix, 0] * line[x_2] +
      Weights[wix, 1] * line[x_1] +
      Weights[wix, 2] * line[x0] +
      Weights[wix, 3] * line[x1] +
      Weights[wix, 4] * line[x2] +
      Weights[wix, 5] * line[x3]);
  end;
end;

procedure TGLROAMPatch.RenderAsStrips(vertices: TAffineVectorList; vertexIndices: TIntegerList;
  texCoords: TTexPointList; scaleFactor: integer);
var
   invf: integer;
   sf: single;
   x, y, baseTop, rowLength : Integer;
   PSize: integer;
   p : TAffineVector;
   raster : PSmallIntRaster;
   row: PSmallIntArray;
   tex : TTexPoint;
   verticesList : PAffineVector;
   texCoordsList : PTexPoint;
   indicesList : PInteger;
  ax, ay: integer;
begin
  Assert((scaleFactor >= 0) and (scaleFactor <= 4), 'FilteredPixel only supports scaleFactors of 1 and 2');

  invf:= 256 div (1 shl scaleFactor);
  sf:= 1 / (1 shl scaleFactor);

  PSize:= FPatchSize shl scaleFactor;

   raster:=FHeightData.SmallIntRaster;
   rowLength:=PSize+1;
   // prepare vertex data
   vertices.Count:=Sqr(rowLength);
   verticesList:=PAffineVector(vertices.List);
   texCoords.Count:=Sqr(rowLength);
   texCoordsList:=PTexPoint(texCoords.List);

  if scaleFactor = 0 then
  begin
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
  end
  else
  begin
    for y:= 0 to PSize do
    begin
      p[1]:=y*sf;
      tex.T:=p[1];

      ay:= y * invf;

      for x:= 0 to PSize do
      begin
        p[0]:=x*sf;
        tex.S:=p[0];

        ax:= x * invf;

        // use linear interpolation along edges
        // to avoid having to hassle with stiching up
        // the seams
        if (y = 0) or (y = PSize) or (x = 0) or (x = PSize) then
          p[2]:= LerpPixel(raster, ax, ay)
        else
          p[2]:= FilteredPixel(raster, ax, ay);

        verticesList^:=p;
        Inc(verticesList);
        texCoordsList^:=tex;
        Inc(texCoordsList);
      end;
    end;
  end;

   // build indices list
   baseTop:=0;
   vertexIndices.Count:=(rowLength*2+2)*PSize-1;
   indicesList:=PInteger(vertexIndices.List);
   y:=0; while y<PSize do begin
      if y>0 then begin
         indicesList^:=baseTop+PSize;
         Inc(indicesList);
      end;
      for x:=baseTop+PSize downto baseTop do begin
         indicesList^:=x;
         PIntegerArray(indicesList)[1]:=rowLength+x;
         Inc(indicesList, 2);
      end;
      indicesList^:=baseTop+rowLength;
      Inc(baseTop, rowLength);
      PIntegerArray(indicesList)[1]:=baseTop+rowLength;
      Inc(indicesList, 2);
      for x:=baseTop to baseTop+PSize do begin
         indicesList^:=rowLength+x;
         PIntegerArray(indicesList)[1]:=x;
         Inc(indicesList, 2);
      end;
      indicesList^:=baseTop+PSize;
      Inc(indicesList);
      Inc(baseTop, rowLength);
      Inc(y, 2);
   end;
   vertexIndices.Count:=vertexIndices.Count-1;
end;
{
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
}
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   FVBOVertHandle:=TGLVBOArrayBufferHandle.Create;
   FVBOTexHandle:=TGLVBOArrayBufferHandle.Create;
   FVBOIndicesHandle:=TGLVBOElementArrayHandle.Create;

finalization

   FVBOVertHandle.Free;
   FVBOTexHandle.Free;
   FVBOIndicesHandle.Free;

   SetROAMTrianglesCapacity(0);

end.
